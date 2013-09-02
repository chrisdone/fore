{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Haskell to JavaScript compiler.

module Fore where

import           Control.Applicative
import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans
import           CoreSyn
import           Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as L
import           Data.Char
import           Data.Data hiding (tyConName)
import           Data.Generics.Aliases
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import           Data.Time
import           DataCon
import           Debug.Trace
import           DynFlags
import           Encoding
import           FastString
import           GHC
import           GHC.Paths (libdir)
import           GHC.Real
import           HscTypes
import qualified Language.ECMAScript3.Parser as ECMA
import qualified Language.ECMAScript3.PrettyPrint as ECMA
import qualified Language.ECMAScript3.Syntax as ECMA
import           Literal
import           Module
import           Name hiding (varName)
import           OccName hiding (varName)
import           Outputable hiding ((<>))
import           Prelude hiding (exp)
import qualified SourceMap as SourceMap
import           SourceMap.Types
import           System.Exit
import           System.IO
import           System.Process.Text.Lazy
import qualified Text.PrettyPrint as Doc
import           TyCon
import           TypeRep
import           Var

--------------------------------------------------------------------------------
-- Top-level compilers

getCore :: FilePath -> IO CoreModule
getCore = getCoreAst

getJs :: CoreModule -> IO [Statement]
getJs = runCompile . compileModule

getTextMappings :: Text -> [Statement] -> IO (Text,[Mapping])
getTextMappings name stmts = do
  forejs <- getForeJs name
  let (text,mappings) = renderStatements (length (T.lines forejs) + 1) stmts
  return (forejs <> text <> run,mappings)

getCode :: [Statement] -> IO Text
getCode stmts = do
  beautify $ fst $ renderStatements 0 stmts

getMappingsJson :: String -> [Mapping] -> Value
getMappingsJson name mappings =
  SourceMap.generate SourceMapping
    { smFile = name ++ ".js"
    , smSourceRoot = Nothing
    , smMappings = mappings
    }

writeFileMapping :: FilePath -> IO ()
writeFileMapping fp = do
  stmts <- getCore fp >>= getJs
  (text,mappings) <- getTextMappings (T.pack name) stmts
  T.writeFile (name ++ ".js") text
  L.writeFile (name ++ mapExt) (Aeson.encode (getMappingsJson name mappings))

  where name = reverse . dropWhile (=='.') . dropWhile (/='.') . reverse $ fp

interp :: Text -> IO ()
interp input = do
  result <- readAllFromProcess "node" [] input
  case result of
    Left err -> error (T.unpack err)
    Right (stderr,stdout) -> do T.putStrLn stderr
                                T.putStrLn stdout

run :: Text
run = "var start = new Date();" <>
      "__(main$ZCMain$main);" <>
      "var end = new Date();" <>
      "console.log((end-start)+'ms')"

getForeJs :: Text -> IO Text
getForeJs name = do
  let sourceMapRef = "//@ sourceMappingURL=" <> name <> T.pack mapExt <> "\n"
  rts <- T.readFile "../js/rts.js"
  base <- T.readFile "../js/base.js"
  classes <- T.readFile "../js/classes.js"
  instances <- T.readFile "../js/instances.js"
  io <- T.readFile "../js/io.js"
  primitives <- T.readFile "../js/primitives.js"
  let header = T.pack (replicate 80 '/') <> "\n// Application/library code\n"
  beautify (T.concat [sourceMapRef
                     ,rts
                     ,base
                     ,classes
                     ,instances
                     ,io
                     ,primitives
                     ,header])

mapExt = ".json"

--------------------------------------------------------------------------------
-- Compilers

-- | The compilation monad.
newtype Compile a = Compile { runCompile :: IO a }
  deriving (Monad,Functor,MonadIO,Applicative)

compileModule :: CoreModule -> Compile [Statement]
compileModule (CoreModule{cm_binds=binds}) =
  fmap concat (mapM compileBind binds)

compileBind :: Bind Var -> Compile [Statement]
compileBind bind =
  case bind of
    NonRec var exp -> fmap return (compileNonRec var exp)
    Rec defs -> mapM compileRec defs

compileNonRec :: Var -> Expr Var -> Compile Statement
compileNonRec var exp = do
  v <- compileVar var
  e <- compileExp exp
  return (Declaration v (original (varName var)) (thunk e))

compileRec :: (Var, Expr Var) -> Compile Statement
compileRec (var,exp) = do
  v <- compileVar var
  e <- compileExp exp
  return (Declaration v (original (varName var)) (thunk e))

compileExp :: Expr Var -> Compile Expression
compileExp exp = (>>= optimizeApp) $
  case exp of
    Var var -> fmap Variable (compileVar var)
    Lit lit -> compileLit lit
    App op arg -> compileApp op arg
    Lam var exp -> compileLam var exp
    Let bind exp -> compileLet bind exp
    Cast exp _ -> compileExp exp
    Case exp var typ alts -> compileCase exp var typ alts

compileVar :: Var -> Compile Identifier
compileVar var = compileName (varName var)

compileName :: Name -> Compile Identifier
compileName name = do
  case nameModule_maybe name of
    Just mname -> compileQVar mname name
    Nothing    -> return (translateName name)

compileQVar :: Module -> Name -> Compile Identifier
compileQVar mod name = do
  when (elem (modulePackageId mod) wiredInPackages &&
        not (elem (printName name) (map printNameParts supported))) $
    error $ "Unsupported built-in identifier: " ++
            T.unpack (printName name) ++
            " (" ++ T.unpack (encodeName name) ++ ")"
  return (translateName name)

compileLit :: Literal -> Compile Expression
compileLit lit = return $
  case lit of
    MachInt i           -> Integer (fromIntegral i)
    MachDouble (n :% d) -> Double (fromIntegral n/fromIntegral d)
    MachFloat (n :% d)  -> Double (fromIntegral n/fromIntegral d)
    MachChar c          -> String [c]
    MachStr s           -> String (unpackFS s)

compileApp :: Expr Var -> Expr Var -> Compile Expression
compileApp op arg = do
  case arg of
    Type{} -> compileExp op
    _ -> do
      case op of
        Var v | any ((== printName (varName v)) . printNameParts) noops -> compileExp arg
        (App (Var v') Type{})
          | printName (varName v') == "base:Control.Exception.Base.patError"
          , Lit (MachStr fs) <- arg -> compilePatError (fastStringToText fs)
        _ -> do
          o <- compileExp op
          a <- compileExp arg
          case o of
            Apply (Variable ident) [_]
              | elem ident (map (Identifier . encodeNameParts) methods) -> return (Apply o [a])
            _ -> return (Apply (force o) [a])

compilePatError err = return $ throwExp loc ("Non-exhaustive patterns in " <> typ) where
  loc = Loc (file,line)
  file = T.takeWhile (/=':') err
  line = read . T.unpack . T.takeWhile isDigit . T.drop 1 . T.dropWhile (/=':') $ err
  typ = T.drop 1 . T.dropWhile (/='|') $ err

-- | Optimize nested IO actions like a>>b>>c to __(a),__(b),c
optimizeApp e =
  case e of
    (Apply (Apply (Variable (Identifier "_"))
                  [(Apply (Apply (Variable (Identifier "base$GHC$Base$zgzg"))
                                 [Variable (Identifier "base$GHC$Base$zdfMonadIO")])
                                 [a])])
                  [b]) -> return $ Sequence [act a,b]
    e -> return e

compileLam :: Var -> Expr Var -> Compile Expression
compileLam var exp = do
  v <- compileVar var
  e <- compileExp exp
  return (lambda [v] (thunk e))

compileLet :: Bind Var -> Expr Var -> Compile Expression
compileLet bind body = do
  case bind of
    NonRec var exp -> compileNonRecLet var exp body
    Rec binds      -> compileRecLet binds body

compileNonRecLet :: Var -> Expr Var -> Expr Var -> Compile Expression
compileNonRecLet var exp body = do
  v <- compileVar var
  e <- compileExp exp
  b <- compileExp body
  return (Apply (Function [v] b)
                [e])

compileRecLet :: [(Var,Expr Var)] -> Expr Var -> Compile Expression
compileRecLet binds body = do
  b <- compileExp body
  decls <- mapM compileVarDecl binds
  let closure = Procedure [] (decls ++ [Return b])
  return (Apply closure [])

compileVarDecl :: (Var,Expr Var) -> Compile Statement
compileVarDecl (var,exp) = do
  v <- compileVar var
  e <- compileExp exp
  return (Declaration v Nothing (thunk e))

compileCase :: Expr Var -> Var -> t -> [(AltCon,[Var],Expr Var)] -> Compile Expression
compileCase exp var typ alts = do
  e <- compileExp exp
  v <- compileVar var

  fmap (bind [v] e)
       (foldM (matchCon (Variable v)) (error "unhandled case")
              -- I have no idea why the DEFAULT case comes first from Core.
              (reverse (filter (not.isDefault) alts ++ filter isDefault alts)))

  where isDefault (con,_,_) = con == DEFAULT

matchCon :: Expression -> Expression -> (AltCon,[Var],Expr Var) -> Compile Expression
matchCon object inner (con,vars,exp) = do
  case con of
    DataAlt con -> matchData object inner vars exp con
    LitAlt lit  -> matchLit object inner vars exp lit
    DEFAULT     -> compileExp exp

matchData :: Expression -> Expression -> [Var] -> Expr Var -> DataCon -> Compile Expression
matchData object inner vars exp con = do
  if printName (dataConName con) == "ghc-prim:GHC.Types.I#"
     then do vs <- mapM compileVar vars
             fmap (bind vs (force object)) (compileExp exp)
     else if printName (dataConName con) == "ghc-prim:GHC.Types.True"
             then do vs <- mapM compileVar vars
                     fmap (bind vs (force object)) (compileExp exp)
             else if printName (dataConName con) == "ghc-prim:GHC.Types.False"
                     then do vs <- mapM compileVar vars
                             fmap (bind vs (force object)) (compileExp exp)
                     else do error ("no-o: " ++ T.unpack (printName (dataConName con)))
                             return inner

matchLit :: Expression -> Expression -> t -> Expr Var -> Literal -> Compile Expression
matchLit object inner vars exp lit =
  case lit of
    MachInt i -> do
      e <- compileExp exp
      return (Conditional (StrictEqual (Integer (fromIntegral i))
                                       (object))
                          e
                          inner)

bind :: [Identifier] -> Expression -> Expression -> Expression
bind vs e = (\i -> Apply (lambda vs i) [e])

-- --------------------------------------------------------------------------------
-- -- Supported stuff

wiredInPackages =
  [primPackageId
  ,integerPackageId
  ,basePackageId
  ,rtsPackageId
  ,thPackageId
  ,dphSeqPackageId
  ,dphParPackageId]

noops =
  [("ghc-prim","GHC.Types","I#")
  ,("ghc-prim","GHC.Types","C#")]

supported =
  methods ++
  rest

isMethod = flip elem (map encodeNameParts methods)

methods =
  [("base","System.IO","print")
  ,("base","GHC.Base",">>")
  ,("base","GHC.Show","show")
  -- ,("ghc-prim","GHC.Classes","==")
  ,("ghc-prim","GHC.Classes","<")
  ,("base","GHC.Num","+")
  ,("base","GHC.Num","-")]

rest =
  [("base","System.IO","putStrLn")
  ,("base","GHC.TopHandler","runMainIO")
  ,("base","GHC.Err","undefined")
  ,("base","GHC.Base","$")
  -- Primitives
  ,("ghc-prim","GHC.CString","unpackCString#")
  ,("ghc-prim","GHC.Tuple","(,)")
  ,("ghc-prim","GHC.Tuple","()")
  ,("ghc-prim","GHC.Types",":")
  ,("ghc-prim","GHC.Types","C#")
  ,("ghc-prim","GHC.Types","I#")
  ,("ghc-prim","GHC.Types","[]")
  ,("ghc-prim","GHC.Classes","$fOrdInt")
  ,("ghc-prim","GHC.Classes","$fEqInt")
  ,("base","GHC.Err","error")
  -- Instances
  ,("base","GHC.Show","$fShow[]")
  ,("base","GHC.Show","$fShowChar")
  ,("base","GHC.Show","$fShowInt")
  ,("base","GHC.Show","$fShow(,)")
  ,("base","GHC.Base","$fMonadIO")
  ,("base","GHC.Num","$fNumInt")
  ,("base","GHC.Base","return")
  ]

--------------------------------------------------------------------------------
-- Compiler combinators

-- | Maybe generate an original name/position value.
original :: Name -> Maybe Original
original name =
  case nameSrcLoc name of
    RealSrcLoc loc -> Just (Original (name,loc))
    _ -> Nothing

-- | Throw an exception as an expression.
throwExp :: Loc -> Text -> Expression
throwExp loc e = Apply (Procedure [] [Throw loc (String (T.unpack e))])
                       []

-- | Make a lambda.
lambda :: [Identifier] -> Expression -> Expression
lambda vs e
  | map Variable vs == [e] = e
  | otherwise              = Function vs e

-- | Translate a GHC name to a JS identifier.
translateName :: Name -> Identifier
translateName = Identifier . encodeName

-- | Encode a GHC name to an identifier.
encodeName :: Name -> Text
encodeName name = T.pack encoded where
  encoded = case nameModule_maybe name of
    Nothing -> zEncodeString (occNameString (nameOccName name))
    Just mod ->
      zEncodeString (packageIdString (modulePackageId mod)) ++ "$" ++
      intercalate "$" (map zEncodeString (words (dotsToSpace (moduleNameString (moduleName mod))))) ++ "$" ++
      zEncodeString (occNameString (nameOccName name))

-- | Convert dots to spaces.
dotsToSpace :: String -> String
dotsToSpace = map replace where
  replace '.' = ' '
  replace c   = c

-- | Print a GHC name to some text.
printName :: Name -> Text
printName name = printed where
  printed = case nameModule_maybe name of
    Nothing -> fastStringToText $ occNameFS (nameOccName name)
    Just mod -> moduleText mod <> "." <> fastStringToText (occNameFS (nameOccName name))

-- | Convert a GHC FastString to a Text. There should be a fast way to
-- convert these.
fastStringToText :: FastString -> Text
fastStringToText = T.decodeUtf8 . L.pack . bytesFS

-- | Print the parts of a name to text.
printNameParts :: (String,String,String) -> Text
printNameParts (package,mod,ident) = T.pack $
  package <> ":" <> mod <> "." <> ident

-- | Encode the parts (package, module, identifier) to text.
encodeNameParts :: (String,String,String) -> Text
encodeNameParts (package,mod,ident) = T.pack $
  zEncodeString (package) ++ "$" ++
  intercalate "$" (map zEncodeString (words (dotsToSpace mod))) ++ "$" ++
  zEncodeString ident

-- | Print the module name (and package) to text.
moduleText :: Module -> Text
moduleText mod = T.pack $
  packageIdString (modulePackageId mod) ++ ":" ++
  moduleNameString (moduleName mod)

-- | Force an expression, unless it's a built-in function which
-- doesn't need to be forced.
force :: Expression -> Expression
force e
  -- | Known function name?
  | any ((e==) . Variable . Identifier . encodeNameParts) supported = e
  -- | It's a method call on a dictionary, which don't need to be forced.
  | Apply (Variable (Identifier name)) _ <- e,
    isMethod name  = e
  --
  | otherwise = Apply (Variable (Identifier "_"))
                      [e]

-- | Force an IO action.
act :: Expression -> Expression
act e = Apply (Variable (Identifier "__"))
              [e]

-- | Make a thunk for an expression, unless it's a constant or a
-- function, in which case it's not necessary.
thunk :: Expression -> Expression
thunk e =
  if isFunction e
     then e
     else if isConstant e
             then e
             else New (Identifier "$")
                      (Function [] e)

-- | Is the given expression a constant e.g. a literal or w/e.
isConstant :: Expression -> Bool
isConstant Integer{}  = True
isConstant Double{}   = True
isConstant String{}   = True
isConstant Variable{} = True
isConstant _          = False

-- | Is the given expression a function?
isFunction :: Expression -> Bool
isFunction Function{} = True
isFunction _          = False

--------------------------------------------------------------------------------
-- Working with Core

-- | Get the core AST of a Haskell file.
getCoreAst :: FilePath -> IO CoreModule
getCoreAst fp = do
  defaultErrorHandler defaultLogAction $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      let dflags' = foldl xopt_set dflags [Opt_Cpp,Opt_ImplicitPrelude,Opt_MagicHash]
      setSessionDynFlags dflags' { optLevel = 2 }
      cm <- compileToCoreSimplified fp
      return cm

--------------------------------------------------------------------------------
-- Printing

-- | Beautify the given JS.
beautify :: Text -> IO Text
beautify src = do
  result <- readAllFromProcess "/home/chris/Projects/js-beautify/python/js-beautify"
                               ["-i","-s","2","-d","-w","80"]
                               src
  case result of
    Left err -> error (T.unpack err)
    Right (_,out) -> return out

-- | Compress the given JS with Closure with advanced compilation.
compress :: Text -> IO Text
compress src = do
  result <- readAllFromProcess "java"
                               ["-jar"
                               ,"/home/chris/Projects/fpco/learning-site/tools/closure-compiler.jar"
                               ,"--compilation_level=ADVANCED_OPTIMIZATIONS"]
                               src
  case result of
    Left err -> error (T.unpack err)
    Right (_,out) -> return out

--------------------------------------------------------------------------------
-- Utilities that should be moved to other modules

-- | Read all stuff from a process.
readAllFromProcess :: FilePath -> [String] -> Text -> IO (Either Text (Text,Text))
readAllFromProcess program flags input = do
  (code,out,err) <- readProcessWithExitCode program flags input
  return $ case code of
    ExitFailure _ -> Left err
    ExitSuccess   -> Right (err, out)

-- | Generically show something.
gshow :: Data a => a -> String
gshow x = gshows x ""

-- | A shows printer which is good for printing Core.
gshows :: Data a => a -> ShowS
gshows = render `extQ` (shows :: String -> ShowS) where
  render t
    | isTuple = showChar '('
              . drop 1
              . commaSlots
              . showChar ')'
    | isNull = showString "[]"
    | isList = showChar '['
             . drop 1
             . listSlots
             . showChar ']'
    | otherwise = showChar '('
                . constructor
                . slots
                . showChar ')'

    where constructor = showString . showConstr . toConstr $ t
          slots = foldr (.) id . gmapQ ((showChar ' ' .) . gshows) $ t
          commaSlots = foldr (.) id . gmapQ ((showChar ',' .) . gshows) $ t
          listSlots = foldr (.) id . init . gmapQ ((showChar ',' .) . gshows) $ t

          isTuple = all (==',') (filter (not . flip elem "()") (constructor ""))
          isNull = null (filter (not . flip elem "[]") (constructor ""))
          isList = constructor "" == "(:)"

-- | Simple shortcut.
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Traverse a data type, left to stop, right to keep going after updating the value.
gtraverseT :: (Data a,Typeable b) => (b -> b) -> a -> a
gtraverseT f =
  gmapT (\x -> case cast x of
                 Nothing -> gtraverseT f x
                 Just b  -> fromMaybe x (cast (f b)))

-- | Something like Show but for things which annoyingly do not have
-- Show but Outputable instead.
showppr :: Outputable a => a -> String
showppr = showSDoc . ppr

--------------------------------------------------------------------------------
-- AST

-- | JavaScript statement.
data Statement
  = Declaration !Identifier !(Maybe Original) !Expression
  | Return !Expression
  | Throw !Loc !Expression
  deriving (Eq,Show)

-- | JavaScript expression.
data Expression
  = Variable !Identifier
  | New !Identifier !Expression
  | Sequence [Expression]
  | Function ![Identifier] !Expression
  | Procedure ![Identifier] ![Statement]
  | Apply !Expression ![Expression]
  | Conditional !Expression !Expression !Expression
  | StrictEqual !Expression !Expression
  | Integer !Integer
  | Double !Double
  | String !String
  deriving (Eq,Show)

-- | Variable name.
newtype Identifier
  = Identifier Text
  deriving (Eq,Show)

-- | State of the JS printer.
data PrintState
  = PrintState { psBuilder  :: !Builder
               , psLine     :: !Int
               , psColumn   :: !Int
               , psMappings :: ![Mapping]
               }

-- | A source location (line,col).
newtype Loc = Loc (Text,Int)
  deriving (Show,Eq)

-- | Original Haskell name.
newtype Original = Original (Name,RealSrcLoc)
  deriving (Eq)

instance Show Original where
  show (Original name) = showppr name

--------------------------------------------------------------------------------
-- Pretty printer

-- | JS pretty printer.
newtype PP a = PP { runPP :: State PrintState a }
  deriving (Functor,Monad,MonadState PrintState)

-- | Render the JS AST to text.
renderStatements :: Int -> [Statement] -> (Text,[Mapping])
renderStatements line = (write &&& mappings) . pp where
  pp = flip execState state . runPP . ppStatements
  write = toLazyText . psBuilder
  state = PrintState mempty line 0 []
  mappings = reverse . psMappings

-- | Pretty print statements.
ppStatements :: [Statement] -> PP ()
ppStatements = mapM_ ppStatement

-- | A pretty printer for the JS.
ppStatement :: Statement -> PP ()
ppStatement s =
  case s of
    Declaration i pos e ->
      do maybe (return ()) (mapping i) pos
         write "var "
         ppIdentifier i
         write " = "
         ppExpression e
         write ";"
         newline
    Return e ->
      do write "return ("
         ppExpression e
         write ");"
    Throw loc e ->
      do mapFrom loc
         write "throw ("
         ppExpression e
         write ");"

-- | Pretty print expressions.
ppExpression :: Expression -> PP ()
ppExpression e =
  case e of
    Variable i -> ppIdentifier i
    New i e ->
      do write "new "
         ppIdentifier i
         write "("
         ppExpression e
         write ")"
    Function is e -> ppProcedure is [Return e]
    Procedure is ss -> ppProcedure is ss
    Apply e es ->
      do ppExpression e
         write "("
         intercalateM ", " (map ppExpression es)
         write ")"
    Conditional p y n ->
      do write "("
         ppExpression p
         write " ? "
         ppExpression y
         write " : "
         ppExpression n
         write ")"
    StrictEqual a b ->
      do ppExpression a
         write " === "
         ppExpression b
    Sequence es ->
      do write "("
         intercalateM ", " (map ppExpression es)
         write ")"
    Integer i -> write (T.pack (show i))
    Double d -> write (T.pack (show d))
    String s -> write (T.pack (ECMA.renderExpression (ECMA.StringLit () s)))

-- | Pretty print a function closure.
ppProcedure :: [Identifier] -> [Statement] -> PP ()
ppProcedure is ss =
  do write "function("
     intercalateM ", " (map ppIdentifier is)
     write "){"
     intercalateM ";" (map ppStatement ss)
     write "}"

-- | Pretty print an identifier.
ppIdentifier :: Identifier -> PP ()
ppIdentifier i =
  case i of
    Identifier text -> write text

--------------------------------------------------------------------------------
-- Utilities for the pretty printer

-- | Intercalate monadic action.
intercalateM :: Text -> [PP a] -> PP ()
intercalateM _ [] = return ()
intercalateM _ [x] = x >> return ()
intercalateM str (x:xs) = do
  x
  write str
  intercalateM str xs

-- | Something to write to the printer builder.
data Write
  = WriteNewline
  | WriteText Text

-- | Write something to the output builder.
build :: Write -> PP ()
build x = do
  ps <- get
  case x of
    WriteNewline ->
      let !column = 0
          !line = psLine ps + 1
      in put ps { psBuilder = psBuilder ps <> fromLazyText "\n"
                , psColumn = column
                , psLine = line
                }
    WriteText text ->
      let !column = psColumn ps + fromIntegral (T.length text)
          !builder = psBuilder ps <> fromLazyText text
      in put ps { psBuilder = builder
                , psColumn = column
                }

-- | Write some text to the builder. Shouldn't contain newlines.
write :: Text -> PP ()
write = build . WriteText

-- | Write a newline to the output builder.
newline :: PP ()
newline = build WriteNewline

-- | Generate a mapping.
mapping :: Identifier -> Original -> PP ()
mapping (Identifier identifier) (Original (name,loc)) = modify $ \ps ->
  ps { psMappings = m ps : psMappings ps
     }

  where m ps = Mapping { mapGenerated = Pos (fromIntegral (psLine ps))
                                            (fromIntegral (psColumn ps))
                       , mapOriginal = Just (Pos (fromIntegral line)
                                                 (fromIntegral col - 1))
                       , mapSourceFile = Just (T.unpack (fastStringToText file))
                       , mapName = Just (T.toStrict (printName name))
                       }
        line = srcLocLine loc
        col = srcLocCol loc
        file = srcLocFile loc

-- | Generate a mapping just for a location.
mapFrom :: Loc -> PP ()
mapFrom (Loc (file,line)) = modify $ \ps ->
  ps { psMappings = m ps : psMappings ps
     }

  where m ps = Mapping { mapGenerated = Pos (fromIntegral (psLine ps))
                                            (fromIntegral (psColumn ps))
                       , mapOriginal = Just (Pos (fromIntegral line)
                                                 0)
                       , mapSourceFile = Just (T.unpack file)
                       , mapName = Nothing
                       }
