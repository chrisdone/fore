{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Haskell to JavaScript compiler.

module Fore where

import           Control.Monad
import           Control.Exception
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.Trans
import           CoreSyn
import           Data.Data hiding (tyConName)
import           Data.Generics.Aliases
import           Data.List
import           Data.Maybe
import           Data.String
import           DataCon
import           Debug.Trace
import           DynFlags
import           Encoding
import           FastString
import           GHC
import           GHC.Paths ( libdir )
import           GHC.Real
import           HscTypes
import qualified Language.ECMAScript3.Parser as ECMA
import qualified Language.ECMAScript3.PrettyPrint as ECMA
import           Language.ECMAScript3.Syntax as ECMA
import           Literal
import           Module
import           Name hiding (varName)
import           OccName hiding (varName)
import           Outputable
import           Outputable
import           Prelude hiding (exp)
import           System.Exit
import           System.IO
import           System.Process
import qualified Text.PrettyPrint as Doc
import           TyCon
import           TypeRep
import           Var

--------------------------------------------------------------------------------
-- Compiling

newtype Compile a = Compile { runCompile :: ReaderT DynFlags IO a }
  deriving (Monad,Functor,MonadIO,MonadReader DynFlags)

hs :: FilePath -> IO ()
hs = readFile >=> putStrLn

core :: FilePath -> IO ()
core fp = do
  (dynflags,core) <- getCoreAst fp
  putStrLn $ showSDoc $ ppr core

js :: FilePath -> IO ()
js fp = do
  (dynflags,core) <- getCoreAst fp
  stmt <- runReaderT (runCompile (compileModule core)) dynflags
  js <- beautify $ Doc.render (ECMA.javaScript (ECMA.Script () stmt))
  forejs <- getForeJs
  writeFile "test.js" (forejs ++ "\n" ++ js ++ run)
  putStrLn js

ast :: FilePath -> IO ()
ast fp = do
   (dynflags,core) <- getCoreAst fp
   stmt <- runReaderT (runCompile (compileModule core)) dynflags
   print (ECMA.Script () stmt)

compileFileRun :: FilePath -> IO ()
compileFileRun fp = do
  (dynflags,core) <- getCoreAst fp
  stmt <- runReaderT (runCompile $ compileModule core) dynflags
  js <- beautify $ Doc.render (ECMA.javaScript (ECMA.Script () stmt))
  forejs <- getForeJs
  writeFile "test.js" (forejs ++ "\n" ++ js ++ run)
  result <- readAllFromProcess "node" ["test.js"] ""
  case result of
    Left err -> error err
    Right (stderr,stdout) -> do putStrLn stderr
                                putStrLn stdout

run = "_(main$ZCMain$main,true);"

getForeJs = do
  rts <- readFile "../js/rts.js"
  classes <- readFile "../js/classes.js"
  instances <- readFile "../js/instances.js"
  io <- readFile "../js/io.js"
  primitives <- readFile "../js/primitives.js"
  return (concat [rts,classes,instances,io,primitives])

compileModule (CoreModule{cm_binds=binds}) =
  mapM compileBind binds

compileBind bind =
  case bind of
    NonRec var exp -> compileNonRec var exp
    Rec defs -> fmap (VarDeclStmt ()) (mapM compileRec defs)

compileNonRec var exp = do
  v <- compileVar var
  e <- compileExp exp
  return (VarDeclStmt () [VarDecl () v (Just (thunk e))])

compileRec (var,exp) = do
  v <- compileVar var
  e <- compileExp exp
  return (VarDecl () v (Just (thunk e)))

compileExp exp = (>>= optimizeApp) $
  case exp of
    Var var -> fmap (VarRef ()) (compileVar var)
    Lit lit -> compileLit lit
    App op arg -> compileApp op arg
    Lam var exp -> compileLam var exp
    Let bind exp -> compileLet bind exp
    Cast exp _ -> compileExp exp
    Case exp var _ alts -> compileCase exp var alts
    e -> error $ gshow exp

compileVar var = compileName (varName var)

compileName name =
  case nameModule_maybe name of
    Just mname -> compileQVar mname name
    Nothing    -> do
      -- warn $ "allowing: " ++ getOccString name
      return (translateName name)

compileQVar mod name = do
  when (elem (modulePackageId mod) wiredInPackages &&
        not (elem (printName name) (map printNameParts supported))) $
    error $ "Unsupported built-in identifier: " ++ printName name ++ " (" ++ encodeName name ++ ")"
  return (translateName name)

compileLit lit = return $
  case lit of
    MachInt i -> IntLit () (fromIntegral i)
    MachDouble (n :% d) -> NumLit () (fromIntegral n/fromIntegral d)
    MachFloat (n :% d) -> NumLit () (fromIntegral n/fromIntegral d)
    MachChar c -> StringLit () [c]
    MachStr s -> StringLit () (unpackFS s)

compileApp op arg = do
  case arg of
    Type{} -> compileExp op
    _ -> do
      case op of
        Var v | any ((== printName (varName v)) . printNameParts) noops -> compileExp arg
        _ -> do
          o <- compileExp op
          a <- compileExp arg
          return (CallExpr () (force o) [a])

optimizeApp e =
  case e of
    (CallExpr () (CallExpr () (VarRef () (Id () "__"))
                 [(CallExpr ()
                            (CallExpr ()
                                      (VarRef () (Id () "base$GHC$Base$zgzg"))
                                      [VarRef () (Id () "base$GHC$Base$zdfMonadIO")])
                                      [a])])
                 [b]) -> return $ ListExpr () [act a,b]
    e -> return e

compileLam var exp = do
  v <- compileVar var
  e <- compileExp exp
  return (lambda [v] (thunk e))

compileLet bind body = do
  case bind of
    NonRec var exp -> compileNonRecLet var exp body
    Rec binds      -> compileRecLet binds body

compileNonRecLet var exp body = do
  v <- compileVar var
  e <- compileExp exp
  b <- compileExp body
  return (CallExpr () (FuncExpr () Nothing [v] [ReturnStmt () (Just b)])
                      [e])

compileRecLet binds body = do
  b <- compileExp body
  decls <- fmap (VarDeclStmt ()) (mapM compileVarDecl binds)
  let closure = FuncExpr () Nothing [] [decls,ReturnStmt () (Just b)]
  return (CallExpr () closure [])

compileVarDecl (var,exp) = do
  v <- compileVar var
  e <- compileExp exp
  return (VarDecl () v (Just (thunk e)))

compileCase exp var alts = do
  e <- compileExp exp
  v <- compileVar var

  fmap (bind [v] e)
       (foldM (matchCon e) (throwExp "unhandled case")
              -- I have no idea why the DEFAULT case comes first from Core.
              (reverse (filter (not.isDefault) alts ++ filter isDefault alts)))

  where isDefault (con,_,_) = con == DEFAULT

matchCon object inner (con,vars,exp) = do
  error (gshow (object,inner))
  case con of
    DataAlt con -> matchData object inner vars exp con
    LitAlt lit  -> matchLit object inner vars exp lit
    DEFAULT     -> compileExp exp

matchData object inner vars exp con = do
  if printName (dataConName con) == "ghc-prim:GHC.Types.I#"
     then do vs <- mapM compileVar vars
             fmap (bind vs object) (compileExp exp)
     else return inner

matchLit object inner vars exp lit =
  case lit of
    MachInt i -> do
      e <- compileExp exp
      return (CondExpr () (InfixExpr () OpStrictEq (force object) (IntLit () (fromIntegral i)))
                          e
                          inner)

bind vs e = (\i -> CallExpr () (lambda vs i) [e])

--------------------------------------------------------------------------------
-- Supported stuff

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
  [("base","System.IO","putStrLn")
  ,("base","System.IO","print")
  ,("base","GHC.Base",">>")
  ,("base","GHC.Show","show")
  ,("ghc-prim","GHC.Classes","==")
  ,("ghc-prim","GHC.Classes","<")]

rest =
  [("base","GHC.TopHandler","runMainIO")
  ,("base","GHC.Err","undefined")
  ,("base","GHC.Base","$")
  ,("base","GHC.Num","+")
  ,("base","GHC.Num","-")
  -- Primitives
  ,("ghc-prim","GHC.CString","unpackCString#")
  ,("ghc-prim","GHC.Tuple","(,)")
  ,("ghc-prim","GHC.Tuple","()")
  ,("ghc-prim","GHC.Types",":")
  ,("ghc-prim","GHC.Types","C#")
  ,("ghc-prim","GHC.Types","I#")
  ,("ghc-prim","GHC.Types","[]")
  ,("ghc-prim","GHC.Classes","$fOrdInt")
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

throwExp e = CallExpr () (FuncExpr () Nothing [] [ThrowStmt () (StringLit () e)]) []

lambda vs e | map (VarRef ()) vs == [e] = e -- identity
            | otherwise = FuncExpr () Nothing vs [ReturnStmt () (Just e)]

translateName = Id () . encodeName where

encodeName name = encoded where
  encoded = case nameModule_maybe name of
    Nothing -> zEncodeString (occNameString (nameOccName name))
    Just mod ->
      zEncodeString (packageIdString (modulePackageId mod)) ++ "$" ++
      intercalate "$" (map zEncodeString (words (dotsToSpace (moduleNameString (moduleName mod))))) ++ "$" ++
      zEncodeString (occNameString (nameOccName name))

dotsToSpace = map replace where
  replace '.' = ' '
  replace c   = c

printName name = printed where
  printed = case nameModule_maybe name of
    Nothing -> occNameString (nameOccName name)
    Just mod ->
      moduleString mod ++ "." ++
      occNameString (nameOccName name)

printNameParts (package,mod,ident) = package ++ ":" ++ mod ++ "." ++ ident
encodeNameParts (package,mod,ident) =
  zEncodeString (package) ++ "$" ++
  intercalate "$" (map zEncodeString (words (dotsToSpace mod))) ++ "$" ++
  zEncodeString ident

moduleString mod =
  packageIdString (modulePackageId mod) ++ ":" ++
  moduleNameString (moduleName mod)

warn s = io $ putStrLn $ "Warning: " ++ s
force e | any ((e==).VarRef () . Id () . encodeNameParts) supported = e
        | CallExpr () (VarRef () (Id () name)) _ <- e,
          isMethod name = e
        | otherwise = CallExpr () (VarRef () (Id () "__")) [e]

act e = CallExpr () (VarRef () (Id () "_")) [e]

thunk e =
  if isFunction e
     then e
     else if isConstant e
             then e
             else NewExpr () (VarRef () (Id () "$"))
                             [FuncExpr () Nothing []
                                       [ReturnStmt () (Just e)]]

isConstant IntLit{} = True
isConstant NumLit{} = True
isConstant StringLit{} = True
isConstant VarRef{} = True
isConstant _       = False

isFunction FuncExpr{} = True
isFunction _          = False

--------------------------------------------------------------------------------
-- Working with Core

-- | Get the core AST of a Haskell file.
getCoreAst fp = do
  defaultErrorHandler defaultLogAction $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      let dflags' = foldl xopt_set dflags [Opt_Cpp,Opt_ImplicitPrelude,Opt_MagicHash]
      setSessionDynFlags dflags'
      cm <- compileToCoreSimplified fp
      return $ (dflags,cm)

-- | Get the ghc -fext-core output of a Haskell file.
getCoreString :: FilePath -> IO String
getCoreString fp = do
  result <- readAllFromProcess "ghc" [fp,"-v0","-fext-core","-O2"] ""
  case result of
    Left err -> error err
    Right (_,_) -> readFile (reverse (dropWhile (/='.') (reverse fp)) ++ "hcr")

--------------------------------------------------------------------------------
-- Printing

-- | Take in JavaScript and output it pretty-printed.
prettyPrintJS :: String -> String
prettyPrintJS src =
  case ECMA.parseScriptFromString "<input>" src of
    Left e -> error (show e)
    Right js -> Doc.render (ECMA.javaScript js)

beautify :: String -> IO String
beautify src = do
  result <- readAllFromProcess "/home/chris/Projects/js-beautify/python/js-beautify" ["-i","-s","2","-d","-w","80"] src
  case result of
    Left err -> error err
    Right (_,out) -> return out

--------------------------------------------------------------------------------
-- Utilities that should be moved to other modules

-- | Read all stuff from a process.
readAllFromProcess :: FilePath -> [String] -> String -> IO (Either String (String,String))
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

io :: MonadIO m => IO a -> m a
io = liftIO

pp :: (MonadIO m,Data a) => a -> m ()
pp = io . putStrLn . gshow
