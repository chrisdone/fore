// Basic IO actions

// putStrLn
var base$System$IO$putStrLn = function(s){
  return new $(function(){
    console.log("%s",__(ghczmprim$GHC$CString$packCStringzh(__(s))));
    return null;
  });
};

// print
var base$System$IO$print = function(i){
  return function(x){
    return new $(function(){
      console.log(__(ghczmprim$GHC$CString$packCStringzh(__(__(i)(x)))));
      return null;
    })
  };
};
