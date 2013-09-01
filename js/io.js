// Basic IO actions

// putStrLn
var base$System$IO$putStrLn = function(s){
  return new $(function(){
    console.log("%s",_(ghczmprim$GHC$CString$packCStringzh(_(s))));
    return null;
  });
};

// print
var base$System$IO$print = function(i){
  return function(x){
    return new $(function(){
      console.log(_(ghczmprim$GHC$CString$packCStringzh(_(_(i)(x)))));
      return null;
    })
  };
};
