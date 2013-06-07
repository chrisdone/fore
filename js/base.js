// Base functions

// $
var base$GHC$Base$zd = function(f){
  return function(g){
    return new $(function(){
      return __(f)(g);
    });
  };
};

// undefined
var base$GHC$Err$undefined = new $(function(){
  throw "undefined";
});

// error
var base$GHC$Err$error = function(s){
  new $(function(){
    throw ghczmprim$GHC$CString$packCStringzh(s);
  });
};
