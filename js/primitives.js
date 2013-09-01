// Primitive data types

// ()
var ghczmprim$GHC$Tuple$Z0T = function(){
  return null;
};

// (,)
var ghczmprim$GHC$Tuple$Z2T = function(x){
  return function(y){
    return [x,y];
  }
};

// C# (Char)
var ghczmprim$GHC$Types$Czh = function(x){
  return x;
};

// I# (Int)
var ghczmprim$GHC$Types$Izh = function(x){
  return x;
};

////////////////////////////////////////////////////////////////////////////////
// List

// :
function ghczmprim$Cons(x,y){
  this.car = x, this.cdr = y;
}

// (:)
var ghczmprim$GHC$Types$ZC = function(x){
  return function(y){
    return new ghczmprim$Cons(x,y);
  }
};

// []
var ghczmprim$GHC$Types$ZMZN = function(x){
  return null;
};


////////////////////////////////////////////////////////////////////////////////
// String

// Make a string from a JS string
var ghczmprim$GHC$CString$unpackCStringzh = function(xs){
  return new $(function(){
    var out = null;
    for(var i = xs.length - 1; i >= 0; i--)
      out = new ghczmprim$Cons(xs[i],out);
    return out;
  });
};

// Make a JS string from a string
var ghczmprim$GHC$CString$packCStringzh = function(l){
  return new $(function(){
    var str = "";
    l = _(l);
    while(l instanceof ghczmprim$Cons) {
      str += l.car;
      l = _(l.cdr);
    }
    return str;
  });
};
