// Class instances

////////////////////////////////////////////////////////////////////////////////
// Show

// Show [a]
var base$GHC$Show$zdfShowZMZN = function(i){
  return function(l){
    return new $(function(){
      var str = "";
      l = _(l);
      while(l instanceof ghczmprim$Cons) {
        str += l.car;
        l = _(l.cdr);
      }
      return ghczmprim$GHC$CString$unpackCStringzh(str);
    });
  };
};

// Show Char
var base$GHC$Show$zdfShowChar = function(x){
  return new $(function(){
    return ghczmprim$GHC$CString$unpackCStringzh("'" + _(x) + "'");
  });
};

// Show Int
var base$GHC$Show$zdfShowInt = function(i){
  return new $(function(){
    return ghczmprim$GHC$CString$unpackCStringzh(_(i).toString());
  });
};

// Show (,)
var base$GHC$Show$zdfShowZLz2cUZR = function(i1){
  return function(i2){
    return function(x){
      return function(y){
        throw "stub: base$GHC$zdfShowZLz2cUZR: " + JSON.stringify(x) + ", " + JSON.stringify(y);
      };
    };
  };
};

////////////////////////////////////////////////////////////////////////////////
// Num

// Num Int
var base$GHC$Num$zdfNumInt =
  new base$GHC$Num(function(x){
    return function(y){
      return new $(function(){
        return _(x) + _(y);
      });
    };
  },function(x){
    return function(y){
      return new $(function(){
        return _(x) - _(y);
      });
    };
  });

////////////////////////////////////////////////////////////////////////////////
// Monad

// Monad IO
var base$GHC$Base$zdfMonadIO =
  new base$GHC$Base$Monad(function(x){
    return x;
  },function(m){
    return function(n){
      return new $(function(){
        __(m);
        return n;
      });
    }
  });
