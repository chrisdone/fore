////////////////////////////////////////////////////////////////////////////////
// Runtime

// A thunk object
function $(value)
{
  this.forced = false;
  this.value = value;
}

// Force the thunk object
$.prototype.force = function(nocache)
{
  return nocache ?
    this.value() :
    (this.forced ?
     this.value :
     (this.value = this.value(), this.forced = true, this.value));
};

// Force a thunkish thing until WHNF
function _(thunkish,nocache)
{
  while (thunkish instanceof $)
    thunkish = thunkish.force(nocache);
  return thunkish;
}

////////////////////////////////////////////////////////////////////////////////
// Built-in data structures

// :
function ghczmprim$Cons(x,y){
  this.car = x, this.cdr = y;
}

////////////////////////////////////////////////////////////////////////////////
// Base functions

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

// >>
var base$GHC$Base$zgzg = function(i){
  return function(m){
    return function(n){
      return new $(function(){
        return _(i(m))(n);
      });
    }
  }
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

// $
var base$GHC$Base$zd = function(f){
  return function(g){
    return new $(function(){
      return _(f)(g);
    });
  };
};

var base$GHC$Show$show = function(i){
  return function(x){
    return i(x);
  };
};

// Run the main IO function.
var base$GHC$TopHandler$runMainIO = function(main){
  return _(main,true);
};

// Num +
var base$GHC$Num$zp = function(i){return i.zp;};

// Num -
var base$GHC$Num$zm = function(i){return i.zm;};

/////////////////////////////////////////////////////////////////////////////////
// Primitives

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

// (,)
var ghczmprim$GHC$Tuple$Z2T = function(x){
  return function(y){
    return [x,y];
  }
};

// (:)
var ghczmprim$GHC$Types$ZC = function(x){
  return function(y){
    return new ghczmprim$Cons(x,y);
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

// []
var ghczmprim$GHC$Types$ZMZN = function(x){
  return null;
};

// ==
var ghczmprim$GHC$Classes$zeze = function(i){
  return function(a){
    return function(b){
      return new $(function(){
        return _(i(a))(b);
      });
    }
  }
};

////////////////////////////////////////////////////////////////////////////////
// Built-in instances

// Show [] instance
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

// Show Char instance
var base$GHC$Show$zdfShowChar = function(x){
  return new $(function(){
    return ghczmprim$GHC$CString$unpackCStringzh("'" + _(x) + "'");
  });
};

// Show Int instance
var base$GHC$Show$zdfShowInt = function(i){
  return new $(function(){
    return ghczmprim$GHC$CString$unpackCStringzh(_(i).toString());
  });
};

// Monad IO instance
var base$GHC$Base$zdfMonadIO = function(m){
  return function(n){
    return new $(function(){
      _(m,true);
      return n;
    });
  }
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
}

function base$GHC$Num(zp,zm){
  this.zp = zp;
  this.zm = zm;
};

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
// Application/library code
