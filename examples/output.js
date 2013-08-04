// Runtime

////////////////////////////////////////////////////////////////////////////////
// Thunks

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
function __(thunkish,nocache)
{
  while (thunkish instanceof $)
    thunkish = thunkish.force(nocache);
  return thunkish;
}

// Force a thunk with no thunk updating
function _(thunkish){
  return __(thunkish,true);
}

////////////////////////////////////////////////////////////////////////////////
// Run helpers

// Run the main IO function.
var base$GHC$TopHandler$runMainIO = function(main){
  return __(main,true);
};
// Pre-defined classes

////////////////////////////////////////////////////////////////////////////////
// Monad

function base$GHC$Base$Monad(return_,bind_){
  this.return_ = return_,
  this.bind_ = bind_;
};

// return
var base$GHC$Base$return = function(i){
  return i.return_;
};

// >>
var base$GHC$Base$zgzg = function(i){
  return i.return_;
};

////////////////////////////////////////////////////////////////////////////////
// Num

function base$GHC$Num(zp,zm){
  this.zp = zp;
  this.zm = zm;
};

// +
var base$GHC$Num$zp = function(i){return i.zp;};

// -
var base$GHC$Num$zm = function(i){return i.zm;};


////////////////////////////////////////////////////////////////////////////////
// Eq

// ==
var ghczmprim$GHC$Classes$zeze = function(i){
  return function(a){
    return function(b){
      return new $(function(){
        return __(i(a))(b);
      });
    }
  }
};

////////////////////////////////////////////////////////////////////////////////
// Show

var base$GHC$Show$show = function(i){
  return function(x){
    return i(x);
  };
};
// Class instances

////////////////////////////////////////////////////////////////////////////////
// Show

// Show [a]
var base$GHC$Show$zdfShowZMZN = function(i){
  return function(l){
    return new $(function(){
      var str = "";
      l = __(l);
      while(l instanceof ghczmprim$Cons) {
        str += l.car;
        l = __(l.cdr);
      }
      return ghczmprim$GHC$CString$unpackCStringzh(str);
    });
  };
};

// Show Char
var base$GHC$Show$zdfShowChar = function(x){
  return new $(function(){
    return ghczmprim$GHC$CString$unpackCStringzh("'" + __(x) + "'");
  });
};

// Show Int
var base$GHC$Show$zdfShowInt = function(i){
  return new $(function(){
    return ghczmprim$GHC$CString$unpackCStringzh(__(i).toString());
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
        return __(x) + __(y);
      });
    };
  },function(x){
    return function(y){
      return new $(function(){
        return __(x) - __(y);
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
        __(m,true);
        return n;
      });
    }
  });
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
    l = __(l);
    while(l instanceof ghczmprim$Cons) {
      str += l.car;
      l = __(l.cdr);
    }
    return str;
  });
};

var main$Main$foo = function(a) {
  return tpl
};
var zdcfoo = function(ds) {
  return new $(function() {
    return ghczmprim$GHC$CString$unpackCStringzh("Mwuhahaha.")
  })
};
var main$Main$zdfFooZLZR = zdcfoo;
var main$Main$main = new $(function() {
  return _(base$System$IO$putStrLn(ghczmprim$GHC$CString$unpackCStringzh(
        "Behold, the number of the beast:"))),
  _(base$System$IO$print(base$GHC$Show$zdfShowInt)(666)),
  _(base$System$IO$putStrLn(ghczmprim$GHC$CString$unpackCStringzh("Mwuhahaha."))),
  __(base$GHC$Base$return(base$GHC$Base$zdfMonadIO))(ghczmprim$GHC$Tuple$Z0T)
});
var main$ZCMain$main = new $(function() {
  return base$GHC$TopHandler$runMainIO(main$Main$main)
});
_(main$ZCMain$main,true);