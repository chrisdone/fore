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
        return _(i(a))(b);
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
