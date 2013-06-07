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
