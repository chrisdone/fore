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
$.prototype.force = function()
{
  return this.forced ?
    this.value :
    (this.value = this.value(), this.forced = true, this.value);
};

// Force a thunkish thing until WHNF
function _(thunkish)
{
  while (thunkish instanceof $)
    thunkish = thunkish.force();
  return thunkish;
}

// Run a thunk with no updating
function __(thunkish){
  while (thunkish instanceof $)
    thunkish = thunkish.value();
  return thunkish;
}

////////////////////////////////////////////////////////////////////////////////
// Run helpers

// Run the main IO function.
var base$GHC$TopHandler$runMainIO = function(main){
  return __(main);
};
