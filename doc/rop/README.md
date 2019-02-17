# ROP (Railway Oriented Programming)
**ROP** is a model from functional programming world. Each function which want to be used in **ROP**, Should yield `{ok, Result::any()}`(success rail) or `{error, Reason::any()}` (failure rail).  
Imagine we have 3 functions `f1`, `f2` and `f3`:  
```txt

 ________
|        |                     
|   f1   |----Success-rail---->
|________|
    \
     \--------Failure-rail---->

 ________
|        |                     
|   f2   |----Success-rail---->
|________|
    \
     \--------Failure-rail---->

 ________
|        |                     
|   f3   |----Success-rail---->
|________|
    \
     \--------Failure-rail---->
```

Using **ROP** we can connect above functions as follows:  
```txt

 ________                       ________                       ________
|        |                     |        |                     |        |
|   f1   |----Success-rail---->|   f2   |----Success-rail---->|   f3   |----Success-rail---->
|________|                     |________|                     |________|
    \                              \                              \
     \--------Failure-rail----------\--------Failure-rail----------\--------Failure-rail---->
```

See `rop_example.erl` file to know how it works in `Breath` Erlang library.
