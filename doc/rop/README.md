# ROP (Railway Oriented Programming)
**ROP** is a functional programming approach to error handling. Each function which want to be used in **ROP**, Should yield `{ok, Result::any()}`(success rail) or `{error, Reason::any()}` (failure rail).  
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

# Example
See `rop_example.erl` file to know how does it work.
```sh
~/breath $ cd doc/rop
~/breath/doc/rop $ make shell
```
```erlang
erl -pa ../../_build/default/lib/breath/ebin
Erlang/OTP 21 [erts-10.0] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe]

Eshell V10.0  (abort with ^G)
1> c(rop_example).
{ok,rop_example}

2> rop_example:test(atom).
{error,{type,atom}}

3> rop_example:test(1).   
{error,{odd,1}}

4> rop_example:test(2).
Number: 40.2
ok
```

What if your function would yeild for example `'oops'` instead of `{ok, _}` or `{error, _}`?  
```erlang
5> rop_example:bad_return_test(). 
** exception error: no case clause matching oops
     in function  rop_example:bad_return_test/0 (rop_example.erl, line 78)
```

What if your function would crash?  
```erlang
6> rop_example:crash_test_1(omg).
will be crashed with reason omg
** exception exit: omg
     in function  rop_example:'-crash_test_1/1-fun-1-'/1 (rop_example.erl, line 86)

7> rop_example:crash_test_2().   
** exception error: no function clause matching rop_example:'-crash_test_2/0-fun-1-'(nomatch) (rop_example.erl, line 93)
```

What if you passed just one argument to a ROP function?  
To test it, uncomment `compile_test_1` function in `rop_example.erl` file and recompile it:  
```erlang
8> c(rop_example).
rop_example.erl:98: A ROP function must has at least two arguments.
error
```

What if you passed for example atom `'foo'` to A ROP function instead of a function call?  
To test it, uncomment `compile_test_2` function in `rop_example.erl` file and recompile it:  
```erlang
9> c(rop_example).
rop_example.erl:102: Each argument of a ROP function must be a function call.
error
```
