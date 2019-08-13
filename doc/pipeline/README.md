# Pipeline
The pipeline transformation solves the issue many functional languages face when composing a series of transformations where the output from one function needs passed as the input to another. For example instead of writing:
```erlang
other_function(other_module:other_function(another_function(another_module:function(Arg)))
```
You can simply use `breath` and write below code and breath transforms it to above code:
```erlang
breath:pipeline(
    Arg
    another_module:function(),
    another_function(),
    other_module:other_function(),
    other_function()
)
```
It simply uses Arg as initialized value and passes it as last parameter of next function, then uses return value of each function as last parameter of next function for the rest. 


# Example
See `pipeline_example.erl` file to see how it works with `Breath` Erlang library.  
```sh
~/breath $ cd doc/pipeline
~/breath/doc/pipeline $ make shell
```
```erlang
erl -pa ../../_build/default/lib/breath/ebin
Erlang/OTP 21 [erts-10.0] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe]

Eshell V10.0  (abort with ^G)
1> c(pipeline_example).
{ok,pipeline_example}

4> pipeline_example:test(1).
Number is 30
ok

3> pipeline_example:test(2).
Number is 90
ok

% During transformation, We do not lose the line number of an expression:
4> pipeline_example:crash_test().                                                               
** exception error: bad argument
     in function  lists:keyfind/3
        called as lists:keyfind(key,1,{})
     in call from pipeline_example:crash_test/0 (pipeline_example.erl, line 38)

5> pipeline_example:crash_test(oops).                                                           
** exception error: "oops"
     in function  pipeline_example:crash_test/1 (pipeline_example.erl, line 46)
```


Also pipeline shows clean compile time errors. To test it, uncomment `compile_test_*` functions and recompile it:
```erlang
6> c(pipeline_example).
pipeline_example.erl:51: 'pipeline' function must has at least two arguments.
pipeline_example.erl:58: Except first argument, every argument of 'pipeline' must be a function call.
pipeline_example.erl:64: Every 'pipeline' function call can contain only one underscore.
error
```
