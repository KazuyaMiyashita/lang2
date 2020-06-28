# lang2
Lispパーサーもどき

```
sbt:lang2> run
[info] running Main 
*** LANG2 CONSOLE***
enter a formula. For example (add 1 (mul 2 3)) . The reuslt is 7.
Built-in functions are `add` and `mul`.
enter :exit to exit this console.

> (add 1 (mul 2 3))
result: 7
> add 1 2
result: 3
> add 1 2 3
* evaluation error *
java.lang.IllegalArgumentException
	at lang2.Func$.$anonfun$add$1(Func.scala:12)
	at lang2.Evaluator$.eval(Evaluator.scala:14)
input: add 1 2 3
tokens: List(Command(add), Numeric(1), Numeric(2), Numeric(3))
term: Apply(add,List(Const(1), Const(2), Const(3)))
evaluated: null
```
