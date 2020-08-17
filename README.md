# Pallas-lang
pallas-lang is a small dynamic language which supports recursion, while loops, if-else statements, closures and comes with strings, booleans, numbers arrays and functions. Functions are first class citizens and can be passed by value.

Have a look at the examples which covers most of the functionality. The Pallas language comes with an interpreter called pallas.


## Setup
Install ocaml, opam and dune see instructions at: <https://ocaml.org/docs/install.html> then run `make install`.

Tryout pallas and run some examples e.g.

`pallas examples/fibonacci`

## Syntax

### variables
```
var number = 10;
number = 5;
var float = 10.5;
var boolean = true;
boolean = false;
var string = "hello, world";
var array = [1, 2, 3, 4, 5];
```
### Functions
```
def helloWorld() do
    println("hello, world!");
end
helloWorld();
```

### while loops
```
var i = 0;
while i < length(array) do
    print(array[i]);
    i = i + 1;
end
```

### if-else statement
```
if 1+4 > 4 do
    println("5 is bigger than 4");
else
    println("something is wrong with my code");
end
```

### for each
```
for i in [1,2,3,4,5] do
    println(i);
end

for i in "hello, world!" do
    println(i);
end
```