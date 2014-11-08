brainfuck-haskell
=================

A brainfuck parser implemented in haskell

Finally nested loops are working!

Sierpinski generated into sierpinski_output.txt

If you want to run your own code:

```

ghci brainfuck.hs 

let s = newState "brainfuck code"
run s

```

if you want to run step by step you can run:

```
let nextState = step state

```

This was a learning excercise for me, code may not be the best. 



