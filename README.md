# Functional and Logic Programming - Functional project
## Simplify-bkg

#### Author: Matúš Škuta (xskuta04)

## Build

By using command `make`, the content of `src` is compiled into program `simplify-bkg`
using `ghc` compiler.

## Run

Program can be run as follows:

```bash
$ ./simplify-bkg (-i|-1|-2) [file]
```

`file` is name of an input file, when not specified program will expect input on 
standard input. Option `-i`, loads and then prints to standard output loaded BKG
into our inside representation. Option `-1` removes rules that do not terminate, and
removes all non terminals that are not used, then the resulting BKG is printed out to
the standard output. Option `-2` firstly does the same as option `-1`, and then removes
unreachable rules and symbols that are no longer used, then the resulting BKG is printed
out to the standard output.

## Description

Program removes unnesessary symbols from the context-free-grammar. Input grammar
is firstly validated, and when invalid grammar is detected, appropriate message
is shown and program is exited. When valid grammar is present on input, then based
on given arguments, we either show back loaded grammar, remove rules that do not 
terminate, or remove all unnessessary symbols and rules from grammar. Code is split 
into four files:

`Types.hs` - Contains definitions of types that we will be using in our program
and specify format in which we will be outputing BKG to the standard output
`ParseInput.hs` - Validates input string if it is valid grammar, and construct 
context free grammar data that we will be using in our program
`Minimize.hs` - Contains algorithms for simplification of context free grammar
`Main.hs` - Main program, that parses arguments and initiates operations based on 
given argument