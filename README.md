# Elm Monkey Interpreter

A [Monkey](https://monkeylang.org/) interpreter written in
[Elm](https://elm-lang.org/).

Monkey is a programming language designed by
[Thorsten Ball](https://thorstenball.com/) that is fully described in his
[interpreter](https://interpreterbook.com/) book.

## Syntax

The syntax of Monkey is scattered throughout the pages of the book. However, I
extracted a [context-free grammar](grammar.ebnf) that you can use to learn the
syntax.

```
let filter = fn (pred, arr) {
  let iter = fn (arr, accumulated) {
    if (len(arr) == 0) {
      accumulated
    } else {
      if (pred(first(arr))) {
        iter(rest(arr), push(accumulated, first(arr)))
      } else {
        iter(rest(arr), accumulated)
      }
    }
  };
  iter(arr, [])
};

let mod = fn (a, b) {
  a - a / b * b
};

let isEven = fn (n) {
  mod(n, 2) == 0
};

filter(isEven, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
```

You can also use the [parser tests](tests/Test/Monkey/Parser.elm) as a guide.

## Semantics

The semantics is defined by the [interpreter](src/Monkey/Interpreter.elm). You
can get a sense for it by reading through the extensive suite of
[interpreter tests](tests/Test/Monkey/Interpreter.elm).
