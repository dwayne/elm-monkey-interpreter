# Elm Monkey Interpreter ([Playground](https://elm-monkey-interpreter.netlify.app/))

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

## Notes

These are some notes to highlight interesting aspects of the project and things
I learned or rediscovered along the way.

TL;DR

- Write context-free grammars for your syntax.
- Separate lexical and syntax analysis.
- Use lexeme parsers.
- ...

### Context-Free Grammar

[grammar.ebnf](grammar.ebnf)

Parser combinators are really convenient for writing parsers. Even so, having
an explicit grammar is still a boon for understanding and documenting the syntax
you want to parse.

There were certain tricky aspects of the syntax where the grammar guided me to
a correct solution of the problem. For example, the `Term`, `Unary`, and
`Operator` productions we very interesting to learn how to parse from
first-principles (i.e. without using pre-existing library functions).

In particular, I had to learn how to eliminate left-recursion in a grammar.
When you eliminate the left-recursion you lose the intended meaning of the
production. For e.g. if you wanted left-associativity you instead get
right-associativity and you have to account for that somehow. I figured out how
to account for it and rediscovered `chainl` in the process, which I called
`binaryLeftAssoc`.

- Fokker, Jeroen. "Functional parsers." International School on Advanced Functional Programming. Springer, Berlin, Heidelberg, 1995.

### Lexical Analysis

[Lexer.elm](src/Monkey/Lexer.elm)

Even with parser combinators it remains useful to separate the lexical analysis
from the syntax analysis. I choose to put all my lexical related combinators
into a separate module. Any combinator that worked at the character level or
cared about trailing whitespace went into that module.

The `lexeme` parser is the saving grace of the module. Every exported function
is a lexeme parser. What that means is that every exported parser handles
trailing whitespace so consumers don't have to. This is tremendously useful if
you don't want to litter the `spaces` parser all over the place. The only time I
use `spaces` in [Parser.elm](src/Monkey/Parser.elm) is to remove any leading
whitespace in the overall program.

Parsec has this concept of lexeme parsers. You get them when you use the
`Text.Parsec.Token` module.

> Every lexical parser from the `ParsecToken` module will skip whitespace after each symbol parsed; parsers which skip trailing whitespace are called lexeme parsers (the lexeme combinator can be used to define them). By skipping trailing whitespace, it is guaranteed that every parser starts at valid input.

- Leijen, Daan. "Parsec, a fast combinator parser." (2001).

### Syntax Analysis

[Parser.elm](src/Monkey/Parser.elm)

WIP

### Evaluation

[Interpreter.elm](src/Monkey/Interpreter.elm), [Eval.elm](src/Monkey/Eval.elm)

WIP

### Testing

[tests](tests/Test/Monkey)

WIP

### My thoughts on the book and the Monkey Programming Language

WIP
