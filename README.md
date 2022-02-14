# [Paml](https://github.com/adampsz/paml)

## What is it?

Paml is a parser combinator library written in Ocaml, inspired by libraries like [Parsec] or [Megaparsec].

## Features

Main features of Paml are:

- ability to parse `string` inputs, as well as any custom data type (after implementing `Paml.T.Input` module)
- monadic interface
- many combinators, including `choice`, `many`, `some`, `option`, `default`, `chain`, ...
- error handling and related combinators, including `label`, `expect`, `catch`, ..., and error pretty-printing:

  ```
  File "<unnamed>", line 1, character 44:

   1 | length([_|T], N) :- length(T, M), N is M + @.
     |                                            ^

  Error: expected `!', `_', float, integer, list, symbol or variable
  ```

- easy debugging (`tap`, `trace`)
- manipulating state of any type,
- error recovery (`report`, `rollback`, `recover`)
- support for indentation-sensitive parsers
- build-in Pratt parser for expressions with operator precedence

## Installing

Currently, only installation using [dune] is possible, as `paml` is not published to opam registry. To do it, type:

```sh
  $ dune build
  $ dune install
```

## Documentation and examples

To build documentation, install [odoc] and type following command:

```sh
  $ dune build @doc
```

Generated documentation will be available at `./_build/default/_doc/_html/`.

Some examples are located in `./examples` directory. To build and run them type following commands:

```sh
  $ dune build @examples
  $ rlwrap ./_build/default/examples/[name].exe
```

## What does 'Paml' mean?

**Pa**_rser combinators in Oc_**aml**, but with single 'a'.

[parsec]: https://github.com/haskell/parsec
[megaparsec]: https://github.com/mrkkrp/megaparsec
[dune]: https://dune.build/
[odoc]: https://github.com/ocaml/odoc
