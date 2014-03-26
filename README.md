lambdainterpreter_haskell
=========================

A very simplistic lambda interpreter.

# Requirements

* Parsec
* ansi-terminal

Both should be available via cabal.

# Compilation

Usually, a simple `ghc LambdaInterpreter -outputdir bin` should be enough.

# Usage

Just invoke `./LambdaInterpreter` with an additional filename containing your definitions (e.g. `./LambdaInterpreter --filename default.lambda`).

To make the user experience nicer, I encourage you to use the little tool `rlwrap`.
