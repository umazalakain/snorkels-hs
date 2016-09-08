Snorkels is a two person strategy board game. You can find the [rules
here][rules]. This is an implementation of Snorkels written in Haskell.

[rules]: http://nestorgames.com/rulebooks/SNORKELS_EN.pdf

It features:

- Support for up to 5 players.
- Colored output.
- Player switching.

Coming up are:

- Computer player agents.
- Network games.


# Installation

User-wide:

    cabal install
    cabal configure
    cabal copy

Sandboxed:

    cabal sandbox
    cabal install
    cabal configure
    cabal run
