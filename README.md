<!-- omit in toc -->
# purescript-glapple
### A Canvas-based game creation library
This library can be used to control the division and render of the game state.

<!-- omit in toc -->
## TOC
- [Installation with Spago](#installation-with-spago)
- [Documentation](#documentation)
## Installation with Spago
in `packages.dhall`
```dhall
...
let upstream = ...
in  upstream
  with glapple =
    { dependencies =
        [ "aff"
        , "arrays"
        , "canvas"
        , "colors"
        , "console"
        , "datetime"
        , "effect"
        , "either"
        , "foldable-traversable"
        , "integers"
        , "math"
        , "maybe"
        , "now"
        , "ordered-collections"
        , "prelude"
        , "psci-support"
        , "refs"
        , "safely"
        , "tailrec"
        , "transformers"
        , "tuples"
        , "unsafe-coerce"
        , "web-events"
        , "web-html"
        , "web-uievents"
        ]
    , repo =
        "https://github.com/yukikurage/purescript-glapple"
    , version =
        "v1.0.1"
    }
...
```
and run

`spago install glapple`

## Documentation
- Docs in the [`docs`](https://github.com/yukikurage/purescript-glapple/tree/master/docs) directory.
- API reference can be configured locally with command:
```sh
spago docs
```
- You can see an example that actually works in Repository (glapple)[https://github.com/yukikurage/glapple].
