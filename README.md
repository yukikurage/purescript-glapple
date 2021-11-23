<!-- omit in toc -->
# purescript-glapple
- [Installation](#installation)
- [Documentation](#documentation)
## Installation
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
        "v1.0.0"
    }
...
```
and run

`spago install glapple`

## Documentation
In the [docs](https://github.com/yukikurage/purescript-glapple/tree/master/docs) directory.
