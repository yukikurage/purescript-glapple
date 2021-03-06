<div>
<img src="https://user-images.githubusercontent.com/55534323/143211862-76a070be-e8d5-44b5-817f-7fbc5f9a76a2.png" style="height:150px;width:150px;">
</div>

<!-- omit in toc -->

# purescript-glapple

### A Canvas-based game creation library

This library can be used to control the division and render of the game state.

<!-- omit in toc -->

## TOC

- [purescript-glapple](#purescript-glapple)
    - [A Canvas-based game creation library](#a-canvas-based-game-creation-library)
  - [TOC](#toc)
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
      , "aff-promise"
      , "arrays"
      , "canvas"
      , "colors"
      , "console"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "integers"
      , "js-date"
      , "math"
      , "maybe"
      , "numbers"
      , "ordered-collections"
      , "prelude"
      , "random"
      , "refs"
      , "safely"
      , "tailrec"
      , "transformers"
      , "tuples"
      , "unsafe-coerce"
      , "web-dom"
      , "web-events"
      , "web-html"
      , "web-uievents"
      ]
    , repo =
        "https://github.com/yukikurage/purescript-glapple"
    , version =
        "v2.1.0"
    }
...
```

and run

`spago install glapple`

## Documentation

- API reference is published on [Pursuit](https://pursuit.purescript.org/packages/purescript-glapple)
- You can see an example that actually works in Repository [`glapple-examples`](https://github.com/yukikurage/glapple-examples).
