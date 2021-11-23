{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "purescript-glapple"
, dependencies =
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
, packages = ./packages.dhall
, license = "MIT"
, sources = [ "src/**/*.purs" ]
, repository = "https://github.com/yukikurage/purescript-glapple"
}
