{ name = "glapple"
, dependencies =
    [ "canvas"
    , "colors"
    , "indexed-monad"
    , "ordered-collections"
    , "record"
    , "safely"
    , "unordered-collections"
    ]
, packages = ./packages.dhall
, license = "MIT"
, sources = [ "src/**/*.purs" ]
, repository = "https://github.com/yukikurage/purescript-glapple"
}
