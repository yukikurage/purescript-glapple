{ name = "glapple"
, dependencies =
    [ "canvas"
    , "colors"
    , "console"
    , "debug"
    , "random"
    , "safely"
    , "unordered-collections"
    ]
, packages = ./packages.dhall
, license = "MIT"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, repository = "https://github.com/yukikurage/purescript-glapple"
}
