{ name = "glapple"
, dependencies =
    [ "canvas"
    , "colors"
    , "safely"
    , "unordered-collections"

    -- devDependencies
    , "console"
    , "debug"
    ]
, packages = ./packages.dhall
, license = "MIT"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, repository = "https://github.com/yukikurage/purescript-glapple"
}
