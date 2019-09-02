{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "argonaut"
    , "console"
    , "effect"
    , "express"
    , "node-postgres"
    , "node-process"
    , "psci-support"
    , "quickcheck"
    , "simple-json"
    , "spec"
    , "spec-quickcheck"
    , "transformers"
    ]
, packages =
    ./packages.dhall
}
