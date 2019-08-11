{ name =
    "net-coroutine"
, dependencies =
    [ "effect"
    , "console"
    , "node-net"
    , "aff"
    , "exceptions"
    , "coroutines"
    , "aff-coroutines"
    , "bytestring"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
