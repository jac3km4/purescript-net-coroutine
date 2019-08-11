
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.2-20190804/packages.dhall sha256:2230fc547841b54bca815eb0058414aa03ed7b675042f8b3dda644e1952824e5

let overrides = {=}

let additions =
    { bytestring =
          { dependencies =
              [ "effect", "catenable-lists", "node-buffer", "functions" ]
          , repo =
              "https://github.com/jac3km4/purescript-bytestring.git"
          , version =
              "v0.0.1"
          }
    }

in  upstream // overrides // additions
