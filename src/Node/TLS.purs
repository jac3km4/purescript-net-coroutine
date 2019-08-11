module Node.TLS where
import Prelude

import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFn2, EffectFnAff, fromEffectFnAff, mkEffectFn2)
import Effect.Exception (Error)
import Node.Net.Socket (Socket)

type NativeTLSClientOptions =
  { enableTrace :: Boolean
  , rejectUnauthorized :: Boolean
  , checkServerIdentity :: Nullable (EffectFn2 String String (Nullable Error))
  }

type TLSClientOptions =
  { enableTrace :: Boolean
  , rejectUnauthorized :: Boolean
  , checkServerIdentity :: Maybe (String -> String -> Effect (Nullable Error))
  }

foreign import _connect :: Fn3 Int String NativeTLSClientOptions (EffectFnAff Socket)

connect :: Int -> String -> TLSClientOptions -> Aff Socket
connect port host options =
  fromEffectFnAff $ runFn3 _connect port host native
  where
    native =
      { enableTrace: options.enableTrace
      , rejectUnauthorized: options.rejectUnauthorized
      , checkServerIdentity: toNullable $ mkEffectFn2 <$> options.checkServerIdentity
      }

clientDefaults :: TLSClientOptions
clientDefaults =
  { enableTrace: false
  , rejectUnauthorized: true
  , checkServerIdentity: Nothing
  }
