{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.API.Flatten where

import Data.Proxy
import GHC.TypeLits
import Servant.API

-- | Flatten (a 'Proxy' to) an API type.
--
--   This performs a number of transformations on the API type so
--   as to end up with all combinators distributed over endpoints.
--   For example, the following API type:
--
--   @
--   type API = 'Capture' "foo" 'Int' ':>'
--     ( 'Capture' "bar" 'String' ':>'
--         ( 'Get' '['JSON'] 'String' ':<|>'
--           'ReqBody' '['JSON'] 'Int' ':>' 'Post' '['JSON'] 'Int'
--         ) ':<|>'
--       'Get' '['JSON'] 'Int'
--     ) ':<|>'
--     'Get' '['JSON'] ['String']
--   @
--
--   gets transformed into:
--
--   @
--   'Capture' "foo" 'Int' ':>' 'Capture' "bar" 'String' ':>' 'Get' '['JSON'] 'String' ':<|>'
--   'Capture' "foo" 'Int' ':>' 'Capture' "bar" 'String' ':>' 'ReqBody' '[JSON] 'Int' ':>' 'Post' '['JSON'] 'Int' ':<|>'
--   'Capture' "foo" 'Int' ':>' 'Get' '['JSON'] 'Int' ':<|>'
--   'Get' '['JSON'] ['String']
--   @
--
--   The main point of doing this is to avoid \"nested types\" for server-side handlers
--   and client functions. See <https://haskell-servant.readthedocs.io/en/stable/cookbook/structuring-apis/StructuringApis.html#structuring-apis this cookbook recipe>
--   (particularly the notes on @FactoringAPI@) for more about \"nested types\".
--
--   To derive \"flat\" client functions for the API type above, @API@, you can do:
--
--   @
--   getfoobar ':<|>' postfoobar ':<|>' getfoo ':<|>' getstrings
--     = 'client' $ 'flatten' ('Proxy' :: 'Proxy' API)
--   @
--
--   To serve an implementation for that API with \"flat\" handler types, you can do:
--
--   @
--   -- we define all our handlers assuming all the arguments are distributed,
--   -- and declare that this is an implementation for @Flat API@, not @API@.
--   server :: Server ('Flat' API)
--   server = (\foo bar -> return $ show (foo + bar))
--       ':<|>' (\foo bar body -> return $ show (foo + bar - body^2))
--       ':<|>' (\foo -> return (foo * 2))
--       ':<|>' (return ["hello", "world"])
--
--   api :: 'Proxy' API
--   api = 'Proxy'
--
--   main :: 'IO' ()
--   main = Network.Wai.Handler.Warp.run 8080 $
--     serve ('flatten' api) server
--   @
flatten :: Proxy api -> Proxy (Flat api)
flatten Proxy = Proxy

-- | Flatten and transform the API type a little bit.
type Flat api = Reassoc (Flatten api)
-- looks like Flatten/Reassoc are missing some opportunities the first time,
-- so we apply them twice for now...

-- | Completely flattens an API type by applying a few simple transformations.
--   The goal is to end up with an API type where things like @a ':>' (b ':<|>' c)@
--   are rewritten to @a ':>' b ':<|>' a ':>' c@, so as to have client with very simple
--   types, instead of "nested clients".
type family Flatten (api :: k) :: k where
  Flatten ((a :: k) :> (b :<|> c)) = Flatten (a :> b) :<|> Flatten (a :> c)
  Flatten ((a :: k) :> b)          = Redex b (Flatten b) a
  Flatten (a :<|> b)               = Flatten a :<|> Flatten b
  Flatten (a :: k)                 = a

type family Redex a b (c :: k) :: * where
  Redex a a first = Flatten first :> a
  Redex a b first = Flatten (first :> b)

-- | Reassociates ':<|>' to the right.
type Reassoc api = ReassocBranch api '[]

-- | Helper type family that "enumerates" the different endpoints left
--   to right.
type family ReassocBranch (currentAPI :: *) (otherEndpoints :: [*]) where
  ReassocBranch (a :<|> b)        rest = ReassocBranch a (b ': rest)
  ReassocBranch a                  '[] = a
  ReassocBranch a          (b ': rest) = a :<|> ReassocBranch b rest

-- * Utilities that we can define on a flat representation

-- | Get the endpoints with given indices in the all-flat
--   representation of the API type, glueing them together
--   with ':<|>'.
type family Nths (idxs :: [Nat]) api where
  Nths  '[i]      api = Nth i api
  Nths  (i ': is) api = Nth i api :<|> Nths is api

-- | Get the endpoint with given index in the all-flat representation
--   of the API type.
type family Nth (i :: Nat) api where
  Nth 0 (a :<|> b) = a
  Nth 0 a          = a
  Nth n (a :<|> b) = Nth (n - 1) b
