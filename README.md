# servant-flatten

Utilities for flattening servant API types

The main function from this library is:

``` haskell
flatten :: Proxy api -> Proxy (Flat api)
```

Its purpose is to "flatten" an API type, by distributing
any factored combinators, so as to end up with completely
flat endpoint descriptions, separated by `:<|>`s.

For example, it turns:

``` haskell
type API = Capture "foo" Int :>
  ( Capture "bar" String :>
      ( Get '[JSON] String :<|>
        ReqBody '[JSON] Int :> Post '[JSON] Int
      ) :<|>
    Get '[JSON] Int
  ) :<|>
  Get '[JSON] [String]
```

into:

``` haskell
Capture "foo" Int :> Capture "bar" String :> Get '[JSON] String :<|>
Capture "foo" Int :> Capture "bar" String :> ReqBody '[JSON] Int :> Post '[JSON] Int :<|>
Capture "foo" Int :> Get '[JSON] Int :<|>
Get '[JSON] [String]
```

See the documentation of `flatten` in `Servant.API.Flatten`
for more.
