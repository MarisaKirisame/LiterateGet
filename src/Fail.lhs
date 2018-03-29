> {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DataKinds, FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}
> module Fail where

> import qualified Get as Get -- You should read it before reading this
> import Data.Proxy
> import Data.Singletons.Prelude.Bool

So last time, we build a Get class and show we can do optional/named parameters with implicit conversion.

But if we look at what we write, there are room for improvement.

To be more specific, all Optional do, is to not have compile error when the search fail, and do something else (use a default value) instead.

So, we can reify 'does the search succeed' into a variable (at class level, that is, a type variable).

> fromVal :: a -> Proxy a
> fromVal _ = Proxy
    
> class TryGet as from ok | as from -> ok where
>   tryGetSing :: Proxy from -> Proxy as -> Sing ok
>   tryGetVal :: from -> Proxy as -> If ok as ()
>   tryGet :: from -> Proxy as -> (If ok as (), Sing ok)
>   tryGet from p = (tryGetVal from p, tryGetSing (fromVal from) p)

This is more general than the old interface.

> instance TryGet as from True => Get.Get from as where
>   get x = tryGetVal x (Proxy :: Proxy as)

