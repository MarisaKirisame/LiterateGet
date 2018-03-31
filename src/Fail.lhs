> {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DataKinds, FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables, KindSignatures, TypeFamilies #-}
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

> instance ok ~ False => TryGet a b ok where
>   tryGetSing _ _ = SFalse
>   tryGetVal _ _ = ()

> instance {-# OVERLAPPING #-} ok ~ True => TryGet a a ok where
>   tryGetSing _ _ = STrue
>   tryGetVal a _ = a

This is more general than the old interface.

> instance TryGet as from True => Get.Get as from where
>   get x = tryGetVal x (Proxy :: Proxy as)

And since the end user usually dont care about failure, we should still expose the Get.Get class to user.
Since it is filled with old inference rule (instance), let's define a new one, doing essentially the same, but with one inference rule (depending on TryGet)

> class Get as from where
>   get :: from -> as

> instance TryGet as from True => Get as from where
>   get x = tryGetVal x (Proxy :: Proxy as)

After we reify failure, Optional Parameters should be easier - just recurse, if we can get the result, use it, and return Default if we cant.

> data Optional a = Passed a | Default

> fromOptional :: a -> Optional a -> a
> fromOptional a Default = a
> fromOptional _ (Passed a) = a

> unOptionalP :: Proxy (Optional a) -> Proxy a
> unOptionalP _ = Proxy

> instance {-# OVERLAPPING #-} (ok ~ True, TryGet a b aOK) => TryGet (Optional a) b ok where 
>   tryGetSing _ _ = STrue
>   tryGetVal b p =
>     case tryGet b (unOptionalP p) of
>       (a, STrue) -> Passed a
>       (_, SFalse) -> Default

This time, let's try to ditch propositional parameters and require all parameter to be named, for simplicity and readability.

> newtype Named name a = Named { unName :: name -> a }
> named = Named . const
> fromName n (Named f) = f n
> data WithName name arg rest = WithName name arg rest

> namedAP :: Proxy (Named name a) -> Proxy a
> namedAP _ = Proxy
> withNameArgP :: Proxy (WithName name arg rest) -> Proxy arg
> withNameArgP _ = Proxy
> withNameRestP :: Proxy (WithName name arg rest) -> Proxy rest
> withNameRestP _ = Proxy

> instance {-# OVERLAPPING #-} TryGet (Named name a) d ok => TryGet (Named name a) (WithName b c d) ok where
>   tryGetSing fp ap = tryGetSing (withNameRestP fp) ap
>   tryGetVal (WithName _ _ r) ap = tryGetVal r ap

> instance {-# OVERLAPPING #-} TryGet a arg ok => TryGet (Named name a) (WithName name arg rest) ok where
>   tryGetSing fp ap = tryGetSing (withNameArgP fp) (namedAP ap)
>   tryGetVal (WithName _ f _) ap =
>     case tryGet f (namedAP ap) of
>       (a, STrue) -> named a
>       (_, SFalse) -> ()

> data Snake = Noodle
> data Hognose = Hoggie
> data Mouse = Micky
> data MySnake = MySnake
> data MyMouse = MyMouse
> data FullPet = FullPet Snake Mouse

> instance {-# OVERLAPPING #-} ok ~ True => TryGet Snake Hognose ok where
>   tryGetSing _ _ = STrue
>   tryGetVal _ _ = Noodle

Let's keep it simple this time, for it isn't our main topic.
And this time, we need 'Optional' in front of 'Named'.

> feedPet a = FullPet (fromName MySnake (get a)) (fromName MyMouse (fromOptional (named Micky) (get a)))

> FullPet _ _ = feedPet (WithName MySnake Hoggie ())
> FullPet _ _ = feedPet (WithName MyMouse Micky (WithName MySnake Hoggie ()))
> FullPet _ _ = feedPet (WithName MySnake Hoggie (WithName MyMouse Micky ()))

That's it. Much simpler than last attempt.
If we want to have propositional argument, just do it as a curried function (get named param from a, get all rest normally).
Personally, I recommend passing all defaultable arguments as the zeroth argument (called config), and all rest as usual.
And config can be passed down to subfunction to let caller config lower level detail.

But we only have WithName to build config, so it is hard to merge two conflict file (although admittingly I never found such use case).
Why dont we try to support merge, just for the sake of doing it?

> type family TFOr (l :: Bool) (r :: Bool) :: Bool
> type instance TFOr True _ = True
> type instance TFOr False r = r

> data And l r = And l r

> andLP :: Proxy (And l r) -> Proxy l
> andLP _ = Proxy
> andRP :: Proxy (And l r) -> Proxy r
> andRP _ = Proxy

> instance {-# OVERLAPPING #-} (TryGet a l lok, TryGet a r rok, ok ~ TFOr lok rok) => TryGet a (And l r) ok where
>   tryGetSing fp ap =
>       case lok of
>         STrue -> STrue
>         SFalse -> rok
>     where
>       lok = tryGetSing (andLP fp) ap
>       rok = tryGetSing (andRP fp) ap
>   tryGetVal (And l r) ap =
>     case lok of
>       STrue -> lval
>       SFalse -> rval
>     where
>       (lval, lok) = tryGet l ap
>       (rval, rok) = tryGet r ap

And for symmetry...
    
> type family TFAnd (l :: Bool) (r :: Bool) :: Bool
> type instance TFAnd True r = r
> type instance TFAnd False _ = False

> instance {-# OVERLAPPING #-} (TryGet l a lok, TryGet r a rok, ok ~ TFAnd lok rok) => TryGet (And l r) a ok where
>   tryGetSing fp ap =
>     case lok of
>       STrue -> rok
>       SFalse -> SFalse
>     where
>       lok = tryGetSing fp (andLP ap)
>       rok = tryGetSing fp (andRP ap)
>   tryGetVal f ap =
>     case (lok, rok) of
>       (STrue, STrue) -> And lval rval
>       (STrue, SFalse) -> ()
>       (SFalse, STrue) -> ()
>       (SFalse, SFalse) -> ()
>     where
>       (lval, lok) = tryGet f (andLP ap)
>       (rval, rok) = tryGet f (andRP ap)

If we can infer value 'a' from value 'b', and vice versa, we need to be able to get 'a' or 'b'.
That is a use case for union type.
To the mathematically inclined, if we see TryGet x y True as subtyping,
And/Or is the Intersection/Union Type, forming a distributed lattice up to mutual subtyping.

> data Or l r = OrL l | OrR r

> orLP :: Proxy (Or l r) -> Proxy l
> orLP _ = Proxy
> orRP :: Proxy (Or l r) -> Proxy r
> orRP _ = Proxy

> instance {-# OVERLAPPING #-} (TryGet a l lok, TryGet a r rok, ok ~ TFAnd lok rok) => TryGet a (Or l r) ok where
>   tryGetSing fp ap =
>       case lok of
>         STrue -> rok
>         SFalse -> SFalse
>     where
>       lok = tryGetSing (orLP fp) ap
>       rok = tryGetSing (orRP fp) ap
>   tryGetVal f ap =
>     case (lok, rok) of
>       (STrue, STrue) ->
>         case f of
>           OrL l -> tryGetVal l ap
>           OrR r -> tryGetVal r ap
>       (STrue, SFalse) -> ()
>       (SFalse, STrue) -> ()
>       (SFalse, SFalse) -> ()
>     where
>       lok = tryGetSing (orLP (fromVal f)) ap
>       rok = tryGetSing (orRP (fromVal f)) ap

> instance {-# OVERLAPPING #-} (TryGet l a lok, TryGet r a rok, ok ~ TFOr lok rok) => TryGet (Or l r) a ok where
>   tryGetSing fp ap =
>     case lok of
>       STrue -> STrue
>       SFalse -> rok
>     where
>       lok = tryGetSing fp (orLP ap)
>       rok = tryGetSing fp (orRP ap)
>   tryGetVal f ap =
>     case (lok, rok) of
>       (STrue, _) -> OrL lval
>       (SFalse, STrue) -> OrR rval
>       (SFalse, SFalse) -> ()
>     where
>       (lval, lok) = tryGet f (orLP ap)
>       (rval, rok) = tryGet f (orRP ap)

And now some boilerplate instance, to specify what to do when instance overlap.

> instance {-# OVERLAPPING #-} (TryGet l (And x y) lok, TryGet r (And x y) rok, ok ~ TFAnd lok rok) => TryGet (And l r) (And x y) ok where
>   tryGetSing fp ap =
>     case lok of
>       STrue -> rok
>       SFalse -> SFalse
>     where
>       lok = tryGetSing fp (andLP ap)
>       rok = tryGetSing fp (andRP ap)
>   tryGetVal f ap =
>     case (lok, rok) of
>       (STrue, STrue) -> And lval rval
>       (STrue, SFalse) -> ()
>       (SFalse, STrue) -> ()
>       (SFalse, SFalse) -> ()
>     where
>       (lval, lok) = tryGet f (andLP ap)
>       (rval, rok) = tryGet f (andRP ap)

> instance {-# OVERLAPPING #-} (TryGet l (Or x y) lok, TryGet r (Or x y) rok, ok ~ TFAnd lok rok) => TryGet (And l r) (Or x y) ok where
>   tryGetSing fp ap =
>     case lok of
>       STrue -> rok
>       SFalse -> SFalse
>     where
>       lok = tryGetSing fp (andLP ap)
>       rok = tryGetSing fp (andRP ap)
>   tryGetVal f ap =
>     case (lok, rok) of
>       (STrue, STrue) -> And lval rval
>       (STrue, SFalse) -> ()
>       (SFalse, STrue) -> ()
>       (SFalse, SFalse) -> ()
>     where
>       (lval, lok) = tryGet f (andLP ap)
>       (rval, rok) = tryGet f (andRP ap)

> instance {-# OVERLAPPING #-} (TryGet l (And x y) lok, TryGet r (And x y) rok, ok ~ TFOr lok rok) => TryGet (Or l r) (And x y) ok where
>   tryGetSing fp ap =
>     case lok of
>       STrue -> STrue
>       SFalse -> rok
>     where
>       lok = tryGetSing fp (orLP ap)
>       rok = tryGetSing fp (orRP ap)
>   tryGetVal f ap =
>     case (lok, rok) of
>       (STrue, _) -> OrL lval
>       (SFalse, STrue) -> OrR rval
>       (SFalse, SFalse) -> ()
>     where
>       (lval, lok) = tryGet f (orLP ap)
>       (rval, rok) = tryGet f (orRP ap)

> instance {-# OVERLAPPING #-} (TryGet (Or x y) l lok, TryGet (Or x y) r rok, ok ~ TFAnd lok rok) => TryGet (Or x y) (Or l r) ok where
>   tryGetSing fp ap =
>       case lok of
>         STrue -> rok
>         SFalse -> SFalse
>     where
>       lok = tryGetSing (orLP fp) ap
>       rok = tryGetSing (orRP fp) ap
>   tryGetVal f ap =
>     case (lok, rok) of
>       (STrue, STrue) ->
>         case f of
>           OrL l -> tryGetVal l ap
>           OrR r -> tryGetVal r ap
>       (STrue, SFalse) -> ()
>       (SFalse, STrue) -> ()
>       (SFalse, SFalse) -> ()
>     where
>       lok = tryGetSing (orLP (fromVal f)) ap
>       rok = tryGetSing (orRP (fromVal f)) ap

> instance {-# OVERLAPPING #-} (ok ~ True, TryGet a (And l r) aOK) => TryGet (Optional a) (And l r) ok where 
>   tryGetSing _ _ = STrue
>   tryGetVal b p =
>     case tryGet b (unOptionalP p) of
>       (a, STrue) -> Passed a
>       (_, SFalse) -> Default

And a hypothetical use case, where we need to accept one of celsius or kelvin for config, default to celsius 0,
alongside with a time.

> data Celsius = Celsius Double
> data Kelvin = Kelvin Double

> data Time = Time Double

> data TT = TT (Either Celsius Kelvin) Time

> getTT a =
>   case (get a) of
>     And t Default -> TT (Left $ Celsius 0) t
>     And t (Passed (OrL c)) -> TT (Left c) t
>     And t (Passed (OrR k)) -> TT (Right k) t

> TT _ _ = getTT (Time 0)
> TT _ _ = getTT (And (Kelvin 0) (Time 0))

That's it. Although we dont have lots of use for this, it is the basic building block for Recursion.lhs.
