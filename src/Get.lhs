Let's write a library for conversion!

> {-# Language MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
> module Get where

> class Get as from where
>   get :: from -> as

> instance Get a a where
>   get = id

And... That's it. All else is mere instance definition.

But we can do cool stuff with this though. Like, Optional Arguments.

> data Here = Here
> newtype Next a = Next { unNext :: a }
> data GetArg index originalIndex context x = GetArg index originalIndex context x

> getArg :: Get a (GetArg b b c d) => b -> c -> d -> a
> getArg b c d = get (GetArg b b c d)

> instance Get a d => Get a (GetArg Here b c (d, e)) where
>   get (GetArg Here b c (d, e)) = get d

Note that we get implicit conversion for parameters for free.

> instance Get a (GetArg b c d e) => Get a (GetArg (Next b) c d (e, f)) where
>   get (GetArg (Next b) c d (e, f)) = get (GetArg b c d e)

And some test case...

> data Cat = Kitty
> data Dog = Doggo
> data Snake = Noodle
> data HogNose = Hoggie
> data Mouse = Micky

Let's hope Disney doesnt see that line. I dont want anything to happen to my family :)

> instance Get Snake HogNose where
>   get Hoggie = Noodle
> data FullPet = FullPet Dog Snake Cat Mouse
> data FeedPet = FeedPet

> instance Get Dog (GetArg a (Next Here) FeedPet ()) where
>   get _ = Doggo

> instance Get Cat (GetArg a (Next (Next Here)) FeedPet ()) where
>   get _ = Kitty

> instance Get Mouse (GetArg a (Next (Next (Next Here))) FeedPet ()) where
>   get _ = Micky

> feedPet a =
>   FullPet
>     (getArg (Next Here) FeedPet a)
>     (getArg Here FeedPet a)
>     (getArg (Next $ Next Here) FeedPet a)
>     (getArg (Next $ Next $ Next Here) FeedPet a)

Haskell infer the right type. You can check it at ghci but the constraint is a mess though.

> FullPet _ _ _ _ = feedPet (Hoggie, ())
> FullPet _ _ _ _ = feedPet (Hoggie, (Doggo, ()))
> FullPet _ _ _ _ = feedPet (Hoggie, (Doggo, (Kitty, ())))
> FullPet _ _ _ _ = feedPet (Hoggie, (Doggo, (Kitty, (Micky, ()))))

A bit tiresome. For every function wanting optional parameters, we have to get a newtype for it's functionContext, make an instance for every optional parameters, with hard coded original index in.
If we rearrange the parameters, we also need to rearrange the original Index. Not cool.

Let's deal with all optional parameters at once instead.

> data Optional a = Passed a | Default
> optional _ f (Passed a) = f a
> optional a _ Default = a
> fromOptional _ (Passed a) = a
> fromOptional a Default = a

> instance {-# OVERLAPPING #-} Get a d => Get (Optional a) (GetArg Here b c (d, e)) where
>   get (GetArg Here b c (d, e)) = Passed $ get d

Why do we reinvent Maybe? Cause we dont want such an instance for Maybe.
In general, to avoid confusion between type that control the search and ordinary type, we create newtype for them everytime, and avoid using isomorphic ordinary type.

> instance {-# OVERLAPPING #-} Get (Optional a) (GetArg b c d f) => Get (Optional a) (GetArg (Next b) c d (e, f)) where
>   get (GetArg (Next b) c d (e, f)) = get (GetArg b c d f)

> instance Get (Optional a) (GetArg b c d ()) where
>   get _ = Default

> feedPetOptional a =
>   FullPet
>     (fromOptional Doggo (getArg (Next Here) FeedPet a))
>     (getArg Here FeedPet a)
>     (fromOptional Kitty (getArg (Next (Next Here)) FeedPet a))
>     (fromOptional Micky (getArg (Next (Next (Next Here))) FeedPet a))

> FullPet _ _ _ _ = feedPetOptional (Hoggie, ())
> FullPet _ _ _ _ = feedPetOptional (Hoggie, (Doggo, ()))
> FullPet _ _ _ _ = feedPetOptional (Hoggie, (Doggo, (Kitty, ())))
> FullPet _ _ _ _ = feedPetOptional (Hoggie, (Doggo, (Kitty, (Micky, ()))))

Note that only two type of GetArg is used: 'index' and 'x'. 'originalIndex' and 'context' is not used anymore, and we can remove them if we rewrite the library. 
It is straightforward, physical work so I wont bore you with details. 

We can also have Named Arguments.

> newtype Named name a = Named { unNamed :: name -> a }
> named = Named . const
> fromName n (Named f) = f n
> data WithName name arg rest = WithName name arg rest

We require the caller to keep 'Named' before 'Optional' to simplify stuff.

instance Get a (GetArg b c d g) => Get a (GetArg b c d (WithName e f g)) where
  get (GetArg b c d (WithName e f g)) = get (GetArg b c d g)

> instance Get b (GetArg c d e f) => Get (Named a b) (GetArg c d e f) where
>   get = named . get
> instance {-# OVERLAPPING #-} Get (Named a b) (GetArg c d e h) => Get (Named a b) (GetArg c d e (WithName f g h)) where
>   get (GetArg c d e (WithName f g h)) = get (GetArg c d e h)
> instance Get (Optional a) (GetArg () b c d) where
>   get _ = Default
> instance {-# OVERLAPPING #-} Get (Optional a) (GetArg () b c ()) where
>   get _ = Default
> instance {-# OVERLAPPING #-} Get b f => Get (Named a b) (GetArg c d e (WithName a f g)) where
>   get (GetArg c d e (WithName a f g)) = named $ get f
> instance {-# OVERLAPPING #-} Get b (GetArg (Next c) d e (f, g)) => Get (Named a b) (GetArg (Next c) d e (f, g)) where
>   get = named . get
> instance {-# OVERLAPPING #-} Get b e => Get (Named a b) (GetArg Here c d (e, f))  where
>   get (GetArg Here c d (e, f)) = named $ get e
> instance {-# OVERLAPPING #-} Get b f => Get (Named a (Optional b)) (GetArg c d e (WithName a f g)) where
>   get (GetArg c d e (WithName a f g)) = named $ Passed $ get f
> instance {-# OVERLAPPING #-} Get b e => Get (Named a (Optional b)) (GetArg Here c d (e, f)) where
>   get (GetArg Here c d (e, f)) = named $ Passed $ get e

Just some boilerplate instance. Nothing interesting.

Let's have dog named only, snake propositional named, cat propositional named optional, and mouse named optional.
Snake come before cat, and both mouse and dog cannot be passed propositionally.

> data DoesntMatter = DM
> data MyDog = MyDog
> data MySnake = MySnake
> data MyCat = MyCat
> data MyMouse = MyMouse

> feedPetNamed a =
>   FullPet
>     (fromName MyDog (getArg () DM a))
>     (fromName MySnake (getArg Here DM a))
>     (fromOptional Kitty $ fromName MyCat (getArg (Next Here) DM a))
>     (fromOptional Micky $ fromName MyMouse (getArg () DM a))

> FullPet _ _ _ _ = feedPetNamed (WithName MyDog Doggo (Hoggie, ()))
> FullPet _ _ _ _ = feedPetNamed (WithName MyDog Doggo $ WithName MySnake Hoggie ())
> FullPet _ _ _ _ = feedPetNamed (WithName MyDog Doggo (Hoggie, (Kitty, ())))
> FullPet _ _ _ _ = feedPetNamed (WithName MyCat Kitty $ WithName MyDog Doggo (Hoggie, ()))
> FullPet _ _ _ _ = feedPetNamed (WithName MyDog Doggo $ WithName MyMouse Micky (Hoggie, ()))
> FullPet _ _ _ _ = feedPetNamed (WithName MyMouse Micky $ WithName MyDog Doggo (Hoggie, ()))

Another option is to have every argument named, possibly optional, never propositional, so that the ugly index will be gone.
It also dont require GetArg anymore.
