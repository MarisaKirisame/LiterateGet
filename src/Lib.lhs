Let's write a library for conversion!

> {-# Language MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
> module Lib where

> class Get a b where
>   get :: a -> b

> instance Get a a where
>   get = id

And... That's it. All else is mere instance definition.

But we can do cool stuff with this though. Like, Optional Arguments.

> data Here = Here
> newtype Next a = Next { unNext :: a }
> data GetArg index originalIndex context x = GetArg index originalIndex context x

> getArg :: Get (GetArg a a b c) d => a -> b -> c -> d
> getArg a b c = get (GetArg a a b c)

> instance Get c e => Get (GetArg Here a b (c, d)) e where
>   get (GetArg Here a b (c, d)) = get c

Note that we get implicit conversion for parameters for free.

> instance Get (GetArg a b c e) f => Get (GetArg (Next a) b c (d, e)) f where
>   get (GetArg (Next a) b c (d, e)) = get (GetArg a b c e)

And some test case...

> data Cat = Kitty
> data Dog = Doggo
> data Snake = Noodle
> data HogNose = Hoggie
> data Mouse = Micky

Let's hope Disney doesnt see that line. I dont want anything to happen to my family :)

> instance Get HogNose Snake where
>   get Hoggie = Noodle
> data FullPet = FullPet Dog Snake Cat Mouse
> data FeedPet = FeedPet

> instance Get (GetArg a (Next Here) FeedPet ()) Dog where
>   get _ = Doggo

> instance Get (GetArg a (Next (Next Here)) FeedPet ()) Cat where
>   get _ = Kitty

> instance Get (GetArg a (Next (Next (Next Here))) FeedPet ()) Mouse where
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

> instance {-# OVERLAPPING #-} Get c e => Get (GetArg Here a b (c, d)) (Optional e) where
>   get (GetArg Here a b (c, d)) = Passed $ get c

Why do we reinvent Maybe? Cause we dont want such an instance for Maybe.
In general, to avoid confusion between type that control the search and ordinary type, we create newtype for them everytime, and avoid using isomorphic ordinary type.

> instance {-# OVERLAPPING #-} Get (GetArg a b c e) (Optional f) => Get (GetArg (Next a) b c (d, e)) (Optional f) where
>   get (GetArg (Next a) b c (d, e)) = get (GetArg a b c e)

> instance Get (GetArg a b c ()) (Optional d) where
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

> instance Get (GetArg a b c f) g => Get (GetArg a b c (WithName d e f)) g where
>   get (GetArg a b c (WithName d e f)) = get (GetArg a b c f)
> instance Get (GetArg a b c d) f => Get (GetArg a b c d) (Named e f) where
>   get = named . get
> instance {-# OVERLAPPING #-} Get (GetArg a b c f) (Named g h) => Get (GetArg a b c (WithName d e f)) (Named g h) where
>   get (GetArg a b c (WithName d e f)) = get (GetArg a b c f)
> instance Get (GetArg () a b c) (Optional d) where
>   get _ = Default
> instance {-# OVERLAPPING #-} Get (GetArg () a b ()) (Optional c) where
>   get _ = Default
> instance {-# OVERLAPPING #-} Get e g => Get (GetArg a b c (WithName d e f)) (Named d g) where
>   get (GetArg a b c (WithName d e f)) = named $ get e
> instance {-# OVERLAPPING #-} Get (GetArg (Next a) b c (d, e)) g => Get (GetArg (Next a) b c (d, e)) (Named f g) where
>   get = named . get
> instance {-# OVERLAPPING #-} Get c f => Get (GetArg Here a b (c, d)) (Named e f) where
>   get (GetArg Here a b (c, d)) = named $ get c
> instance {-# OVERLAPPING #-} Get e g => Get (GetArg a b c (WithName d e f)) (Named d (Optional g)) where
>   get (GetArg a b c (WithName d e f)) = named $ Passed $ get e
> instance {-# OVERLAPPING #-} Get c f => Get (GetArg Here a b (c, d)) (Named e (Optional f)) where
>   get (GetArg Here a b (c, d)) = named $ Passed $ get c

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
