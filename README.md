Fraxl
---

[Documentation](http://elvishjerricco.github.io/fraxl/fraxl-0.1.0.0/)

Fraxl is a library based on Facebook's [Haxl](https://github.com/facebook/Haxl).
The goal is to decompose Haxl into more general parts,
in order to form a stronger composition with better type safety and purity.

Usage
---

Using Fraxl is fairly similar to Haxl.
You define a request data type (often a GADT), and a `DataSource` instance.
With this, Fraxl is able to perform requests concurrently.

```haskell
data MySource a where
  MyString :: MySource String
  MyInt :: MySource Int

class Monad m => DataSource f m
  fetch :: ASeq f a -> m (ASeq m a)

instance MonadIO m => DataSource MySource m where
  fetch ANil = return ANil
  fetch (ACons f fs) = ((:&) . liftIO . wait) <$> liftIO (async $ downloadSource f) <*> fetch fs
```

You'll notice a few things here.
For one, a data source can choose what monad it lives in.
Unlike Haxl, which only lets you live in `IO`,
Fraxl is a monad transformer, allowing you to use arbitrary underlying monads.
Thus, maintaining state between fetches can be left up to the data source.
This can be used for several things, such as caching or session management.

`ASeq :: (* -> *) -> * -> *` is similar to a heterogenous list.
It is the data structure used by the fast free applicative.
Interpreting this is akin to interpreting the free applicative.

The `fetch` method takes a list of `f` requests,
and for each request, returns an `m` action that waits on the response.
That is, `fetch` should start background threads for requests,
and return all the actions for Fraxl to block with until they complete.
This way, Fraxl can have many requests start their work in parallel,
and call all their wait-actions together.

Composition
---

Fraxl is a composition of general tools.
At the base of this composition is a free monad transformer
([the basis of which is described here](http://elvishjerricco.github.io/2016/04/08/applicative-effects-in-free-monads.html)).
This is because Fraxl (and Haxl) is necessarily a free monad.
It's taking arbitrary data sources of kind `* -> *`,
and constructing a monad out of them.
Since there exists a free monad transformer with applicative optimization,
there's no reason not to use it and get the transformer structure for free.

**NOTE:** *This free monad is not in the latest version of `free` yet.
So `free` is used as a git submodule to gain access to it.*

The next layer of the composition is the free applicative.
The free monad with applicative optimization uses any applicative
(rather than any functor, as with the traditional free monad).
Since the free applicative uses any type of kind `* -> *`,
it is the perfect candidate for this layer.
It allows Fraxl to see all the requests made in
an applicative computation at once, which is how Fraxl can parallelize them.

The final layer is the data source layer.
It is user-specified, but will often be a dependent open union.
The dependent open union is essentially a nested either type
over any number of types.
If all of those types are data sources, the dependent open union allows
Fraxl to handle all of them as one data source, in one layer of Fraxl.
The nice thing about this is that it makes it type safe to use a data source.
Whereas Haxl will simply trust that you know what you're doing,
Fraxl will make it a type error to forget to initialize a data source,
or call a computation without guaranteeing its data source is available.

The data source layer can be easily modified.
Caching is a substitution of this layer that replaces the data
source with one that caches the results of the original.
It does this with a dependent map, whose keys are requests,
and whose values are `MVars` of the results.
If an uncached request is requested,
an empty `MVar` is inserted into the cache map, the original `fetch` is called,
and the result is stored in the `MVar`.
If a cached request is requested,
the wait-action returned will simply be `readMVar`.

---

Check out [the example](examples/src/Main.hs) for a demonstration.
