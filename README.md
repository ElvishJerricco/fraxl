Fraxl
---

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
  fetch :: f a -> m (m a)

instance MonadIO m => DataSource MySource m where
  fetch a = do
    asyncOp <- liftIO $ async $ downloadSource a
    return (wait asyncOp)
```

You'll notice a few things here.
For one, a data source can choose what monad it lives in.
Unlike Haxl, which only lets you live in `IO`,
Fraxl is a monad transformer, allowing you to use arbitrary underlying monads.
Thus, maintaining state between fetches can be left up to the data source.

The `fetch` method takes a request and returns a way to wait on the result.
That is, `fetch` should start a background thread,
and return a way for Fraxl to block until it completes.
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
It does this with a dependent map.

---

Check out [the example](examples/src/Main.hs) for a demonstration.
