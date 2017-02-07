Combining Effectful Interpreters
===

This is one method to combine separate free algebras and their (monadic) interpreters.

This is a literate Haskell document. You can run it as is through `ghc`. IE: `stack runghc article.lhs`. As such we need some boilerplate to get everything running:

> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE FlexibleContexts #-}
>
> import Prelude hiding ((.))
>
> import Control.Category ((.))
> import Control.Monad.Free
> import Control.Monad.IO.Class
> import Control.Monad.State
> import Control.Natural
> import Data.Functor.Identity
>
> import Data.Comp
> import Data.Comp.Ops
>

The end result will be to do things like this:

> main = do
>   putStrLn "Hello, world"

Lets define some Types for a simple DB with two tables: User and Item

> data User = User
>   { name :: String
>   ,  age :: Int
>   ,  items :: [Item]
>   ,  userDBId :: Int
>   } deriving (Show)

> data Item = Item
>   { title :: String
>   , stuff :: String
>   , itemDBId :: Int
>   } deriving (Show)

Here is an algebra we can use with `Free`:

> data DBRequest a
>   = GetUserById Int (User -> a)
>   | GetUserByName String (Maybe User -> a)
>   | GetItemById Int (Item -> a)
>   deriving (Functor)

These are some helper functions to make our algebra work with other algebras. Standard compdata usage. See [Data types à la carte](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/div-classtitledata-types-a-la-cartediv/14416CB20C4637164EA9F77097909409) to learn about the principals used.

> getUserById ::
>   (Functor f, MonadFree f m, DBRequest :<: f)
>   => Int -> m User
> getUserById dbid = liftF . inj $ GetUserById dbid id
>
> getUserByName ::
>   (Functor f, MonadFree f m, DBRequest :<: f)
>   => String -> m (Maybe User)
> getUserByName name = liftF . inj $ GetUserByName name id
>
> getItemById ::
>   (Functor f, MonadFree f m, DBRequest :<: f)
>   => Int -> m Item
> getItemById dbid = liftF . inj $ GetItemById dbid id

Ok, let's emulate the DB here so we can pretend we don't call the DB through IO.

> data DB a = DB
>   { unDB :: IO a
>   } deriving (Functor)
>
> instance Applicative DB where
>   pure a = DB $ pure a
>   (DB f) <*> (DB a) = DB $ f <*> a
> instance Monad DB where
>   (DB a) >>= f = DB $ a >>= \x -> unDB (f x)
> instance MonadIO DB where
>   liftIO = DB
>
> dbUserById :: Int -> DB User
> dbUserById id = pure $ User "Frank" id [] id
>
> dbUserByName :: String -> DB User
> dbUserByName name = pure $ User name 13 [] 13
>
> dbItemById :: Int -> DB Item
> dbItemById id = pure $ Item "Thingy" "stuffs" id

And a simple free program based on this algebra:

> getUsersName ::
>   (Functor f, MonadFree f m, DBRequest :<: f)
>   => Int -> m String
> getUsersName dbid = do
>   user <- getUserById dbid
>   pure $ name user

Ok that is easy to interpret though:

> simpleResult :: String
> simpleResult = iter t (getUsersName 10)
>   where
>     t (GetUserById id next) = next (User "Bob" id [] id)
>     t (GetUserByName name next) = next (Just $ User name 0 [] 0)
>     t (GetItemById id next) = next (Item "item" "test" id)

So lets make it slightly more complex by adding another algebra so we can do some logging when we need to debug later.

> data Logging a
>  = Log String a
>  deriving (Functor)

> logString ::
>  (Functor f, MonadFree f m, Logging :<: f)
>  => String -> m ()
> logString str = liftF . inj $ Log str ()

Here is an example of a program that combines those algebras:

> realProgram :: Free (Logging :+: DBRequest) String
> realProgram = do
>   user <- getUserById 12
>   logString $
>     "Got user #" ++
>     (show 12) ++
>     ", their age is: " ++
>     (show $ age user)
>   pure $ name user

Now, using `Control.Monad.Free.foldFree`, Interpreters can just be natural transformations from a Functor f to a Monad m: `f :~> m`:

> interpret
>   :: (Functor f, Monad m)
>   => f :~> m -> Free f a -> m a
> interpret t prog = foldFree (run t) prog

> dbInterpret :: DBRequest :~> DB
> dbInterpret = nat t
>   where
>     t (GetUserById id next) = dbUserById id >>= pure . next
>     t (GetUserByName name next) = fmap Just (dbUserByName name) >>= pure . next
>     t (GetItemById id next) = dbItemById id >>= pure . next

> dbToIO :: DB :~> IO
> dbToIO = nat unDB

> logInterpret :: Logging :~> IO
> logInterpret = nat t
>   where
>     t (Log str next) = putStrLn str >> pure next

Now we can define a simple combinator that can interpret two algebras to one Monad:

> combineInterpreters :: f :~> m -> g :~> m -> (f :+: g) :~> m
> combineInterpreters f g = nat t
>   where
>     t (Inl l) = (run f) l
>     t (Inr r) = (run g) r

And now we can run our program that contains our two algebras:

> realInterpreter :: (Logging :+: DBRequest) :~> IO
> realInterpreter = combineInterpreters  logInterpret (dbToIO . dbInterpret)

> runProgram :: IO String
> runProgram = interpret realInterpreter realProgram

We can test our programs with test interpreters

> testDB :: DBRequest :~> Identity
> testDB = nat t
>   where
>    t (GetUserById id next) = pure $ next (User "test" 20 [] id)
>    t (GetUserByName name next) = pure $ next Nothing
>    t (GetItemById id next) = pure $ next (Item "test" "test" id)

> testLogger :: Logging :~> State [String]
> testLogger = nat t
>  where
>   t (Log str next) = modify (\arr -> arr ++ [str]) >> pure next

> identityToState :: Identity :~> State [String]
> identityToState = nat (pure . runIdentity)

> testInterpreter :: (Logging :+: DBRequest) :~> State [String]
> testInterpreter = combineInterpreters testLogger (identityToState . testDB)

Now we can test our program's logic.

> testProgram :: (String, [String])
> testProgram = runState (interpret testInterpreter realProgram) []
