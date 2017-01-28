Combining Effectful Interpreters
===

This is one method to combine completely separate Free Algebras and their Interpreters.

These are necessary pragmas

> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE FlexibleContexts #-}

Lets import necessary packages

> import Control.Monad.Free
> import Control.Monad.IO.Class
> import Control.Natural
> import Prelude hiding ((.))
> import Control.Category ((.))
>
> import Data.Comp
> import Data.Comp.Ops
>
> main = putStrLn "Hello, World!"

Lets define some Types for a simple DB with two tables User and Item

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

Here is an algebra we can use with `Free`

> data DBRequest a
>   = GetUserById Int (User -> a)
>   | GetUserByName String (Maybe User -> a)
>   | GetItemById Int (Item -> a)
>   deriving (Functor)

And some helper functions to make our algebra work with other algebras. Standard Data.Comp stuff

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

Ok now lets actually emulate(through the magic of Monads!) the DB

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

-- Example here

So lets make it slightly more complex with another algebra

> data Logging a
>  = Log String a
>  deriving (Functor)

> logString ::
>  (Functor f, MonadFree f m, Logging :<: f)
>  => String -> m ()
> logString str = liftF . inj $ Log str ()

> realProgram :: Free (Logging :+: DBRequest) String
> realProgram = do
>   user <- getUserById 12
>   logString $ show $ age user
>   pure $ name user
