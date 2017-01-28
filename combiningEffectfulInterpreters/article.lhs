Combining Effectful Interpreters
===

This is one method to combine completely separate Free Algebras and their Interpreters.

These are necessary pragmas

> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE FlexibleContexts #-}

Lets import necessary packages

> import Control.Monad.Free
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
