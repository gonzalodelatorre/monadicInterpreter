module Monads where

import           AST




class Monad m => MonadState m where
    lookfor :: Variable -> m Int
    update :: Variable -> Int -> m ()

class Monad m => MonadError m where
    -- Throws an error
    throw :: Error -> m a

