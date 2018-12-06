module Control.Concurrent.Core.Data.Describe where
import           Universum 

class Describe a where
    describe :: a -> Text
