module Control.Concurrent.Core.Data.Describe where
import           Control.Concurrent.Prelude 

class Describe a where
    describe :: a -> Text
