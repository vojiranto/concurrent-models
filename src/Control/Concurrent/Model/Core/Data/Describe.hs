module Control.Concurrent.Model.Core.Data.Describe where
import           Control.Concurrent.Prelude 

class Describe a where
    describe :: a -> Text
