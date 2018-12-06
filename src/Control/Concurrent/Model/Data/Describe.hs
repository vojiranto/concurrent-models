module Control.Concurrent.Model.Data.Describe where
import           Control.Concurrent.Prelude 

class Describe a where
    describe :: a -> Text
