module Control.Concurrent.Core.Data.TextId
    ( TextId
    , HaveTextId(..)
    , Describe(..)
    , newTextId
    ) where

import           Universum hiding (ToText(..))
import           Data.Hashable
import           Control.Concurrent.Core.Data.Describe
import           Data.Base58String.Bitcoin
import           Control.Concurrent.Unique

newtype TextId = TextId Text deriving (Eq, Ord)

newTextId :: IO TextId
newTextId = TextId . toText . fromBinary . hash <$> newUnique

class HaveTextId a where
    getTextId :: a -> TextId

instance HaveTextId TextId where
    getTextId = id

instance Describe TextId where
    describe (TextId textId) = "[id " <> textId <> "]"