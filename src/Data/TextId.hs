module Data.TextId where

import           Universum hiding (ToText(..))
import           Data.Hashable
import           Data.Base58String.Bitcoin
import           Control.Concurrent.Unique

newTextId :: IO Text
newTextId = toText . fromBinary . hash <$> newUnique