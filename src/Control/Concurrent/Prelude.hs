module Control.Concurrent.Prelude ( module X ) where

import           Universum as X hiding (
    head, tail, init, last, tryTakeMVar, tryReadMVar, tryPutMVar, newEmptyMVar,
    swapMVar, readMVar)
import           Universum.Unsafe as X hiding (at)
import           Control.Lens.TH                                            as X
import           Control.Lens.At as X (at, Index, IxValue, At)
import           Control.Lens.Getter as X (Getting, to)
import           Data.Typeable                                              as X
import           Data.Dynamic                                               as X
import           Control.Monad.Free as X hiding (wrap)
import           Control.Concurrent.Chan as X
import           Control.Concurrent.STM.TChan as X
import           Control.Concurrent as X hiding (MVar, putMVar, takeMVar, newMVar)