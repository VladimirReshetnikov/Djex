-- | Scoped two-lane execution with deterministic observation order.
--
-- Both actions start before either result is observed.  Each result is forced
-- completely in its worker.  The left consumer runs before the right result is
-- observed, regardless of completion order.  'withAsync' supplies masked
-- spawning plus cancel-and-wait cleanup when a worker, consumer, or caller
-- raises an exception.
module Language.Haskell.Djex.REPL.Parallel
  ( parallelPairEligible
  , runParallelPairOrdered
  ) where

import Control.Concurrent.Async (wait, withAsync)
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)

-- | Whether a two-backend command may use the strict buffered worker path.
-- Timed commands and streaming selection deliberately retain their original
-- serial IO boundaries.
parallelPairEligible :: Int -> Bool -> Bool -> Bool
parallelPairEligible jobs hasTimeout streamsResults =
  jobs >= 2 && not hasTimeout && not streamsResults

-- | Start two actions together, force their results in their own workers, and
-- consume them in stable left-to-right order.
runParallelPairOrdered
  :: (NFData left, NFData right)
  => IO left
  -> IO right
  -> (left -> IO ())
  -> (right -> IO ())
  -> IO ()
runParallelPairOrdered left right consumeLeft consumeRight =
  withAsync (left >>= evaluate . force) $ \leftWorker ->
    withAsync (right >>= evaluate . force) $ \rightWorker -> do
      wait leftWorker >>= consumeLeft
      wait rightWorker >>= consumeRight
