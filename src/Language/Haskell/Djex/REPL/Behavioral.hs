-- | Behavioral admission of complete checked candidate handles. Search and
-- observation budgets are charged before a predicate can reject a candidate.
module Language.Haskell.Djex.REPL.Behavioral
  ( presentBehavioralCandidates ) where

import Control.Monad (forM_, unless, when)
import Data.List (intercalate)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hFlush, stdout)

import Language.Haskell.Djex
import Language.Haskell.Djex.Command
import Language.Haskell.Djex.REPL.BehavioralWorker
import Language.Haskell.Synthesis.Behavioral (BehavioralQuery)

-- No candidate is reconstructed, given another candidate's evidence, or
-- fetched to refill a rejected slot. First stops at the first True; best/all
-- inspect at most the same finite raw observation window. Lookahead retains
-- its batch meaning inside that window.
presentBehavioralCandidates
  :: Ord rank
  => PresentationOptions
  -> BehavioralContext
  -> BehavioralQuery
  -> (candidate -> Either RenderError String)
  -> (candidate -> Either RenderError String)
  -> (candidate -> rank)
  -> ([candidate] -> [candidate])
  -> Either Diagnostic [QueryResult metadata candidate]
  -> IO ExitCode
presentBehavioralCandidates options context query expression render rank order checkedResults =
  withBehavioralEvaluator context $ \preflight check -> do
    valid <- preflight query
    if valid /= BehavioralPassed then diagnosticFailure $ contextualDiagnostic Error
      "DJEX_REPL_BEHAVIORAL_PREFLIGHT" "behavioral predicate could not be checked"
      (show valid)
    else run check
 where
  run check = case checkedResults of
    Left failure -> diagnosticFailure failure
    Right results -> present check results
  present check results = do
    (accepted, outcomes, progress, capped) <- batches check window [] [] Nothing Nothing
      initialLookahead results
    let chosen = case mode of
          SelectAll -> order accepted
          SelectFirst -> accepted
          _ -> case accepted of
            [] -> []
            _ -> let best = minimum $ map rank accepted
                 in filter ((== best) . rank) accepted
    rendered <- pure $ traverse render chosen
    case rendered of
      Left failure -> diagnosticFailure $ contextualDiagnostic Error
        "DJEX_REPL_BEHAVIORAL_RENDER" "behavioral candidate could not be rendered"
        $ show failure
      Right blocks -> do
        forM_ (zip [0 :: Int ..] blocks) $ \(index, block) -> do
          when (index > 0) $ putStrLn "\n-- or\n"
          putStrLn block
        hFlush stdout
        emitDiagnostic $ contextualDiagnostic Info "DJEX_REPL_BEHAVIORAL_OBSERVATIONS"
          "behavioral predicate observations"
          $ intercalate ", "
            [ "checked=" ++ show (length outcomes)
            , "true=" ++ show (count isPassed outcomes)
            , "false=" ++ show (count isFalse outcomes)
            , "error=" ++ show (count isError outcomes)
            , "timeout=" ++ show (count isTimeout outcomes)
            , "window=" ++ show window ]
        unless (not $ null chosen) $ emitDiagnostic $ codedDiagnostic Info
          "DJEX_REPL_BEHAVIORAL_NO_MATCH"
          "no candidate passed the behavioral predicate in the observed search"
        forM_ (take 3 $ filter isError outcomes) $ \failure -> emitDiagnostic $
          contextualDiagnostic Warning "DJEX_REPL_BEHAVIORAL_CHECK"
            "candidate behavioral check was not accepted" $ show failure
        when (any isTimeout outcomes) $ emitDiagnostic $ codedDiagnostic Warning
          "DJEX_REPL_BEHAVIORAL_TIMEOUT"
          "predicate compilation or execution exceeded its 30-second deadline"
        when capped $ emitDiagnostic $ contextualDiagnostic Warning
          "DJEX_REPL_BEHAVIORAL_WINDOW" "behavioral observation window reached"
          "rejected and failed candidates consumed their original slots; search was not refilled"
        forM_ (progressTruncationDiagnostic progress) emitDiagnostic
        pure ExitSuccess
  mode = presentationSelection options
  window = max 0 $ presentationQualityWindow options
  initialLookahead = case mode of SelectBestLookahead n -> max 0 n; _ -> maxBound
  count predicate = length . filter predicate
  isPassed BehavioralPassed = True
  isPassed _ = False
  isFalse BehavioralFalse = True
  isFalse _ = False
  isTimeout BehavioralTimedOut = True
  isTimeout _ = False
  isError (BehavioralCompilationError _) = True
  isError (BehavioralRuntimeError _) = True
  isError (BehavioralUnavailable _) = True
  isError _ = False
  terminal BehavioralTimedOut = True
  terminal (BehavioralUnavailable _) = True
  terminal _ = False

  -- The first guard precedes inspection of either result or candidate tails.
  batches _ remaining accepted outcomes progress _ _ _ | remaining <= 0 =
    pure (reverse accepted, reverse outcomes, progress, True)
  batches _ _ accepted outcomes progress _ _ [] =
    pure (reverse accepted, reverse outcomes, progress, False)
  batches check remaining accepted outcomes _ best quiet (result : rest) = do
    let batch = resultSearch result
        progress = Just $ batchProgress batch
    (left, nextAccepted, nextOutcomes, stopped) <- candidates check remaining
      accepted outcomes $ batchCandidates batch
    let nextBest = case nextAccepted of
          [] -> Nothing
          _ -> Just $ minimum $ map rank nextAccepted
        improved = case (best, nextBest) of
          (Nothing, Just _) -> True
          (Just old, Just new) -> new < old
          _ -> False
        nextQuiet = if improved then initialLookahead
          else if best /= Nothing then quiet - 1 else quiet
        lookaheadDone = case mode of
          SelectBestLookahead _ -> nextBest /= Nothing && nextQuiet <= 0
          _ -> False
    if stopped || lookaheadDone then
      pure (reverse nextAccepted, reverse nextOutcomes, progress, left <= 0)
    else batches check left nextAccepted nextOutcomes progress nextBest nextQuiet rest

  candidates _ remaining accepted outcomes _ | remaining <= 0 =
    pure (remaining, accepted, outcomes, True)
  candidates _ remaining accepted outcomes [] =
    pure (remaining, accepted, outcomes, False)
  candidates check remaining accepted outcomes (candidate : rest) = do
    outcome <- case expression candidate of
      Left failure -> pure $ BehavioralCompilationError $ show failure
      Right term -> check query term
    let nextAccepted = if isPassed outcome then candidate : accepted else accepted
        nextOutcomes = outcome : outcomes
        nextRemaining = remaining - 1
    if terminal outcome || (mode == SelectFirst && isPassed outcome)
      then pure (nextRemaining, nextAccepted, nextOutcomes, True)
      else candidates check nextRemaining nextAccepted nextOutcomes rest
