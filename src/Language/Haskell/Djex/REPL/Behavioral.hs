-- | Behavioral admission of complete checked candidate handles. Search and
-- observation budgets are charged before a predicate can reject a candidate.
module Language.Haskell.Djex.REPL.Behavioral
  ( presentBehavioralCandidates ) where

import Control.DeepSeq (force)
import Control.Exception (evaluate, mask)
import Control.Monad (forM_, unless, when)
import Data.List (intercalate)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, modifyIORef')
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hFlush, stdout)
import System.Timeout (timeout)

import Language.Haskell.Djex
import Language.Haskell.Djex.Command
import Language.Haskell.Djex.REPL.BehavioralWorker
import Language.Haskell.Synthesis.Behavioral (BehavioralQuery (..))

-- A reserved row owns one observed compilation failure. Optional graph fields
-- are installed only after they have been fully evaluated within that same
-- candidate's deadline; emitting a timed-out sample must not resume rendering.
data CompilationSample = CompilationSample
  { compilationSampleOrdinal :: Int
  , compilationSampleRequestedType :: String
  , compilationSampleOriginalExpression :: String
  , compilationSampleOriginalOutcome :: BehavioralOutcome
  , compilationSampleElaboration :: Maybe (String, Either String String)
  , compilationSampleFinalOutcome :: Maybe BehavioralOutcome
  }

renderCompilationSample :: CompilationSample -> String
renderCompilationSample sample = intercalate "\n"
  [ "candidate observation: " ++ show (compilationSampleOrdinal sample)
  , "candidate evidence: " ++ maybe "inspection incomplete within candidate deadline" fst detail
  , "requested type: " ++ compilationSampleRequestedType sample
  , "original expression: " ++ compilationSampleOriginalExpression sample
  , "original check: " ++ show (compilationSampleOriginalOutcome sample)
  , maybe "elaboration unavailable: inspection incomplete within candidate deadline"
      (either ("elaboration unavailable: " ++) ("elaborated expression: " ++) . snd) detail
  , "final check: " ++ maybe "interrupted before an outcome was recorded" show
      (compilationSampleFinalOutcome sample)
  ]
 where
  detail = compilationSampleElaboration sample

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
  -> (candidate -> (String, Either String String))
  -> (candidate -> Either RenderError String)
  -> (candidate -> rank)
  -> ([(candidate, Maybe String)] -> [(candidate, Maybe String)])
  -> Either Diagnostic [Either Diagnostic (QueryResult metadata candidate)]
  -> IO ExitCode
presentBehavioralCandidates options context query expression elaborate render rank order checkedResults =
  withBehavioralEvaluator context $ \preflight check -> do
    samples <- newIORef []
    retries <- newIORef (0 :: Int)
    observations <- newIORef (0 :: Int)
    let reserveSample ordinal term original = atomicModifyIORef' samples $ \retained ->
          if length retained >= 3 then (retained, False)
          else (retained ++ [CompilationSample ordinal (behavioralType query)
                 term original Nothing Nothing], True)
        updateSample ordinal update = modifyIORef' samples replace
         where
          -- Keep at most three materialized row constructors; later unsampled
          -- observations must not build a chain of deferred map updates.
          replace [] = []
          replace (sample : rest) =
            let updated = if compilationSampleOrdinal sample == ordinal
                  then update sample else sample
                remaining = replace rest
            in updated `seq` remaining `seq` (updated : remaining)
        assess candidate = do
          modifyIORef' observations (+ 1)
          ordinal <- readIORef observations
          -- An annotation retry does not grant a second candidate deadline.
          -- Cancelling an active worker exchange retires its owned child.
          checked <- timeout 30000000 $ assessAt ordinal candidate
          let outcome = maybe (BehavioralTimedOut, Nothing) id checked
          updateSample ordinal $ \sample -> sample
            { compilationSampleFinalOutcome = Just $ fst outcome }
          pure outcome
        assessAt ordinal candidate = case expression candidate of
          Left failure -> pure (BehavioralCompilationError $ show failure, Nothing)
          Right term -> mask $ \restore -> do
            original <- restore $ check query term
            case original of
              BehavioralCompilationError _ -> do
                -- Reserve immediately after the observed failure, before any
                -- interruptible graph rendering or retry. Only this bounded
                -- IORef update is masked; both compiler calls stay cancellable.
                retained <- reserveSample ordinal term original
                restore $ do
                  alternative <-
                    if retained then do
                      detail <- evaluate $ force $ elaborate candidate
                      updateSample ordinal $ \sample -> sample
                        { compilationSampleElaboration = Just detail }
                      pure $ snd detail
                    else pure $ snd $ elaborate candidate
                  case alternative of
                    Right revised | revised /= term -> do
                      modifyIORef' retries (+ 1)
                      outcome <- check query revised
                      pure (outcome, if outcome == BehavioralPassed then Just revised else Nothing)
                    _ -> pure (original, Nothing)
              _ -> pure (original, Nothing)
    valid <- preflight query
    if valid /= BehavioralPassed then diagnosticFailure $ contextualDiagnostic Error
      "DJEX_REPL_BEHAVIORAL_PREFLIGHT" "behavioral predicate could not be checked"
      (show valid)
    else do
      result <- run assess
      attempted <- readIORef retries
      when (attempted > 0) $ emitDiagnostic $ contextualDiagnostic Info
        "DJEX_REPL_BEHAVIORAL_ELABORATION" "source-graph elaboration retries"
        $ "checks=" ++ show attempted ++ "; retries retain the same candidate and original observation slot"
      readIORef samples >>= mapM_ (emitDiagnostic . contextualDiagnostic Info
        "DJEX_REPL_BEHAVIORAL_SOURCE_SAMPLE" "bounded candidate compilation sample"
        . renderCompilationSample)
      pure result
 where
  run check = case checkedResults of
    Left failure -> diagnosticFailure failure
    Right results -> present check results
  present check results = do
    (accepted, outcomes, progress, capped, streamFailure) <- batches check window [] [] Nothing Nothing
      initialLookahead results
    let chosen = case mode of
          SelectAll -> order accepted
          SelectFirst -> accepted
          _ -> case accepted of
            [] -> []
            _ -> let best = minimum $ map (rank . fst) accepted
                 in filter ((== best) . rank . fst) accepted
    rendered <- pure $ traverse renderAccepted chosen
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
        maybe (pure ExitSuccess) diagnosticFailure streamFailure
  mode = presentationSelection options
  renderAccepted (candidate, Nothing) = render candidate
  renderAccepted (_, Just term) = Right $ case presentationRenderMode options of
    RenderExpression -> term
    RenderDefinition -> behavioralName query ++ " = " ++ term
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
    pure (reverse accepted, reverse outcomes, progress, True, Nothing)
  batches _ _ accepted outcomes progress _ _ [] =
    pure (reverse accepted, reverse outcomes, progress, False, Nothing)
  batches _ _ accepted outcomes progress _ _ (Left failure : _) =
    pure (reverse accepted, reverse outcomes, progress, False, Just failure)
  batches check remaining accepted outcomes _ best quiet (Right result : rest) = do
    let batch = resultSearch result
        progress = Just $ batchProgress batch
    (left, nextAccepted, nextOutcomes, stopped) <- candidates check remaining
      accepted outcomes $ batchCandidates batch
    let nextBest = case nextAccepted of
          [] -> Nothing
          _ -> Just $ minimum $ map (rank . fst) nextAccepted
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
      pure (reverse nextAccepted, reverse nextOutcomes, progress, left <= 0, Nothing)
    else batches check left nextAccepted nextOutcomes progress nextBest nextQuiet rest

  candidates _ remaining accepted outcomes _ | remaining <= 0 =
    pure (remaining, accepted, outcomes, True)
  candidates _ remaining accepted outcomes [] =
    pure (remaining, accepted, outcomes, False)
  candidates check remaining accepted outcomes (candidate : rest) = do
    (outcome, revised) <- check candidate
    let nextAccepted = if isPassed outcome then (candidate, revised) : accepted else accepted
        nextOutcomes = outcome : outcomes
        nextRemaining = remaining - 1
    if terminal outcome || (mode == SelectFirst && isPassed outcome)
      then pure (nextRemaining, nextAccepted, nextOutcomes, True)
      else candidates check nextRemaining nextAccepted nextOutcomes rest
