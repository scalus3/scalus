# Regenerating Cardano Ledger Test Vectors

This document describes how to regenerate the test vectors used in the Scalus conformance tests.

## Overview

The test vectors in `vectors.tar.gz` are generated from the cardano-ledger repository's ImpSpec test suite.

**IMPORTANT:** Scalus only needs UTXO-related test vectors (tests containing "UTXO" in their name).
Other vectors (GOV, DELEG, EPOCH, ENACT, RATIFY, etc.) are not used and should NOT be included
to keep the archive under GitHub's 100MB file size limit.

The vectors contain:
- Transaction CBOR (`cbor` field)
- Old ledger state CBOR (`oldLedgerState` field)
- New ledger state CBOR (`newLedgerState` field, if transaction succeeded)
- Success flag (`success` field)

## When to Regenerate

Regenerate vectors when:
1. The Plutus cost model changes (causing budget mismatches)
2. The ledger state format changes
3. New test cases are needed

## Prerequisites

- Nix with flakes enabled
- About 30-60 minutes for a full run

## Steps

### 1. Clone cardano-ledger

```bash
cd /tmp
git clone https://github.com/IntersectMBO/cardano-ledger
cd cardano-ledger
```

### 2. Create the dump-vectors executable

Create `eras/conway/impl/dump-vectors/Main.hs`:

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Executable to dump Conway ImpSpec test vectors
module Main where

import Cardano.Ledger.Binary (EncCBOR, serialize')
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (Era, EraRule, Tx, TopTx, eraProtVerLow)
import Cardano.Ledger.Shelley.LedgerState (LedgerState)
import Control.Monad.IO.Class (liftIO)
import Control.State.Transition.Extended (STS (..), TRC (..))
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as BSL
import Data.IORef
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Test.Cardano.Ledger.Conway.ImpTest ()
import Test.Cardano.Ledger.Shelley.ImpTest (
  ImpInit,
  LedgerSpec,
  ShelleyEraImp,
  modifyImpInitPostSubmitTxHook,
 )
import Test.Cardano.Ledger.Conway.Imp (conwayEraGenericSpec)
import Test.Hspec
import Test.Hspec.Core.Runner (defaultConfig, configFilterPredicate, hspecWith)
import Test.ImpSpec (ImpM, withImpInit)
import Data.List (isInfixOf, intercalate)
import Cardano.Ledger.BaseTypes (Globals)
import System.IO.Unsafe (unsafePerformIO)
import Cardano.Ledger.Conway.Rules ()

{-# NOINLINE globalCounter #-}
globalCounter :: IORef Int
globalCounter = unsafePerformIO $ newIORef 0

{-# NOINLINE globalOutputDir #-}
globalOutputDir :: IORef FilePath
globalOutputDir = unsafePerformIO $ newIORef "eras/conway/impl/dump"

toHexCbor :: forall era a. (Era era, EncCBOR a) => a -> Text
toHexCbor x = T.decodeUtf8 $ Base16.encode $ serialize' (eraProtVerLow @era) x

dumpHook ::
  forall era t.
  ( Era era
  , ShelleyEraImp era
  , EncCBOR (LedgerState era)
  , EncCBOR (Tx TopTx era)
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Signal (EraRule "LEDGER" era) ~ Tx TopTx era
  ) =>
  Globals ->
  TRC (EraRule "LEDGER" era) ->
  Either
    (NonEmpty (PredicateFailure (EraRule "LEDGER" era)))
    (State (EraRule "LEDGER" era), [Event (EraRule "LEDGER" era)]) ->
  ImpM t ()
dumpHook _ trc result = liftIO $ do
  baseDir <- readIORef globalOutputDir
  idx <- atomicModifyIORef' globalCounter (\n -> (n + 1, n))
  let TRC (_, oldState, tx) = trc
  let
    txCbor = toHexCbor @era tx
    oldStateCbor = toHexCbor @era oldState
    (success, newStateCbor) = case result of
      Left _ -> (False, Nothing)
      Right (newState, _) -> (True, Just $ toHexCbor @era newState)
    jsonObj = object $
      [ "cbor" .= txCbor
      , "oldLedgerState" .= oldStateCbor
      , "success" .= success
      ] ++ maybe [] (\ns -> ["newLedgerState" .= ns]) newStateCbor
  createDirectoryIfMissing True baseDir
  let fileName = baseDir </> show idx <> ".json"
  BSL.writeFile fileName (encode jsonObj)
  putStrLn $ "  Dumped: " ++ fileName

withDumpHook ::
  forall era.
  ( Era era
  , ShelleyEraImp era
  , EncCBOR (LedgerState era)
  , EncCBOR (Tx TopTx era)
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Signal (EraRule "LEDGER" era) ~ Tx TopTx era
  ) =>
  SpecWith (ImpInit (LedgerSpec era)) ->
  SpecWith (ImpInit (LedgerSpec era))
withDumpHook = modifyImpInitPostSubmitTxHook (dumpHook @era)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir] -> writeIORef globalOutputDir dir
    [] -> writeIORef globalOutputDir "eras/conway/impl/dump"
    _ -> error "Usage: conway-dump-vectors [output-dir]"
  outDir <- readIORef globalOutputDir
  putStrLn $ "Dumping test vectors to: " ++ outDir
  putStrLn "NOTE: Only running UTXO-related specs to keep archive size under 100MB"
  createDirectoryIfMissing True outDir
  -- Only run UTXO-related specs using hspec's --match filter
  hspecWith (defaultConfig { configFilterPredicate = Just (matchUTXO) }) $
    withImpInit $ withDumpHook @ConwayEra $ conwayEraGenericSpec @ConwayEra
  putStrLn "Done!"
  where
    matchUTXO path = "UTXO" `isInfixOf` joinPath path
    joinPath = intercalate "/" . map fst
```

### 3. Modify Conway.Imp to export conwayEraGenericSpec

Edit `eras/conway/impl/testlib/Test/Cardano/Ledger/Conway/Imp.hs`:

Change:
```haskell
module Test.Cardano.Ledger.Conway.Imp (spec) where
```
To:
```haskell
module Test.Cardano.Ledger.Conway.Imp (spec, conwayEraGenericSpec) where
```

### 4. Add executable to cabal file

Add to `eras/conway/impl/cardano-ledger-conway.cabal`:

```cabal
executable conway-dump-vectors
  main-is: Main.hs
  hs-source-dirs: dump-vectors
  default-language: Haskell2010
  ghc-options:
    -Wall -Wcompat -threaded -rtsopts -with-rtsopts=-N
  build-depends:
    aeson,
    base,
    base16-bytestring,
    bytestring,
    cardano-ledger-binary,
    cardano-ledger-conway,
    cardano-ledger-core,
    cardano-ledger-shelley:{cardano-ledger-shelley, testlib},
    directory,
    filepath,
    hspec,
    hspec-core,
    ImpSpec,
    small-steps,
    testlib,
    text,
```

### 5. Build and run

```bash
nix develop --command cabal build conway-dump-vectors
nix develop --command cabal run conway-dump-vectors
```

This will generate JSON files in `eras/conway/impl/dump/`.

### 6. Package for Scalus

The archive must stay under 100MB (GitHub's file size limit). The UTXO filter in the code
above should ensure only UTXO-related vectors are generated. If needed, you can manually
remove non-UTXO directories:

```bash
cd /tmp/cardano-ledger/eras/conway/impl/dump

# Remove non-UTXO directories (keep pparams-by-hash!)
python3 -c "
import os, shutil
for name in os.listdir('.'):
    if os.path.isdir(name) and 'UTXO' not in name.upper() and name != 'pparams-by-hash':
        shutil.rmtree(name)
        print(f'Removed: {name}')
"

# Create archive
cd /tmp/cardano-ledger
tar -czvf vectors.tar.gz eras/conway/impl/dump/

# Verify size is under 100MB
ls -lh vectors.tar.gz

cp vectors.tar.gz /path/to/scalus/scalus-cardano-ledger-it/src/test/resources/
```

## Notes

- With UTXO-only filter, the run takes ~5-10 minutes (vs 30-60 minutes for full suite)
- Output files are numbered sequentially (0.json, 1.json, etc.)
- The `success` field indicates whether the transaction passed ledger validation
- Failed transactions only have `oldLedgerState`, successful ones also have `newLedgerState`

## Verification

After updating vectors, run the conformance tests:

```bash
sbtn "scalusCardanoLedgerIt/testOnly scalus.testing.conformance.CardanoLedgerConformanceTest"
```

All tests should pass with `EvaluatorMode.Validate` mode.
