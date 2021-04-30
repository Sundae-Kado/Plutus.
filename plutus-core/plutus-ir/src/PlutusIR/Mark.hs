module PlutusIR.Mark
    ( markNonFreshTerm
    , markNonFreshType
    , markNonFreshProgram
    ) where

import qualified PlutusCore.Core           as PLC

import           PlutusCore.Name
import           PlutusCore.Quote

import           PlutusIR.Transform.Rename

import           PlutusIR.Core

markNonFreshTerm
    :: (PLC.HasUniques (Term tyname name uni fun ann), MonadQuote m)
    => Term tyname name uni fun ann -> m ()
markNonFreshTerm = markNonFreshMax . undefined

markNonFreshType = undefined

markNonFreshProgram = undefined
