module DhallPlus
  ( parse, parse', parseT, parseT'
  , parseFile, parseFile', parseFileT, parseFileT', parseFileTS, parseFileTS'
  , tryDhall, tryDhall' )
where

-- base --------------------------------

import Control.Exception       ( SomeException )
import Control.Monad           ( join, return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Either             ( either )
import Data.Function           ( (&) )
import System.IO               ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- dhall -------------------------------

import qualified  Dhall

import Dhall  ( FromDhall, InputSettings
              , defaultInputSettings, inputWithSettings, rootDirectory
              , sourceName )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- enclosed-exceptions -----------------

import Control.Exception.Enclosed  ( tryAnyDeep )

-- fpath -------------------------------

import FPath.AsFilePath  ( AsFilePath( filepath ) )
import FPath.File        ( FileAs )

-- lens --------------------------------

import System.FilePath.Lens  ( directory )

-- monaderro-io ------------------------

import MonadError.IO.Error       ( AsIOError )
import MonadError     ( mapMError )

-- monadio-plus ------------------------

import MonadIO.OpenFile   ( readFile )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⩺) )
import Data.MoreUnicode.Monad    ( (≫) )
import Data.MoreUnicode.Lens     ( (⊣), (⫥), (⊢) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DhallPlus.Error  ( AsDhallError, DhallError, DhallIOError, mkDhallError )

--------------------------------------------------------------------------------

tryAnyME ∷ (MonadIO μ, MonadError ε η, NFData α) ⇒
           (SomeException → ε) → IO α → μ (η α)
tryAnyME f = mapMError f ⩺ liftIO ∘ (either throwError return ⩺ tryAnyDeep)

tryDhall ∷ (AsDhallError ε, MonadError ε μ, MonadIO μ, NFData α) ⇒ IO α → μ α
--tryDhall io = fmap (first mkDhallError) $ tryAnyDeep io
tryDhall = join ∘ tryAnyME mkDhallError

tryDhall' ∷ (MonadIO μ, MonadError DhallError μ, NFData α) ⇒ IO α → μ α
tryDhall' = tryDhall

----------------------------------------

parseT ∷ (AsDhallError ε, MonadError ε μ, MonadIO μ, NFData α) ⇒
         Dhall.Decoder α → Text → μ α
parseT t = tryDhall ∘ Dhall.input t

parseT' ∷ (MonadIO μ, MonadError DhallError μ, NFData α) ⇒
          Dhall.Decoder α → Text → μ α
parseT' = parseT

parse ∷ (AsDhallError ε, MonadError ε μ, MonadIO μ, NFData α, FromDhall α) ⇒
        Text → μ α
parse = parseT Dhall.auto

parse' ∷ (MonadIO μ, MonadError DhallError μ, NFData α, FromDhall α) ⇒
         Text → μ α
parse' = parse

----------------------------------------

parseFile ∷ (MonadIO μ, AsDhallError ε, AsIOError ε, MonadError ε μ,
             FileAs γ, AsFilePath γ, NFData α, FromDhall α) ⇒
            γ → μ α
parseFile = parseFileT Dhall.auto

--------------------

parseFile' ∷ (MonadIO μ, MonadError DhallIOError μ,
              FileAs γ, AsFilePath γ, NFData α, FromDhall α) ⇒
             γ → μ α
parseFile' = parseFile

----------------------------------------

parseFileT ∷ (MonadIO μ, AsDhallError ε, AsIOError ε, MonadError ε μ,
              FileAs γ, AsFilePath γ, NFData α) ⇒
             Dhall.Decoder α → γ → μ α
parseFileT t fn =
  let baseDir       = (fn ⫥ filepath) ⊣ directory
      inputSettings = defaultInputSettings & sourceName    ⊢ (fn ⫥ filepath)
                                           & rootDirectory ⊢ baseDir
   in parseFileTS inputSettings t fn

--------------------

parseFileT' ∷ (MonadIO μ, MonadError DhallIOError μ,
               FileAs γ, AsFilePath γ, NFData α) ⇒
             Dhall.Decoder α → γ → μ α
parseFileT' = parseFileT

----------------------------------------

{- | parse a Dhall file, specifying the expected type, and input settings -}
parseFileTS ∷ (MonadIO μ, AsDhallError ε, AsIOError ε, MonadError ε μ,
               FileAs γ, NFData α) ⇒
             InputSettings → Dhall.Decoder α → γ → μ α
parseFileTS s t fn = readFile fn ≫ tryDhall ∘ inputWithSettings s t

--------------------

parseFileTS' ∷ (MonadIO μ, MonadError DhallIOError μ, FileAs γ, NFData α) ⇒
               InputSettings → Dhall.Decoder α → γ → μ α
parseFileTS' = parseFileTS

-- that's all, folks! ----------------------------------------------------------
