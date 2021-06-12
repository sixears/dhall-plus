{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module T.DhallPlus
  ( tests )
where

-- base --------------------------------

import Control.Monad.IO.Class  ( MonadIO )
import Data.Either             ( Either( Right ) )
import Data.Function           ( ($) )
import Data.Maybe              ( isJust )
import Data.String             ( String )
import GHC.Num                 ( Integer )
import Numeric.Natural         ( Natural )
import System.Exit             ( ExitCode )
import System.IO               ( IO )
import Text.Show               ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- dhall -------------------------------

import qualified  Dhall

import Dhall  ( FromDhall )

-- monaderror-io -----------------------

import MonadError  ( ѥ )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens      ( (⩼) )
import Data.MoreUnicode.Monad     ( (≫) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), assertBool, testCase )

-- tasty-plus ------------------------------

import TastyPlus  ( assertLeft, runTestsP, runTestTree )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DhallPlus               ( parse, parseT, tryDhall )
import DhallPlus.Error         ( AsDhallParseError( _DhallParseError )
                               , AsDhallSomeError( _DhallSomeError )
                               , DhallError, _DhallTypeSrcError )

--------------------------------------------------------------------------------

tryDhallTests ∷ TestTree
tryDhallTests =
  let tryD ∷ (MonadIO μ, MonadError DhallError η, NFData α) ⇒ IO α → μ (η α)
      tryD = ѥ ∘ tryDhall
      isErrorType p e = assertBool (show e) (isJust $ e ⩼ p)
      isTypeSrcError  = isErrorType _DhallTypeSrcError
      isParseError    = isErrorType _DhallParseError
      isSomeError     = isErrorType _DhallSomeError
   in testGroup "tryDhall"
                [ testCase "integer success" $
                      tryD (Dhall.input Dhall.integer "+7") ≫ (Right 7 @=?)
                , testCase "natural success" $
                      tryD (Dhall.input Dhall.natural  "7") ≫ (Right 7 @=?)
                , testCase "natural ≢ integer" $
                      tryD (Dhall.input Dhall.integer  "7") ≫
                          assertLeft isTypeSrcError
                , testCase "integer ≢ natural" $
                      tryD (Dhall.input Dhall.natural  "+7") ≫
                          assertLeft isTypeSrcError
                , testCase "parseError" $
                      tryD (Dhall.input Dhall.natural  "[") ≫
                          assertLeft isParseError
                , testCase "import error" $
                      tryD (Dhall.input Dhall.natural  "./nonesuch") ≫
                          assertLeft isSomeError
                ]

----------------------------------------

parseTests ∷ TestTree
parseTests =
  let parse_ ∷ (NFData α, FromDhall α, MonadIO μ, MonadError DhallError η) ⇒
               Text → μ (η α)
      parse_ = ѥ ∘ parse
      isErrorType p e = assertBool (show e) (isJust $ e ⩼ p)
      isTypeSrcError  = isErrorType _DhallTypeSrcError
      isParseError    = isErrorType _DhallParseError
      isSomeError     = isErrorType _DhallSomeError
   in testGroup "parse"
                [ testCase "integer success" $
                      parse_ "+7" ≫ (Right (7 ∷ Integer) @=?)
                , testCase "natural success" $
                      parse_ "7" ≫ (Right (7 ∷ Natural) @=?)
                , testCase "natural ≢ integer" $
                      parse_ @Integer "7" ≫ assertLeft isTypeSrcError
                , testCase "integer ≢ natural" $
                      parse_ @Natural "+7" ≫ assertLeft isTypeSrcError
                , testCase "parseError" $
                      parse_ @Natural "[" ≫ assertLeft isParseError
                , testCase "import error" $
                      parse_ @Natural "./nonesuch" ≫ assertLeft isSomeError
                ]

----------------------------------------

parseTTests ∷ TestTree
parseTTests =
  let parseT_ ∷ (NFData α, FromDhall α, MonadIO μ, MonadError DhallError η) ⇒
                Dhall.Decoder α → Text → μ (η α)
      parseT_ y = ѥ ∘ parseT y
      isErrorType p e = assertBool (show e) (isJust $ e ⩼ p)
      isTypeSrcError  = isErrorType _DhallTypeSrcError
      isParseError    = isErrorType _DhallParseError
      isSomeError     = isErrorType _DhallSomeError
   in testGroup "parseT"
                [ testCase "integer success" $
                      parseT_ Dhall.integer "+7" ≫ (Right 7 @=?)
                , testCase "natural success" $
                      parseT_ Dhall.natural "7" ≫ (Right 7 @=?)
                , testCase "natural ≢ integer" $
                      parseT_ Dhall.integer "7" ≫ assertLeft isTypeSrcError
                , testCase "integer ≢ natural" $
                      parseT_ Dhall.natural "+7" ≫ assertLeft isTypeSrcError
                , testCase "parseTError" $
                      parseT_ Dhall.natural "[" ≫ assertLeft isParseError
                , testCase "import error" $
                      parseT_ Dhall.natural "./nonesuch" ≫ assertLeft isSomeError
                ]

------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "DhallPlus" [ tryDhallTests, parseTests, parseTTests ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ String → IO ExitCode
_tests = runTestsP tests


-- that's all, folks! ----------------------------------------------------------
