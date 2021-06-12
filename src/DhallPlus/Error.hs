module DhallPlus.Error
  ( AsDhallError( _DhallError )
  , AsDhallIOError( _DhallIOError )
  , AsDhallParseError( _DhallParseError )
  , AsDhallSomeError( _DhallSomeError )
  , AsDhallTypeSrcError( _DhallTypeSrcError )
  , DhallError, DhallIOError
  , _DIEDhallError, _DIEIOError
  , mkDhallError, mkDhallError'
  , mkDhallIOError, mkDhallIOError'
  )
where

-- base --------------------------------

import Control.Exception  ( Exception, SomeException, fromException )
import Data.Bool          ( Bool( False ) )
import Data.Eq            ( Eq( (==) ) )
import Data.Either        ( Either( Left, Right ) )
import Data.Function      ( id )
import Data.Maybe         ( Maybe( Just ) )
import Data.Void          ( Void )
import Text.Show          ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode      ( (∧) )
import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

-- dhall -------------------------------

import qualified  Dhall.Parser
import qualified  Dhall.TypeCheck

import Dhall.Parser     ( Src )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism', prism )
import Control.Lens.Review  ( (#) )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError( _IOError ), IOError )

--------------------------------------------------------------------------------

class AsDhallTypeSrcError ε where
  _DhallTypeSrcError ∷ Prism' ε (Dhall.TypeCheck.TypeError Src Void)

--------------------

class AsDhallParseError ε where
  _DhallParseError ∷ Prism' ε Dhall.Parser.ParseError

--------------------

class AsDhallSomeError ε where
  _DhallSomeError ∷ Prism' ε SomeException

------------------------------------------------------------

data DhallError = DhallParseError Dhall.Parser.ParseError
                | DhallTypeError  (Dhall.TypeCheck.TypeError Src Void)
                | DhallSomeError SomeException
  deriving Show

--------------------

instance Exception DhallError

--------------------

instance AsDhallTypeSrcError DhallError where
  _DhallTypeSrcError = prism DhallTypeError
                             (\ case DhallTypeError e → Right e; e → Left e)

--------------------

instance AsDhallParseError DhallError where
  _DhallParseError = prism DhallParseError
                           (\ case DhallParseError e → Right e; e → Left e)

--------------------

instance AsDhallSomeError DhallError where
  _DhallSomeError = prism DhallSomeError
                          (\ case DhallSomeError e → Right e; e → Left e)

--------------------

instance Eq DhallError where
  (DhallParseError e) == (DhallParseError e') =
      Dhall.Parser.unwrap e ≡ Dhall.Parser.unwrap e'
    ∧ Dhall.Parser.input  e ≡ Dhall.Parser.input  e'

  (DhallParseError _) == _ = False

  (DhallTypeError e) == (DhallTypeError e') =
      Dhall.TypeCheck.current e ≡ Dhall.TypeCheck.current e'
    ∧ show e ≡ show e'

  (DhallTypeError _) == _ = False

  (DhallSomeError _) == _ = False

--------------------

class AsDhallError ε where
  _DhallError ∷ Prism' ε DhallError

instance AsDhallError DhallError where
  _DhallError = id

------------------------------------------------------------

data DhallIOError = DIEDhallError DhallError
                  | DIEIOError    IOError
  deriving Show

instance Exception DhallIOError

_DIEDhallError ∷ Prism' DhallIOError DhallError
_DIEDhallError = prism DIEDhallError
                       (\ case DIEDhallError e → Right e; e → Left e)

_DIEIOError ∷ Prism' DhallIOError IOError
_DIEIOError = prism DIEIOError (\ case DIEIOError e → Right e; e → Left e)

--------------------

instance AsDhallError DhallIOError where
  _DhallError = prism DIEDhallError
                      (\ case DIEDhallError e → Right e; e → Left e)

--------------------

instance AsDhallTypeSrcError DhallIOError where
  _DhallTypeSrcError = _DhallError ∘ _DhallTypeSrcError

--------------------

instance AsDhallParseError DhallIOError where
  _DhallParseError = _DhallError ∘ _DhallParseError

--------------------

instance AsDhallSomeError DhallIOError where
  _DhallSomeError = _DhallError ∘ _DhallSomeError

--------------------

instance AsIOError DhallIOError where
  _IOError = prism DIEIOError (\ case DIEIOError e → Right e; e → Left e)

--------------------

instance Eq DhallIOError where
  (DIEDhallError e) == (DIEDhallError e') = e ≡ e'
  (DIEDhallError _) == _ = False

  (DIEIOError e) == (DIEIOError e') = e ≡ e'
  (DIEIOError _) == _ = False

--------------------

class AsDhallIOError ε where
  _DhallIOError ∷ Prism' ε DhallIOError

instance AsDhallIOError DhallIOError where
  _DhallIOError = id

------------------------------------------------------------

mkDhallError ∷ AsDhallError ε ⇒ SomeException → ε
mkDhallError (fromException @Dhall.Parser.ParseError → Just e) =
  _DhallError # DhallParseError e
mkDhallError (fromException @(Dhall.TypeCheck.TypeError Src Void) → Just e) =
  _DhallError # DhallTypeError e
mkDhallError e =
  _DhallError # DhallSomeError e

--------------------

mkDhallError' ∷ SomeException → DhallError
mkDhallError' = mkDhallError

----------------------------------------

mkDhallIOError ∷ AsDhallIOError ε ⇒ SomeException → ε
mkDhallIOError (fromException @IOError → Just e) =
 _DhallIOError # DIEIOError e
mkDhallIOError e =
 _DhallIOError # DIEDhallError (mkDhallError e)

--------------------

mkDhallIOError' ∷ SomeException → DhallIOError
mkDhallIOError' = mkDhallIOError


-- that's all, folks! ----------------------------------------------------------