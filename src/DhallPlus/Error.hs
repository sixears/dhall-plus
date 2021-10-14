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
import Data.Eq            ( Eq( (==) ) )
import Data.Function      ( ($), (&), id )
import Data.Void          ( Void )
import GHC.Stack          ( CallStack, HasCallStack, callStack )
import Text.Show          ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode      ( (∧) )
import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- dhall -------------------------------

import qualified  Dhall.Parser
import qualified  Dhall.TypeCheck

import Dhall.Parser     ( Src )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- lens --------------------------------

import Control.Lens.Lens    ( lens )
import Control.Lens.Prism   ( Prism', prism )
import Control.Lens.Review  ( (#) )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError( _IOError ), IOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool    ( pattern 𝕱 )
import Data.MoreUnicode.Either  ( pattern 𝕷, pattern 𝕽 )
import Data.MoreUnicode.Lens    ( (⊣), (⊢) )
import Data.MoreUnicode.Maybe   ( pattern 𝕵 )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

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

data DhallError = DhallParseError Dhall.Parser.ParseError CallStack
                | DhallTypeError  (Dhall.TypeCheck.TypeError Src Void) CallStack
                | DhallSomeError  SomeException CallStack
  deriving Show

--------------------

instance Exception DhallError

--------------------

instance HasCallstack DhallError where
  callstack = lens (\ case (DhallParseError _ cs) → cs
                           (DhallTypeError  _ cs) → cs
                           (DhallSomeError  _ cs) → cs
                   )
                   (\ de cs →
                       case de of
                         (DhallParseError pe _) → DhallParseError pe cs
                         (DhallTypeError  te _) → DhallTypeError te cs
                         (DhallSomeError  se _) → DhallSomeError se cs
                   )

--------------------

instance Printable DhallError where
  print (DhallParseError pe _) = P.string $ show pe
  print (DhallTypeError  te _) = P.string $ show te
  print (DhallSomeError  se _) = P.string $ show se

--------------------

instance AsDhallTypeSrcError DhallError where
  _DhallTypeSrcError = prism (\ te → DhallTypeError te callStack)
                             (\ case DhallTypeError e _ → 𝕽 e; e → 𝕷 e)

--------------------

instance AsDhallParseError DhallError where
  _DhallParseError = prism (\ pe → DhallParseError pe callStack)
                           (\ case DhallParseError e _ → 𝕽 e; e → 𝕷 e)

--------------------

instance AsDhallSomeError DhallError where
  _DhallSomeError = prism (\ se → DhallSomeError se callStack)
                          (\ case DhallSomeError e _ → 𝕽 e; e → 𝕷 e)

--------------------

instance Eq DhallError where
  (DhallParseError e _) == (DhallParseError e' _) =
      Dhall.Parser.unwrap e ≡ Dhall.Parser.unwrap e'
    ∧ Dhall.Parser.input  e ≡ Dhall.Parser.input  e'

  (DhallTypeError e _) == (DhallTypeError e' _) =
      Dhall.TypeCheck.current e ≡ Dhall.TypeCheck.current e'
    ∧ show e ≡ show e'

  _ == _ = 𝕱

--------------------

class AsDhallError ε where
  _DhallError ∷ Prism' ε DhallError

instance AsDhallError DhallError where
  _DhallError = id

------------------------------------------------------------

data DhallIOError = DIEDhallError DhallError
                  | DIEIOError    IOError
  deriving Show

--------------------

instance Exception DhallIOError

--------------------

instance HasCallstack DhallIOError where
  callstack = lens (\ case (DIEDhallError de)  → de  ⊣ callstack
                           (DIEIOError    ioe) → ioe ⊣ callstack)
                   (\ dioe cs →
                       case dioe of
                         (DIEDhallError de) →
                           DIEDhallError $ de & callstack ⊢ cs
                         (DIEIOError ioe) →
                           DIEIOError $ ioe & callstack ⊢ cs
                   )

--------------------

instance Printable DhallIOError where
  print (DIEDhallError de)  = print de
  print (DIEIOError    ioe) = print ioe

_DIEDhallError ∷ Prism' DhallIOError DhallError
_DIEDhallError = prism DIEDhallError
                       (\ case DIEDhallError e → 𝕽 e; e → 𝕷 e)

_DIEIOError ∷ Prism' DhallIOError IOError
_DIEIOError = prism DIEIOError (\ case DIEIOError e → 𝕽 e; e → 𝕷 e)

--------------------

instance AsDhallError DhallIOError where
  _DhallError = prism DIEDhallError
                      (\ case DIEDhallError e → 𝕽 e; e → 𝕷 e)

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
  _IOError = prism DIEIOError (\ case DIEIOError e → 𝕽 e; e → 𝕷 e)

--------------------

instance Eq DhallIOError where
  (DIEDhallError e) == (DIEDhallError e') = e ≡ e'
  (DIEDhallError _) == _ = 𝕱

  (DIEIOError e) == (DIEIOError e') = e ≡ e'
  (DIEIOError _) == _ = 𝕱

--------------------

class AsDhallIOError ε where
  _DhallIOError ∷ Prism' ε DhallIOError

instance AsDhallIOError DhallIOError where
  _DhallIOError = id

------------------------------------------------------------

mkDhallError ∷ (AsDhallError ε, HasCallStack) ⇒ SomeException → ε
mkDhallError (fromException @Dhall.Parser.ParseError → 𝕵 e) =
  _DhallError # DhallParseError e callStack
mkDhallError (fromException @(Dhall.TypeCheck.TypeError Src Void) → 𝕵 e) =
  _DhallError # DhallTypeError e callStack
mkDhallError e =
  _DhallError # DhallSomeError e callStack

--------------------

mkDhallError' ∷ SomeException → DhallError
mkDhallError' = mkDhallError

----------------------------------------

mkDhallIOError ∷ AsDhallIOError ε ⇒ SomeException → ε
mkDhallIOError (fromException @IOError → 𝕵 e) =
 _DhallIOError # DIEIOError e
mkDhallIOError e =
 _DhallIOError # DIEDhallError (mkDhallError e)

--------------------

mkDhallIOError' ∷ SomeException → DhallIOError
mkDhallIOError' = mkDhallIOError


-- that's all, folks! ----------------------------------------------------------
