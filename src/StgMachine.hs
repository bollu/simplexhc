{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StgMachine where
import StgLanguage

import Numeric 
import qualified Data.Map as M
import Control.Monad.Trans.Class
import Control.Lens
import Control.Applicative
import Data.Either.Utils
import Control.Monad.State
import Control.Monad.Except
import Data.Traversable

-- for <>
import Data.Monoid

-- hoistError
import Control.Monad.Error.Hoist

-- readMaybe
import Data.String.Utils

data Continuation
data UpdateFrame


-- | Represents an STG Address
newtype Addr = Addr { _getAddr :: Int } deriving(Eq, Ord)
makeLenses ''Addr

instance Show Addr where
    show addr = "0x" ++ (addr ^. getAddr & (\x -> showHex x ""))


data Value = ValueAddr Addr | ValuePrimInt Int
    deriving (Eq, Ord, Show)
makePrisms ''Value

-- | Stack of 'Value'
type ArgumentStack = [Value]
type ReturnStack = [Continuation]
type UpdateStack = [UpdateFrame]
type Heap = M.Map Addr Closure

-- | Maps identifiers to addresses of the closures
type GlobalEnvironment = M.Map Identifier Addr

-- | has bindings of free variables with a 'LambdaForm'
data Closure = Closure { 
    _closureLambda :: Lambda,
    _closureFreeVars :: [Value]
}

makeLenses ''Closure

data Constructor
type LocalEnvironment = M.Map Identifier Value
data Code = CodeEval ExprNode LocalEnvironment | 
            CodeEnter Addr |
            CodeReturnConstructor Constructor [Value] |
            CodeReturnInt Int

makeLenses ''Code


data MachineState = MachineState {
    _argumentStack :: ArgumentStack,
    _returnStack :: ReturnStack,
    _updateStack :: UpdateStack,
    _heap :: Heap,
    _globalEnvironment :: GlobalEnvironment,
    _code :: Code
}
makeLenses ''MachineState

newtype MachineT a = MachineT { runMachineTa :: ExceptT StgError (State MachineState) a }
            deriving (Functor, Applicative, Monad
               , MonadState MachineState
               , MonadError StgError)


isMachineStateFinal :: MachineState -> Bool
isMachineStateFinal m = False

-- | All possible errors when interpreting STG code.
data StgError = 
        -- | 'lookupIdentifier' failed
        StgErrorLookupFailed Identifier LocalEnvironment GlobalEnvironment | 
        -- | 'rawNumberToValue' failed
        StgErrorUnableToMkPrimInt RawNumber  | 
        -- | 'valueToAddr' failed
        StgErrorUnableToMkAddrFromValue Value deriving (Show)


-- | Try to lookup 'Identifier' in the local & global environments. Fail if unable to lookup.
lookupIdentifier :: LocalEnvironment -> Identifier -> MachineT Value
lookupIdentifier localEnv ident = do
        globalEnv <- use globalEnvironment

        let localLookup = (localEnv ^. at ident)
        let globalLookup = (ValueAddr <$> (globalEnv ^. at ident))

        let errormsg = StgErrorLookupFailed ident localEnv globalEnv
        maybeToEither errormsg (localLookup <|> globalLookup)


rawNumberToValue :: RawNumber -> Either StgError Value
rawNumberToValue raw = maybeToEither errormsg mval where
    mval = raw ^. getRawNumber & maybeRead & (fmap ValuePrimInt)
    errormsg = StgErrorUnableToMkPrimInt raw

lookupAtom :: LocalEnvironment -> Atom -> MachineT Value
lookupAtom _ (AtomRawNumber r) = hoistError id (rawNumberToValue r)
lookupAtom localEnv (AtomIdentifier ident) = lookupIdentifier localEnv ident


valueToAddr :: Value -> MachineT Addr
valueToAddr (ValueAddr addr) = return addr
valueToAddr (val @(ValuePrimInt i)) = throwError (StgErrorUnableToMkAddrFromValue val)


stepMachine :: MachineT ()
stepMachine = do
    code <- use code
    case code of
        CodeEval f local -> stepCodeEval local f


-- | 'CodeEval' execution
stepCodeEval :: LocalEnvironment -> ExprNode -> MachineT ()
stepCodeEval local expr = do
    case expr of
        ExprNodeFnApplication f xs -> stepEvalFnApplication local f xs  


stepEvalFnApplication :: LocalEnvironment -> Identifier -> [Atom] -> MachineT ()
stepEvalFnApplication local fnName vars = do
     fnAddr <- lookupIdentifier local fnName >>= valueToAddr
     localVals <- for vars (lookupAtom local)
     argumentStack <>= localVals
     code .= CodeEnter fnAddr


     return ()
