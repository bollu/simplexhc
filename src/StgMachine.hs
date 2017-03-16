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
newtype ClosureFreeVars = ClosureFreeVars { _getFreeVars :: [Value] } deriving(Show)
makeLenses ''ClosureFreeVars
data Closure = Closure { 
    _closureLambda :: Lambda,
    _closureFreeVars :: ClosureFreeVars
} deriving (Show)

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
        StgErrorEnvLookupFailed Identifier LocalEnvironment GlobalEnvironment | 
        -- | 'lookupAddrInHeap' failed
        StgErrorHeapLookupFailed Addr Heap |
        -- | 'rawNumberToValue' failed
        StgErrorUnableToMkPrimInt RawNumber  | 
        -- | 'valueToAddr' failed
        StgErrorUnableToMkAddrFromValue Value |
        -- | 'takeNArgs' failed
        StgErrorNotEnoughArgsOnStack Int ArgumentStack deriving (Show)


-- | Try to lookup 'Identifier' in the local & global environments. Fail if unable to lookup.
lookupIdentifier :: LocalEnvironment -> Identifier -> MachineT Value
lookupIdentifier localEnv ident = do
        globalEnv <- use globalEnvironment

        let localLookup = (localEnv ^. at ident)
        let globalLookup = (ValueAddr <$> (globalEnv ^. at ident))

        let errormsg = StgErrorEnvLookupFailed ident localEnv globalEnv
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


lookupAddrInHeap :: Addr -> MachineT Closure
lookupAddrInHeap addr = do
    machineHeap <- use heap
    let mclosure = machineHeap ^. at addr :: Maybe Closure
    let errormsg = StgErrorHeapLookupFailed addr machineHeap :: StgError
    let eclosure = (maybeToEither errormsg mclosure) :: Either StgError Closure
    hoistError id eclosure



-- pop n values off the argument stack
takeNArgs :: Int -> MachineT [Value]
takeNArgs n = do
    machineArgStack <- use argumentStack
    if length machineArgStack < n
        then throwError $ StgErrorNotEnoughArgsOnStack n machineArgStack 
        else do
            let args = take n machineArgStack
            argumentStack %= drop n
            return args

stepMachine :: MachineT ()
stepMachine = do
    code <- use code
    case code of
        CodeEval f local -> stepCodeEval local f
        CodeEnter addr -> stepCodeEnter addr

-- | 'CodeEval' execution
stepCodeEval :: LocalEnvironment -> ExprNode -> MachineT ()
stepCodeEval local expr = do
    case expr of
        ExprNodeFnApplication f xs -> stepCodeEvalFnApplication local f xs  
        ExprNodeLet 


stepCodeEvalFnApplication :: LocalEnvironment -> Identifier -> [Atom] -> MachineT ()
stepCodeEvalFnApplication local fnName vars = do
     fnAddr <- lookupIdentifier local fnName >>= valueToAddr
     localVals <- for vars (lookupAtom local)
     argumentStack <>= localVals
     code .= CodeEnter fnAddr


     return ()

stepCodeEvalLet

stepCodeEnter :: Addr -> MachineT ()
stepCodeEnter addr = 
    do
        closure <- lookupAddrInHeap addr
        case closure of
            Closure {
                _lambda = Lambda {
                    _lambdaShouldUpdate = False
                }
            } -> stepCodeEnterIntoNonupdatableClosure lambda freeVars
            other -> undefined -- $ "cannot handle closure like: " ++ (show other)


-- provide the lambda and the list of free variables for binding
stepCodeEnterIntoNonupdatableClosure :: Closure -> MachineT ()
stepCodeEnterIntoNonupdatableClosure closure = do
    let l = closure ^. lambda
    let boundVarIdentifiers = l ^. lambdaBoundVarIdentifiers
    let freeVarIdentifiers = l ^. lambdaFreeVarIdentifiers
    let evalExpr =  l ^. lambdaExprNode

    boundVarVals <- boundVarIdentifiers & length & takeNArgs
    let localFreeVars = M.fromList (zip freeVarIdentifiers
                                               freeVarVals)
    let localBoundVars = M.fromList (zip boundVariableIdentifiers
                                                boundVarVals)
    let localEnv = localFreeVars `M.union` localBoundVars
    code .= CodeEval evalExpr localEnv


