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

data Continuation = Continuation { continuationAlts :: [CaseAltVariant],
                                   continuationEnv :: LocalEnvironment
                                }
data UpdateFrame


-- | Represents an STG Address
newtype Addr = Addr { _getAddr :: Int } deriving(Eq, Ord)
instance Show Addr where
    show addr = "0x" ++ (addr & _getAddr & (\x -> showHex x ""))

data Value = ValueAddr Addr | ValuePrimInt Int
    deriving (Eq, Ord, Show)

-- | Stack of 'Value'
type ArgumentStack = [Value]
type ReturnStack = [Continuation]
type UpdateStack = [UpdateFrame]
type Heap = M.Map Addr Closure



-- | Maps identifiers to addresses of the closures
type GlobalEnvironment = M.Map Identifier Addr

-- | has bindings of free variables with a 'LambdaForm'
newtype ClosureFreeVars = ClosureFreeVars { _getFreeVars :: [Value] } deriving(Show)
data Closure = Closure { 
    _closureLambda :: Lambda,
    _closureFreeVars :: ClosureFreeVars
} deriving (Show)


type LocalEnvironment = M.Map Identifier Value
data Code = CodeEval ExprNode LocalEnvironment | 
            CodeEnter Addr |
            CodeReturnConstructor Constructor [Value] |
            CodeReturnInt Int



data MachineState = MachineState {
    _argumentStack :: ArgumentStack,
    _returnStack :: ReturnStack,
    _updateStack :: UpdateStack,
    _heap :: Heap,
    _heapCount :: Int,
    _globalEnvironment :: GlobalEnvironment,
    _code :: Code
}

newtype MachineT a = MachineT { runMachineTa :: ExceptT StgError (State MachineState) a }
            deriving (Functor, Applicative, Monad
               , MonadState MachineState
               , MonadError StgError)



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


makeLenses ''ClosureFreeVars
makePrisms ''Value
makeLenses ''Closure
makeLenses ''Code
makeLenses ''MachineState
makeLenses ''Addr


isMachineStateFinal :: MachineState -> Bool
isMachineStateFinal m = False

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

allocateOnHeap :: Closure -> MachineT Addr
allocateOnHeap cls = do
  count <- use heapCount
  machineHeap <- use heap
  heap %= (at (Addr count) .~ Just cls)
  heapCount += 1
  return (Addr count)
  

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
        CodeReturnInt i -> stepCodeReturnInt i

-- | 'CodeEval' execution
stepCodeEval :: LocalEnvironment -> ExprNode -> MachineT ()
stepCodeEval local expr = do
    case expr of
        ExprNodeFnApplication f xs -> stepCodeEvalFnApplication local f xs  
        ExprNodeLet isReucursive bindings inExpr -> stepCodeEvalLet local  isReucursive bindings inExpr
        ExprNodeCase expr alts -> stepCodeEvalCase local expr alts
        ExprNodeRawNumber num -> stepCodeEvalRawNumber num

stepCodeEvalFnApplication :: LocalEnvironment -> Identifier -> [Atom] -> MachineT ()
stepCodeEvalFnApplication local fnName vars = do
     fnAddr <- lookupIdentifier local fnName >>= valueToAddr
     localVals <- for vars (lookupAtom local)
     argumentStack <>= localVals
     code .= CodeEnter fnAddr


     return ()

stepCodeEvalLet :: LocalEnvironment -> IsLetRecursive -> [Binding] -> ExprNode -> MachineT ()
stepCodeEvalLet local isLetRecursive bindings inExpr = undefined

returnStackPush :: Continuation -> MachineT ()
returnStackPush cont = do
  returnStack %= (\rs -> cont:rs)

returnStackPop :: MachineT Continuation
returnStackPop = do
   empty <- use (returnStack . to empty)
   return undefined

stepCodeEvalCase :: LocalEnvironment -> ExprNode -> [CaseAlt] -> MachineT ()
stepCodeEvalCase local expr alts = do
  returnStackPush (Continuation alts local)
  code .= CodeEval expr local

stepCodeEvalRawNumber :: RawNumber -> MachineT ()
stepCodeEvalRawNumber rawnum = do
  code .= CodeReturnInt (rawnum ^. getRawNumber & read)

-- | codeEnter execution
stepCodeEnter :: Addr -> MachineT ()
stepCodeEnter addr = 
    do
        closure <- lookupAddrInHeap addr
        case closure of
            Closure {
                _closureLambda = Lambda {
                    _lambdaShouldUpdate = False
                }
            } -> stepCodeEnterIntoNonupdatableClosure closure
            other -> undefined -- $ "cannot handle closure like: " ++ (show other)


-- provide the lambda and the list of free variables for binding
stepCodeEnterIntoNonupdatableClosure :: Closure -> MachineT ()
stepCodeEnterIntoNonupdatableClosure closure = do
    let l = closure ^. closureLambda
    let boundVarIdentifiers = l ^. lambdaBoundVarIdentifiers
    let freeVarIdentifiers = l ^. lambdaFreeVarIdentifiers
    let evalExpr =  l ^. lambdaExprNode

    boundVarVals <- boundVarIdentifiers & length & takeNArgs
    let localFreeVars = M.fromList (zip freeVarIdentifiers
                                               (closure ^. closureFreeVars . getFreeVars))
    let localBoundVars = M.fromList (zip boundVarIdentifiers
                                                boundVarVals)
    let localEnv = localFreeVars `M.union` localBoundVars
    code .= CodeEval evalExpr localEnv

-- | codeReturnInt execution
codeReturnInt :: Int -> MachineT ()
codeReturnInt i = do
  cont <- popContinuationStack 
  

