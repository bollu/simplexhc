{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module StgMachine where
import StgLanguage


import Text.PrettyPrint as PP
import Numeric 
import qualified Data.Map as M
import Control.Monad.Trans.Class
import Control.Lens
import Data.Map.Lens
import Control.Applicative
import Data.Either.Utils
import Control.Monad.State
import Control.Monad.Except
import Data.Traversable
import Data.Foldable

-- for <>
import Data.Monoid

-- hoistError
import Control.Monad.Error.Hoist

-- readMaybe
import Data.String.Utils

data Continuation = Continuation { _continuationAlts :: [CaseAltType],
                                   _continuationEnv :: LocalEnvironment
                                }

instance Prettyable Continuation where
  mkDoc Continuation{..} = text "alts:" $$ 
                           (_continuationAlts  & map mkDoc & vcat & mkNest)
data UpdateFrame

instance Prettyable UpdateFrame where
  mkDoc _ = text "update-frame"


-- | Represents an STG Address
newtype Addr = Addr { _getAddr :: Int } deriving(Eq, Ord)
instance Prettyable Addr where
    mkDoc addr = text $ "0x" ++ (addr & _getAddr & (\x -> showHex x ""))

instance Show Addr where
    show = renderStyle showStyle . mkDoc

data Value = ValueAddr Addr | ValuePrimInt Int
    deriving (Eq, Ord)


instance Prettyable Value where
  mkDoc (ValueAddr addr) = text "v-" PP.<> mkDoc addr
  mkDoc (ValuePrimInt int) = text ("v-" ++ show int)

instance Show Value where
    show = renderStyle showStyle . mkDoc

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

instance (Prettyable k, Prettyable v) => Prettyable (M.Map k v) where
  mkDoc m = vcat (fmap (uncurry mkKvDoc) (M.toList m)) where
              mkKvDoc key val = mkDoc key <+> text "->" <+> mkDoc val

data Code = CodeEval ExprNode LocalEnvironment | 
            CodeEnter Addr |
            CodeUninitialized |
            CodeReturnConstructor Constructor [Value] |
            CodeReturnInt Int deriving(Show) 

instance Prettyable Code where
  mkDoc (CodeEval expr env) = text "Eval" <+> mkDoc expr <+> mkDoc env
  mkDoc (CodeEnter addr) = text "Enter" <+> mkDoc addr
  mkDoc (CodeReturnConstructor cons values) = 
    text "ReturnConstructor" <+> 
    mkDoc cons <+> 
    (values & map mkDoc & hsep)
  mkDoc (CodeReturnInt i) = text "ReturnInt" <+> text (show i)




data MachineState = MachineState {
    _argumentStack :: ArgumentStack,
    _returnStack :: ReturnStack,
    _updateStack :: UpdateStack,
    _heap :: Heap,
    _globalEnvironment :: GlobalEnvironment,
    _code :: Code
}

instance Prettyable MachineState where
  mkDoc MachineState{..} = 
   text "args:" <+> argsDoc $$
   text "return:" <+> returnDoc $$
   text "update:" <+> updateDoc $$
   text "heap:" <+> heapDoc $$
   text "env:" <+> globalEnvDoc $$
   text "code:" <+> code where
    argsDoc = _argumentStack & map mkDoc & hsep
    returnDoc = _returnStack & map mkDoc & hsep
    updateDoc = _updateStack & map mkDoc & hsep
    heapDoc = _heap & text . show
    globalEnvDoc = _globalEnvironment & text . show
    code = _code & text . show


newtype MachineT a = MachineT { unMachineT :: ExceptT StgError (State MachineState) a }
            deriving (Functor, Applicative, Monad
               , MonadState MachineState
               , MonadError StgError)



-- | All possible errors when interpreting STG code.
data StgError = 
        -- | 'compileProgram' could not find main
        StgErrorUnableToFindMain | 
        -- | 'lookupIdentifier' failed
        StgErrorEnvLookupFailed Identifier LocalEnvironment GlobalEnvironment | 
        -- | 'lookupAddrInHeap' failed
        StgErrorHeapLookupFailed Addr Heap |
        -- | 'rawNumberToValue' failed
        StgErrorUnableToMkPrimInt RawNumber  | 
        -- | 'valueToAddr' failed
        StgErrorUnableToMkAddrFromValue Value |
        -- | 'takeNArgs' failed
        StgErrorNotEnoughArgsOnStack Int ArgumentStack |
        -- | 'continuationGetVariable' found no variable
        StgErrorCaseAltsHasNoVariable Continuation |
        -- | 'continuationGetVariable' found too many variables
        StgErrorCaseAltsHasMoreThanOneVariable Continuation [CaseAlt Identifier] | 
        -- | 'caseAltsGetUniqueMatch' found overlapping patterns
        -- | FIXME: find a better repr for the CaseAlt. currently cumbersome
        StgErrorCaseAltsOverlappingPatterns | 
        -- | `returnStackPop` finds no continuation to return to
        StgErrorReturnStackEmpty


makeLenses ''ClosureFreeVars
makePrisms ''Value
makeLenses ''Closure
makeLenses ''Code
makeLenses ''MachineState
makeLenses ''Addr
makeLenses ''Continuation


-- runExceptT :: ExceptT e (State s) a -> State s (Either e a)

uninitializedMachineState :: MachineState
uninitializedMachineState = MachineState {
    _argumentStack=[],
    _returnStack = [],
    _updateStack = [],
    _heap=M.empty,
    _globalEnvironment=M.empty,
    _code=CodeUninitialized
}

compileProgram :: Program -> Either StgError MachineState
compileProgram prog = let (mVal, machineState) = runState (runExceptT . unMachineT $ setupBindings) uninitializedMachineState
  in
  -- (mVal *> machineState) is too cryptic for my current taste
  case mVal of
    Left err -> Left err
    Right _ -> Right machineState
  where
    setupBindings :: MachineT ()
    setupBindings = do 
      for_ prog allocateBinding
      mainAddr <-  use globalEnvironment >>= (\x -> hoistError (const StgErrorUnableToFindMain) (x ^. at (Identifier "main"))) :: MachineT Addr
      -- NOTE: this is different from STG paper. Does this even work?
      code .= CodeEnter mainAddr
    allocateBinding :: Binding -> MachineT ()
    allocateBinding binding = do
        let lambda = binding ^. bindingLambda
        let name = binding ^. bindingName
        -- FIXME: make local envirorment ReaderT? How does ReaderT interact with StateT?
        -- FIXME: JIT the machine? :)
        let localenv = M.empty  -- when machine starts, no local env.
        addr <- (mkClosureFromLambda lambda localenv) >>= allocateOnHeap
        globalEnvironment %= ((at name) .~ Just addr )


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




mkClosureFromLambda :: Lambda -> LocalEnvironment -> MachineT Closure
mkClosureFromLambda lambda localenv = 
    do
      freeVarVals <- for (lambda ^. lambdaFreeVarIdentifiers) (lookupIdentifier  localenv)
      let cls = Closure {
        _closureLambda = lambda,
        _closureFreeVars = ClosureFreeVars (freeVarVals)
      }
      return cls

allocateOnHeap :: Closure -> MachineT Addr
allocateOnHeap cls = do
  count <- use (heap . to (M.size))
  heap %= (at (Addr count) .~ Just cls)
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



stepCodeEvalCase :: LocalEnvironment -> ExprNode -> [CaseAltType] -> MachineT ()
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


-- | Return the variable if the continuation contains an alternative
-- for it. 
continuationGetVariableAlt :: Continuation -> Either StgError (CaseAlt Identifier)
continuationGetVariableAlt cont = 
  let vars = cont ^.. continuationAlts . each . _CaseAltVariable
  in
    case vars of
      [] -> Left (StgErrorCaseAltsHasNoVariable cont)
      [v] -> Right v
      vs -> Left (StgErrorCaseAltsHasMoreThanOneVariable cont vs)

caseAltsGetUniqueMatch :: Eq a => [CaseAlt a] -> a -> Maybe (Either StgError (CaseAlt a))
caseAltsGetUniqueMatch pats val = 
  let matches = pats ^.. each . filtered (\alt -> alt ^. caseAltLHS == val)
  in
    case matches of
      [] -> Nothing
      [alt] -> Just (Right alt)
      alts -> Just (Left (StgErrorCaseAltsOverlappingPatterns))

returnStackPop :: MachineT Continuation
returnStackPop = do
  isempty <- use (returnStack . to null)
  if isempty
  then
    throwError StgErrorReturnStackEmpty
  else do
    top <- returnStack %%= (\(r:rs) -> (r, rs))
    return top

-- | codeReturnInt execution
stepCodeReturnInt :: Int -> MachineT ()
stepCodeReturnInt i = do
  cont <- returnStackPop
  return undefined
  
  
  

