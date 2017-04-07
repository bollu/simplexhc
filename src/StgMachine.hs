{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}

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
                           (_continuationAlts  & map mkDoc & vcat)

instance Show Continuation where
    show = renderStyle showStyle . mkDoc


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
  mkDoc (ValueAddr addr) = text "val:" PP.<> mkDoc addr
  mkDoc (ValuePrimInt int) = text ("val:" ++ show int)

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
instance Prettyable ClosureFreeVars where
  mkDoc freeVars = _getFreeVars freeVars & map mkDoc & punctuate (text ",") & hsep
data Closure = Closure { 
    _closureLambda :: Lambda,
    _closureFreeVars :: ClosureFreeVars
} deriving (Show)

instance Prettyable Closure where
  mkDoc (Closure{..}) = text "cls:<<" <+> mkDoc _closureLambda $$ text "|" <+> mkDoc  _closureFreeVars <+> text ">>"


type LocalEnvironment = M.Map Identifier Value

instance (Prettyable k, Prettyable v) => Prettyable (M.Map k v) where
  mkDoc m = (fmap (uncurry mkKvDoc) (M.toList m))  & punctuate (text ";") & vcat where
              mkKvDoc key val = mkDoc key <+> text "->" <+> mkDoc val

data Code = CodeEval ExprNode LocalEnvironment | 
            CodeEnter Addr |
            CodeUninitialized |
            CodeReturnConstructor Constructor [Value] |
            CodeReturnInt Int deriving(Show) 

instance Prettyable Code where
  mkDoc (CodeEval expr env) = text "Eval" <+> braces (mkDoc expr) <+> text "|Local:" <+> braces(mkDoc env)
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
   text "*** code:" $$ code $+$
   text "*** args:" $$ argsDoc $+$
   text "*** return:" $$ returnDoc $+$
   text "*** update:" $$ updateDoc $+$
   text "*** heap:" $$ heapDoc $+$
   text "*** env:" $$ globalEnvDoc $+$
   text "---" where
    argsDoc = _argumentStack & map mkDoc & hsep
    returnDoc = _returnStack & map mkDoc & hsep
    updateDoc = _updateStack & map mkDoc & hsep
    heapDoc = _heap & mkDoc
    globalEnvDoc = _globalEnvironment & mkDoc 
    code = _code & mkDoc

instance Show MachineState where
    show = renderStyle showStyle . mkDoc

data MachineProgress = MachineStepped | MachineHalted deriving(Show, Eq)

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
        StgErrorReturnStackEmpty |
        -- | `unwrapAlts` failed, unable to unwrap raw number
        StgErrorExpectedCaseAltRawNumber CaseAltType deriving(Show)

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

runMachineT :: MachineT a -> MachineState -> Either StgError (a, MachineState)
runMachineT machineT state = let (mVal, machineState) = runState (runExceptT . unMachineT $ machineT) state in
                    -- TODO: refactor with fmap
                    case mVal of
                      Left err -> Left err
                      Right val -> Right (val, machineState)

compileProgram :: Program -> Either StgError MachineState
compileProgram prog = snd <$> (runMachineT setupBindings uninitializedMachineState)

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
        addr <- (mkClosureFromLambda lambda localenv) >>= allocateClosureOnHeap
        globalEnvironment %= ((at name) .~ Just addr )

isExprPrimitive :: ExprNode -> Bool
isExprPrimitive (ExprNodeRawNumber _) = True
isExprPrimitive _ = False

isMachineStateFinal :: MachineState -> Bool
isMachineStateFinal m = case m ^. code of
                          (CodeEval expr _) -> isExprPrimitive expr
                          _ -> False

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




mkClosureFromLambda :: Lambda -> LocalEnvironment -> MachineT Closure
mkClosureFromLambda lambda localenv = 
    do
      freeVarVals <- for (lambda ^. lambdaFreeVarIdentifiers) (lookupIdentifier  localenv)
      let cls = Closure {
        _closureLambda = lambda,
        _closureFreeVars = ClosureFreeVars (freeVarVals)
      }
      return cls

allocateClosureOnHeap :: Closure -> MachineT Addr
allocateClosureOnHeap cls = do
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


stepMachine :: MachineT MachineProgress
stepMachine = do
    code <- use code
    case code of
        CodeEval f local -> stepCodeEval local f
        CodeEnter addr -> stepCodeEnter addr
        CodeReturnInt i -> stepCodeReturnInt i

-- | 'CodeEval' execution
stepCodeEval :: LocalEnvironment -> ExprNode -> MachineT MachineProgress
stepCodeEval local expr = do
    case expr of
        ExprNodeFnApplication f xs -> stepCodeEvalFnApplication local f xs  
        ExprNodeLet isReucursive bindings inExpr -> stepCodeEvalLet local  isReucursive bindings inExpr
        ExprNodeCase expr alts -> stepCodeEvalCase local expr alts
        ExprNodeRawNumber num -> stepCodeEvalRawNumber num

stepCodeEvalFnApplication :: LocalEnvironment -> Identifier -> [Atom] -> MachineT MachineProgress
stepCodeEvalFnApplication local lhsName vars = do
     lhsValue <- lookupIdentifier local lhsName
     -- it doesn't have the address of a function, don't continue
     case lhsValue ^? _ValueAddr of
        Nothing -> return MachineHalted
        Just fnAddr ->  do
               localVals <- for vars (lookupAtom local)
               argumentStack <>= localVals
               code .= CodeEnter fnAddr
               return MachineStepped



stepCodeEvalLet :: LocalEnvironment -> IsLetRecursive -> [Binding] -> ExprNode -> MachineT MachineProgress
stepCodeEvalLet local isLetRecursive bindings inExpr = do
  let lookupEnv = local
  closureNameAddrMap <- for bindings  (\b -> do
                                              cls <- mkClosureFromLambda(b ^. bindingLambda) lookupEnv
                                              addr <- allocateClosureOnHeap cls
                                              return (b ^. bindingName, addr)
                                      )

  let newlocal = foldl  (\local (name, addr) -> M.insert name (ValueAddr addr) local)  local closureNameAddrMap
  code .= CodeEval inExpr newlocal
  return MachineStepped

returnStackPush :: Continuation -> MachineT ()
returnStackPush cont = do
  returnStack %= (\rs -> cont:rs)


stepCodeEvalCase :: LocalEnvironment -> ExprNode -> [CaseAltType] -> MachineT MachineProgress
stepCodeEvalCase local expr alts = do
  returnStackPush (Continuation alts local)
  code .= CodeEval expr local
  return MachineStepped

stepCodeEvalRawNumber :: RawNumber -> MachineT MachineProgress
stepCodeEvalRawNumber rawnum = do
  code .= CodeReturnInt (rawnum ^. getRawNumber & read)
  return MachineStepped

-- | codeEnter execution
stepCodeEnter :: Addr -> MachineT MachineProgress
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
stepCodeEnterIntoNonupdatableClosure :: Closure -> MachineT MachineProgress
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
    return MachineStepped


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

unwrapAlts :: [CaseAltType] -> Prism' CaseAltType a -> (CaseAltType -> StgError) -> MachineT [a]
unwrapAlts [] p err = return []
unwrapAlts (a:as) p err = case a ^? p of
                      Just a -> do
                                  as' <- unwrapAlts as p err
                                  return $ (a:as')
                      Nothing -> throwError (err a)


-- | codeReturnInt execution
stepCodeReturnInt :: Int -> MachineT MachineProgress
stepCodeReturnInt i = do
  cont <- returnStackPop
  unwrapped_alts <- unwrapAlts (cont ^. continuationAlts) _CaseAltRawNumber StgErrorExpectedCaseAltRawNumber
  return MachineStepped
 
  
  
  
  

