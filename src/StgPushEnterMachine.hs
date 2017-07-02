{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveTraversable #-}

module StgPushEnterMachine where
import StgLanguage


import Data.Text.Prettyprint.Doc as PP
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

import ColorUtils

-- for <>
import Data.Monoid

-- hoistError
import Control.Monad.Error.Hoist

-- readMaybe
import Data.String.Utils



data Continuation = Continuation { _continuationAlts :: ![CaseAltType],
                                   _continuationEnv :: !LocalEnvironment
                                }

instance Pretty Continuation where
  pretty Continuation{..} = vsep [pretty "alts:",
                                 _continuationAlts  & map pretty & vcat]

instance Show Continuation where
    show = prettyToString


data UpdateFrame = UpdateFrame {
                                 -- | the argument stack that was present when the update frame was created.
                                 _updateFrameArgumentStack :: !ArgumentStack,
                                 -- | The return stack that was present when the update frame was created.
                                 _updateFrameReturnStack :: !ReturnStack,
                                 -- | The address of the heap closure to be updated.
                                 _updateFrameAddress :: Addr
                               }
instance Pretty UpdateFrame where
  pretty UpdateFrame{..} = 
    indent 4 (vsep [pretty "Argument Stack: "
                  , pretty _updateFrameArgumentStack
                  , pretty "Return Stack: "
                  , pretty _updateFrameReturnStack])


-- | Represents an STG Address
newtype Addr = Addr { _getAddr :: Int } deriving(Eq, Ord)
instance Pretty Addr where
    pretty addr = styleAddr PP.<> (pretty $ "0x" ++ (addr & _getAddr & (\x -> showHex x ""))) PP.<> styleReset

instance Show Addr where
    show = prettyToString

data Value = ValueAddr Addr | ValuePrimInt Int
    deriving (Eq, Ord)


instance Pretty Value where
  pretty (ValueAddr addr) = mkStyleTag (pretty "val:") PP.<> pretty addr
  pretty (ValuePrimInt int) = mkStyleTag (pretty "val:") PP.<> pretty int PP.<> pretty "#" 

instance Show Value where
    show = prettyToString

-- | Stack of 'Value'
newtype Stack a = Stack { _unStack :: [a] } deriving(Functor, Monoid, Foldable, Traversable)

stackLength :: Stack a -> Int
stackLength = length . _unStack

stackEmpty :: Stack a
stackEmpty = Stack []

instance Pretty a => Pretty (Stack a) where
  pretty (Stack []) = pretty "EMPTY"
  pretty (Stack xs) = 
    vsep 
      [pretty "count: "  <+> pretty  (length xs),
      mkStyleAnnotation (pretty "TOP"), 
      zipWith (<+>) (fmap  intlabel [1..]) (fmap pretty xs) & vsep,
      mkStyleAnnotation (pretty "BOTTOM")]
    where
      intlabel :: Int -> Doc ann
      intlabel i = mkStyleAnnotation (pretty "|" <+> pretty i <+> colon)

instance Pretty a => Show (Stack a) where
  show = prettyToString

type ArgumentStack = Stack Value
type ReturnStack = Stack Continuation
type UpdateStack = Stack UpdateFrame
type Heap = M.Map Addr Closure


-- | Maps VarName names to addresses of the closures
type GlobalEnvironment = M.Map VarName Addr

-- | has bindings of free variables with a 'LambdaForm'
newtype ClosureFreeVals = ClosureFreeVals { _getFreeVals :: [Value] } deriving(Show)
instance Pretty ClosureFreeVals where
  pretty freeVars = _getFreeVals freeVars & map pretty & punctuate comma  & hsep
data Closure = Closure { 
    _closureLambda :: !Lambda,
    _closureFreeVals :: !ClosureFreeVals
} deriving (Show)

instance Pretty Closure where
  -- TODO: allow closure to invoke a custom renderer for free variables in the lambdaForm
  pretty (Closure{..}) = (mkStyleTag (pretty "cls:")) <+> pretty "["
                         <+> pretty _closureLambda PP.<> envdoc <+> pretty "]" where
            envdoc = if length ( _getFreeVals ( _closureFreeVals)) == 0
                    then pretty ""
                    else pretty " | Free variable vals: " <+> pretty  _closureFreeVals 
                        


type LocalEnvironment = M.Map VarName Value

instance (Pretty k, Pretty v) => Pretty (M.Map k v) where
  pretty m = 
    fmap (uncurry mkKvDoc) (M.toList m)  & punctuate semi & vsep where
        mkKvDoc key val = pretty key <+> pretty "->" <+> pretty val

data Code = CodeEval ExprNode LocalEnvironment | 
            CodeEnter Addr |
            CodeUninitialized |
            CodeReturnConstructor Constructor [Value] |
            CodeReturnInt StgInt deriving(Show)

instance Pretty Code where
  pretty (CodeEval expr env) = pretty "Eval" <+> braces (pretty expr) <+> pretty "|Local:" <+> braces(pretty env)
  pretty (CodeEnter addr) = pretty "Enter" <+> pretty addr
  pretty (CodeReturnConstructor cons values) = 
    pretty "ReturnConstructor" <+>  
      (pretty (cons ^. constructorName) <+> 
     (values & map pretty & punctuate comma & hsep & braces) & parens)
  pretty (CodeReturnInt i) = pretty "ReturnInt" <+> pretty i


newtype Log = Log { unLog :: [Doc ()] } deriving(Monoid)

instance Show Log where
  show l = unLog l & vsep & docToString

data PushEnterMachineState = PushEnterMachineState {
    _argumentStack :: !ArgumentStack,
    _returnStack :: !ReturnStack,
    _updateStack :: !UpdateStack,
    _heap :: !Heap,
    _globalEnvironment :: !GlobalEnvironment,
    _code :: !Code,
    _currentLog :: !Log,
    _oldLog :: !Log
}


instance Pretty PushEnterMachineState where
  pretty PushEnterMachineState{..} = 
    vsep [
     heading (pretty "@@@ Code:"), code, line,
     heading (pretty "@@@ Args:"), argsDoc, line,
     heading (pretty "@@@ Return:"), returnDoc, line,
     heading (pretty "@@@ Update:"), updateDoc, line,
     heading (pretty "@@@ Heap:"), heapDoc, line,
     heading (pretty "@@@ Env:"), globalEnvDoc, line,
     heading (pretty "---")] where
        argsDoc = _argumentStack & pretty
        returnDoc = _returnStack & pretty
        updateDoc = _updateStack & pretty
        heapDoc = _heap & pretty
        globalEnvDoc = _globalEnvironment & pretty 
        code = _code & pretty
        currentLogDoc = _currentLog & unLog & vsep

instance Show PushEnterMachineState where
    show = prettyToString

data MachineProgress = MachineStepped | MachineHalted deriving(Show, Eq)

newtype MachineT a = MachineT { unMachineT :: ExceptT StgError (State PushEnterMachineState) a }
            deriving (Functor, Applicative, Monad
               , MonadState PushEnterMachineState
               , MonadError StgError)

-- | All possible errors when interpreting STG code.
data StgError = 
        -- | 'compileProgram' could not find main
        StgErrorUnableToFindMain | 
        -- | 'lookupVariable' failed
        StgErrorEnvLookupFailed !VarName !LocalEnvironment !GlobalEnvironment | 
        -- | 'lookupAddrInHeap' failed
        StgErrorHeapLookupFailed !Addr !Heap |
        -- | 'rawNumberToValue' failed
        StgErrorUnableToMkPrimInt !RawNumber  | 
        -- | 'takeNArgs' failed
        StgErrorNotEnoughArgsOnStack !Int !ArgumentStack |
        -- | 'continuationGetVariable' found no variable
        StgErrorCaseAltsHasNoVariable !Continuation |
        -- | 'continuationGetVariable' found too many variables
        StgErrorCaseAltsHasMoreThanOneVariable !Continuation ![CaseAlt VarName] | 
        -- | 'caseAltsGetUniqueMatch' found overlapping patterns
        -- | FIXME: find a better repr for the CaseAlt. currently cumbersome
        StgErrorCaseAltsOverlappingPatterns | 
        -- | `returnStackPop` finds no continuation to return to
        StgErrorReturnStackEmpty |
        -- | `unwrapAlts` failed, unable to unwrap raw number
        StgErrorExpectedCaseAltInt !CaseAltType | 
        -- | `unwrapAlts` failed, unable to unwrap Constructor
        StgErrorExpectedCaseAltConstructor Constructor !CaseAltType | 
        -- | 'xxx' failed, no matching pattern match found
        StgErrorNoMatchingAltPatternInt StgInt [CaseAlt StgInt] |
        -- | 'xxx' failed, no matching pattern match found
        StgErrorNoMatchingAltPatternConstructor Constructor [CaseAlt ConstructorPatternMatch] |
        -- | tried to pop empty update frame
        StgErrorUpdateStackEmpty |
        -- | tried to update an address where no previous value exists
        StgErrorHeapUpdateHasNoPreviousValue Addr deriving(Show)

makeLenses ''ClosureFreeVals
makePrisms ''Value
makeLenses ''Closure
makePrisms ''Code
makeLenses ''PushEnterMachineState
makeLenses ''Addr
makeLenses ''Continuation
makeLenses ''Stack
makeLenses ''UpdateFrame

uninitializedPushEnterMachineState :: PushEnterMachineState
uninitializedPushEnterMachineState = PushEnterMachineState {
    _argumentStack=stackEmpty,
    _returnStack = stackEmpty,
    _updateStack = stackEmpty,
    _heap=M.empty,
    _globalEnvironment=M.empty,
    _code=CodeUninitialized,
    _oldLog=mempty,
    _currentLog=mempty
}

maybeToMachineT :: Maybe a -> StgError -> MachineT a
maybeToMachineT (Nothing) err = throwError err
maybeToMachineT (Just a) err = return a 

runMachineT :: MachineT a -> PushEnterMachineState -> Either StgError (a, PushEnterMachineState)
runMachineT machineT state = let (mVal, machineState) = runState (runExceptT . unMachineT $ machineT) state in
                    -- TODO: refactor with fmap
                    case mVal of
                      Left err -> Left err
                      Right val -> Right (val, machineState)


allocateBinding :: LocalEnvironment -> Binding -> MachineT (VarName, Addr)
allocateBinding localenv binding =  do
    let lambda = binding ^. bindingLambda
    let name = binding ^. bindingName
    addr <- (mkClosureFromLambda lambda localenv) >>= allocateClosureOnHeap
    return (name, addr)

gVarNamesToIntIntrinsics :: M.Map VarName (Int -> Int -> Int)
gVarNamesToIntIntrinsics = M.fromList $ [(VarName "plus#", (+))]

-- HACK: I'm mapping intrinsics to negative addresses.
-- Ideally, this should be cleaner but I really don't care right now
-- mapIntrinsicsToAddrs :: MachineT ()
-- mapIntrinsicsToAddrs = do
--   for_ zip ([-1, -2,..](M.keys gVarNamesToIntIntrinsics) (\(i, name) -> globalEnvironment %= (at name) .~ Just i)


-- allocate the bindings on the heap, and return the mapping
-- between variable names to addresses
compileProgram :: Program -> Either StgError PushEnterMachineState
compileProgram prog = snd <$> (runMachineT setupBindings uninitializedPushEnterMachineState)
  where
    setupBindings :: MachineT ()
    setupBindings = do 
      let localenv = M.empty  -- when machine starts, no local env.
      nameAddrPairs <- for prog (allocateBinding localenv) :: MachineT [(VarName, Addr)]
      globalEnvironment .= M.fromList nameAddrPairs
      -- Do I actually need this? mapIntrinsicsToAddrs

      mainAddr <-  use globalEnvironment >>= (\x -> maybeToMachineT (x ^. at (VarName "main")) StgErrorUnableToFindMain) :: MachineT Addr
      -- NOTE: this is different from STG paper. Does this even work?
      setCode $ CodeEnter mainAddr

isExprPrimitive :: ExprNode -> Bool
isExprPrimitive (ExprNodeInt _) = True
isExprPrimitive _ = False

isPushEnterMachineStateFinal :: PushEnterMachineState -> Bool
isPushEnterMachineStateFinal m = case m ^. code of
                          (CodeEval expr _) -> isExprPrimitive expr
                          _ -> False

-- | Try to lookup 'VarName' in the local & global environments. Fail if unable to lookup.
lookupVariable :: LocalEnvironment -> VarName -> MachineT Value
lookupVariable localEnv ident = do
        globalEnv <- use globalEnvironment
        let localLookup = (localEnv ^. at ident)
        let globalLookup = (ValueAddr <$> (globalEnv ^. at ident))

        let errormsg = StgErrorEnvLookupFailed ident localEnv globalEnv
        maybeToEither errormsg (localLookup <|> globalLookup)


stgIntToValue :: StgInt -> Value
stgIntToValue si = ValuePrimInt (unStgInt si)

lookupAtom :: LocalEnvironment -> Atom -> MachineT Value
lookupAtom _ (AtomInt r) = return $ stgIntToValue r
lookupAtom localEnv (AtomVarName ident) = lookupVariable localEnv ident




mkClosureFromLambda :: Lambda -> LocalEnvironment -> MachineT Closure
mkClosureFromLambda lambda localenv = 
    do
      freeVarVals <- for (lambda ^. lambdaFreeVarIdentifiers) (lookupVariable  localenv)
      let cls = Closure {
        _closureLambda = lambda,
        _closureFreeVals = ClosureFreeVals (freeVarVals)
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
    mclosure `maybeToMachineT` errormsg

-- pop n values off the argument stack
takeNArgs :: Int -> MachineT [Value]
takeNArgs n = do
    appendLog $ pretty "popping" <+> pretty n <+> pretty "off of the argument stack"

    argStackList <- use (argumentStack . unStack)
    if length argStackList < n
        then do
            appendLog $ pretty "length of argument stack:" <+> pretty (length argStackList) <+> pretty "< n:" <+> pretty n
            throwError $ StgErrorNotEnoughArgsOnStack n (Stack argStackList)
        else do
            let args = take n argStackList
            argumentStack .= Stack (drop n argStackList)
            return args


stepMachine :: MachineT MachineProgress
stepMachine = do
    code <- use code
    
    nowOldLog <- use currentLog
    oldLog <>= nowOldLog
    currentLog .= mempty
    appendLog $ pretty "evaluating code:" <+> pretty code

    case code of
        CodeEval f local -> stepCodeEval local f
        CodeEnter addr -> stepCodeEnter addr
        CodeReturnInt i -> stepCodeReturnInt i
        CodeReturnConstructor cons consvals -> stepCodeReturnConstructor cons consvals


setCode :: Code -> MachineT ()
setCode c = do
    appendLog $ pretty "setting code to: " <+> pretty c
    code .= c

updateStackPop :: MachineT UpdateFrame
updateStackPop = stackPop updateStack StgErrorUpdateStackEmpty


-- TODO: find out how to make this look nicer
heapUpdateAddress :: Addr -> Closure -> MachineT ()
heapUpdateAddress addr cls = do
    appendLog $ vsep [pretty "updating heap at address:" <+> pretty addr <+> pretty "with closure:",
                      indent 4 (pretty "new closure:" <+> pretty cls)]
    h <- use heap
    case h ^. at addr of
        Nothing -> do
                appendError $ pretty "heap does not contain address:" <+> pretty addr
                throwError $ StgErrorHeapUpdateHasNoPreviousValue addr
        Just oldcls -> do
                let h' = at addr .~ Just cls $ h
                heap .= h'
                return ()

-- | create the standard closure for a constructor
-- | << (freeVarIds \n {} -> c freeVarIds), consVals >>
_mkConstructorClosure :: Constructor -> [Value] -> Closure
_mkConstructorClosure c consVals = Closure {
    _closureLambda = Lambda {
            _lambdaShouldUpdate = False,
            _lambdaBoundVarIdentifiers = [],
            _lambdaFreeVarIdentifiers =  freeVarIds,
            _lambdaExprNode = ExprNodeConstructor cons
        },
        _closureFreeVals = ClosureFreeVals consVals
    }
    where
       freeVarIds = map (VarName . (\x -> "id" ++ show x)) [1..(length consVals)]
       cons = Constructor (c ^. constructorName) (map AtomVarName freeVarIds)

stepCodeUpdatableReturnConstructor :: Constructor -> [Value] -> MachineT MachineProgress
stepCodeUpdatableReturnConstructor cons values = do
    appendLog $ pretty "using updatable return constructor."
    frame <- updateStackPop

    let as = frame ^. updateFrameArgumentStack
    let rs = frame ^. updateFrameReturnStack
    let addr = frame ^. updateFrameAddress

    returnStack .= rs
    argumentStack .= as

    let consClosure = _mkConstructorClosure cons values
    heapUpdateAddress addr (consClosure)

    return $ MachineStepped


stepCodeNonUpdatableReturnConstructor :: Constructor -> [Value] -> MachineT MachineProgress
stepCodeNonUpdatableReturnConstructor cons values = do
    appendLog $ pretty "using non-updatable return constructor."
    returnStackEmpty <- use $ returnStack . to null
    if returnStackEmpty then
        return MachineHalted
    else do
      cont <- returnStackPop

      let unwrapErr = StgErrorExpectedCaseAltConstructor cons
      unwrapped_alts <- unwrapAlts (cont ^. continuationAlts) _CaseAltConstructor  unwrapErr

      let filterErr = StgErrorNoMatchingAltPatternConstructor cons unwrapped_alts
      let consname = cons ^. constructorName
      let pred (ConstructorPatternMatch name _) = name == consname

      matchingAlt <- (filterEarliestAlt unwrapped_alts pred) `maybeToMachineT` filterErr
      let (ConstructorPatternMatch _ varnames) = matchingAlt ^. caseAltLHS

      -- assert ((length varnames) == (length values))
      let newValuesMap = M.fromList (zip varnames values)
      let modifiedEnv =  newValuesMap `M.union` (cont ^. continuationEnv)
      setCode $ CodeEval (matchingAlt ^. caseAltRHS) modifiedEnv
      return MachineStepped


shouldUseUpdatableStepForReturnConstructor :: PushEnterMachineState -> Bool
shouldUseUpdatableStepForReturnConstructor PushEnterMachineState {..} = length _argumentStack == 0 &&
                                                       length _returnStack == 0 &&
                                                       length _updateStack > 0

stepCodeReturnConstructor :: Constructor -> [Value] -> MachineT MachineProgress
stepCodeReturnConstructor cons values = do
   useUpdate <- gets shouldUseUpdatableStepForReturnConstructor
   if useUpdate then
      stepCodeUpdatableReturnConstructor cons values
   else
     stepCodeNonUpdatableReturnConstructor cons values



appendError :: Doc () -> MachineT ()
appendError = appendLog . mkStyleError

appendLog :: Doc () -> MachineT ()
appendLog s = currentLog <>= Log [s]

-- | 'CodeEval' execution
stepCodeEval :: LocalEnvironment -> ExprNode -> MachineT MachineProgress
stepCodeEval local expr = do
    appendLog $ pretty "stepCodeEval called"
    case expr of
        ExprNodeFnApplication f xs -> stepCodeEvalFnApplication local f xs  
        ExprNodeLet isReucursive bindings inExpr -> stepCodeEvalLet local  isReucursive bindings inExpr
        ExprNodeCase expr alts -> stepCodeEvalCase local expr alts
        ExprNodeInt i -> stepCodeEvalInt i
        ExprNodeConstructor cons -> stepCodeEvalConstructor local cons



stepCodeEvalConstructor :: LocalEnvironment -> Constructor -> MachineT MachineProgress
stepCodeEvalConstructor local (cons @ (Constructor consname consAtoms)) = do
    consVals <- for consAtoms (lookupAtom local)
    setCode $ CodeReturnConstructor cons consVals
    return MachineStepped


stepIntIntrinsic :: (Int -> Int -> Int) -> [Atom] -> MachineT  MachineProgress
stepIntIntrinsic f atoms = do
    -- TODO: make this a lens match that can fail make sure the function call looks like this
    let [AtomInt (StgInt x1), AtomInt (StgInt x2)] = atoms
    setCode $ CodeReturnInt $ StgInt (f x1 x2)
    return MachineStepped


stepCodeEvalFnApplication :: LocalEnvironment -> VarName -> [Atom] -> MachineT MachineProgress
stepCodeEvalFnApplication local lhsName vars = do
     case (M.lookup lhsName gVarNamesToIntIntrinsics) of
       Just f -> stepIntIntrinsic f vars
       -- we have no intrinsic, so do the usual lookup stuff
       Nothing -> do
        lhsValue <- lookupVariable local lhsName
        -- it doesn't have the address of a function, don't continue
        case lhsValue ^? _ValueAddr of
            Nothing -> return MachineHalted
            Just fnAddr ->  do
                  localVals <- for vars (lookupAtom local)
                  argumentStack `stackPushN` localVals
                  setCode $ CodeEnter fnAddr
                  return MachineStepped


_updateEnvWithBinding :: LocalEnvironment -> Binding -> MachineT LocalEnvironment
_updateEnvWithBinding l b = do
                  (name, addr) <- allocateBinding l b
                  let l' = l & at name .~ Just (ValueAddr addr)
                  return l'

stepCodeEvalLet :: LocalEnvironment -> IsLetRecursive -> [Binding] -> ExprNode -> MachineT MachineProgress
stepCodeEvalLet locals isLetRecursive bindings inExpr = do
  newLocals <- foldlM _updateEnvWithBinding locals bindings
  let updatedLocals = newLocals `M.union` locals
  setCode $ CodeEval inExpr updatedLocals
  return MachineStepped


stackPushN :: Lens' PushEnterMachineState (Stack a) -> [a] -> MachineT ()
stackPushN stackLens as'  = do
            as <- use (stackLens . unStack)
            stackLens .= Stack (as' ++ as)


returnStackPush :: Continuation -> MachineT ()
returnStackPush cont = do
  returnStack %= (\(Stack rs) -> Stack (cont:rs))


stepCodeEvalCase :: LocalEnvironment -> ExprNode -> [CaseAltType] -> MachineT MachineProgress
stepCodeEvalCase local expr alts = do
  returnStackPush (Continuation alts local)
  setCode $ CodeEval expr local
  return MachineStepped

stepCodeEvalInt :: StgInt -> MachineT MachineProgress
stepCodeEvalInt i = do
  setCode $ CodeReturnInt i
  return MachineStepped

isClosureUpdatable :: Closure -> Bool
isClosureUpdatable cls = cls ^. closureLambda ^. lambdaShouldUpdate

-- | codeEnter execution
stepCodeEnter :: Addr -> MachineT MachineProgress
stepCodeEnter addr = 
    do
        closure <- lookupAddrInHeap addr
        if isClosureUpdatable closure
        then stepCodeEnterIntoUpdatableClosure addr closure
        else stepCodeEnterIntoNonupdatableClosure addr closure

-- Enter a as rs where heap[ a -> (vs \u {} -> e) ws_f] 
-- Eval e local {} {} (as, rs, a):us heap where
--    local = [vs -> ws_f]
-- | Addr is the address of the closure
-- | Closure is the closure
stepCodeEnterIntoUpdatableClosure :: Addr -> Closure -> MachineT MachineProgress
stepCodeEnterIntoUpdatableClosure addr closure = do
    appendLog $ pretty closure <+> pretty "is updatable."
    let l = closure ^. closureLambda
    let boundVars = l ^. lambdaBoundVarIdentifiers
    let freeVarIds = l ^. lambdaFreeVarIdentifiers
    let evalExpr =  l ^. lambdaExprNode

    -- is there a better way to format this?
    if (length boundVars /= 0) then do
        appendLog $ pretty "updatable closure has bound variables:" <+> pretty boundVars
        error "updatable closure has bound variables"
    else do
        let localFreeVars = M.fromList (zip freeVarIds
                                                (closure ^. closureFreeVals . getFreeVals))
        let localEnv = localFreeVars

        -- push an update frame
        as <- use argumentStack
        rs <- use returnStack
        stackPushN updateStack [(UpdateFrame as rs addr)]
        appendLog $ pretty "pushed update frame"

        -- empty argument and return stack so that them being deref'd will trigger an update
        argumentStack .= stackEmpty
        returnStack .= stackEmpty

        setCode $ CodeEval evalExpr localEnv
        return MachineStepped



-- | old closure, new closure, current argument stack
-- | we are trying to update the old closure to capture the stuff the new closure
-- | does. So, we create a closure which executes the new closure's code from the
-- | old closure's context. Yes this is mind bending.
-- | rule 17 in the STG paper.
-- heap[addr] = old (vs \n xs -> e) ws_f
-- length (as) < length (xs)
-- new (vs ++ xs1 \n xs2 -> e) (ws_f ++ as)
-- I now understand what this does: this replaces a partially applied function
-- with an equivalent function that doesn't need to evaluate the application
-- That is, write:
--      f = \x y 
--      h = f g
-- as
--      h = \y where "x" is replaced by "g" in f's body
--      some form of "partial eta-reduction"
--
--      0x2 -> cls: [ {} \u {} -> var:flip {var:tuple} ];
--      0x2 -> cls: [ {var:f} \n {var:x, var:y} -> var:f {var:y, var:x} | Free variable vals:  val:0x0 ];
--      Notice how we expanded flip tuple out, and the "tuple" parameter became
--      a free variable.
mkEnterUpdateNewClosure :: Closure -> Closure ->  [Value] -> MachineT Closure
mkEnterUpdateNewClosure toUpdate cur as = do
  appendLog $ vsep [ pretty "updating old closure:" <+> pretty toUpdate 
                     ,pretty "with information from closure:" <+> pretty cur]
  let nStackArgs = length as
  let curFreeIds = cur ^. closureLambda ^. lambdaFreeVarIdentifiers
  -- xs1 ++ xs2 = xs
  let curBoundIds = cur ^. closureLambda . lambdaBoundVarIdentifiers
  let (boundToFree, stillBound) = (take nStackArgs curBoundIds, drop nStackArgs curBoundIds)

  appendLog $  vsep [pretty "current all bound variables:", nest 4 ((fmap pretty curBoundIds) & sep)]
  appendLog $  vsep [pretty "new bound variables to free:", nest 4 ((fmap pretty boundToFree) & sep)]
  appendLog $  vsep [pretty "new free variable values: ", nest 4 ((fmap pretty as) & sep)]
  appendLog $  vsep [pretty "new bound variables still bound:", nest 4 ((fmap pretty stillBound) & sep)]
  
  return $ cur {
    _closureFreeVals = ClosureFreeVals (cur ^. closureFreeVals . getFreeVals ++ as),
    _closureLambda = (cur ^. closureLambda) {
        _lambdaFreeVarIdentifiers = curFreeIds ++ boundToFree,
        _lambdaBoundVarIdentifiers = stillBound
        }
    }

-- provide the lambda and the list of free variables for binding
stepCodeEnterIntoNonupdatableClosure :: Addr -> Closure -> MachineT MachineProgress
stepCodeEnterIntoNonupdatableClosure addr closure = do
    appendLog $ pretty closure <+> pretty "is not updatable."
    let l = closure ^. closureLambda
    let boundVars = l ^. lambdaBoundVarIdentifiers
    let freeVars = l ^. lambdaFreeVarIdentifiers
    let evalExpr =  l ^. lambdaExprNode

    argStack <- use argumentStack
    let argStackLength = length argStack

    -- we don't have enough arguments to feed the closure, so we pop the update stack
    -- and pull values out
    if argStackLength < length boundVars
    -- we need to pop the update stack
    then do
        appendLog $ vsep [pretty "insufficient number of arguments on argument stack.",
                          nest 4 (vsep [pretty "needed:" <+> pretty (length boundVars) <+> pretty "bound values",
                                        pretty "had:" <+> pretty argStackLength <+> pretty "on argument stack.",
                                        pretty argStack])]
        appendLog $ pretty "looking for update frame to satisfy arguments..."
        uf@UpdateFrame {
              _updateFrameAddress=addru,
              _updateFrameArgumentStack=argStacku,
              _updateFrameReturnStack=rsu
        } <- updateStackPop
        appendLog $ vsep [pretty "found update frame: ",  pretty uf]

        let argStackList =  argStack ^. unStack

        toUpdateClosure <- lookupAddrInHeap addru
        newClosure <- (mkEnterUpdateNewClosure toUpdateClosure closure argStackList)
        heapUpdateAddress addru newClosure

        returnStack .= rsu
        argumentStack <>= argStacku

        code .= CodeEnter addr
        return MachineStepped
       
    else do
        boundVarVals <- boundVars & length & takeNArgs
        let localFreeVals = M.fromList (zip freeVars
                                                (closure ^. closureFreeVals . getFreeVals))
        let localBoundVals = M.fromList (zip boundVars
                                                    boundVarVals)
        let localEnv = localFreeVals `M.union` localBoundVals
        setCode $ CodeEval evalExpr localEnv
        return MachineStepped


-- | Return the variable if the continuation contains an alternative
-- for it. 
continuationGetVariableAlt :: Continuation -> Either StgError (CaseAlt VarName)
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

stackPop :: Lens' PushEnterMachineState (Stack a) -> StgError -> MachineT a
stackPop stacklens err = do
  isempty <- use (stacklens . to null)
  if isempty
  then
    throwError err
  else do
    top <- stacklens %%= (\(Stack (x:xs)) -> (x, Stack xs))
    return top

returnStackPop :: MachineT Continuation
returnStackPop = stackPop returnStack StgErrorReturnStackEmpty

unwrapAlts :: [CaseAltType] -> Prism' CaseAltType a -> (CaseAltType -> StgError) -> MachineT [a]
unwrapAlts [] p err = return []
unwrapAlts (a:as) p err = case a ^? p of
                      Just a -> do
                                  as' <- unwrapAlts as p err
                                  return $ (a:as')
                      Nothing -> throwError (err a)

filterEarliestAlt :: Eq a => [CaseAlt a] -> (a -> Bool) -> Maybe (CaseAlt a)
filterEarliestAlt [] _  = Nothing
filterEarliestAlt (c:cs) pred = if pred (c ^. caseAltLHS)
                                    then Just c
                                    else filterEarliestAlt cs pred

-- | codeReturnInt execution
stepCodeReturnInt :: StgInt -> MachineT MachineProgress
stepCodeReturnInt i = do
  cont <- returnStackPop
  unwrapped_alts <- unwrapAlts (cont ^. continuationAlts) _CaseAltInt StgErrorExpectedCaseAltInt
  let err = StgErrorNoMatchingAltPatternInt i unwrapped_alts
  alt <- (filterEarliestAlt unwrapped_alts (== i )) `maybeToMachineT`  err
  setCode $  CodeEval (alt ^. caseAltRHS) (cont ^. continuationEnv)
  return MachineStepped

genMachineTrace :: PushEnterMachineState -> ([PushEnterMachineState], Maybe StgError)
genMachineTrace state = 
  case runMachineT stepMachine state of
      Left err -> ([], Just err)
      Right (progress, state') -> if progress == MachineHalted
                                 then ([state], Nothing)
                                 else let (traceNext, err) = genMachineTrace state' in 
                                      (state':traceNext, err)

genFinalPushEnterMachineState :: PushEnterMachineState -> Either StgError PushEnterMachineState
genFinalPushEnterMachineState state =
    case runMachineT stepMachine state of
      Left err -> Left err
      Right (progress, state') -> if progress == MachineHalted
                                    then Right state'
                                    else genFinalPushEnterMachineState state'
