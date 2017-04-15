{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveTraversable #-}

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
    mkDoc addr = styleAddr PP.<> (text $ "0x" ++ (addr & _getAddr & (\x -> showHex x ""))) PP.<> styleReset

instance Show Addr where
    show = renderStyle showStyle . mkDoc

data Value = ValueAddr Addr | ValuePrimInt Int
    deriving (Eq, Ord)


instance Prettyable Value where
  mkDoc (ValueAddr addr) = mkStyleTag (text "val:") PP.<> mkDoc addr
  mkDoc (ValuePrimInt int) = mkStyleTag (text "val:") PP.<> text (show int) PP.<> text "#" 

instance Show Value where
    show = renderStyle showStyle . mkDoc

-- | Stack of 'Value'
newtype Stack a = Stack { _unStack :: [a] } deriving(Functor, Monoid, Foldable, Traversable)
stackEmpty :: Stack a
stackEmpty = Stack []

instance Prettyable a => Prettyable (Stack a) where
  mkDoc (Stack []) = text "EMPTY"
  mkDoc (Stack xs) = text ("count: " ++ show (length xs)) $+$ text "TOP" $+$ ((fmap mkDoc xs) & sep) $+$ text "BOTTOM"

instance Prettyable a => Show (Stack a) where
  show = renderStyle showStyle . mkDoc

type ArgumentStack = Stack Value
type ReturnStack = Stack Continuation
type UpdateStack = Stack UpdateFrame
type Heap = M.Map Addr Closure


-- | Maps VarName names to addresses of the closures
type GlobalEnvironment = M.Map VarName Addr

-- | has bindings of free variables with a 'LambdaForm'
newtype ClosureFreeVars = ClosureFreeVars { _getFreeVars :: [Value] } deriving(Show)
instance Prettyable ClosureFreeVars where
  mkDoc freeVars = _getFreeVars freeVars & map mkDoc & punctuate (text ",") & hsep
data Closure = Closure { 
    _closureLambda :: !Lambda,
    _closureFreeVars :: !ClosureFreeVars
} deriving (Show)

instance Prettyable Closure where
  mkDoc (Closure{..}) = (mkStyleTag (text "cls:<<"))
                         <+> mkDoc _closureLambda $$
                         text "|" <+> mkDoc  _closureFreeVars <+> (mkStyleTag (text ">>"))


type LocalEnvironment = M.Map VarName Value

instance (Prettyable k, Prettyable v) => Prettyable (M.Map k v) where
  mkDoc m = (fmap (uncurry mkKvDoc) (M.toList m))  & punctuate (text ";") & vcat where
              mkKvDoc key val = mkDoc key <+> text "->" <+> mkDoc val

data Code = CodeEval ExprNode LocalEnvironment | 
            CodeEnter Addr |
            CodeUninitialized |
            CodeReturnConstructor Constructor [Value] |
            CodeReturnInt RawNumber deriving(Show) 

instance Prettyable Code where
  mkDoc (CodeEval expr env) = text "Eval" <+> braces (mkDoc expr) <+> text "|Local:" <+> braces(mkDoc env)
  mkDoc (CodeEnter addr) = text "Enter" <+> mkDoc addr
  mkDoc (CodeReturnConstructor cons values) = 
    text "ReturnConstructor" <+>  
      (mkDoc (cons ^. constructorName) <+> 
     (values & map mkDoc & punctuate comma & hsep & braces) & parens)
  mkDoc (CodeReturnInt i) = text "ReturnInt" <+> text (show i)




data MachineState = MachineState {
    _argumentStack :: !ArgumentStack,
    _returnStack :: !ReturnStack,
    _updateStack :: !UpdateStack,
    _heap :: !Heap,
    _globalEnvironment :: !GlobalEnvironment,
    _code :: !Code
}


instance Prettyable MachineState where
  mkDoc MachineState{..} = 
   heading (text "*** Code:") $$ code $+$
   heading (text "*** Args:") $$ argsDoc $+$
   heading (text "*** Return:") $$ returnDoc $+$
   heading (text "*** Update:") $$ updateDoc $+$
   heading (text "*** Heap:") $$ heapDoc $+$
   heading (text "*** Env:") $$ globalEnvDoc $+$
   heading (text "---") where
    argsDoc = _argumentStack & mkDoc
    returnDoc = _returnStack & mkDoc
    updateDoc = _updateStack & mkDoc
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
        StgErrorExpectedCaseAltRawNumber !CaseAltType | 
        -- | `unwrapAlts` failed, unable to unwrap Constructor
        StgErrorExpectedCaseAltConstructor Constructor !CaseAltType | 
        -- | 'xxx' failed, no matching pattern match found
        StgErrorNoMatchingAltPatternRawNumber RawNumber [CaseAlt RawNumber] |
        -- | 'xxx' failed, no matching pattern match found
        StgErrorNoMatchingAltPatternConstructor Constructor [CaseAlt ConstructorPatternMatch] deriving(Show)

makeLenses ''ClosureFreeVars
makePrisms ''Value
makeLenses ''Closure
makePrisms ''Code
makeLenses ''MachineState
makeLenses ''Addr
makeLenses ''Continuation
makeLenses ''Stack

-- runExceptT :: ExceptT e (State s) a -> State s (Either e a)

uninitializedMachineState :: MachineState
uninitializedMachineState = MachineState {
    _argumentStack=stackEmpty,
    _returnStack = stackEmpty,
    _updateStack = stackEmpty,
    _heap=M.empty,
    _globalEnvironment=M.empty,
    _code=CodeUninitialized
}

maybeToMachineT :: Maybe a -> StgError -> MachineT a
maybeToMachineT (Nothing) err = throwError err
maybeToMachineT (Just a) err = return a 

runMachineT :: MachineT a -> MachineState -> Either StgError (a, MachineState)
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

-- allocate the bindings on the heap, and return the mapping
-- between variable names to addresses
compileProgram :: Program -> Either StgError MachineState
compileProgram prog = snd <$> (runMachineT setupBindings uninitializedMachineState)
  where
    setupBindings :: MachineT ()
    setupBindings = do 
      let localenv = M.empty  -- when machine starts, no local env.
      nameAddrPairs <- for prog (allocateBinding localenv) :: MachineT [(VarName, Addr)]
      globalEnvironment .= M.fromList nameAddrPairs

      mainAddr <-  use globalEnvironment >>= (\x -> maybeToMachineT (x ^. at (VarName "main")) StgErrorUnableToFindMain) :: MachineT Addr
      -- NOTE: this is different from STG paper. Does this even work?
      code .= CodeEnter mainAddr

isExprPrimitive :: ExprNode -> Bool
isExprPrimitive (ExprNodeRawNumber _) = True
isExprPrimitive _ = False

isMachineStateFinal :: MachineState -> Bool
isMachineStateFinal m = case m ^. code of
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


rawNumberToValue :: RawNumber -> Either StgError Value
rawNumberToValue raw = maybeToEither errormsg mval where
    mval = raw ^. getRawNumber & maybeRead & (fmap ValuePrimInt)
    errormsg = StgErrorUnableToMkPrimInt raw

lookupAtom :: LocalEnvironment -> Atom -> MachineT Value
lookupAtom _ (AtomRawNumber r) = hoistError id (rawNumberToValue r)
lookupAtom localEnv (AtomVarName ident) = lookupVariable localEnv ident




mkClosureFromLambda :: Lambda -> LocalEnvironment -> MachineT Closure
mkClosureFromLambda lambda localenv = 
    do
      freeVarVals <- for (lambda ^. lambdaFreeVarIdentifiers) (lookupVariable  localenv)
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
    mclosure `maybeToMachineT` errormsg

-- pop n values off the argument stack
takeNArgs :: Int -> MachineT [Value]
takeNArgs n = do
    argStackList <- use (argumentStack . unStack)
    if length argStackList < n
        then throwError $ StgErrorNotEnoughArgsOnStack n (Stack argStackList)
        else do
            let args = take n argStackList
            argumentStack .= Stack (drop n argStackList)
            return args


stepMachine :: MachineT MachineProgress
stepMachine = do
    code <- use code
    case code of
        CodeEval f local -> stepCodeEval local f
        CodeEnter addr -> stepCodeEnter addr
        CodeReturnInt i -> stepCodeReturnInt i
        CodeReturnConstructor cons consvals -> stepCodeReturnConstructor cons consvals



stepCodeReturnConstructor :: Constructor -> [Value] -> MachineT MachineProgress
stepCodeReturnConstructor cons values = do
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
      code .= CodeEval (matchingAlt ^. caseAltRHS) modifiedEnv
      return MachineStepped

-- | 'CodeEval' execution
stepCodeEval :: LocalEnvironment -> ExprNode -> MachineT MachineProgress
stepCodeEval local expr = do
    case expr of
        ExprNodeFnApplication f xs -> stepCodeEvalFnApplication local f xs  
        ExprNodeLet isReucursive bindings inExpr -> stepCodeEvalLet local  isReucursive bindings inExpr
        ExprNodeCase expr alts -> stepCodeEvalCase local expr alts
        ExprNodeRawNumber num -> stepCodeEvalRawNumber num
        ExprNodeConstructor cons -> stepCodeEvalConstructor local cons


stepCodeEvalConstructor :: LocalEnvironment -> Constructor -> MachineT MachineProgress
stepCodeEvalConstructor local (cons @ (Constructor consname consAtoms)) = do
    consVals <- for consAtoms (lookupAtom local)
    code .= CodeReturnConstructor cons consVals
    return MachineStepped

stepCodeEvalFnApplication :: LocalEnvironment -> VarName -> [Atom] -> MachineT MachineProgress
stepCodeEvalFnApplication local lhsName vars = do
     lhsValue <- lookupVariable local lhsName
     -- it doesn't have the address of a function, don't continue
     case lhsValue ^? _ValueAddr of
        Nothing -> return MachineHalted
        Just fnAddr ->  do
               localVals <- for vars (lookupAtom local)
               argumentStack `stackPushN` localVals
               code .= CodeEnter fnAddr
               return MachineStepped



stepCodeEvalLet :: LocalEnvironment -> IsLetRecursive -> [Binding] -> ExprNode -> MachineT MachineProgress
stepCodeEvalLet locals isLetRecursive bindings inExpr = do
  let lookupEnv = locals
  closureNameAddrPairs <- for bindings (allocateBinding lookupEnv) :: MachineT [(VarName, Addr)]

  let closureNameValuePairs = closureNameAddrPairs & traverse . _2 %~ ValueAddr

  -- TODO: rewrite with M.union
  let newLocals = M.fromList closureNameValuePairs
  let updatedLocals = newLocals `M.union` locals
  -- let updatedLocals = foldl  (\local (name, addr) -> M.insert name (ValueAddr addr) local)  local closureNameAddrMap
  code .= CodeEval inExpr updatedLocals
  return MachineStepped


stackPushN :: Lens' MachineState (Stack a) -> [a] -> MachineT ()
stackPushN stackLens as'  = do
            as <- use (stackLens . unStack)
            stackLens .= Stack (as' ++ as)


returnStackPush :: Continuation -> MachineT ()
returnStackPush cont = do
  returnStack %= (\(Stack rs) -> Stack (cont:rs))


stepCodeEvalCase :: LocalEnvironment -> ExprNode -> [CaseAltType] -> MachineT MachineProgress
stepCodeEvalCase local expr alts = do
  returnStackPush (Continuation alts local)
  code .= CodeEval expr local
  return MachineStepped

stepCodeEvalRawNumber :: RawNumber -> MachineT MachineProgress
stepCodeEvalRawNumber rawnum = do
  code .= CodeReturnInt rawnum
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
    let boundVars = l ^. lambdaBoundVarIdentifiers
    let freeVars = l ^. lambdaFreeVarIdentifiers
    let evalExpr =  l ^. lambdaExprNode

    boundVarVals <- boundVars & length & takeNArgs
    let localFreeVars = M.fromList (zip freeVars
                                               (closure ^. closureFreeVars . getFreeVars))
    let localBoundVars = M.fromList (zip boundVars
                                                boundVarVals)
    let localEnv = localFreeVars `M.union` localBoundVars
    code .= CodeEval evalExpr localEnv
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

returnStackPop :: MachineT Continuation
returnStackPop = do
  isempty <- use (returnStack . to null)
  if isempty
  then
    throwError StgErrorReturnStackEmpty
  else do
    top <- returnStack %%= (\(Stack (r:rs)) -> (r, Stack rs))
    return top

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
stepCodeReturnInt :: RawNumber -> MachineT MachineProgress
stepCodeReturnInt i = do
  cont <- returnStackPop
  unwrapped_alts <- unwrapAlts (cont ^. continuationAlts) _CaseAltRawNumber StgErrorExpectedCaseAltRawNumber
  let err = StgErrorNoMatchingAltPatternRawNumber i unwrapped_alts
  alt <- (filterEarliestAlt unwrapped_alts (== i )) `maybeToMachineT`  err
  code .= CodeEval (alt ^. caseAltRHS) (cont ^. continuationEnv)
  return MachineStepped




genMachineTrace :: MachineState -> ([MachineState], Maybe StgError)
genMachineTrace state = 
  case runMachineT stepMachine state of
      Left err -> ([], Just err)
      Right (progress, state') -> if progress == MachineHalted
                                 then ([state], Nothing)
                                 else let (traceNext, err) = genMachineTrace state' in 
                                      (state':traceNext, err)

genFinalMachineState :: MachineState -> Either StgError MachineState
genFinalMachineState state =
    case runMachineT stepMachine state of
      Left err -> Left err
      Right (progress, state') -> if progress == MachineHalted
                                    then Right state'
                                    else genFinalMachineState state'
