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

newtype MachineT a = MachineT { runMachineTa :: StateT MachineState (Either StgError) a }
            deriving (Functor, Applicative, Monad
               , MonadState MachineState
               , MonadError StgError)


isMachineStateFinal :: MachineState -> Bool
isMachineStateFinal m = False

-- | All possible errors when interpreting STG code.
data StgError = 
        StgErrorLookupFailed Identifier LocalEnvironment GlobalEnvironment deriving (Show) -- ^ 'lookupVariable' failed


-- | Try to lookup 'Identifier' in the local & global environments. Fail if unable to lookup.
lookupVariable :: LocalEnvironment -> Identifier -> MachineT Value
lookupVariable localEnv ident = do
        globalEnv <- use globalEnvironment

        let localLookup = (localEnv ^. at ident)
        let globalLookup = (ValueAddr <$> (globalEnv ^. at ident))

        let errormsg = StgErrorLookupFailed ident localEnv globalEnv
        maybeToEither errormsg (localLookup <|> globalLookup)



stepMachine :: MachineT ()
stepMachine = do
    code <- use code
    case code of
        CodeEval f local -> stepCodeEval f local


-- | 'CodeEval' execution
stepCodeEval :: ExprNode -> LocalEnvironment -> MachineT ()
stepCodeEval expr local = do
    case expr of
        ExprNodeFnApplication f xs -> stepEvalFnApplication f xs local 


stepEvalFnApplication :: Identifier -> [Atom] -> LocalEnvironment -> MachineT ()
stepEvalFnApplication fn vars local = do
     var <- (lookupVariable local) fn
     return ()
