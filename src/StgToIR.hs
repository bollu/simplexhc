module StgToIR where
import StgLanguage
import ColorUtils

import IR
import IRBuilder
import Control.Monad.State
import Control.Monad.Except


programToModule :: Program -> Module
programToModule p = error "undefined for now"
