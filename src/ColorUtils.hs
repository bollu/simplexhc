module ColorUtils where

import System.Console.ANSI
import Text.PrettyPrint as PP

colorAddr :: Doc
colorAddr = zeroWidthText (setSGRCode [SetColor Foreground Vivid Green, SetUnderlining SingleUnderline])


colorTag :: Doc
colorTag = zeroWidthText (setSGRCode [SetColor Foreground Vivid White, SetConsoleIntensity BoldIntensity])

colorHeading :: Doc
colorHeading = zeroWidthText (setSGRCode [SetColor Foreground Vivid Blue])

colorReset :: Doc
colorReset = zeroWidthText (setSGRCode [Reset])

heading :: Doc -> Doc
heading d = colorHeading PP.<> d PP.<> colorReset
