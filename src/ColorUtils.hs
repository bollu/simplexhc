module ColorUtils where

import System.Console.ANSI
import Text.PrettyPrint as PP

styleAddr :: Doc
styleAddr = zeroWidthText (setSGRCode [SetColor Foreground Vivid Green, SetUnderlining SingleUnderline])

styleTag :: Doc
-- styleTag = zeroWidthText (setSGRCode [SetUnderlining SingleUnderline])
styleTag = zeroWidthText (setSGRCode [SetColor Foreground Dull Yellow])

styleError :: Doc
styleError = zeroWidthText (setSGRCode [SetColor Foreground Vivid Red])

mkStyleTag :: Doc -> Doc
mkStyleTag tag = styleTag <> tag <> styleReset

styleHeading :: Doc
styleHeading = zeroWidthText (setSGRCode [SetColor Foreground Vivid Blue])

styleReset :: Doc
styleReset = zeroWidthText (setSGRCode [Reset])

heading :: Doc -> Doc
heading d = styleHeading PP.<> d PP.<> styleReset

mkStyleError :: Doc -> Doc
mkStyleError doc = styleError <> doc <> styleReset
