module ColorUtils where

import System.Console.ANSI
import Text.PrettyPrint as PP

styleAddr :: Doc
styleAddr = zeroWidthText (setSGRCode [SetColor Foreground Vivid Green, SetUnderlining SingleUnderline])

styleTag :: Doc
-- styleTag = zeroWidthText (setSGRCode [SetUnderlining SingleUnderline])
styleTag = zeroWidthText (setSGRCode [SetColor Foreground Dull Yellow])


-- | use for tags on atoms, etc.
mkStyleTag :: Doc -> Doc
mkStyleTag tag = styleTag <> tag <> styleReset

styleHeading :: Doc
styleHeading = zeroWidthText (setSGRCode [SetColor Foreground Vivid Blue])

styleReset :: Doc
styleReset = zeroWidthText (setSGRCode [Reset])

heading :: Doc -> Doc
heading d = styleHeading PP.<> d PP.<> styleReset

styleError :: Doc
styleError = zeroWidthText (setSGRCode [SetColor Foreground Vivid Red])

-- | use to style errors
mkStyleError :: Doc -> Doc
mkStyleError doc = styleError <> doc <> styleReset

styleAnnotation :: Doc
styleAnnotation = zeroWidthText (setSGRCode [SetColor Foreground Vivid White])

-- | use to style simplexhc annotations that are not part of the source language.
-- | For example, used to number stack arguments
mkStyleAnnotation :: Doc -> Doc
mkStyleAnnotation n = styleAnnotation <> n <> styleReset
