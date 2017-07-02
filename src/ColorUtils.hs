module ColorUtils where

import System.Console.ANSI
import Data.Text.Prettyprint.Doc as PP
-- PI = PrettyprinterInternal
import Data.Text.Prettyprint.Doc.Internal as PI
import Data.Text.Prettyprint.Doc.Render.Text as PP
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL


mkNest :: Doc a -> Doc a
mkNest = indent 4


docToString :: Doc a -> String
docToString doc = TL.unpack  (renderLazy (layoutPretty defaultLayoutOptions doc))

prettyToString :: Pretty a => a -> String
prettyToString a = docToString (pretty a)

-- TODO: this is a hack in GHC as well. Do not do this, use the "ann" in Doc
-- to annotate.
zeroWidthText :: String -> Doc a
zeroWidthText s = PI.Text 0 (T.pack s)

styleAddr :: Doc a
styleAddr = zeroWidthText (setSGRCode [SetColor Foreground Vivid Green, SetUnderlining SingleUnderline])

styleTag :: Doc a
-- styleTag = zeroWidthText (setSGRCode [SetUnderlining SingleUnderline])
styleTag = zeroWidthText (setSGRCode [SetColor Foreground Dull Yellow])



-- | use for tags on atoms, etc.
mkStyleTag :: Doc a -> Doc a
mkStyleTag tag = styleTag <> tag <> styleReset

styleHeading :: Doc a
styleHeading = zeroWidthText (setSGRCode [SetColor Foreground Vivid Blue])

styleReset :: Doc a
styleReset = zeroWidthText (setSGRCode [Reset])

heading :: Doc a -> Doc a
heading d = styleHeading PP.<> d PP.<> styleReset

styleError :: Doc a
styleError = zeroWidthText (setSGRCode [SetColor Foreground Vivid Red])

-- | use to style errors
mkStyleError :: Doc a -> Doc a
mkStyleError doc = styleError <> doc <> styleReset

styleAnnotation :: Doc a
styleAnnotation = zeroWidthText (setSGRCode [SetColor Foreground Vivid White])

-- | use to style simplexhc annotations that are not part of the source language.
-- | For example, used to number stack arguments
mkStyleAnnotation :: Doc a -> Doc a
mkStyleAnnotation n = styleAnnotation <> n <> styleReset
