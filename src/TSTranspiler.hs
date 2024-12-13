module TSTranspiler where
import Text.PrettyPrint qualified as PP
import TSSyntax

transpileTS :: Block -> String
transpileTS = pretty
