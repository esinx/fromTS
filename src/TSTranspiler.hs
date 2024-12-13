module TSTranspiler where
import Text.PrettyPrint qualified as PP
import TSSyntax

transpileTS :: Block -> String
transpileTS (Block stmts) =
    foldr (\cur acc -> pretty cur ++ acc) "" stmts
