import Prelude hiding ((<>))
import Language.C.Parser (execParser_, expressionP)
import Language.C.Data (NodeInfo, identToString)
import Language.C.Data.InputStream (inputStreamFromString)
import Language.C.Data.Position (nopos)
import Language.C.Pretty (pretty, Pretty)
import Language.C.Syntax.AST
import Text.PrettyPrint

-- | Parenthesize a single initializer
parensInitializer :: CInitializer NodeInfo -> Doc
parensInitializer (CInitExpr e _) = prettyParens e
parensInitializer (CInitList initl _) = parensInitializers initl

-- | Parenthesize an initializer list
parensInitializers :: CInitializerList NodeInfo -> Doc
parensInitializers initl = braces (hsep (punctuate comma (map p initl)))
  where p ([], initializer) = parensInitializer initializer
        p (mems, initializer) =
          hcat (punctuate (text ".") (map pretty mems))
          <+> text "=" <+> parensInitializer initializer

-- | A copy of the 'Pretty' instance for 'CExpression' that inserts
-- parentheses.
prettyParens :: CExpression NodeInfo -> Doc
prettyParens (CComma exprs _) =
  parens (hsep (punctuate comma (map prettyParens exprs)))
prettyParens (CAssign op e1 e2 _) =
  parens (prettyParens e1 <+> pretty op <+> prettyParens e2)
prettyParens (CCond e1 e2 e3 _) =
  parens (prettyParens e1
          <+> text "?" <+> maybe empty prettyParens e2
          <+> text ":" <+> prettyParens e3)
prettyParens (CBinary op e1 e2 _) =
  parens (prettyParens e1 <+> pretty op <+> prettyParens e2)
prettyParens (CCast decl expr _) =
  parens (text "(" <> pretty decl <> text ")" <+> prettyParens expr)
prettyParens (CUnary CPostIncOp e _) = parens (prettyParens e <> text "++")
prettyParens (CUnary CPostDecOp e _) = parens (prettyParens e <> text "--")
prettyParens (CUnary op e@(CUnary _ _ _) _) =
  parens (pretty op <+> parens (prettyParens e))
prettyParens (CUnary op e _) = parens (pretty op <> prettyParens e)
prettyParens (CSizeofExpr e _) =
  parens (text "sizeof" <> parens (prettyParens e))
prettyParens (CSizeofType decl _) =
  parens (text "sizeof" <> parens (pretty decl))
prettyParens (CAlignofExpr e _) = parens (text "__alignof" <> prettyParens e)
prettyParens (CAlignofType d _) = parens (text "__alignof" <> parens (pretty d))
prettyParens (CComplexReal e _) = parens (text "__real" <+> prettyParens e)
prettyParens (CComplexImag e _) = parens (text "__imag" <+> prettyParens e)
prettyParens (CIndex e1 e2 _) =
  parens (prettyParens e1 <> brackets (prettyParens e2))
prettyParens (CCall e args _) =
  prettyParens e <> parens (sep (punctuate comma (map prettyParens args)))
prettyParens (CMember e ident deref _) =
  parens (prettyParens e
          <> text (if deref then "->" else ".")
          <> text (identToString ident))
prettyParens (CVar ident _) = text (identToString ident)
prettyParens (CConst c) = pretty c
prettyParens (CCompoundLit decl initl _) =
  parens (parens (pretty decl) <+> parensInitializers initl)
prettyParens (CStatExpr stat _) = parens (pretty stat)
prettyParens (CLabAddrExpr ident _) = text "&&" <> text (identToString ident)
prettyParens (CGenericSelection e alist _) =
  parens (text "_Generic" <>
          parens
            (hsep (punctuate comma (prettyParens e : map pAssoc alist))))
  where pAssoc (mty, e1) = maybe (text "default") pretty mty
                           <> text ":" <+> prettyParens e1
prettyParens (CBuiltinExpr builtin) = pretty builtin

-- | Parse a C expression and insert all parentheses.
parseToParens :: String -> String
parseToParens = either (error . show) (show . prettyParens)
              . flip (execParser_ expressionP) nopos
              . inputStreamFromString

-- | Remove the outermost pair of balanced parentheses. Assumes
-- parentheses /are/ balanced.
stripOuterParens :: String -> String
stripOuterParens ('(' : xs) = init xs
stripOuterParens s = s

main :: IO ()
main = getLine >>= putStrLn . stripOuterParens . parseToParens
