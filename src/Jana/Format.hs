module Jana.Format where

import Prelude hiding (GT, LT, EQ)
import Data.List (intersperse)
import Text.PrettyPrint
import qualified Data.Map as Map
import Jana.Ast


commasep = hsep . punctuate (char ',')


formatType (Int _)   = text "int"
formatType (Stack _) = text "stack"

formatIdent :: Ident -> Doc
formatIdent id = text (ident id)

formatLval :: Lval -> Doc
formatLval (Var id) = formatIdent id
formatLval (Lookup id expr) = formatIdent id <> brackets (formatExpr expr)

formatModOp AddEq = text "+="
formatModOp SubEq = text "-="
formatModOp XorEq = text "^="

-- Operators and their precedence
-- Should match the operator table in Jana.Parser
opMap = Map.fromList [
    (Mul , ("*",  4))
  , (Div , ("/",  4))
  , (Mod , ("%",  4))

  , (Add , ("+",  3))
  , (Sub , ("-",  3))

  , (GE  , (">=", 2))
  , (GT  , (">",  2))
  , (LE  , ("<=", 2))
  , (LT  , ("<",  2))
  , (EQ  , ("=",  2))
  , (NEQ , ("!=", 2))

  , (And , ("&",  1))
  , (Or  , ("|",  1))
  , (Xor , ("^",  1))

  , (LAnd, ("&&", 0))
  , (LOr , ("||", 0))
  ]


formatBinOp = text . fst . (opMap Map.!)


formatExpr = f 0
  where f _ (Number num _)   = integer num
        f _ (LV lval _)      = formatLval lval
        f _ (Empty id _)     = text "empty" <> parens (formatIdent id)
        f _ (Top id _)       = text "top" <> parens (formatIdent id)
        f _ (Nil _)          = text "nil"
        f d (BinOp op e1 e2) =
          let opd = opPrec op in
            parens' (d > opd) (f opd e1 <+> formatBinOp op <+> f opd e2)
        opPrec = snd . (opMap Map.!)
        parens' bool = if bool then parens else id


formatVdecl (Scalar typ id _) =
  formatType typ <+> formatIdent id

formatVdecl (Array id size _) =
  text "int" <+> formatIdent id <> brackets (integer size)


formatStmts = vcat . map formatStmt


formatStmt (Assign modOp lval expr _) =
  formatLval lval <+> formatModOp modOp <+> formatExpr expr

formatStmt (If e1 s1 s2 e2 _) =
  text "if" <+> formatExpr e1 <+> text "then" $+$
    nest 4 (formatStmts s1) $+$
  elsePart $+$
  text "fi" <+> formatExpr e2
  where elsePart | null s2   = empty
                 | otherwise = text "else" $+$ nest 4 (formatStmts s2)

formatStmt (From e1 s1 s2 e2 _) =
  text "from" <+> formatExpr e1 <+> keyword $+$
    vcat inside $+$
  text "until" <+> formatExpr e2
  where (keyword:inside) = doPart ++ loopPart
        doPart   | null s1   = []
                 | otherwise = [text "do", nest 4 (formatStmts s1)]
        loopPart | null s2   = [empty]
                 | otherwise = [text "loop", nest 4 (formatStmts s2)]

formatStmt (Push id1 id2 _) =
  text "push" <> parens (formatIdent id1 <> comma <+> formatIdent id2)

formatStmt (Pop id1 id2 _) =
  text "pop" <> parens (formatIdent id1 <> comma <+> formatIdent id2)

formatStmt (Local (typ1, id1, e1) s (typ2, id2, e2) _) =
  text "local" <+> localDecl typ1 id1 e1 $+$
  formatStmts s $+$
  text "delocal" <+> localDecl typ2 id2 e2
  where localDecl typ id expr =
          formatType typ <+> formatIdent id <+> equals <+> formatExpr expr

formatStmt (Call id args _) =
  text "call" <+> formatIdent id <> parens (commasep $ map formatIdent args)

formatStmt (Uncall id args _) =
  text "uncall" <+> formatIdent id <> parens (commasep $ map formatIdent args)

formatStmt (Swap id1 id2 _) =
  formatIdent id1 <+> text "<=>" <+> formatIdent id2

formatStmt (Skip _) =
  text "skip"


formatDelocal (Local _ _ (typ, id, e) _) =
  text "delocal" <+> formatType typ <+> formatIdent id <+> equals <+> formatExpr e
formatDelocal _ = undefined


formatMain (ProcMain vdecls body _) =
  text "procedure main()" $+$
    nest 4 (vcat (map formatVdecl vdecls) $+$
            text "" $+$
            formatStmts body)


formatParams = commasep . map formatParam
  where formatParam (typ, id) = formatType typ <+> formatIdent id


formatProc proc =
  text "procedure" <+> formatIdent (procname proc) <>
  parens (formatParams $ params proc) $+$
    nest 4 (formatStmts $ body proc)


formatProgram (Program main procs) =
  vcat (intersperse (text "") $ map formatProc procs) $+$
  text "" $+$
  formatMain main



instance Show Type where
  show = render . formatType

instance Show Ident where
  show = render . formatIdent

instance Show Lval where
  show = render . formatLval

instance Show Expr where
  show = render . formatExpr

instance Show Stmt where
  show = render . formatStmt

instance Show Vdecl where
  show = render . formatVdecl

instance Show Proc where
  show = render . formatProc

instance Show ProcMain where
  show = render . formatMain

instance Show Program where
  show = render . formatProgram
