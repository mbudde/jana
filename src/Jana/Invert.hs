module Jana.Invert where

import Jana.Ast


invertMain :: ProcMain -> ProcMain
invertMain (ProcMain vdecls body) = ProcMain vdecls (invertStmts body)

invertProc :: Proc -> Proc
invertProc proc = proc { body = invertStmts $ body proc }

invertStmts :: [Stmt] -> [Stmt]
invertStmts = reverse . map invertStmt

invertStmt :: Stmt -> Stmt
invertStmt (Assign modOp lval expr) =
  Assign (invertModOp modOp) lval expr
  where invertModOp AddEq = SubEq
        invertModOp SubEq = AddEq
        invertModOp XorEq = XorEq
invertStmt (If e1 ifPart elsePart e2) =
  If e2 (invertStmts ifPart) (invertStmts elsePart) e1
invertStmt (From e1 doPart loopPart e2) =
  From e2 (invertStmts doPart) (invertStmts loopPart) e1
invertStmt (Push id1 id2) = Pop  id1 id2
invertStmt (Pop  id1 id2) = Push id1 id2
invertStmt (Local assign1 body assign2) =
  Local assign2 (invertStmts body) assign1
invertStmt (Call funId args) =
  Uncall funId args
invertStmt (Uncall funId args) =
  Call funId args
invertStmt stmt@(Swap _ _) = stmt
invertStmt Skip = Skip

