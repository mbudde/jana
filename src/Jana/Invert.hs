module Jana.Invert where

import Jana.Ast

invertProgram :: Program -> Program
invertProgram (Program mains procs) =
  Program (map invertMain mains) (map invertProc procs)

invertMain :: ProcMain -> ProcMain
invertMain (ProcMain vdecls body pos) = ProcMain vdecls (invertStmts body) pos

invertProc :: Proc -> Proc
invertProc proc = proc { body = invertStmts $ body proc }

invertStmts :: [Stmt] -> [Stmt]
invertStmts = reverse . map invertStmt

invertStmt :: Stmt -> Stmt
invertStmt (Assign modOp lval expr pos) =
  Assign (invertModOp modOp) lval expr pos
  where invertModOp AddEq = SubEq
        invertModOp SubEq = AddEq
        invertModOp XorEq = XorEq
invertStmt (If e1 ifPart elsePart e2 pos) =
  If e2 (invertStmts ifPart) (invertStmts elsePart) e1 pos
invertStmt (From e1 doPart loopPart e2 pos) =
  From e2 (invertStmts doPart) (invertStmts loopPart) e1 pos
invertStmt (Push id1 id2 pos) = Pop  id1 id2 pos
invertStmt (Pop  id1 id2 pos) = Push id1 id2 pos
invertStmt (Local assign1 body assign2 pos) =
  Local assign2 (invertStmts body) assign1 pos
invertStmt (Call funId args pos) =
  Uncall funId args pos
invertStmt (Uncall funId args pos) =
  Call funId args pos
invertStmt stmt@(UserError{}) = stmt
invertStmt stmt@(Swap{}) = stmt
invertStmt stmt@(Prints{}) = stmt
invertStmt (Skip pos) = Skip pos

