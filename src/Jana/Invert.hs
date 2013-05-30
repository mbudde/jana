module Jana.Invert where

import Jana.Ast

invertProgram :: Program -> Program
invertProgram (Program mains procs) =
  Program mains (map invertProcGlobally procs)

invertProc :: Proc -> Proc
invertProc proc = proc { body = invertStmts False $ body proc }

invertProcGlobally :: Proc -> Proc
invertProcGlobally proc = proc { body = invertStmts True $ body proc } 

invertStmts :: Bool -> [Stmt] -> [Stmt]
invertStmts global = reverse . map (invertStmt global)

invertStmt :: Bool -> Stmt -> Stmt
invertStmt _ (Assign modOp lval expr pos) =
  Assign (invertModOp modOp) lval expr pos
  where invertModOp AddEq = SubEq
        invertModOp SubEq = AddEq
        invertModOp XorEq = XorEq
invertStmt global (If e1 ifPart elsePart e2 pos) =
  If e2 (invertStmts global ifPart) (invertStmts global elsePart) e1 pos
invertStmt global (From e1 doPart loopPart e2 pos) =
  From e2 (invertStmts global doPart) (invertStmts global loopPart) e1 pos
invertStmt _ (Push id1 id2 pos) = Pop  id1 id2 pos
invertStmt _ (Pop  id1 id2 pos) = Push id1 id2 pos
invertStmt global (Local assign1 body assign2 pos) =
  Local assign2 (invertStmts global body) assign1 pos
invertStmt True stmt@(Call{}) =
  stmt
invertStmt False (Call funId args pos) =
  Uncall funId args pos
invertStmt True stmt@(Uncall{}) =
  stmt
invertStmt False (Uncall funId args pos) =
  Call funId args pos
invertStmt _ stmt@(UserError{}) = stmt
invertStmt _ stmt@(Swap{}) = stmt
invertStmt _ stmt@(Prints{}) = stmt
invertStmt _ stmt@(Skip{}) = stmt

