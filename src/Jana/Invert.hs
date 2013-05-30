module Jana.Invert where

import Jana.Ast

data InvertMode = Globally | Locally

invertProgram :: Program -> Program
invertProgram (Program mains procs) =
  Program mains (map invertProcGlobally procs)

invertProc :: Proc -> Proc
invertProc proc = proc { body = invertStmts Locally $ body proc }

invertProcGlobally :: Proc -> Proc
invertProcGlobally proc = proc { body = invertStmts Globally $ body proc }

invertStmts :: InvertMode -> [Stmt] -> [Stmt]
invertStmts mode = reverse . map (invertStmt mode)

invertStmt :: InvertMode -> Stmt -> Stmt
invertStmt _ (Assign modOp lval expr pos) =
  Assign (invertModOp modOp) lval expr pos
  where invertModOp AddEq = SubEq
        invertModOp SubEq = AddEq
        invertModOp XorEq = XorEq
invertStmt mode (If e1 ifPart elsePart e2 pos) =
  If e2 (invertStmts mode ifPart) (invertStmts mode elsePart) e1 pos
invertStmt mode (From e1 doPart loopPart e2 pos) =
  From e2 (invertStmts mode doPart) (invertStmts mode loopPart) e1 pos
invertStmt _ (Push id1 id2 pos) = Pop  id1 id2 pos
invertStmt _ (Pop  id1 id2 pos) = Push id1 id2 pos
invertStmt mode (Local assign1 body assign2 pos) =
  Local assign2 (invertStmts mode body) assign1 pos
invertStmt Locally (Call funId args pos) =
  Uncall funId args pos
invertStmt Locally (Uncall funId args pos) =
  Call funId args pos
invertStmt Globally stmt@(Call{}) = stmt
invertStmt Globally stmt@(Uncall{}) = stmt
invertStmt _ stmt@(UserError{}) = stmt
invertStmt _ stmt@(Swap{}) = stmt
invertStmt _ stmt@(Prints{}) = stmt
invertStmt _ stmt@(Skip{}) = stmt

