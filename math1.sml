datatype expr
	= EAdd of expr * expr
	| EInt of int
	| EMul of expr * expr
	| ENeg of expr
	| ESub of expr * expr
	| EVec of int list

datatype value = VInt of int
				| VVec of int list

fun eval (EAdd (e1, e2)) = (eval e1) + (eval e2)
	| eval (EInt  i) = i
	| eval (EMul (e1, e2)) = e1 * e2
	| eval (ENeg (e1, e2)) = expr
	| eval (ESub (e1, e2)) = e1 - e2
	| eval (EVec (e1, e2)) = int list