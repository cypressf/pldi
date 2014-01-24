datatype expr
	= EAdd of expr * expr
	| EInt of int
	| EMul of expr * expr
	| ENeg of expr
	| ESub of expr * expr
	| EVec of int list