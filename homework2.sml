
(*
 * Code for HOMEWORK 2
 *
 *)

Control.Print.printDepth := 100;

(* Internal representation *)

datatype value = VInt of int
               | VBool of bool
               | VList of value list
      	       | VPair of value * value
               | VFun of function

and expr = EVal of value
         | EAdd of expr * expr
         | ESub of expr * expr
         | EMul of expr * expr
         | ENeg of expr
         | EEq of expr * expr
         | EIf of expr * expr * expr
         | ELet of string * expr * expr
         | EIdent of string
         | ECall of string * expr

         | ECons of expr * expr
         | EIsEmpty of expr
         | EHead of expr
         | ETail of expr

         | EPair of expr * expr
         | EFirst of expr
         | ESecond of expr

         | ESlet of (string * expr) list * expr

         | ECallE of expr * expr

and function = FDef of string * expr   



(* Functions to create errors *)

fun evalError msg = raise Fail ("Eval Error @ "^msg)

fun unimplemented msg = raise Fail ("CODE NOT IMPLEMENTED - "^msg)




(* Primitive operations *)

fun applyAdd (VInt i1) (VInt i2) = VInt (i1+i2)
  | applyAdd _ _ = evalError "applyAdd"

fun applyMul (VInt i1) (VInt i2) = VInt (i1*i2)
  | applyMul _ _ = evalError "applyMul"

fun applyNeg (VInt i) = VInt (~ i)
  | applyNeg _ = evalError "applyNeg"

fun applyEq (VInt i1) (VInt i2) = VBool (i1 = i2)
  | applyEq (VBool b1) (VBool b2) = VBool (b1 = b2)
  | applyEq _ _ = evalError "applyEq"

fun applySub v1 v2 = applyAdd v1 (applyNeg v2)




(* COMPLETE THE FOLLOWING FOR QUESTION 1 *)

fun applyPair v1 v2 = VPair (v1, v2)

fun applyFirst (VPair (v1, v2)) = v1
  | applyFirst _ = evalError "You can only get first elements of VPairs"

fun applySecond (VPair (v1, v2)) = v2
  | applySecond _ = evalError "You can only get second elements of VPairs"



(* COMPLETE THE FOLLOWING FOR QUESTION 3 *)

fun applyCons v (VList l) = VList (v::l)
  | applyCons v _ = evalError "You can only append something to a list."

fun applyIsEmpty (VList []) = VBool true
  | applyIsEmpty _ = VBool false

fun applyHead _ = unimplemented "applyHead"

fun applyTail _ = unimplemented "applyTail"






(* Substitution function -- COMPLETE the missing cases *)

fun isInBnds id bnds =
  List.foldr (fn ((id',e), total) => total orelse id'=id) false bnds

fun subst (EVal v) id e = EVal v
  | subst (EAdd (e1,e2)) id e = EAdd (subst e1 id e, subst e2 id e)
  | subst (ESub (e1,e2)) id e = ESub (subst e1 id e, subst e2 id e)
  | subst (EMul (e1,e2)) id e = EMul (subst e1 id e, subst e2 id e)
  | subst (ENeg e1) id e = ENeg (subst e1 id e)
  | subst (EEq (e1,e2)) id e = EEq (subst e1 id e, subst e2 id e)
  | subst (EIf (e1,e2,e3)) id e = EIf (subst e1 id e, 
                                       subst e2 id e,
                                       subst e3 id e)
  | subst (ELet (id',e1,e2)) id e = 
      if id = id'
      then ELet (id',subst e1 id e, e2)
      else ELet (id',subst e1 id e, subst e2 id e)
  | subst (EIdent id') id e = if id = id'
                                then e
                              else EIdent id'
  | subst (ECall (n,e1)) id e = ECall (n,subst e1 id e)
  | subst (ECons (e1,e2)) id e = unimplemented "subst/ECons"
  | subst (EIsEmpty e1) id e = unimplemented "subst/EIsEmpty"
  | subst (EHead e1) id e = unimplemented "subst/EHead"
  | subst (ETail e1) id e = unimplemented "subst/ETail"
  | subst (EPair (e1,e2)) id e = EPair (subst e1 id e, subst e2 id e)
  | subst (EFirst e1) id e = EFirst (subst e1 id e)
  | subst (ESecond e1) id e = ESecond (subst e1 id e)
  | subst (ESlet (bnds,e1)) id e =
    if (isInBnds id bnds) then ESlet ((substBnds bnds id e), e1)
    else ESlet ((substBnds bnds id e), (subst e1 id e))

  | subst (ECallE (e1,e2)) id e = unimplemented "subst/ECallE"

and substBnds [] id e = []
  | substBnds ((id', e')::tl) id e =
      (id', (subst e' id e))::(substBnds tl id e)



(* Lookup a function name in the function environment *)

fun lookup name [] = evalError ("lookup - "^name)
  | lookup name ((n,f)::fenv) = 
      if (n = name)
        then f
      else lookup name fenv 



(* Evaluation function -- COMPLETE the missing cases *)

fun eval _ (EVal v) = v
  | eval fenv (EAdd (e1,e2)) = applyAdd (eval fenv e1) (eval fenv e2)
  | eval fenv (ESub (e1,e2)) = applySub (eval fenv e1) (eval fenv e2)
  | eval fenv (EMul (e1,e2)) = applyMul (eval fenv e1) (eval fenv e2)
  | eval fenv (ENeg e) = applyNeg (eval fenv e)
  | eval fenv (EEq (e1,e2)) = applyEq (eval fenv e1) (eval fenv e2)
  | eval fenv (EIf (e1,e2,e3)) = evalIf fenv (eval fenv e1) e2 e3
  | eval fenv (ELet (n,e1,e2)) = evalLet fenv n (eval fenv e1) e2
  | eval fenv (EIdent id) = unimplemented "eval/EIdent"
  | eval fenv (ECall (name,e)) = 
                evalCall fenv (lookup name fenv) (eval fenv e)
  | eval fenv (ESlet (bnds,e)) = evalSLet fenv (evalBindings fenv bnds) e
  | eval fenv (ECons (e1,e2)) = unimplemented "eval/ECons"
  | eval fenv (EIsEmpty e) = unimplemented "eval/EIsEmpty"
  | eval fenv (EHead e) = unimplemented "eval/EHead"
  | eval fenv (ETail e) = unimplemented "eval/ETail"
  | eval fenv (EPair (e1,e2)) = applyPair (eval fenv e1) (eval fenv e2)
  | eval fenv (EFirst e) = applyFirst (eval fenv e)
  | eval fenv (ESecond e) = applySecond (eval fenv e)
  | eval fenv (ECallE (func, e)) = unimplemented "eval/ECallE"

and evalCall fenv (FDef (param,body)) arg = 
      eval fenv (subst body param (EVal arg))

and evalIf fenv (VBool true) ethen eelse = eval fenv ethen
  | evalIf fenv (VBool false) ethen eelse = eval fenv eelse
  | evalIf _ _ _ _ = evalError "evalIf"

and evalLet fenv id v body = eval fenv (subst body id (EVal v))

and evalBindings fenv [] = []
  | evalBindings fenv ((id, e)::tl) = (id, (EVal (eval fenv e)))::(evalBindings fenv tl)

and evalSLet fenv [] e = eval fenv e
  | evalSLet fenv ((id, v)::tl) e = evalSLet fenv tl (subst e id v)

(* Sample functions for testing *)

val succ = ("succ",
            FDef ("n",
	          EAdd (EIdent "n", EVal (VInt 1))))

val pred = ("pred",
	    FDef ("n",
		  ESub (EIdent "n", EVal (VInt 1))))

val exp = ("exp",
    	   FDef ("args",
		 ELet ("a", EFirst (EIdent "args"),
		       ELet ("n", ESecond (EIdent "args"),
			     EIf (EEq (EIdent "n", EVal (VInt 0)),
				  EVal (VInt 1),
				  EMul (EIdent "a",
					ECall ("exp",
					       EPair (EIdent "a",
						      ECall ("pred",
							     EIdent "n")))))))))

val addT = ("addT", 
	    FDef ("args",
		  ELet ("a", EFirst (EIdent "args"),
			ELet ("b", ESecond (EIdent "args"),
			      EAdd (EIdent "a", EIdent "b")))))

val swap = ("swap",
	    FDef ("args",
		  ELet ("x", EFirst (EIdent "args"),
			ELet ("y", ESecond (EIdent "args"),
			      ESlet ([("x",EIdent "y"),("y",EIdent "x")],
				     EPair (EIdent "x", 
					    EIdent "y"))))))

val append = ("append", 
	      FDef ("args",
		    ELet ("xs",EFirst (EIdent "args"),
			  ELet ("ys",ESecond (EIdent "args"),
				EIf (EIsEmpty (EIdent "xs"),
				     EIdent "ys",
				     ECons (EHead (EIdent "xs"),
					    ECall ("append",
						   EPair (ETail (EIdent "xs"),
							  EIdent "ys"))))))))

val length = ("length",
	      FDef ("xs",
		    EIf (EIsEmpty (EIdent "xs"),
			 EVal (VInt 0),
			 EAdd (EVal (VInt 1),
			       ECall ("length", ETail (EIdent "xs"))))))



val twice = ("twice",
             FDef ("args",
		   ELet ("f", EFirst (EIdent "args"),
			 ELet ("x", ESecond (EIdent "args"),
			       ECallE (EIdent "f", 
				       ECallE (EIdent "f",
					       EIdent "x"))))))

val mapf = ("mapf",
	    FDef ("args",
		  ELet ("f", EFirst (EIdent "args"),
			ELet ("xs", ESecond (EIdent "args"),
			      EIf (EIsEmpty (EIdent "xs"),
				   EVal (VList []),
				   ECons (ECallE (EIdent "f",
						  EHead (EIdent "xs")),
					  ECall ("mapf",
						 EPair (EIdent "f",
							ETail (EIdent "xs")))))))))

(*- eval [] (EFirst (EPair (EVal (VInt 1),
                          EVal (VInt 2))))
- eval [] (ESecond (EPair (EVal (VInt 1),
                           EVal (VInt 2))))
- eval [] (EFirst (ELet ("x", EVal (VInt 1),
                         EPair (EIdent "x", EVal (VInt 2)))))
- exp
- pred
- eval [exp, pred] (ECall ("exp", EPair (EVal (VInt 4),
EVal (VInt 9))));
val it = VInt 262144 : value*)

(*applyCons (VInt 1) (VList [VInt 2, VInt 3, VInt 4]);
applyCons (VBool true) (VList [VInt 1, VBool false, VInt 2]);
applyCons (VList []) (VList []);
applyCons (VList [VInt 1, VInt 2]) (VList [VInt 3, VInt 4]);
applyCons (VInt 1) (VInt 2);*)

(*
applyIsEmpty (VList []);
applyIsEmpty (VList [VInt 1]);
applyIsEmpty (VList [VInt 1, VInt 2]);
applyIsEmpty (VInt 1);
applyIsEmpty (VBool true);
*)