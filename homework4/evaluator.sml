(*
 *   CODE FOR HOMEWORK 4
 *)


structure Evaluator = struct

  structure I = InternalRepresentation



  exception Evaluation of string

  fun evalError msg = raise Evaluation msg


  (*
   *   Primitive operations
   *)

  fun primPlus (I.VInt a) (I.VInt b) = I.VInt (a+b)
    | primPlus _ _ = evalError "primPlus"

  fun primMinus (I.VInt a) (I.VInt b) = I.VInt (a-b)
    | primMinus _ _ = evalError "primMinus"

  fun unwrapBool (I.VBool a) = a
    | unwrapBool _ = evalError "unwrapBool"

  fun primEq (I.VInt a) (I.VInt b) = I.VBool (a=b)
    | primEq (I.VBool a) (I.VBool b) = I.VBool (a=b)
    | primEq (I.VList (x::xs)) (I.VList (y::ys)) = I.VBool ((unwrapBool (primEq x y)) andalso (unwrapBool (primEq (I.VList xs) (I.VList ys))))
    | primEq (I.VList (x::xs)) (I.VList []) = I.VBool false
    | primEq (I.VList []) (I.VList (x::xs)) = I.VBool false
    | primEq (I.VList []) (I.VList []) = I.VBool true
    | primEq _ _ = I.VBool false

  fun primLess (I.VInt a) (I.VInt b) = I.VBool (a<b)
    | primLess _ _ = I.VBool false

  val primNil = I.VList []

  fun primCons a (I.VList b) = I.VList (a::b)
    | primCons _ _ = evalError "primCons"

  fun primHd (I.VList (hd::tl)) = hd
    | primHd _ = evalError "primHd"

  fun primTl (I.VList (hd::tl)) = I.VList tl
    | primTl _ = evalError "primTl"

  fun primInterval (I.VInt i) (I.VInt j) = if j<i then I.VList [] else I.VList (List.tabulate (j-i+1, fn x => I.VInt (x+i)))
    | primInterval _ _ = evalError "primInterval"



  fun lookup (name:string) [] = evalError ("failed lookup for "^name)
    | lookup name ((n,v)::env) =
        if (n = name) then
	  v
	else lookup name env


  (*
   *   Evaluation functions
   *
   *)

  fun extract_tuples (I.VRecord tuples) = tuples
    | extract_tuples _ = evalError "EField" 


  fun eval _ (I.EVal v) = v
    | eval env (I.EFun (n,e)) = I.VClosure (n,e,env)
    | eval env (I.EIf (e,f,g)) = evalIf env (eval env e) f g
    | eval env (I.ELet (name,e,f)) = evalLet env name (eval env e) f
    | eval env (I.ELetFun (name,param,e,f)) = evalLetFun env name param e f
    | eval env (I.EIdent n) = lookup n env
    | eval env (I.EApp (e1,e2)) = evalApp env (eval env e1) (eval env e2)
    | eval env (I.EPrimCall1 (f,e1)) = f (eval env e1)
    | eval env (I.EPrimCall2 (f,e1,e2)) = f (eval env e1) (eval env e2)
    | eval env (I.ERecord fs) = I.VRecord (map (fn (n,e) => (n, (eval env e))) fs)
    | eval env (I.EField (e,s)) = lookup s (extract_tuples (eval env e))
    | eval env (I.EList es) = I.VList (map (eval env) es)

  and evalApp _ (I.VClosure (n,body,env)) v = eval ((n,v)::env) body
    | evalApp _ (I.VRecClosure (f,n,body,env)) v = let
	  val new_env = [(f,I.VRecClosure (f,n,body,env)),(n,v)]@env
      in
	  eval new_env body
      end
    | evalApp _ _ _ = evalError "cannot apply non-functional value"

  and evalIf env (I.VBool true) f g = eval env f
    | evalIf env (I.VBool false) f g = eval env g
    | evalIf _ _ _ _ = evalError "evalIf"

  and evalLet env id v body = eval ((id,v)::env) body

  and evalLetFun env id param expr body = let
      val f = I.VRecClosure (id, param, expr, env)
  in
      eval ((id,f)::env) body
  end


  (*
   *   Initial environment (already in a form suitable for the environment)
   *)
  
  fun primMap (I.VClosure (n,e,env)) (I.VList xs) =
    I.VList (List.map (
      fn x => eval ((n,x)::env) e
      ) xs)
    | primMap _ _ = evalError "primMap"

  fun primFilter (I.VClosure (n,e,env)) (I.VList xs) =
    I.VList (List.filter (
      fn x =>
        (case eval ((n,x)::env) e
          of I.VBool b => b
          | _ => evalError "primFilter function didn't return bool")
      ) xs)
    | primFilter _ _ = evalError "primFilter"

  val initialEnv =
      [("add", I.VClosure ("a",
			   I.EFun ("b",
				   I.EPrimCall2 (primPlus,
						 I.EIdent "a",
						 I.EIdent "b")),
			   [])),
       ("sub", I.VClosure ("a",
			   I.EFun ("b",
				   I.EPrimCall2 (primMinus,
						 I.EIdent "a",
						 I.EIdent "b")),
			   [])),
       ("equal", I.VClosure ("a",
			  I.EFun ("b",
				  I.EPrimCall2 (primEq,
						I.EIdent "a",
						I.EIdent "b")),
			  [])),
       ("less", I.VClosure ("a",
			    I.EFun ("b",
				    I.EPrimCall2 (primLess,
						  I.EIdent "a",
						  I.EIdent "b")),
			    [])),
       ("nil", primNil),
       ("cons", I.VClosure ("a",
          I.EFun ("b",
            I.EPrimCall2 (primCons,
              I.EIdent "a",
              I.EIdent "b")),
          [])),
       ("hd",
          I.VClosure ("l"
              , I.EPrimCall1 (primHd, I.EIdent "l")
              , [])),
       ("tl", I.VClosure ("l"
              , I.EPrimCall1 (primTl, I.EIdent "l")
              , [])),
       ("interval", I.VClosure ("a",
         I.EFun ("b",
           I.EPrimCall2 (primInterval,
             I.EIdent "a",
             I.EIdent "b")),
         [])),
       ("map", I.VClosure ("f",
        I.EFun ("xs",
          I.EPrimCall2(primMap,
            I.EIdent "f",
            I.EIdent "xs")),
        [])),
       ("filter", I.VClosure ("f",
        I.EFun ("xs",
          I.EPrimCall2(primFilter,
            I.EIdent "f",
            I.EIdent "xs")),
        []))]


end
