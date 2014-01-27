(*
 * PLDI (Spring 2014)
 *
 * Code for HOMEWORK 1
 *
 * Cypress
 * iamcypress@gmail.com
 * I used List.foldr in Question 2.
 *)



fun scaleVec a [] = []
  | scaleVec a (x::xs) = (a*x)::(scaleVec a xs)

fun addVec [] [] = []
  | addVec (x::xs) (y::ys) = (x+y)::(addVec xs ys)
  | addVec _ _ = []

fun inner [] [] = 0
  | inner (x::xs) (y::ys) = (x*y) + (inner xs ys)
  | inner _ _ = 0



(* Question 1 *)

fun gcd a b =
    if a = 0 then abs b
    else if b = 0 then abs a
    else if (abs a) > (abs b) then gcd b (a mod b)
    else if (abs b) > (abs a) then gcd a (b mod a)
    else abs a

fun lcm a b = 
    if (gcd a b) = 0 then 0
    else (abs (a * b)) div (gcd a b)

fun exp a n =
    if n <= 0 then 1
    else a * (exp a (n-1))

(* Note: because we treat n < 0 as n = 0, some
    strange behavior arises when a < 0 and n > 1. *)
fun tetra a n =
    if n <= 0 then 1
    else exp a (tetra a (n-1))



(* Question 2 *)

fun sum xs = List.foldr (fn (next, total) => total + next) 0 xs

fun prod xs = List.foldr (fn (next, total) => total * next) 1 xs

fun every_other xs =
    let
        fun helper (x::xs) res true = helper xs (res@[x]) false
            | helper [] res _ = res
            | helper (x::xs) res false = helper xs res true
    in
        helper xs [] true
    end

fun flatten xss = List.foldr (fn (next, total) =>  next @ total) [] xss

fun heads xss =
    List.foldr
    (fn (next, total) =>
        case next of
            [] => total
            | hd::tl => hd::total)
    []
    xss

fun tails xss = 
    List.foldr
    (fn (next, total) =>
        case next of
            [] => total
            | hd::tl => [tl] @ total)
    []
    xss

fun scaleMat a m = List.map (fn v => scaleVec a v) m

fun addMat [] [] = []
  | addMat (x::xs) (y::ys) = [(addVec x y)] @ (addMat xs ys)
  | addMat _ _ = []

(* QUESTIONS 3 & 4 *)

exception TypeError of string

exception DivisionByZero of string

datatype value = VInt of int
         | VVec of int list
         | VMat of int list list
         | VRat of int * int

datatype expr = EInt of int
        | EVec of int list
        | EMat of int list list
        | EAdd of expr * expr
        | ESub of expr * expr
        | EMul of expr * expr
        | ENeg of expr
        | EDiv of expr * expr

fun simplifyRat (num, den) = 
    let
        val divisor = gcd num den
        val num = (num div divisor)
        val den = (den div divisor)
    in
        if den = 1 then
            VInt num
        else if den < 0 then
            VRat (~num, ~den)
        else
            VRat (num, den)
    end

fun addRat (rnum, rden) (snum, sden) = 
    simplifyRat (rnum * sden + snum * rden, rden * sden)
    
fun mulRat r s = raise Fail "mulRat not implemented"

fun negRat r = raise Fail "negRat not implemented"

fun applyAdd (VInt i) (VInt j) = VInt (i+j)
  | applyAdd (VVec v) (VVec w) = VVec (addVec v w)
  | applyAdd (VMat m1) (VMat m2) = VMat (addMat m1 m2)
  | applyAdd _ _ = raise TypeError "applyAdd"

fun applyMul (VInt i) (VInt j) = VInt (i*j)
  | applyMul (VInt i) (VVec v) = VVec (scaleVec i v)
  | applyMul (VVec v) (VVec w) = VInt (inner v w)
  | applyMul (VMat m) (VInt i) = VMat (scaleMat i m)
  | applyMul (VInt i) (VMat m) = VMat (scaleMat i m)
  | applyMul _ _ = raise TypeError "applyMul"

fun applyNeg (VInt i) = VInt (~ i)
  | applyNeg (VVec v) = VVec (scaleVec ~1 v)
  | applyNeg (VMat m) = VMat (scaleMat ~1 m)
  | applyNeg _ = raise TypeError "applyNeg"

fun applySub a b = applyAdd a (applyNeg b)


fun eval (EInt i) = VInt i
  | eval (EAdd (e,f)) = applyAdd (eval e) (eval f)
  | eval (ESub (e,f)) = applySub (eval e) (eval f)
  | eval (EMul (e,f)) = applyMul (eval e) (eval f)
  | eval (ENeg e) = applyNeg (eval e)
  | eval (EVec v) = VVec v
  | eval (EMat m) = VMat m
  | eval (EDiv (e,f)) = raise Fail "eval/EDiv not implemented"
