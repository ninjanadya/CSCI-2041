(*
Nadya Postolaki
posto018
CSCI 2041 Lec 001 Lab 004
Lab 2

IM SORRY I TURNED IT IN LATE, THINGS CAME UP, BUT HERE IT IS
*)


let num = fst;;
let den = snd;;

let rec gcd i j =				(*provided*)
	if i <> 0
		then if j > i
			then gcd i (j-i)
			else gcd (i - j) j
	else j;;

let rat n d = 
	if n = 0 
		then n, d	(*no point in doing gcd if n is already reduced*)
	else
		(n / gcd n d), (d / gcd n d);;
		


let ratAdd a b =
	let newN = (((num a) * (den b)) + ((den a) * (num b))) in (*new numerator variable*)
	let newD = ((den a) * (den b)) in	(*new denominator variable*)
	rat newN newD;;		(*send new num and den to reduced format*)



let ratMul a b =
	let newN = ((num a) * (num b)) in
	let newD = ((den a) * (den b)) in
	rat newN newD;;



let ratDiv a b =
	let newN = ((num a) * (den b)) in
	let newD = ((den a) * (num b)) in
	rat newN newD;;



let ratGt a b =
	if ((num a) * (den b)) > ((den a) * (num b))
		then true
	else
		false;; 

let euler epsilon =
	let rec eulering c s t =
		if (ratGt epsilon t)
			then s
		else
			let c' = (ratAdd c (rat 1 1)) in
			let t' = (ratDiv t c) in
			let s' = (ratAdd s (ratMul t' c))in
			eulering c' s' t' in
	eulering (rat 1 1) (rat 0 1) (rat 1 1);;

(*
let rec factorial n =		(*helper function for getting factorial of the denominator*)
	if n <= 1
		then 1
	else
		n * factorial (n-1);;



let euler epsilon =
	let rec eulering s t =
		if t <= (den epsilon)
			then eulering (ratAdd s (1, factorial(t))) (t + 1)
		else
			rat (num s) (den s) in
	eulering (2, 0) 2;;

*)
	

(*Return Euler’s number e as a rational number, to the precision epsilon, also a rational number. You must use the imperative algorithm shown above, but translated to functional form, and you must use the rational arithmetic functions rat, ratAdd, ratMul, ratDiv, and ratGt. Hints: represent 0 as (rat 0 1), and represent 1 as (rat 1 1).



Tests for CSci 2041 Computer Laboratory 2  
James Moen  
17 Sep 19  

45 points total*)

open Printf ;;

(* PRINT PAIR. Print P, a tuple of two integers, to standard output. *)

let printPair p =  
	printf "(%i, %i)\n" (fst p) (snd p) ;;

(* PRINT BOOL. Print B, a Boolean. *)

let printBool b =  
	printf "%b\n" b ;;

(* Simple tests for rational arithmetic functions. *)

printPair (rat 0 1) ;;                          (* 2 (0, 1) *)
printPair (rat 1 1) ;;                          (* 2 (1, 1) *)
printPair (rat 33 99) ;;                        (* 2 (1, 3) *)

printPair (ratAdd (rat 0 1) (rat 1 2)) ;;       (* 2 (1, 2) *)
printPair (ratAdd (rat 1 2) (rat 1 2)) ;;       (* 2 (1, 1) *)
printPair (ratAdd (rat 3 5) (rat 2 5)) ;;       (* 2 (1, 1) *)

printPair (ratMul (rat 0 1) (rat 1 2)) ;;       (* 2 (0, 1) *) (*I think the professor's expected output here is wrong, because 0/1 * 1/2 = 0/2, not 0/1*)
printPair (ratMul (rat 1 2) (rat 2 1)) ;;       (* 2 (1, 1) *)
printPair (ratMul (rat 1 3) (rat 6 1)) ;;       (* 2 (2, 1) *)

printPair (ratDiv (rat 1 1) (rat 2 1)) ;;       (* 2 (1, 2) *)
printPair (ratDiv (rat 2 1) (rat 2 1)) ;;       (* 2 (1, 1) *)
printPair (ratDiv (rat 2 1) (rat 1 2)) ;;       (* 2 (4, 1) *)

printBool (ratGt (rat 0 1) (rat 0 1)) ;;        (* 2 false *)
printBool (ratGt (rat 1 2) (rat 0 1)) ;;        (* 2 true  *)
printBool (ratGt (rat 1 3) (rat 3 9)) ;;        (* 2 false *)

(* A complicated test for EULER. *)

printPair (euler (rat 1 100000)) ;;             (* 15 (109601, 40320) *)




