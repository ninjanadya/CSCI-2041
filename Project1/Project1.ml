(*
Nadya Postolaki
CSCI 2041
posto018
Project 1
*)

type proposition =
	False |
	True |
	Var of string |
	And of proposition * proposition |
	Or of proposition * proposition |
	Not of proposition |
	Imply of proposition * proposition |
	Equiv of proposition * proposition ;;

type conditional =
	IffyFalse |
	IffyTrue |
	IffyVar of string |
	If of conditional * conditional * conditional ;;


let rec ifify prop =
	match prop with
		Var a		-> IffyVar a |
		True		-> IffyTrue |
		False		-> IffyFalse |
		Not a 		-> If ((ifify a), IffyFalse, IffyTrue) |
		And (a, b) 	-> If ((ifify a), (ifify b), IffyFalse) |
		Or (a, b) 	-> If ((ifify a), IffyTrue, (ifify b)) |
		Imply (a, b)-> If ((ifify a), (ifify b), IffyTrue) |
		Equiv (a, b)-> If ((ifify a), (ifify b), (If ((ifify b), IffyFalse, IffyTrue)));;
			
let rec normalize cond =
	match cond with
		IffyTrue	-> IffyTrue |
		IffyFalse	-> IffyFalse |
		IffyVar a	-> IffyVar a |
		If (pi, a, b)	->
			match pi with
				If (pi2, a2, b2) -> normalize (If (pi2, (If (a, a2, b2)), (If (b, a2, b2))))|
					_ -> If ((normalize pi), (normalize a), (normalize b));; 
					 	
let substitute cond variable boolean =
	let rec substituting variable =
		match cond with
			variable	-> boolean |
			IffyTrue	-> IffyTrue |
			IffyFalse	-> IffyFalse |
			IffyVar a	-> IffyTrue 
(*			If (x, y, z)-> substituting x; substituting y; substituting z
*)	in substituting cond;;

					
let rec simplify cond =
	match cond with
		IffyTrue						-> true |
		IffyFalse						-> false |
		IffyVar a						-> true |
		If (IffyTrue, a, b)				-> simplify a |
		If (IffyFalse, a, b)			-> simplify b |
		If (pi, IffyTrue, IffyFalse)	-> simplify pi |
		If (pi, a, b)					-> 
				match a with
					b	->	simplify a |
					_	->	simplify (If (pi, (substitute a pi IffyTrue), (substitute b pi IffyFalse)));;

let tautology prop =
	if simplify (normalize (ifify prop))
		then true
	else
		false;;





(*

tautology (Or ((Var "a"),(Not (Var "a"))));;
*)




















