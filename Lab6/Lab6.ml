(*  
CSci 2041 Lab Assignment 6    
James Moen    
14 Oct 19  

It's worth 30 points.
*)


type proposition =
  Var of string |
  And of proposition * proposition |
  Or of proposition * proposition |
  Not of proposition ;;


let unorify proposition=
	let rec unorifying proposition=
		match proposition with
			Var a -> Var a|
			Not (Not a) -> unorifying a|
			Not (Or (a, b)) -> And ((unorifying a), (unorifying b))|
			Not a -> Not (unorifying a)|
			And (a, b) -> And ((unorifying a), (unorifying b))|
			Or (a, b) -> unorifying(Not (And ((Not a), (Not b))))			
	in unorifying proposition;;
			








(* Unorify the proposition a. *)
unorify (Var "a");;
(* 2 points if you get: Var "a" *)

(* Unorify the proposition ¬ a. *)
unorify (Not (Var "a"));;
(* 3 points if you get: Not (Var "a") *)

(* Unorify the proposition ¬ ¬ a. *)
unorify (Not (Not (Var "a")));;
(* 5 points if you get: Var "a" *)

(* Unorify the proposition ¬ (a ∨ b). *)
unorify (Not (Or (Var "a", Var "b")));;
(* 5 points if you get: And (Var "a", Var "b") *)

(* Unorify the proposition ¬ (a ∧ b). *)
unorify (Not (And (Var "a", Var "b")));;
(* 5 points if you get: Not (And (Var "a", Var "b")) *)

(* Unorify the proposition ¬ a ∨ ¬ b. *)
unorify (Or ((Not (Var "a")), (Not (Var "b"))));;
(* 5 points if you get: Not (And (Var "a", Var "b")) *)

(* Unorify the proposition ¬ ¬ a ∨ ¬ b. *)
unorify (Or ((Not (Not (Var "a"))), (Not (Var "b"))));;
(* 5 points if you get: Not (And (Not (Var "a"), Var "b")) *)
