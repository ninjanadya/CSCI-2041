(*
Nadya Postolaki
CSCI 2041
Lab 7
*)

open List;;

(*  
CSci 2041 Lab Assignment 7    

James Moen    
22 Oct 19  

30 points.
*)

let some pred things =
	let rec somming things =
		match things with
			[] 		-> false |
			hd::tl 	-> 
				if pred hd
					then true
				else
					somming tl
	in somming things;;


some (fun number -> number mod 2 = 0) [] ;;          (* 1 pt. false *)
some (fun number -> number mod 2 = 0) [1] ;;         (* 1 pt. false *)
some (fun number -> number mod 2 = 0) [2] ;;         (* 1 pt. true  *)
some (fun number -> number mod 2 = 0) [1 ; 2 ; 3] ;; (* 1 pt. true  *)
some (fun number -> number mod 2 = 0) [1 ; 3 ; 5] ;; (* 1 pt. false *)


let every pred things =
	let rec everything things =
		match things with
			[] 		-> true |
			hd::tl 	->
				if pred hd
					then everything tl
				else
					false
	in everything things;;
	


every (fun number -> number mod 2 = 0) [] ;;          (* 1 pt. true  *)
every (fun number -> number mod 2 = 0) [1] ;;         (* 1 pt. false *)
every (fun number -> number mod 2 = 0) [2] ;;         (* 1 pt. true  *)
every (fun number -> number mod 2 = 0) [1 ; 2 ; 3] ;; (* 1 pt. false *)
every (fun number -> number mod 2 = 0) [2 ; 4 ; 6] ;; (* 1 pt. true  *)


let mapcon func things =
	let rec mapconning things =
		if things = []
			then func things
		else
			func things@mapconning (tl things)
	in mapconning things;;


mapcon (fun thing -> thing) [] ;;
(* 1 pt. [] *)

mapcon (fun thing -> thing) [1 ; 2 ; 3] ;;
(* 2 pt. [1 ; 2 ; 3 ; 2 ; 3 ; 3] *)

mapcon (fun thing -> [List.length thing]) [1 ; 2 ; 3] ;;
(* 2 pt. [3 ; 2 ; 1 ; 0] *)


let uniquify things =
	let rec uniquifying things =
		match things with
			[]		-> things |
			hd::tl	->
				if every (fun number -> number != hd) tl
					then [hd] 
						
				else
					[]
in mapcon uniquifying things;;

uniquify [] ;;               (* 1 pt. [] *)
uniquify [1] ;;              (* 1 pt. [1] *)
uniquify [1 ; 2] ;;          (* 1 pt. [1 ; 2] *)
uniquify [1 ; 1] ;;          (* 1 pt. [1] *)
uniquify [1 ; 2; 1; 3; 1] ;; (* 1 pt. [2 ; 3 ; 1] *)


exception EnumerationOutOfRange;;
let enumeration start stop step =	
	let count = ref start in
		let enumerating () = 
			if !count <= stop 
				then	let temp = !count in 
			 			count := !count + step; temp
			else		raise EnumerationOutOfRange
		in enumerating;;
		
	

let oneThruThree = enumeration 1 3 1 ;;
let byTwos       = enumeration 0 4 2 ;;
oneThruThree () ;;                                               (* 1 pt.  1 *)
byTwos () ;;                                                     (* 1 pt.  0 *)
oneThruThree () ;;                                               (* 1 pt.  2 *)
oneThruThree () ;;                                               (* 1 pt.  3 *)

try oneThruThree () with EnumerationOutOfRange -> -1 | _ -> 0 ;; (* 2 pt. -1 *)
byTwos () ;;                                                     (* 1 pt.  2 *)

try oneThruThree () with EnumerationOutOfRange -> -1 | _ -> 0 ;; (* 1 pt. -1 *)
byTwos () ;;                                                     (* 1 pt.  4 *)

try byTwos () with EnumerationOutOfRange -> -1 | _ -> 0 ;;       (* 1 pt. -1 *)

