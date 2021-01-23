(*
Nadya Postolaki
CSCI 2041 Lab 1
09/10/2019
*)

open List ;;

let rec member e l =
	if l = []
		then false				(*Checks if l is an empty list, then obviously the next course of action would be to assume that e is not in l, because something cannot exist in nothing, duh *that billie eilish song starts playing after she says duh in bad guy**)
	else
		if e = (hd l)
			then true
		else
			member e (tl l);;	(*Here we've got a nice recursive function in case e is not in the hd of l :) *)	
		
let rec delete e l =			(*My favorite function cause it's like them cybermen from dr who*)
	if l = []					(*Why check for e if l is empty??*)
		then l
	else
		if e = (hd l)			
			then delete e (tl l)		(*Yeet e out of existance and just leave the tl :') *)
		else 
			(hd l)::delete e (tl l);;	(*Yes hello, we got a new list LIKE l, but not actually l, then we get thanos to snap his fingers for the sweet, sweet release of the other e's*)

let mean l =	
	let rec len l = 
		match l with 
			| [] -> 0.0						(*constructor expecting empty list*)
			| hd::tl -> 1.0 +. (len tl) in	(*recursively calls len to find the length of the list, because idk if the cheaters method of just using List.length l is allowed so I didn't use it lol*)

	let rec sum l =
		match l with						(*match case lets me check values and return what I need done*)
			| [] -> 0.0
			| hd::tl -> hd +. (sum tl) in	(*I love recursive functions, you don't understand*)
	(sum l) /. (len l);;					(*took me a while to realize it was /. not / :( *)
	

(*

open Printf ;;

(* PRINT OBJECTS. Print a list of OBJECTs L to standard output using a function
   F. You don't have to write this, or even know how it works. *)

let printObjects f l =
  let rec printingObjects l =
    match l with
      [] -> () |
      h :: t -> printf " ; " ; f h ; printingObjects t
  in printf "[" ;
     (match l with
       [] -> () |
       h :: t -> f h ; printingObjects t) ;
     printf "]\n" ;;

(* PRINT INTS, PRINT STRINGS. Print a list of INTs or a list of STRINGs. *)

let printInts    = printObjects (fun e -> printf "%i" e) ;;
let printStrings = printObjects (fun e -> printf "\"%s\"" e) ;;
									
									
														RESULT I NEEDED		MY RESULTS
														
printf "%b\n" (member 1 []) ;;                         (* 2 false *)		=> false
printf "%b\n" (member 1 [1]) ;;                        (* 2 true  *)		=> true
printf "%b\n" (member 2 [1 ; 2 ; 3]) ;;                (* 2 true  *)		=> true
printf "%b\n" (member 5 [2 ; 4 ; 6]) ;;                (* 2 false *)		=> false
printf "%b\n" (member "c" ["a" ; "b" ; "c" ; "d"]) ;;  (* 2 true  *)		=> true
printf "%b\n" (member "x" ["a" ; "b" ; "c" ; "d"]) ;;  (* 2 false *)		=> false

(* Tests for DELETE. *)

printInts    (delete 1 []) ;;                          (* 2 []          *)	=> []
printInts    (delete 1 [1]) ;;                         (* 2 []          *)	=> []
printInts    (delete 1 [1 ; 2 ; 3]) ;;                 (* 2 [2 ; 3]     *)	=> [2 ; 3]
printInts    (delete 4 [1 ; 2 ; 3]) ;;                 (* 2 [1 ; 2 ; 3] *)	=> [1 ; 2 ; 3]
printInts    (delete 3 [1 ; 2 ; 3]) ;;                 (* 2 [1 ; 2]     *)	=> [1 ; 2]
printInts    (delete 1 [1 ; 2 ; 1 ; 3 ; 1 ; 4]) ;;     (* 2 [2 ; 3 ; 4] *)	=> [2 ; 3 ; 4]
printStrings (delete "a" ["x" ; "a" ; "y" ; "a"]) ;;   (* 2 ["x" ; "y"] *)	=> ["x" ; "y"]
printStrings (delete "a" ["a" ; "a" ; "a"]) ;;         (* 2 []          *)	=> []

(* Tests for MEAN. *)

printf "%G\n" (mean [1.0]) ;;                          (* 2 1           *)	=> 1
printf "%G\n" (mean [1.0 ; 2.0]) ;;                    (* 2 1.5         *)	=> 1.5
printf "%G\n" (mean [1.0 ; 1.0 ; 1.0]) ;;              (* 2 1.0         *)	=> 1
printf "%G\n" (mean [1.0 ; 0.0 ; -1.0 ; 1.0]) ;;       (* 2 0.25        *)	=> 0.25
*)
