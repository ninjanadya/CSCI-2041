(*  Nadya Postolaki
THIS LAB WAS BY FAR THE MOST DIFFICULT ONE IVE ENCOUNTERED, IMMA TELL YOU THAT



CSci 2041 
Lab Assignment 5    

James Moen    
08 Oct 19  

It's worth 30 points.*)

open List;;

let choose etc things =
	let rec choosing things =
		match things with
			[] -> ()|
			fstOfThings::tlThings -> 
				etc fstOfThings;
				choosing tlThings
		in choosing things;;			

let rec allbut things thing =
	if things = []
		then things
	else
		if thing = (hd things)
			then (tl things)
		else
			(hd things)::allbut (tl things) thing;;

let permute etc things =
	let rec permuting unpermutedThings permutedThings =
		if unpermutedThings = []
			then etc permutedThings
		else
			choose (fun theHead -> permuting (allbut unpermutedThings theHead) (theHead::permutedThings)) unpermutedThings
		in permuting things [];;
	
	

(*You may need to use anonymous functions, called lambda expressions in some other languages. In OCaml, an anonymous function that takes a parameter named p and returns the value of an expression e can be written as (fun p -> e).

The function permute is a CPS function, so it does not return a useful value. If you write it in the obvious way, then it will return (). It must use something like permuting (described in the previous section) as a helper. It must also use choose and allbut as helpers.*)


(* PRINT THINGS. Print a list of THINGS using a FORMAT string. *)

let printThings format things =
  let rec printingThings things =
    match things
    with [] -> () |
         firstThing :: otherThings ->
           Printf.printf " ; " ;
           Printf.printf format firstThing ;
           printingThings otherThings
  in Printf.printf "[" ;
     (match things
      with [] -> () |
           firstThing :: otherThings ->
              Printf.printf format firstThing ;
              printingThings otherThings) ;
     Printf.printf "]\n" ;;

(* 
Tests. 

Each test is worth some number of points. 
If your functions produce the results that the test expects, then you get the points. 
Your score for   this assignment will be the total number of points in all the tests. 

*)

printThings "%i" (allbut [] 0) ;;            (* 1 pt [] *)

printThings "%i" (allbut [0; 1; 2] 0) ;;     (* 1 pt [1; 2] *)

printThings "%i" (allbut [0; 1; 2] 1) ;;     (* 1 pt [0; 2] *)

printThings "%i" (allbut [0; 1; 2] 2) ;;     (* 1 pt [0; 1] *)

printThings "%i" (allbut [0; 1; 2] 7734) ;;  (* 1 pt [0; 1; 2] *)



(* In the following tests, it doesn't matter what CHOOSE returns. 
All we care about is what the tests print. *)

choose (fun thing -> Printf.printf "%i" thing) [] ;;
(* 1 pt. if it prints nothing. *)

choose (fun thing -> Printf.printf "%i " thing) [1] ;;
(* 1 pt. if it prints: 1 *)

choose (fun thing -> Printf.printf "%i " thing) [0; 1; 2] ;;
(* 3 pts. if it prints: 0 1 2. *)



(* In the following tests, it also doesn't matter what PERMUTE returns. 
All we care about is what the tests print. *)

permute (fun things -> printThings "%i" things) [] ;;
(* 5 pts. if it prints nothing. *)

permute (fun things -> printThings "%i" things) [0] ;;
(* 5 pts. if it prints [0]. *)

permute (fun things -> printThings "%i" things) [0; 1; 2] ;;
(* 10 pts. if it prints this:

[2 ; 1 ; 0]
[1 ; 2 ; 0]
[2 ; 0 ; 1]
[0 ; 2 ; 1]
[1 ; 0 ; 2]
[0 ; 1 ; 2]  

Your lists might appear in a different order, 
but each of the six lists must be printed exactly once, 
and no list must be printed more than once. *)
