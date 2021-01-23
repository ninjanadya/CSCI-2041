(*
Nadya Postolaki
Dmitri Ivanov
*) 

(*
  CSci 2041 Lab Assignment 9

    James Moen
    03 Nov 19

  30 points.
*)

(* Define the function PRINTF, among other things. *)

open Printf ;;

(* THING. The type of a Pure Lisp object, from the lectures. *)

type
  thing =
    Closure of thing * thing * environment |
    Cons of thing * thing |
    Nil |
    Number of int |
    Symbol of string
and
  environment = (string * thing) list ;;

	
let rec printingThing thing =
	let rec printingThings things =
		match things with
			Nil				-> () |
			Number n			-> printf "%i" n|
			Symbol o			-> printf "%s" o|
			Cons (a, b)			-> printf " " ; printingThing a ; printingThings b|
			Closure (_, _, _) 	-> ()
		in
		match thing with
			Nil		-> printf "nil"|
			Number n	-> printf "%i" n|
			Symbol o	-> printf "%s" o|
			Cons (a, b)	-> printf "("; printingThing a; printingThings b; printf ")" |
			_			-> ();;



let printThing thing =
	printingThing thing; printf "\n";;

(* Tests begin here, simple ones first. The comments say what must be printed
   to get points. Case matters, all the blanks must be in their right places,
   and there must be no extra blanks. We don't care what PRINT THING returns,
   only what it prints. *)

printThing Nil ;;                                             (* 2 pts. nil *)

printThing (Number 7734) ;;                                  (* 2 pts. 7734 *)

printThing (Symbol "lobyms") ;;                            (* 2 pts. lobyms *)

(* More complex tests, involving lists. *)

printThing                                                    (* 2 pts. (a) *)
  (Cons (Symbol "a", Nil)) ;;

printThing                                                  (* 2 pts. (a b) *)
  (Cons (Symbol "a",
    Cons (Symbol "b", Nil))) ;;

printThing                                                (* 2 pts. (a b c) *)
  (Cons (Symbol "a",
     Cons (Symbol "b",
       Cons (Symbol "c", Nil)))) ;;

printThing                                              (* 2 pts. ((a) b c) *)
  (Cons (
     Cons (Symbol "a", Nil),
     Cons (Symbol "b",
       Cons (Symbol "c", Nil)))) ;;

printThing                                              (* 2 pts. ((a b) c) *)
  (Cons (
     Cons (Symbol "a",
       Cons (Symbol "b", Nil)),
     Cons (Symbol "c", Nil))) ;;

printThing                                              (* 2 pts. (a (b c)) *)
  (Cons (Symbol "a",
    Cons (
      Cons (Symbol "b",
        Cons (Symbol "c", Nil)),
      Nil))) ;;

printThing                                              (* 2 pts. ((a b c)) *)
  (Cons (
    Cons (Symbol "a",
      Cons (Symbol "b",
        Cons (Symbol "c", Nil))),
    Nil)) ;;

(* The big finish. All that horror below is the internal OCaml form of Pure
   Lisp code that defines the factorial function. It looks like this when it's
   properly indented.

   (define !
     (lambda (n)
       (if
         (= n 0)
         1
         (∗ n (! (- n 1))))))

   Your function PRINT THING need not print in indented form, so it will print
   this instead, for 10 pts.

   (define ! (lambda (n) (if (= n 0) 1 (∗ n (! (- n 1))))))
*)

printThing
  (Cons (Symbol "define",
    Cons (Symbol "!",
     Cons
      (Cons (Symbol "lambda",
        Cons (Cons (Symbol "n", Nil),
         Cons
          (Cons (Symbol "if",
            Cons (Cons (Symbol "=", Cons (Symbol "n", Cons (Number 0, Nil))),
             Cons (Number 1,
              Cons
               (Cons (Symbol "*",
                 Cons (Symbol "n",
                  Cons
                   (Cons (Symbol "!",
                     Cons
                      (Cons (Symbol "-",
                        Cons (Symbol "n", Cons (Number 1, Nil))),
                      Nil)),
                   Nil))),
               Nil)))),
          Nil))),
      Nil)))) ;;


