(*
NADYA POSTOLAKI
DMITRI IVANOV

  
CSci 2041 Lab Assignment 8    
James Moen    
28 Oct 19  

30 points.
*)

(* 
You can use the functions MAKE STREAM, FIRST, and REST as helpers in your   
functions. The stream NATURALS and the functions EVEN, ODD, TRUTH, and TAKE   
are used only in the tests. 
*)

(* 
MAKE STREAM. Return a new stream. THIS is the first element of the stream.   
STATE is any appropriate object. NEXT is a function taking THIS and STATE   
as its arguments: it returns a 2-tuple with a new THIS and a new STATE. 
*)

let makeStream this state next =
  ((this, state), next) ;;

(* FIRST. Return the first element of a stream. *)

let first ((this, _), _) =
  this ;;

(* REST. Return a stream with its first element removed. *)

let rest ((this, state), next) =
  (next this state, next) ;;

(* NATURALS. A stream of 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ...  
We don't use STATE in this stream, so we'll let it be (). *)

let naturals =
  makeStream 0 () (fun this state -> (this + 1, ())) ;;

(* EVEN, ODD. Test if integers are respectively even and odd. *)

let even number = (number mod 2 = 0) ;;
let odd  number = (number mod 2 = 1) ;;

(* TRUTH. Return TRUE. *)

let truth number = true ;;

(* TAKE. Return a list of the first COUNT elements from STREAM. *)

let rec take count stream =
  match count  with 0 -> [] |
       _ -> first stream :: take (count - 1) (rest stream) ;;

let advance predicate stream =
	if predicate (first stream)
		then stream
	else
		rest stream;;

take 5 (advance truth naturals) ;;    (* 1 pt. [0; 1; 2; 3; 4] *)
take 5 (advance odd   naturals) ;;    (* 2 pt. [1; 2; 3; 4; 5] *)
take 5 (advance even  naturals) ;;    (* 2 pt. [0; 1; 2; 3; 4] *)

let filter predicate stream =
	makeStream (first (advance predicate stream)) (rest (advance predicate stream)) (fun this state -> ((first (advance predicate (state))), rest(advance predicate (state))))
;;

take 5 (filter truth naturals) ;;    (* 2 pt. [0; 1; 2; 3; 4] *)
take 5 (filter odd   naturals) ;;    (* 4 pt. [1; 3; 5; 7; 9] *)
take 5 (filter even  naturals) ;;    (* 4 pt. [0; 2; 4; 6; 8] *)

let rec from count stream =
	if count = 0
		then stream
	else
		from (count - 1) (rest stream);;

take 5 (from 0 naturals) ;;    (* 1 pt. [0; 1; 2; 3; 4] *)
take 5 (from 1 naturals) ;;    (* 2 pt. [1; 2; 3; 4; 5] *)
take 5 (from 5 naturals) ;;    (* 2 pt. [5; 6; 7; 8; 9] *)

let skip count stream =
	makeStream (first (stream)) stream (fun this state -> ((first (from count state)), (from count state)));;

take 10 (skip 1 naturals) ;;(* 2 pt. [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] *)
take 10 (skip 4 naturals) ;;(* 4 pt. [0; 4; 8; 12; 16; 20; 24; 28; 32; 36] *)
take 10 (skip 100 naturals) ;;(* 4 pt. [0; 100; 200; 300; 400; 500; 600; 700; 800; 900]*) 




