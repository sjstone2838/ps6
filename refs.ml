(**********************************************************************
			  CS51 Problem Set 6
			     Spring 2016
		       Refs, Streams, and Music

			     Part 1: Refs
 **********************************************************************)

(* The type of mutable lists. *)
type 'a mlist = Nil | Cons of 'a * 'a mlist ref

(* Write a function has_cycle that returns whether a mutable list has
   a cycle.  You may want a recursive helper function. Don't worry
   about space usage. *)

let rec cycle_helper mlst lst =
	match mlst with
	| Nil -> None
	| Cons (_, t) -> 
		match !t with
		| Nil -> None
		| Cons (_, { contents = Nil }) -> None
		| Cons (_, t2) -> 				
		if List.exists (fun x -> x == t2) lst then Some t
		else cycle_helper !t (t::lst)
;;

let has_cycle (lst : 'a mlist) : bool =
	match (cycle_helper lst []) with
	| None -> false
	| _ -> true
;;

(* Some mutable lists for testing. *)
let list0 = Cons(0,ref Nil);;
let list1a = Cons(1, ref Nil);;
let list1b = Cons(1, ref list1a);;
let list2b = Cons(2, ref list1b);;

let reflist = ref (Cons(2, ref Nil));;
let list2 = Cons(1, ref (Cons (2, reflist)));;
reflist := list2;;

let reflist2 = ref (Cons(2, ref Nil));;
let list3 = Cons(1, ref (Cons (2, ref(Cons (3, reflist2)))));;
reflist2 := list3;;

assert (has_cycle list0 = false);;
assert (has_cycle list1b = false);;
assert (has_cycle list2 = true);;
assert (has_cycle list3 = true);;

(* Write a function flatten that flattens a list (removes its cycles
   if it has any) destructively. Again, you may want a recursive
   helper function and you shouldn't worry about space. *)
let flatten (lst : 'a mlist) : unit =
  match cycle_helper lst [] with 
    | None -> ()
    | Some t -> t:= Nil
;;

flatten list2;;
assert (list2 = Cons (1, ref (Cons (2, ref Nil))));;

(* Write mlength, which nondestructively finds the number of nodes in
   a mutable list that may have cycles. *)

let rec length_helper mlst lst n =
	match mlst with
	| Nil -> n
	| Cons (_, t) -> 
		match !t with
		| Nil -> n + 1
		| Cons (_, { contents = Nil }) -> n + 2
		| Cons (_, t2) -> 				
		if List.exists (fun x -> x == t2) lst then n + 1
		else length_helper !t (t::lst) (n+1)
;;

let mlength (lst : 'a mlist) : int =
  length_helper lst [] 0;;

assert (mlength (Cons (1, ref Nil)) = 1);;
assert (mlength (Cons (2, ref (Cons (1, ref Nil)))) = 2);;
assert (mlength (Cons (3, ref (Cons (2, ref (Cons (1, ref Nil)))))) = 3);;

(* Please give us an honest estimate of how long this part took you to
   complete.  We care about your responses and will use them to help
   guide us in creating future assignments. *)
let minutes_spent : int = 120;;
