(**********************************************************************
			  CS51 Problem Set 6
			     Spring 2016
		       Refs, Streams, and Music

	    Part 2: Series acceleration and infinite trees
 **********************************************************************)

(*---------------------------------------------------------------------
Part 2.1 : Faster approximations of pi

In nativeLazyStreams.ml, we provide the definitions of lazy streams
using OCaml's native Lazy module as presented in lecture, up to and
including code for approxiating pi through partial sums of the terms in
a Taylor series. In the next problem, you'll use streams to find
faster approximations for pi. *)

open NativeLazyStreams ;;

(* Recall from lecture the use of streams to generate approximations
of pi of whatever accuracy. Try it. You should be able to reproduce
the following:

   # within 0.01 pi_sums ;;
   - : int * float = (199, 3.13659268483881615)
   # within 0.001 pi_sums ;;
   - : int * float = (1999, 3.14109265362104129)
   # within 0.0001 pi_sums ;;
   - : int * float = (19999, 3.14154265358982476)

Notice that it takes about 2000 terms in the Taylor series to get
within .001 of the value of pi.  This method converges quite
slowly. But we can increase the speed dramatically by averaging
adjacent elements in the approximation stream. *)

(* Write a function average that takes a float stream and returns a
stream of floats each of which is the average of adjacent values in
the input stream. For example:

   # first 5 (average (to_float nats)) ;;
   - : float list = [0.5; 1.5; 2.5; 3.5; 4.5]
 *)
let average (s : float stream) : float stream =
  failwith "average not implemented" ;;

(* Now instead of using the stream of approximations in pi_sums, you
can instead use the stream of averaged pi_sums, which converges much
more quickly. Test that it requires far fewer steps to get within,
say, 0.001 of pi. *)

(* An even better accelerator of convergence for series of this sort
is Aitken's method. The formula is given in the problem set
writeup. Write a function to apply this accelerator to a stream, and
use it to generate approximations of pi. *)

let aitken (s: float stream) : float stream =
  failwith "aitken not implemented" ;;

(* Fill out the following table, recording how many steps are need to
get within different epsilons of pi.

epsilon    pi_sums     averaged method     aitken method 
0.1
0.01
0.001
0.0001
*)

  
(*---------------------------------------------------------------------
Part 2.2 : Infinite trees

Just as streams are a lazy form of list, we can have a lazy form of
trees. In the definition below, each node in a lazy tree of type 'a
LazyTrees.tree holds a value of some type 'a, and a (conventional,
finite) list of one or more (lazy) child trees. Complete the
implementation by writing print_depth, tmap, tmap2, and bfenumerate.
We recommend implementing them in that order. *)

type 'a treeval = Node of 'a * 'a tree list
 and 'a tree = 'a treeval Lazy.t ;;
  
(* Infinite trees shouldn't have zero children. This exception is
   available to raise in case that eventuality comes up. *)
exception Finite_tree ;;

(* Returns the element of type 'a stored at the root node of tree t of
   type 'a tree. *)
let node (t : 'a tree) : 'a =
  failwith "node not implemented" ;;

(* Returns the list of children of the root node of a tree of type 'a
   tree. *)
let children (t : 'a tree) : 'a tree list =
  failwith "children not implemented" ;;
  
(* Prints a representation of the first n levels of the tree t
   indented indent spaces. You can see some examples of the intended
   output of print_depth below. *)
let rec print_depth (n: int) (indent: int) (t: 'a tree) : unit =
  failwith "print_depth not implemented" ;;
  
(* Returns a tree obtained by mapping the function f over each node in
   t. *)
let rec tmap (f: 'a -> 'b) (t: 'a tree) : 'b tree =
  failwith "tmap not implemented" ;;
  
(* Returns the tree obtained by applying the function f to
   corresponding nodes in t1 and t2, which must have the same
   "shape". If they don't an Invalid_argument exception is raised. *)
let rec tmap2 (f: 'a -> 'b -> 'c) (t1: 'a tree) (t2: 'b tree) : 'c tree =
  failwith "tmap2 not implemented" ;;
  
(* Returns a LazyStreams.stream of the nodes in the list of trees
   tslist enumerated in breadth first order, that is, the root nodes
   of each of the trees, then the level onenodes, and so forth. There
   is an example of bfenumerate being applied below. *)
let rec bfenumerate (tslist : 'a tree list) : 'a NativeLazyStreams.stream =
  failwith "bfenumerate not implemented";;

(* Now use your implementation to generate some interesting infinite trees. *)
    
(* Define an infinite binary tree all of whose nodes hold the integer 1. *)
let rec onest :int tree = lazy (failwith "onest not implemented") ;;

(* Define a function levels that returns an infinite binary tree
   where the value of each node in the tree is its level or depth in
   the tree, starting with the argument n. For example:

    # print_depth 2 0 (levels 0) ;;
    0...
     1...
      2...
      2...
     1...
      2...
      2...
    - : unit = ()
 *)
let rec levels (n: int) : int tree = failwith "levels not implemented" ;;

(* Define an infinite binary tree nats where the value of each node in
   the tree is consecutively numbered in breadth-first order starting
   with 0. For example:

    # print_depth 2 0 nats ;;
    0...
     1...
      3...
      4...
     2...
      5...
      6...
    - : unit = ()
    # first 10 (bfenumerate [nats]) ;;
    - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
 *)
let rec nats : int tree = lazy (failwith "nats not implemented") ;;

(*---------------------------------------------------------------------
Time estimate 
*)

(* Please give us an honest estimate of how long this part took you to
   complete.  We care about your responses and will use them to help
   guide us in creating future assignments. *)
let minutes_spent : int = -1
