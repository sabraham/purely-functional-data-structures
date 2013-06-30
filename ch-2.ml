(* Chapter 2 *)

(* STACK signature *)
module type STACK =
sig
  type 'a stack
  exception Empty
  exception Subscript
  val empty : 'a stack
  val is_empty : 'a stack -> bool
  val cons : 'a -> 'a stack -> 'a stack
  val head : 'a stack -> 'a (* Raises Empty *)
  val tail : 'a stack -> 'a stack (* Raises Empty *)
  val concat : 'a stack -> 'a stack -> 'a stack
  val update : 'a stack -> int -> 'a -> 'a stack (* Raises Subscript *)
end

(* Stack struct *)
module ListStack : STACK =
struct
  type 'a stack = 'a list
  exception Empty
  exception Subscript
  let empty = []
  let is_empty = function [] -> true | _ -> false
  let cons x s =
    x :: s
  let head s =
    match s with
        [] -> raise Empty
      | x::_ -> x
  let tail s =
    match s with
        [] -> raise Empty
      | _::xs -> xs
  let rec concat xs ys =
    match xs with
        [] -> ys
      | xh::xt -> xh::(concat xt ys)
  let rec update s i v =
    match i, s with
        _, [] -> raise Empty
      | 0, x::xs -> v::xs
      | i, x::xs -> x::(update xs (i - 1) v)
end

module CustomStack : STACK =
struct
  type 'a stack =
      Nil
    | Cons of 'a * 'a stack
  exception Empty
  exception Subscript
  let empty = Nil
  let is_empty = function Nil -> true | _ -> false
  let cons x s =
    Cons(x, s)
  let head s =
    match s with
        Nil -> raise Empty
      | Cons(x, _) -> x
  let tail s =
    match s with
        Nil -> raise Empty
      | Cons(_, xs) -> xs
  let rec concat xs ys =
    match xs with
        Nil -> ys
      | Cons(xh, xt) -> Cons(xh, concat xt ys)
  let rec update s i v =
    match s with
        Nil -> raise Subscript
      | Cons(x, xs) when i = 0 -> Cons(v, xs)
      | Cons(x, xs) -> Cons(x, (update xs (i - 1) v))
end

(* Exercise 2.1 *)

let suffixes xs =
  let rec suffixes' xs acc =
    match xs with
        [] -> List.rev ([]::acc)
      | y::ys -> suffixes' ys (xs::acc)
  in suffixes' xs []

(* ORDERED Signature *)

type order = Less | Greater | Equal

module type ORDERED =
sig
  type t
  val compare : t -> t -> order
end

(* SET Signature *)

module type SET =
sig
  type elem
  type set
  val empty : set
  val insert : elem -> set -> set
  val member : elem -> set -> bool
end

(* Unbalanced Set functor *)

module UnbalancedSet (Element : ORDERED) : SET =
struct
  type elem = Element.t
  type tree = EmptyTree | Node of tree * elem * tree
  type set = tree
  let empty = EmptyTree
  let rec insert x s =
    match s with
        EmptyTree -> Node(EmptyTree, x, EmptyTree)
      | Node(left, k, right) ->
        match Element.compare x k with
            Equal   -> s
          | Less    -> Node(insert x left, k, right)
          | Greater -> Node(left, k, insert x right)
  let rec member x s =
    match s with
        EmptyTree -> false
      | Node(left, k, right) ->
        match Element.compare x k with
            Equal   -> true
          | Less    -> member x left
          | Greater -> member x right
end

(* Exercise 2.2
   Member check of set in 2 * h + 1 comparisons (h = height of tree)
*)

module type TWOWAY_ORDERED =
  sig
    type t
    val lt : t -> t -> bool
    val eq : t -> t -> bool
  end

module UnbalancedTwoWaySet (Element : TWOWAY_ORDERED) : SET =
struct
  type elem = Element.t
  type tree = EmptyTree | Node of tree * elem * tree
  type set = tree
  let empty = EmptyTree
  let rec insert x s =
    match s with
        EmptyTree -> Node(EmptyTree, x, EmptyTree)
      | Node(left, k, right) ->
        if Element.lt x k then Node(insert x left, k, right)
        else if Element.eq x k then s
        else Node(left, k, insert x right)
  let rec member x s =
    let rec member' x s cand =
      match x, s with
          x, EmptyTree -> if x = cand then true else false
        | x, Node(left, k, right) ->
          if Element.lt x k
          then member' x left cand
          else member' x right k
    in match x, s with
        _, EmptyTree -> false
      | x, Node(left, k, right) ->
        if Element.lt x k
        then member x left
        else member' x right k
end

(* Exercises 2.3 and 2.4
   Forgive the Java-like module name :(
*)
module OptimizedUnbalancedTwoWaySet (Element : TWOWAY_ORDERED) : SET =
struct
  type elem = Element.t
  type tree = EmptyTree | Node of tree * elem * tree
  type set = tree
  let empty = EmptyTree
  let rec insert x s =
    let rec insert' x s head =
      match s with
          EmptyTree -> Node(EmptyTree, x, EmptyTree)
        | Node(left, k, right) ->
          if Element.lt x k then Node(insert x left, k, right)
          else if Element.eq x k then head
          else Node(left, k, insert x right)
    in insert' x s s
  let rec member x s =
    let rec member' x s cand =
      match x, s with
          x, EmptyTree -> if x = cand then true else false
        | x, Node(left, k, right) ->
          if Element.lt x k
          then member' x left cand
          else member' x right k
    in match x, s with
        _, EmptyTree -> false
      | x, Node(left, k, right) ->
        if Element.lt x k
        then member x left
        else member' x right k
end

(* Exercise 2.5 *)

module type TREE =
sig
  type 'a tree
  val complete 'a -> int -> 'a tree
end

module Tree =
struct
  type 'a tree = EmptyTree | Node of 'a tree * 'a * 'a tree
  let rec complete x d =
    match d with
        1 -> Node(EmptyTree, x, EmptyTree)
      | _ ->
        let child = complete x (d - 1) in
        Node(child, x, child)
  let rec balanced x n =
    match n with
        0 -> EmptyTree
      | 1 -> Node(EmptyTree, x, EmptyTree)
      | _ ->
        let nn = n - 1 in
        let left_n = (nn / 2) + (nn mod 2) in
        let right_n = nn - left_n in
        Node(balanced x left_n,
             x,
             balanced x right_n)
end

(* FiniteMap Signature *)
module type FINITEMAP =
sig
  type key
  type 'a map
  exception NotFound
  val empty : 'a map
  val bind : key -> 'a map -> 'a map
  val lookup : key -> 'a map -> 'a (* raise NotFound if key is not found *)
end
