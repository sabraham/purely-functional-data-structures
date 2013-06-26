(* Chapter 2 *)

exception Empty
exception Subscript

(* STACK signature *)
module type STACK =
sig
  type 'a stack
  val empty : 'a stack
  val is_empty : 'a stack -> bool
  val cons : 'a -> 'a stack -> 'a stack
  val head : 'a stack -> 'a
  val tail : 'a stack -> 'a stack
  val concat : 'a stack -> 'a stack -> 'a stack
  val update : 'a stack -> int -> 'a -> 'a stack
end

(* Stack struct *)
module ListStack : STACK =
struct
  type 'a stack = 'a list
  let empty = []
  let is_empty s =
    s = []
  let cons x s =
    x :: s
  let head s =
    match s with
        [] -> raise Empty
          x::_ -> x
  let tail s =
    match s with
        [] -> raise Empty
      | _::xs -> xs
  let rec concat xs ys =
    match xs with
        [] -> ys
      | xh::xt -> xh::(concat xt ys)
  let rec update s i v =
    match s with
        [] -> raise Empty
      | x::xs ->
        if i = 0 then v::xs else x::(update xs (i - 1) v)
end

module CustomStack : STACK =
struct
  type 'a stack =
      Nil
    | Cons of 'a * 'a stack
  let empty = Nil
  let is_empty s =
    match s with
        Nil -> true
      | _ -> false
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
      | Cons(x, xs) ->
        if i = 0 then Cons(v, xs) else Cons(x, (update xs (i - 1) v))
end

(* Exercise 2.1 *)

let suffixes xs =
  let rec suffixes' xs acc =
    match xs with
        [] -> List.rev ([]::acc)
      | y::ys -> suffixes' ys (xs::acc)
  in suffixes' xs []

(* ORDERED Signature *)

module type ORDERED =
sig
  type t
  val compare : t -> t -> int
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
            0 -> Node(EmptyTree, x, EmptyTree)
          | n when n < 0 -> Node(insert x left, k, right)
          | _ -> Node(left, k, insert x right)
  let rec member x s =
    match s with
        EmptyTree -> false
      | Node(left, k, right) ->
        match Element.compare x k with
            0 -> true
          | n when n < 0 -> member x left
          | _ -> member x right
end
