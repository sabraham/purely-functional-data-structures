type order = Less | Greater | Equal

module type ORDERED =
sig
  type t
  val compare : t -> t -> order
end

module type HEAP =
sig
  module Elem : ORDERED
  type heap
  exception Empty
  val empty : heap
  val is_empty : heap -> bool
  val insert : Elem.t -> heap -> heap
  val merge : heap -> heap -> heap
  val find_min : heap -> Elem.t (* Raises Empty if heap is empty *)
  val delete_min : heap -> heap
  val from_list : Elem.t list -> heap
end

module Heap (Elem : ORDERED) : HEAP with type Elem.t = Elem.t =
struct
  module Elem = Elem
  type heap = EmptyTree | Tree of int * Elem.t * heap * heap
  exception Empty
  let empty = EmptyTree
  let is_empty = function
    | EmptyTree -> true
    | _ -> false
  let rank = function
    | EmptyTree -> 0
    | Tree(r, _, _, _) -> r
  let makeT(x, a, b) =
    if (rank a) >= (rank b)
    then Tree(rank b + 1, x, a, b)
    else Tree(rank a + 1, x, b, a)
  let rec merge h1 h2 = match h1, h2 with
      h, EmptyTree -> h
    | EmptyTree, h -> h
    | Tree(_, x, a1, b1), Tree(_, y, a2, b2) ->
      match Elem.compare x y with
          Less | Equal -> makeT(x, a1, merge b1 h2)
        | Greater -> makeT(y, a2, merge h1 b2)
  let find_min h = match h with
      EmptyTree -> raise Empty
    | Tree(_, x, a, b) -> x
  let delete_min h = match h with
      EmptyTree -> raise Empty
    | Tree(_, x, a, b) -> merge a b
  let rec insert x h = match h with
      EmptyTree -> Tree(1, x, EmptyTree, EmptyTree)
    | Tree(_, y, left, right) ->
      match Elem.compare x y with
          Less | Equal -> makeT(x, left, insert y right)
        | Greater -> makeT(y, left, insert x right)
  let from_list l = match l with
      [] -> raise Empty
    | l ->
      let rec from_list' l = match l with
          [] | _::[] -> l
        | x::y::rest -> [merge x y]@from_list' rest in
      let singleton x = makeT(x, EmptyTree, EmptyTree) in
      let hl = from_list' (List.map singleton l) in
      List.hd hl
end

module IntOrdered : ORDERED with type t = int =
struct
  type t = int
  let c = compare
  let compare x y =
    if (x < y) then Less
    else if (x = y) then Equal
    else Greater
end


module WeightHeap (Elem : ORDERED) : HEAP with type Elem.t = Elem.t =
struct
  module Elem = Elem
  type heap = EmptyTree | Tree of int * Elem.t * heap * heap
  exception Empty
  let empty = EmptyTree
  let is_empty = function
    | EmptyTree -> true
    | _ -> false
  let size = function
    | EmptyTree -> 0
    | Tree(s, _, _, _) -> s
  let makeT(x, a, b) =
    if (size a) >= (size b)
    then Tree(size a + size b + 1, x, a, b)
    else Tree(size a + size b + 1, x, b, a)
  let rec merge h1 h2 = match h1, h2 with
      h, EmptyTree -> h
    | EmptyTree, h -> h
    | Tree(s1, x, a1, b1), Tree(s2, y, a2, b2) ->
      match Elem.compare x y with
          Less | Equal ->
            if size a1 + s2 >= size b1 + s2
            then Tree(s1 + s2, x, merge a1 h2, b1)
            else Tree(s1 + s2, x, a1, merge b1 h2)
        | Greater ->
          if size a2 + s2 >= size b2 + s2
          then Tree(s1 + s2, y, merge a2 h1, b2)
          else Tree(s1 + s2, y, a2, merge b2 h1)
  let find_min h = match h with
      EmptyTree -> raise Empty
    | Tree(_, x, a, b) -> x
  let delete_min h = match h with
      EmptyTree -> raise Empty
    | Tree(_, x, a, b) -> merge a b
  let rec insert x h = match h with
      EmptyTree -> Tree(1, x, EmptyTree, EmptyTree)
    | Tree(r, y, left, right) ->
      match Elem.compare x y with
          Less | Equal -> makeT(x, left, insert y right)
        | Greater -> makeT(y, left, insert x right)
  let from_list l = match l with
      [] -> raise Empty
    | l ->
      let rec from_list' l = match l with
          [] | _::[] -> l
        | x::y::rest -> [merge x y]@from_list' rest in
      let singleton x = makeT(x, EmptyTree, EmptyTree) in
      let hl = from_list' (List.map singleton l) in
      List.hd hl
end
