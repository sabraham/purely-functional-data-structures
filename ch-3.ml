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
  end

module Heap (Elem : ORDERED) : HEAP =
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
    let insert x h = match h with
        EmptyTree -> Tree(0, x, EmptyTree, EmptyTree)
      | Tree(r, y, a, b) ->
        match Elem.compare x y with
            Less | Equal -> makeT(x, a, insert y b)
          | Greater -> makeT(y, a, insert x b)


  end
