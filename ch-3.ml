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
    
