signature Generic =
sig
  include Generic TO_JSON
end

functor MkGeneric(Arg: Generic) =
struct
  structure Open = MkGeneric(Arg)
  open Arg Open
  structure ToJSONRep = Open.Rep
end

structure Generic =
  MkGeneric (structure Open = WithToJSON(Generic) open Generic Open)
