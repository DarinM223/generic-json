signature Generic =
sig
  include Generic FROM_JSON
end

functor MkGeneric(Arg: Generic) =
struct
  structure Open = MkGeneric(Arg)
  open Arg Open
  structure FromJSONRep = Open.Rep
end

structure Generic =
  MkGeneric (structure Open = WithFromJSON(Generic) open Generic Open)