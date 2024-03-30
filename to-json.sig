signature TO_JSON =
sig
  structure ToJSONRep: OPEN_REP
  val toJson: ('a, 'b) ToJSONRep.t -> 'a -> JSON.value
end

signature TO_JSON_CASES =
sig
  include CASES TO_JSON
  sharing Open.Rep = ToJSONRep
end

signature WITH_TO_JSON_DOM = CASES
