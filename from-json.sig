signature FROM_JSON =
sig
  structure FromJSONRep: OPEN_REP
  val fromJson: ('a, 'b) FromJSONRep.t -> JSON.value -> 'a
end

signature FROM_JSON_CASES =
sig
  include CASES FROM_JSON
  sharing Open.Rep = FromJSONRep
end

signature WITH_FROM_JSON_DOM = CASES
