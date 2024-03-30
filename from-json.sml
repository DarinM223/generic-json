functor WithFromJSON(Arg: WITH_FROM_JSON_DOM): FROM_JSON_CASES =
struct
  structure FromJSONRep =
    LayerRep
      (open Arg
       type 'a t = JSON.value -> 'a
       type 'a s = JSON.value -> 'a
       type ('a, 'k) p = JSON.value -> 'a)

  structure Open = LayerCases(open Arg FromJSONRep)
end
