functor WithFromJSON(Arg: WITH_FROM_JSON_DOM): FROM_JSON_CASES =
struct
  structure FromJSONRep =
    LayerRep
      (open Arg
       type 'a t = JSON.value -> 'a option
       type 'a s = JSON.value -> 'a option
       type ('a, 'k) p = JSON.value -> 'a option)

  structure Open =
    LayerCases
      (infix &
       fun iso f (_, a2b) = Option.map a2b o f
       val isoProduct = raise Fail "undefined"
       val isoSum = raise Fail "undefined"
       val op*` = raise Fail "undefined"
       val op+` = raise Fail "undefined"
       val C0 = raise Fail "undefined"
       val C1 = raise Fail "undefined"
       val op--> = raise Fail "undefined"
       val T = raise Fail "undefined"
       val R = raise Fail "undefined"
       val Y = raise Fail "undefined"
       val array = raise Fail "undefined"
       val bool = raise Fail "undefined"
       val char = raise Fail "undefined"
       val string = raise Fail "undefined"
       val unit = raise Fail "undefined"
       val word = raise Fail "undefined"
       val vector = raise Fail "undefined"
       val list = raise Fail "undefined"
       val real = raise Fail "undefined"
       val int = raise Fail "undefined"
       val word32 = raise Fail "undefined"
       val word8 = raise Fail "undefined"
       val largeReal = raise Fail "undefined"
       val fixedInt = raise Fail "undefined"
       val largeInt = raise Fail "undefined"
       val largeWord = raise Fail "undefined"
       fun hole () _ =
         raise Fail "hole not defined for FromJSON"
       fun refc f = Option.map ref o f
       fun data f a = f a
       fun tuple f a = f a
       fun record f a = f a
       fun exn _ =
         raise Fail "exn not defined for FromJSON"
       fun regExn0 _ _ =
         raise Fail "regExn0 not defined for FromJSON"
       fun regExn1 _ _ _ =
         raise Fail "regExn1 not defined for FromJSON"
       open Arg FromJSONRep)
end
