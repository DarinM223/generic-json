functor WithToJSON(Arg: WITH_TO_JSON_DOM): TO_JSON_CASES =
struct
  structure ToJSONRep =
    LayerRep
      (open Arg
       type 'a t = 'a -> JSON.value
       type 'a s = 'a -> JSON.value
       type ('a, 'k) p = 'a -> JSON.value)
  val toJson = ToJSONRep.This.getT
  structure Open =
    LayerCases
      (infix &
       fun iso f (b2a, _) = f o b2a
       fun isoProduct f (c2a, _) = f o c2a
       fun isoSum f (b2a, _) = f o b2a
       fun op*` (aT, bT) (a & b) =
         JSON.ARRAY [aT a, bT b]
       fun op+` (aT, _) (Sum.INL a) = aT a
         | op+` (_, bT) (Sum.INR b) = bT b
       fun C0 con () =
         JSON.OBJECT [("tag", JSON.STRING (Generics.Con.toString con))]
       fun C1 con aT a =
         JSON.OBJECT
           [("tag", JSON.STRING (Generics.Con.toString con)), ("value", aT a)]
       fun op--> _ = raise Fail "--> not defined for ToJSON"
       fun T f a = f a
       fun R _ f a = f a
       val Y = Tie.function
       fun array f =
         JSON.ARRAY o List.map f o Array.toList
       val bool = JSON.BOOL
       val char = JSON.STRING o String.str
       val string = JSON.STRING
       val unit = fn () => JSON.NULL
       val word = JSON.INT o Word.toLargeInt
       fun vector f =
         JSON.ARRAY o List.map f o Vector.toList
       fun list f = JSON.ARRAY o List.map f
       val real = JSON.FLOAT
       val int = JSON.INT o Int.toLarge
       val word32 = word
       val word8 = word o Word8.toWord
       val largeReal = JSON.FLOAT o Real.fromLarge IEEEReal.TO_NEAREST
       val fixedInt = fn _ => raise Fail "fixedInt not defined for ToJSON"
       val largeInt = fn _ => raise Fail "largeInt not defined for ToJSON"
       val largeWord = fn _ => raise Fail "largeWord not defined for ToJSON"
       fun hole () _ = raise Fail "hole not defined for ToJSON"
       fun refc f (ref a) = f a
       fun data f a = f a
       fun tuple f a = f a
       fun record f a = f a
       fun exn _ = raise Fail "exn not defined for ToJSON"
       fun regExn0 _ _ =
         raise Fail "regExn0 not defined for ToJSON"
       fun regExn1 _ _ _ =
         raise Fail "regExn1 not defined for ToJSON"
       open Arg ToJSONRep)
end
