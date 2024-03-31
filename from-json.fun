functor WithFromJSON(Arg: WITH_FROM_JSON_DOM): FROM_JSON_CASES =
struct
  structure OptionUtils =
  struct
    open Option
    fun op>>= (SOME a, f) = f a
      | op>>= (NONE, _) = NONE
  end

  structure FromJSONRep =
    LayerRep
      (open Arg
       type 'a t = JSON.value -> 'a option
       type 'a s = JSON.value -> 'a option
       type ('a, 'k) p = JSON.value -> 'a option)

  val fromJson = FromJSONRep.This.getT

  structure Open =
    LayerCases
      (open TopLevel
       infix & >>=
       fun iso f (_, a2b) = Option.map a2b o f
       fun isoProduct f (_, c2a) = Option.map c2a o f
       fun isoSum f (_, b2a) = Option.map b2a o f

       fun op*` (aT, bT) (JSON.ARRAY [a, b]) =
             let open OptionUtils
             in aT a >>= (fn a => bT b >>= (fn b => SOME (a & b)))
             end
         | op*` _ _ = NONE

       fun op+` (aT, bT) v =
         case aT v of
           SOME a => SOME (Sum.INL a)
         | NONE =>
             (case bT v of
                SOME b => SOME (Sum.INR b)
              | NONE => NONE)

       fun C0 con (JSON.OBJECT [("tag", JSON.STRING conName)]) =
             if Generics.Con.toString con = conName then SOME () else NONE
         | C0 _ _ = NONE
       fun C1 con aT (JSON.OBJECT [("tag", JSON.STRING conName), ("value", a)]) =
             if Generics.Con.toString con = conName then aT a else NONE
         | C1 _ _ _ = NONE
       fun op--> _ =
         raise Fail "--> not defined for FromJSON"

       fun T f a = f a
       fun R _ f a = f a
       val Y = Tie.function
       fun bool (JSON.BOOL b) = SOME b
         | bool _ = NONE
       fun char (JSON.STRING s) = Char.fromString s
         | char _ = NONE
       fun string (JSON.STRING s) = SOME s
         | string _ = NONE
       fun unit JSON.NULL = SOME ()
         | unit _ = NONE
       fun word (JSON.INT i) =
             SOME (Word.fromLargeInt i)
         | word _ = NONE
       fun list f (JSON.ARRAY vs) =
             (List.foldr
                (fn (e, acc) =>
                   let open OptionUtils
                   in acc >>= (fn acc => f e >>= (fn e => SOME (e :: acc)))
                   end) (SOME []) vs)
         | list _ _ = NONE
       val array = fn f => Option.map Array.fromList o list f
       val vector = fn f => Option.map Vector.fromList o list f
       fun real (JSON.FLOAT f) = SOME f
         | real _ = NONE
       fun int (JSON.INT i) =
             SOME (Int.fromLarge i)
         | int _ = NONE
       val word32 = Option.map Word32.fromWord o word
       val word8 = Option.map Word8.fromWord o word
       val largeReal = Option.map Real.toLarge o real
       val fixedInt = fn _ => raise Fail "fixedInt not defined for FromJSON"
       val largeInt = fn _ => raise Fail "largeInt not defined for FromJSON"
       val largeWord = fn _ => raise Fail "largeWord not defined for FromJSON"
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
