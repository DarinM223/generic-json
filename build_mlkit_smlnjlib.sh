cat > mlkit-stubs.sml <<EOL
structure Unsafe =
struct
  structure Basis =
  struct
    structure Array = Array
    structure Vector = Vector
    structure CharArray = CharArray
    structure CharVector = CharVector
    structure Word8Array = Word8Array
    structure Word8Vector = Word8Vector
  end

  structure Vector = struct val sub = Basis.Vector.sub end

  structure Array =
  struct
    val sub = Basis.Array.sub
    val update = Basis.Array.update
    val create = Basis.Array.array
  end

  structure CharArray =
  struct
    open Basis.CharArray
    fun create i =
      array (i, chr 0)
  end

  structure CharVector =
  struct
    open Basis.CharVector
    fun create i =
      Basis.CharArray.vector (Basis.CharArray.array (i, chr 0))
    fun update (vec, i, el) =
      raise Fail "Unimplemented: Unsafe.CharVector.update"
  end

  structure Word8Array =
  struct
    open Basis.Word8Array
    fun create i = array (i, 0w0)
  end

  structure Word8Vector =
  struct
    open Basis.Word8Vector
    fun create i =
      Basis.Word8Array.vector (Basis.Word8Array.array (i, 0w0))
    fun update (vec, i, el) =
      raise Fail "Unimplemented: Unsafe.Word8Vector.update"
  end

  structure Real64Array =
  struct
    open Basis.Array
    type elem = Real.real
    type array = elem array
    fun create i = array (i, 0.0)
  end
end;
signature IEEE_REAL =
   sig
      exception Unordered

      datatype real_order = LESS | EQUAL | GREATER | UNORDERED

      datatype float_class =
         NAN
       | INF
       | ZERO
       | NORMAL
       | SUBNORMAL

      datatype rounding_mode =
         TO_NEAREST
       | TO_NEGINF
       | TO_POSINF
       | TO_ZERO

      type decimal_approx = {class: float_class,
                             digits: int list,
                             exp: int,
                             sign: bool}

      val fromString: string -> decimal_approx option
      val getRoundingMode: unit -> rounding_mode
      val scan: (char, 'a) StringCvt.reader
                -> (decimal_approx, 'a) StringCvt.reader
      val setRoundingMode: rounding_mode -> unit
      val toString: decimal_approx -> string
   end;
structure IEEEReal: IEEE_REAL =
   struct
      exception Unordered
      datatype real_order = LESS | EQUAL | GREATER | UNORDERED

      datatype float_class =
         INF
       | NAN
       | NORMAL
       | SUBNORMAL
       | ZERO

      datatype rounding_mode =
         TO_NEAREST
       | TO_NEGINF
       | TO_POSINF
       | TO_ZERO

      type decimal_approx =
         {class: float_class,
          digits: int list,
          exp: int,
          sign: bool}

      val setRoundingMode = fn _ => raise Fail "IEEEReal.setRoundingMode"
      val getRoundingMode = fn _ => raise Fail "IEEEReal.getRoundingMode"

      fun 'a scan reader (state: 'a) = raise Fail "IEEEReal.scan"

      val fromString = fn _ => raise Fail "IEEEReal.fromString"
      val toString = fn _ => raise Fail "IEEEReal.toString"
   end;
signature REAL =
   sig
      type real

      structure Math: MATH where type real = real

      val != : real * real -> bool
      val * : real * real -> real
      val *+ : real * real * real -> real
      val *- : real * real * real -> real
      val + : real * real -> real
      val - : real * real -> real
      val / : real * real -> real
      val <  : real * real -> bool
      val <= : real * real -> bool
      val == : real * real -> bool
      val >  : real * real -> bool
      val >= : real * real -> bool
      val ?= : real * real -> bool
      val ~ : real -> real
      val abs: real -> real
      val ceil: real -> Int.int
      val checkFloat: real -> real
      val class: real -> IEEEReal.float_class
      val compare: real * real -> order
      val compareReal: real * real -> IEEEReal.real_order
      val copySign: real * real -> real
      val floor: real -> Int.int
      val fmt: StringCvt.realfmt -> real -> string
      val fromDecimal: IEEEReal.decimal_approx -> real option
      val fromInt: int -> real
      val fromLarge: IEEEReal.rounding_mode -> LargeReal.real -> real
      val fromLargeInt: LargeInt.int -> real
      val fromManExp: {man: real, exp: int} -> real
      val fromString: string -> real option
      val isFinite: real -> bool
      val isNan: real -> bool
      val isNormal: real -> bool
      val max: real * real -> real
      val maxFinite: real
      val min: real * real -> real
      val minNormalPos: real
      val minPos: real
      val negInf: real
      val nextAfter: real * real -> real
      val posInf: real
      val precision: int
      val radix: int
      val realCeil: real -> real
      val realFloor: real -> real
      val realMod: real -> real
      val realRound: real -> real
      val realTrunc: real -> real
      val rem: real * real -> real
      val round: real -> Int.int
      val sameSign: real * real -> bool
      val scan: (char, 'a) StringCvt.reader -> (real, 'a) StringCvt.reader
      val sign: real -> int
      val signBit: real -> bool
      val split: real -> {whole: real, frac: real}
      val toDecimal: real -> IEEEReal.decimal_approx
      val toInt: IEEEReal.rounding_mode -> real -> int
      val toLarge: real -> LargeReal.real
      val toLargeInt: IEEEReal.rounding_mode -> real -> LargeInt.int
      val toManExp: real -> {man: real, exp: int}
      val toString: real -> string
      val trunc: real -> Int.int
      val unordered: real * real -> bool
   end;
structure Real : REAL =
struct
   open Real
   val minPos = 4.9406564584124654E~324;
   val minNormalPos = 2.2250738585072014E~308;
   val maxFinite = 1.7976931348623157E308;
   fun *+ (a, b, c) = a * b + c
   fun *- (a, b, c) = a * b - c
   fun ?= (x, y) =
      if isNan x then true
      else if isNan y then true
      else == (x, y)
   fun checkFloat x =
      if isNan x then raise Div
      else if not (isFinite x) then raise Overflow
      else x
   fun class x =
      if isNan x then IEEEReal.NAN
      else if not (isFinite x) then IEEEReal.INF
      else if == (x, 0.0) then IEEEReal.ZERO
      else if (abs x) < minPos then IEEEReal.SUBNORMAL
      else IEEEReal.NORMAL
   fun compareReal (x, y) =
      (case compare (x, y) of
          LESS => IEEEReal.LESS
        | EQUAL => IEEEReal.EQUAL
        | GREATER => IEEEReal.GREATER)
      handle IEEEReal.Unordered => IEEEReal.UNORDERED
   fun copySign (x: real, y: real) : real = raise Fail "Real.copySign"
   fun fromDecimal d = raise Fail "Real.fromDecimal"
   fun fromLarge (_ : IEEEReal.rounding_mode) (r : LargeReal.real) : real = r
   fun fromLargeInt i = valOf (fromString (LargeInt.toString i))
   fun fromManExp {man: real, exp: int} : real = raise Fail "Real.fromManExp"
   fun isNormal x = (class x = IEEEReal.NORMAL)
   fun nextAfter (x: real, y: real) : real = raise Fail "Real.nextAfter"
   val precision = 53
   val radix = 2
   fun realCeil (r : real) : real = raise Fail "Real.realCeil"
   fun realFloor (_ : real) : real = raise Fail "Real.realFloor"
   fun realMod (_ : real) : real = raise Fail "Real.realMod"
   fun realRound (_ : real) : real = raise Fail "Real.realRound"
   fun realTrunc (_ : real) : real = raise Fail "Real.realTrunc"
   fun rem (_ : real, _ : real) : real = raise Fail "Real.rem"
   fun signBit r = sameSign (r, ~1.0)
   fun split (_ : real) : {whole : real, frac : real} = raise Fail "Real.split"
   fun toDecimal (_ : real) : IEEEReal.decimal_approx = raise Fail "Real.toDecimal"
   fun toInt rm r =
      case rm of
         IEEEReal.TO_NEGINF => floor r
       | IEEEReal.TO_POSINF => ceil r
       | IEEEReal.TO_ZERO => trunc r
       | IEEEReal.TO_NEAREST => round r
   fun toLarge (r: real) : LargeReal.real = r
   fun toLargeInt rm r = raise Fail "Real.toLargeInt"
   fun toManExp (_ : real) : {man: real, exp: int} = raise Fail "Real.toManExp"
   fun unordered (x, y) = isNan x orelse isNan y
end;
EOL

cat > smlnj-lib.mlb <<EOL
mlkit-stubs.sml
\$(SML_LIB)/smlnj-lib/Util/smlnj-lib.mlb
EOL

cat > smlnj-lib2.mlb <<EOL
\$(SML_LIB)/basis/basis.mlb
EOL

mlton -stop f smlnj-lib.mlb \
    | grep -v ".mlb" \
    | grep -v "/usr/local/lib/mlton/sml/basis/" \
    | grep -v "/usr/local/lib/mlton/targets/" \
    >> smlnj-lib2.mlb
rm smlnj-lib.mlb
mv smlnj-lib2.mlb smlnj-lib.mlb