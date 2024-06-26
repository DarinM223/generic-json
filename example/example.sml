structure Example =
struct
  open TopLevel
  infix & *` +`

  type 'a person = {name: string, age: int, data: 'a}
  val person = fn a_ =>
    let
      open Generic
    in
      record' (R' "name" string *` R' "age" int *` R' "data" a_)
        ( fn {name, age, data} => name & age & data
        , fn name & age & data => {name = name, age = age, data = data}
        )
    end

  structure Bop =
  struct
    datatype t = Add | Sub | Mul | Div
    val t =
      let
        open Generic
      in
        data' (C0' "Add" +` C0' "Sub" +` C0' "Mul" +` C0' "Div")
          ( fn Div => INR ()
             | Mul => INL (INR ())
             | Sub => INL (INL (INR ()))
             | Add => INL (INL (INL ()))
          , fn INR () => Div
             | INL (INR ()) => Mul
             | INL (INL (INR ())) => Sub
             | INL (INL (INL ())) => Add
          )
      end
  end

  structure Anf =
  struct
    type var = string
    val var = Generic.string
    datatype value = Int of int | Var of var | Glob of var
    val value =
      let
        open Generic
      in
        data' (C1' "Int" int +` C1' "Var" var +` C1' "Glob" var)
          ( fn Glob ? => INR ? | Var ? => INL (INR ?) | Int ? => INL (INL ?)
          , fn INR ? => Glob ? | INL (INR ?) => Var ? | INL (INL ?) => Int ?
          )
      end

    datatype t =
      Halt of value
    | Fun of var * var list * t * t
    | Join of var * var option * t * t
    | Jump of var * value option
    | App of var * var * value list * t
    | Bop of var * Bop.t * value * value * t
    | If of value * t * t
    | Tuple of var * value list * t
    | Proj of var * var * int * t
    val t =
      let
        open Tie
        val Y = Generic.Y
      in
        fix Y (fn t =>
          let
            open Generic
          in
            data'
              (C1' "Halt" value +` C1' "Fun" (tuple4 (var, list var, t, t))
               +` C1' "Join" (tuple4 (var, option var, t, t))
               +` C1' "Jump" (tuple2 (var, option value))
               +` C1' "App" (tuple4 (var, var, list value, t))
               +`
               C1' "Bop"
                 (tuple' (T var *` T Bop.t *` T value *` T value *` T t)
                    ( fn (t0, t1, t2, t3, t4) => t0 & t1 & t2 & t3 & t4
                    , fn t0 & t1 & t2 & t3 & t4 => (t0, t1, t2, t3, t4)
                    )) +` C1' "If" (tuple3 (value, t, t))
               +` C1' "Tuple" (tuple3 (var, list value, t))
               +` C1' "Proj" (tuple4 (var, var, int, t)))
              ( fn Proj ? => INR ?
                 | Tuple ? => INL (INR ?)
                 | If ? => INL (INL (INR ?))
                 | Bop ? => INL (INL (INL (INR ?)))
                 | App ? => INL (INL (INL (INL (INR ?))))
                 | Jump ? => INL (INL (INL (INL (INL (INR ?)))))
                 | Join ? => INL (INL (INL (INL (INL (INL (INR ?))))))
                 | Fun ? => INL (INL (INL (INL (INL (INL (INL (INR ?)))))))
                 | Halt ? => INL (INL (INL (INL (INL (INL (INL (INL ?)))))))
              , fn INR ? => Proj ?
                 | INL (INR ?) => Tuple ?
                 | INL (INL (INR ?)) => If ?
                 | INL (INL (INL (INR ?))) => Bop ?
                 | INL (INL (INL (INL (INR ?)))) => App ?
                 | INL (INL (INL (INL (INL (INR ?))))) => Jump ?
                 | INL (INL (INL (INL (INL (INL (INR ?)))))) => Join ?
                 | INL (INL (INL (INL (INL (INL (INL (INR ?))))))) => Fun ?
                 | INL (INL (INL (INL (INL (INL (INL (INL ?))))))) => Halt ?
              )
          end)
      end
  end

  datatype stmt =
    Assign of string * expr
  | If of expr * stmt list * stmt list
  and expr =
    Stmt of stmt
  | Int of int
  | Bop of expr * expr
  val expr & stmt =
    let
      open Tie
      val Y = Generic.Y
    in
      fix (Y *` Y) (fn expr & stmt =>
        let
          open Generic
        in
          data'
            (C1' "Stmt" stmt +` C1' "Int" int +` C1' "Bop" (tuple2 (expr, expr)))
            ( fn Bop ? => INR ? | Int ? => INL (INR ?) | Stmt ? => INL (INL ?)
            , fn INR ? => Bop ? | INL (INR ?) => Int ? | INL (INL ?) => Stmt ?
            )
          &
          data'
            (C1' "Assign" (tuple2 (string, expr))
             +` C1' "If" (tuple3 (expr, list stmt, list stmt)))
            ( fn If ? => INR ? | Assign ? => INL ?
            , fn INR ? => If ? | INL ? => Assign ?
            )
        end)
    end

  val testStmt: stmt list =
    [ Assign ("x", Bop (Int 1, Int 2))
    , If (Int 1, [Assign ("y", Int 1)], [Assign ("z", Int 2)])
    ]

  val tmpFileName = "tmp.txt"
  val outstream = TextIO.openOut tmpFileName
  val json = Generic.toJson (Generic.list stmt) testStmt

  val () = JSONPrinter.print (outstream, json)
  val () = TextIO.closeOut outstream
  val file = JSONParser.openFile tmpFileName
  val json = JSONParser.parse file

  val testStmt = Option.valOf (Generic.fromJson (Generic.list stmt) json)
  val () = print (Generic.show (Generic.list stmt) testStmt)
  val () = print "\n"
end
