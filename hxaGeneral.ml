(*------------------------------------------------------------------------------

   HXA General Library (OCaml 4.10)
   Harrison Ainsworth / HXA7241 : 2015, 2020

   http://www.hxa.name/tools/

   License: AGPL -- https://www.gnu.org/licenses/agpl-3.0.html

------------------------------------------------------------------------------*)




(* using Unix *)
(* using Str *)




(* ---- types ---- *)

type 'a ress  = ('a , string) result
type 'a resx  = ('a , exn)    result

type 'a list1 = ('a * 'a list)

type sysExit =
   | EXIT_OK
   | EXIT_USAGE       | EXIT_DATAERR     | EXIT_NOINPUT    | EXIT_NOUSER
   | EXIT_NOHOST      | EXIT_UNAVAILABLE | EXIT_SOFTWARE   | EXIT_OSERR
   | EXIT_OSFILE      | EXIT_CANTCREAT   | EXIT_IOERR      | EXIT_TEMPFAIL
   | EXIT_PROTOCOL    | EXIT_NOPERM      | EXIT_CONFIG
   | EXIT_UNSPECIFIED

exception Intolerable of (sysExit * string)




(* ---- functions ---- *)

(* -- data -- *)

let ofList1 (l1:'a list1) : 'a list =
   (fst l1) :: (snd l1)


let ofList1o (l1o:'a list1 option) : 'a list =
   match l1o with
   | Some l1 -> ofList1 l1
   | None    -> []


let toList1 (l:'a list) : 'a list1 option =
   match l with
   | []     -> None
   | h :: t -> Some (h , t)


let ( @< ) (l:'a list) (a:'a) : 'a list =
   l @ (a :: [])


(* -- print, exception-default, assert, exit -- *)

let print_string_flush (s:string) : unit =
   Printf.printf "%s%!" s


let traceHead (trace:bool) (module_:string) (function_:string)
   (title:string)
   : unit =
   if trace
   then Printf.printf "\n### %s.%s - %s\n\n%!" module_ function_ title


let traceString (trace:bool) (label:string) (content:string) : unit =
   if trace
   then Printf.printf "%s%s\n%!" label content


let traceRess (trace:bool) (label:string) (toString:'a -> string)
   (content:'a ress)
   : unit =
   traceString
      trace
      label
      (Result.fold ~ok:toString ~error:((^) "*** Error: ") content)


let excToDefaultf ~(default:unit -> 'a) ~(f:unit -> 'a) : 'a =
   try f () with
   | Out_of_memory | Stack_overflow | Sys.Break as x -> raise x
   | _                                               -> default ()


let excToDefault (default:'a) (f:unit -> 'a) : 'a =
   excToDefaultf ~default:(Fun.const default) ~f


(* @exceptions: whatever can be raised by Printf.ksprintf *)
let assertLog_x (outputter:string->unit) (b:bool) (message:string) : bool =
   if not b then Printf.ksprintf outputter "%s\n%!" message ;
   b


let exitcm (code:int) (messageMain:string) (messageDetail:string) : 'a =
   let message =
      messageMain ^ (if messageDetail = "" then "" else ": " ^ messageDetail)
   in
   Printf.eprintf "*** Failed: %s\n%!" message ;
   exit code


let exite (sysexit:sysExit) (message:string) : 'a =
   match sysexit with
   | EXIT_OK          -> exit 0
   | EXIT_USAGE       -> exitcm  64 "command line usage error"  message
   | EXIT_DATAERR     -> exitcm  65 "user data format error"    message
   | EXIT_NOINPUT     -> exitcm  66 "cannot open input"         message
   | EXIT_NOUSER      -> exitcm  67 "addressee unknown"         message
   | EXIT_NOHOST      -> exitcm  68 "host name unknown"         message
   | EXIT_UNAVAILABLE -> exitcm  69 "service unavailable"       message
   | EXIT_SOFTWARE    -> exitcm  70 "internal software error"   message
   | EXIT_OSERR       -> exitcm  71 "system error"              message
   | EXIT_OSFILE      -> exitcm  72 "critical OS file missing"  message
   | EXIT_CANTCREAT   -> exitcm  73 "cannot create output file" message
   | EXIT_IOERR       -> exitcm  74 "input/output error"        message
   | EXIT_TEMPFAIL    -> exitcm  75 "temporary failure"         message
   | EXIT_PROTOCOL    -> exitcm  76 "remote error in protocol"  message
   | EXIT_NOPERM      -> exitcm  77 "permission denied"         message
   | EXIT_CONFIG      -> exitcm  78 "configuration error"       message
   | EXIT_UNSPECIFIED -> exitcm 114 "unspecified/unknown error" message


(* -- string, numerical, timer -- *)

let string_of_char (c:char) : string =
   String.make 1 c


let string_of_byte (b:int)  : string =
   String.make 1 (char_of_int (b land 0xFF))


let clamp ~(lo:'a) ~(up:'a) (n:'a) : 'a =
   (min (max lo n) up)


let isNan (f:float) : bool =
   match classify_float f with
   | FP_nan                                           -> true
   | FP_normal | FP_subnormal | FP_zero | FP_infinite -> false


let minMaxMean (things:float list) : (float * float * float) =
   let min , max , sum = List.fold_left
      (fun (mini,maxi,sum) e -> ( (min e mini) , (max e maxi) , (e +. sum) ))
      ( max_float , min_float , 0.0 )
      things
   and count = float_of_int (List.length things) in
   (min , max , (sum /. count))


let timerWall (f:'a -> 'b) (input:'a) : ('b * float) =
   let timeBegin = Unix.gettimeofday () in
   let output = f input in
   let timeEnd = Unix.gettimeofday () in
   ( output , timeEnd -. timeBegin )


(* -- function combinators -- *)

let id = Fun.id

let ko = Fun.const

let tr = Fun.flip

let ( % ) f g x = g (f x)

let ( %> ) = ( % )

let ne = Fun.negate


(* -- heterogenous (product) map -- *)

let hemap2 (f0,f1:('a0 -> 'b0) * ('a1 -> 'b1)) (a0,a1:'a0 * 'a1) : ('b0 * 'b1) =
   ( f0 a0 , f1 a1 )




(* ---- std lib module augmentations ---- *)

module Option_ :
sig
   val default  : 'a           -> 'a option -> 'a
   val valuef   : (unit -> 'a) -> 'a option -> 'a
   val unify    : (unit -> 'a) -> 'a option -> 'a
   val defaultf : (unit -> 'a) -> 'a option -> 'a
   val diverge  : 'a option -> ('a option * unit option)
   val mapUnify : ('a -> 'b) -> (unit -> 'b) -> 'a option -> 'b
   val toBool   : 'a option -> bool
   val fromBool : bool -> unit option
   val classify : ('a -> bool) -> 'a -> 'a option
   val toRes    : 'e -> 'o option -> ('o,'e) result
   val toResU   : 'o option -> ('o, unit) result
   val resoptToOptres : (('o, 'e) result) option -> ('o option, 'e) result
   val toExc_x  : (unit -> exn) -> 'a option -> 'a
   val fromExc  : (unit -> 'a) -> 'a option
   val and2     : ('a option) -> ('b option) -> (('a * 'b) option)
   val or2      : ('a option) -> ('a option) -> ('a option)
   (*val merge2   : 'a -> ('a -> 'a -> 'a) -> ('a option * 'a option) -> 'a*)
end
=
struct
   let default (d:'a) (o:'a option) : 'a =
      Option.value ~default:d o

   let valuef (f:unit -> 'a) (o:'a option) : 'a =
      match o with
      | Some v -> v
      | None   -> f ()

   let unify = valuef

   let defaultf = valuef

   let diverge (o:'a option) : ('a option * unit option) =
      match o with
      | Some a -> (Some a , None   )
      | None   -> (None   , Some ())

   let mapUnify (fs:'a -> 'b) (fn:unit -> 'b) (o:'a option) : 'b =
      (Option.map fs o) |> (defaultf fn)

   let toBool = Option.is_some
   (*let toBool (o:'a option) : bool =
      match o with
      | Some _ -> true
      | None   -> false*)

   let fromBool (b:bool) : unit option =
      if b then Some () else None

   let classify (pred:'a -> bool) (a:'a) : 'a option =
      if pred a then Some a else None

   let toRes (e:'e) (oo:'o option) : ('o,'e) result =
      Option.to_result ~none:e oo
   (*let toRes (e:'e) (oo:'o option) : ('o,'e) result =
      match oo with
      | Some o -> Ok    o
      | None   -> Error e*)

   let toResU (oo:'o option) : ('o, unit) result =
      toRes () oo

   let resoptToOptres (resopt:(('o,'e) result) option)
      : (('o option),'e) result =

      match resopt with
      | Some res ->
         begin match res with
         | Ok o    -> Ok (Some o)
         | Error e -> Error e
         end
      | None -> Ok None

      (*
      o r -> ro
      ----+----
      s o |  os (value)
      s e |  e
      n - |  on
      n - |  on
      *)

   let toExc_x (f:unit -> exn) (o:'a option) : 'a =
      match o with
      | Some a -> a
      | None   -> raise (f ())

   let fromExc (f:unit -> 'a) : 'a option =
      excToDefaultf ~default:(Fun.const None) ~f:(f % Option.some)

   let and2 (o0:'o0 option) (o1:'o1 option) : ('o0 * 'o1) option =
      match o0 , o1 with
      | Some s0 , Some s1 -> Some (s0 , s1)
      | Some _  , None    -> None
      | None    , Some _  -> None
      | None    , None    -> None

   let or2 (o0:'a option) (o1:'a option) : ('a option) =
      match o0 with
      | Some _ -> o0
      | None   -> o1

   (*let merge2 (nul:'a) (sum:'a -> 'a -> 'a) (e0oe1o:'a option * 'a option)
      : 'a =
      match e0oe1o with
      | Some e0 , Some e1 -> sum e0 e1
      | None    , Some e1 -> e1
      | Some e0 , None    -> e0
      | None    , None    -> nul*)
end


module Result_ :
sig
   val default      : 'o         -> ('o,'e) result -> 'o
   val valuef       : ('e -> 'o) -> ('o,'e) result -> 'o
   val defaultf     : ('e -> 'o) -> ('o,'e) result -> 'o
   val map : (('a -> 'c) * ('b -> 'd)) -> ('a,'b) result -> ('c,'d) result
   (*val resMap_      : ?ok:('a -> 'c) -> ?er:('b -> 'd) -> ('a,'b) result ->
      ('c,'d) result*)
   val okMap        : ('o0 -> 'o1) -> ('o0,'e) result -> ('o1,'e) result
   val errorMap     : ('e0 -> 'e1) -> ('o,'e0) result -> ('o,'e1) result
   val diverge      : ('o,'e) result -> ('o option * 'e option)
   val toOpt        : ('o,'e) result -> 'o option
   val optresToResopt : ('o option, 'e) result -> (('o, 'e) result) option
   val toExc_x      : ('e -> exn) -> ('o,'e) result -> 'o
   val fromExc      : (unit -> 'o) -> ('o , exn) result
   val fromExc2     : 'e -> (unit -> 'o) -> ('o,'e) result
   val ressOr2      :
      string ->
      ('o0 * 'o1) ->
      ((('o0,string) result) * (('o1,string) result)) ->
      ((('o0 * 'o1) , string) result)
   val ressAnd2     :
      string ->
      ((('o0,string) result) * (('o1,string) result)) ->
      ((('o0 * 'o1) , string) result)
   val ressAnd3 :
      string ->
      ((('o0,string) result) * (('o1,string) result) * (('o2,string) result)) ->
      ((('o0 * 'o1 * 'o2) , string) result)
end
=
struct
   let default (d:'o) (r:('o,'e) result) : 'o =
      Result.value ~default:d r

   let valuef (f:('e -> 'o)) (r:('o,'e) result) : 'o =
      match r with
      | Ok o    -> o
      | Error e -> f e

   let defaultf = valuef

   let map ((fo,fe):('o0 -> 'o1)*('e0 -> 'e1)) (r:('o0,'e0) result)
      : ('o1,'e1) result =
      match r with
      | Ok    o -> Ok    (fo o)
      | Error e -> Error (fe e)

   (* won't compile to correct types -- id forces it to ('a -> 'a) *)
   (*let resMap_ ?(ok:('a -> 'c) = Fun.id) ?(er:('b -> 'd) = Fun.id)
      (r:('a,'b) result)
      : ('c,'d) result =
      match r with
      | Ok    o -> Ok    (ok o)
      | Error e -> Error (er e)*)

   let okMap    = Result.map
   (*let okMap    (f:('o0 -> 'o1)) (r:('o0,'e) result) : ('o1,'e) result =
      resMap (f , id) r*)

   let errorMap = Result.map_error
   (*let errorMap (f:('e0 -> 'e1)) (r:('o,'e0) result) : ('o,'e1) result =
      resMap (id , f) r*)

   let diverge (r:('o,'e) result) : ('o option * 'e option) =
      match r with
      | Ok    o -> (Some o , None)
      | Error e -> (None , Some e)

   let toOpt = Result.to_option
   (*let toOpt (r:('o,'e) result) : 'o option =
      match r with
      | Ok    o -> Some o
      | Error _ -> None*)

   let optresToResopt (optres:(('o option),'e) result)
      : (('o,'e) result) option =

      match optres with
      | Ok opt ->
         begin match opt with
         | Some s -> Some (Ok s)
         | None   -> None
         end
      | Error e -> Some (Error e)

      (*
      r o -> or
      ----+----
      o s |  so (value)
      o n |  n
      e - |  se
      e - |  se
      *)

   let toExc_x (f:'e -> exn) (r:('o,'e) result) : 'o =
      match r with
      | Ok    o -> o
      | Error e -> raise (f e)

   let fromExc (f:unit -> 'o) : ('o , exn) result =
      try Ok (f ()) with
      | Out_of_memory | Stack_overflow | Sys.Break as x -> raise x
      | x                                               -> Error x

   let fromExc2 (e:'e) (f:unit -> 'o) : ('o,'e) result =
      excToDefault (Error e) (f % Result.ok)

   (*let bool_of_result (r:('o,'e) result) : bool =
      match r with
      | Ok _    -> true
      | Error _ -> false

   let result_of_bool (b:bool) : ('o,'e) result =
      if b then Ok () else Error ()*)

   let ressOr2
      (joiner:string)
      (default0 , default1 : ('o0 * 'o1))
      (r0 , r1 : (('o0,string) result) * (('o1,string) result))
      : (('o0 * 'o1) , string) result =

      match (r0,r1) with
      | (Ok o0    , Ok o1   ) -> Ok (o0 , o1)
      | (Error _  , Ok o1   ) -> Ok (default0 , o1)
      | (Ok o0    , Error _ ) -> Ok (o0 , default1)
      | (Error e0 , Error e1) -> Error (e0 ^ joiner ^ e1)

   let ressAnd2
      (joiner:string)
      (r0 , r1 : (('o0,string) result) * (('o1,string) result))
      : (('o0 * 'o1) , string) result =

      match (r0,r1) with
      | (Ok o0    , Ok o1   ) -> Ok (o0 , o1)
      | (Error e0 , Ok _    ) -> Error e0
      | (Ok _     , Error e1) -> Error e1
      | (Error e0 , Error e1) -> Error (e0 ^ joiner ^ e1)

   let ressAnd3
      (joiner:string)
      (r0 , r1 , r2 :
         (('o0,string) result) * (('o1,string) result) * (('o2,string) result))
      : (('o0 * 'o1 * 'o2) , string) result =

      (ressAnd2 joiner
         ((ressAnd2 joiner (r0 , r1)) , r2))
      |>
      (okMap (fun ((o1,o2),o3) -> (o1,o2,o3)))
end


module Int_ :
sig
   val digitsDec : int -> int
   val modw      : int -> int -> int
end
=
struct
   let digitsDec (i:int) : int =
      i  |> float_of_int |> abs_float |> (max 1.0)
         |> log10 |> floor |> int_of_float |> succ

   let modw (x:int) (y:int) : int =
      if x >= 0
      then x mod y
      else let y = abs y in (y - 1) - ((-x - 1) mod y)
end


module Char_ :
sig
   val isAlpha    : char -> bool
   val isDigit    : char -> bool
   val isDigitOct : char -> bool
   val isDigitHex : char -> bool
   val isSign     : char -> bool
   val isAscii    : char -> bool
   val isBlank    : char -> bool
   val isNewline  : char -> bool
   val isCrOrLf   : char -> bool
end
=
struct
   let isAlpha (c:char) : bool =
      match c with | 'a'..'z' | 'A'..'Z' -> true | _ -> false

   let isDigit (c:char) : bool =
      match c with | '0'..'9' -> true | _ -> false

   let isDigitOct (c:char) : bool =
      match c with | '0'..'7' -> true | _ -> false

   let isDigitHex (c:char) : bool =
      match c with | '0'..'9' | 'a'..'f' | 'A'..'F' -> true | _ -> false

   let isSign (c:char) : bool =
      (c = '-') || (c = '+')

   let isAscii (c:char) : bool =
      (int_of_char c) <= 127

   let isBlank (c:char) : bool =
      match c with
      | ' ' | '\x09' | '\x0A' | '\x0B' | '\x0C' | '\x0D' -> true
      | _                                                -> false

   let isNewline (c:char) : bool =
      c = '\n'

   let isCrOrLf (c:char) : bool =
      (c = '\r') || (c = '\n')
end


module String_ :
sig
   val isEmpty     : string -> bool
   val notEmpty    : string -> bool
   val lastPos     : string -> int
   val repeat      : string -> int -> string
   val lead        : string -> int -> string
   val trail       : string -> int -> string
   val leadTrail   : string -> int -> (string * string)
   val last        : string -> char
   val subo        : int -> int -> string -> string option
   val subc        : string -> int -> int -> string
   val subp        : string -> int -> int -> string
   val subpc       : string -> int -> int -> string
   val isFirstChar : (char -> bool) -> string -> bool
   val index       : char -> ?start:int -> string -> int option
   val indexp      : (char -> bool) -> ?start:int -> string -> int option
   val indexl      : char -> ?start:int -> string -> int
   val indexpl     : (char -> bool) -> ?start:int -> string -> int
   val rindexp     : (char -> bool) -> string -> int option
   val containsp   : (char -> bool) -> string -> bool
   val filter      : (char -> bool) -> string -> string
   val filterAscii : string -> string
   val check       : (char -> bool) -> string -> bool
   val halvep      : char -> string -> (string * string * int) option
   val halve       : char -> string -> (string * string) option
   val splitp      : ?ls:((string * int) list) -> (char -> bool) -> string ->
                     (string * int) list
   val split       : (char -> bool) -> string -> string list
   val trimTrunc   : (string * int) -> (string , string) result
   val truncate    : int -> string -> string
   val capitaliseAll : string -> string
   val toInt       : ?zeroPadded:(bool * int) -> ?widthMaxed:int ->
                     ?signed:bool -> string -> int option
   val ofOpt       : string option -> string
   val toOpt       : string -> string option
end
=
struct
   let isEmpty (s:string) : bool =
      (String.length s) = 0

   let notEmpty (s:string) : bool =
      not (isEmpty s)

   let lastPos (s:string) : int =
      (String.length s) - 1

   let repeat (s:string) (i:int) : string =
      let b = Buffer.create 16 in
      if i > 0 then for _ = 1 to i do Buffer.add_string b s done ;
      Buffer.contents b

   let lead (s:string) (pos:int) : string =
      let posc = clamp ~lo:0 ~up:(String.length s) pos in
      String.sub s 0 posc

   let trail (s:string) (pos:int) : string =
      let posc = clamp ~lo:0 ~up:(String.length s) pos in
      String.sub s posc ((String.length s) - posc)

   let leadTrail (s:string) (pos:int) : (string * string) =
      ( lead s pos , trail s pos )

   let last (s:string) : char =
      s.[(String.length s) - 1]

   let subo (pos:int) (len:int) (s:string) : string option =
      try Some (String.sub s pos len) with
      | Invalid_argument _ -> None

   let subc (s:string) (pos:int) (len:int) : string =
      let wholeLen = String.length s in
      let posc     = clamp ~lo:0 ~up:wholeLen          pos in
      let lenc     = clamp ~lo:0 ~up:(wholeLen - posc) len in
      String.sub s posc lenc

   let subp (s:string) (startpos:int) (endpos:int) : string =
      String.sub s startpos (endpos - startpos)

   let subpc (s:string) (startpos:int) (endpos:int) : string =
      let wholeLen = String.length s in
      let startc   = clamp ~lo:0      ~up:wholeLen startpos in
      let endc     = clamp ~lo:startc ~up:wholeLen endpos in
      subc s startpos (endc - startc)

   let isFirstChar (pred:char -> bool) (s:string) : bool =
      ((String.length s) > 0) && pred s.[0]

   let index (c:char) ?(start:int = 0) (s:string) : int option =
      try Some (String.index_from s start c) with
      | Invalid_argument _ | Not_found -> None

   let indexp (pred: char -> bool) ?(start:int = 0) (s:string) : int option =
      let len = String.length s in
      let rec recur (i:int) (s:string) : int option =
         if i < len
         then if pred s.[i] then Some i else recur (i + 1) s
         else None
      in
      recur (if start < 0 then len else start) s

   let indexl (c:char) ?(start:int = 0) (s:string) : int =
      Option_.defaultf (fun () -> String.length s) (index c ~start s)

   let indexpl (pred: char -> bool) ?(start:int = 0) (s:string) : int =
      Option_.defaultf (fun () -> String.length s) (indexp pred ~start s)

   let rindexp (pred: char -> bool) (s:string) : int option =
      let rec recur (s:string) (i:int) : int option =
         let i = i - 1 in
         if i >= 0
         then if pred s.[i] then Some i else recur s i
         else None
      in
      recur s (String.length s)

   let containsp (pred: char -> bool) (s:string) : bool =
      (indexp pred s) |> Option_.toBool

   let filter (pred: char -> bool) (s:string) : string =
      let len = String.length s in
      let buf = Buffer.create len in
      for i = 0 to (len - 1) do
         if pred s.[i] then Buffer.add_char buf s.[i] ;
      done ;
      Buffer.contents buf

   let filterAscii : (string -> string) =
      filter Char_.isAscii

   let check (pred: char -> bool) (s:string) : bool =
      match rindexp (Fun.negate pred) s with
      | Some _ -> false
      | None   -> true

   let halvep (div:char) (str:string) : (string * string * int) option =
      Option.map
         (fun pos ->
            (  String.sub str 0 pos ,
               String.sub str (pos + 1) ((String.length str) - (pos + 1)) ,
               pos ))
         (index div str)

   let halve (div:char) (str:string) : (string * string) option =
      (halvep div str)
      |> (Option.map (fun (left , right , _) -> (left , right)))

   let rec splitp ?(ls:(string * int) list = []) (pred: char -> bool)
      (s:string)
      : (string * int) list =
      match rindexp pred s with
      | Some pos ->
         let half1 = String.sub s 0 pos
         and half2 = String.sub s (pos + 1) ((String.length s) - (pos + 1)) in
         splitp ~ls:((half2 , (pos + 1)) :: ls) pred half1
      | None -> ((s , 0) :: ls)

   let split (pred: char -> bool) (s:string) : string list =
      let lsi = splitp pred s in
      fst (List.split lsi)

   let trimTrunc ((s:string) , (max:int)) : (string , string) result =
      let st = String.trim s in
      if (String.length st) <= max
      then Ok st
      else Error ("too long (> " ^ (string_of_int max) ^ ")")

   let truncate (max:int) (s:string) : string =
      if String.length s <= max then s else String.sub s 0 max

   let capitaliseAll (input:string) : string =
      let finder = Str.regexp {|^[a-z]\|[^a-zA-Z][a-z]|}
      and substituter (whole:string) : string =
         let wordStart = Str.matched_string whole in
         match (String.length wordStart) with
         | 1 ->
            (* very first char of string *)
            String.capitalize_ascii wordStart
         | 2 ->
            (* for word-break of: blank, '_', '-' *)
            if
               isFirstChar
                  (fun c -> (Char_.isBlank c) || (c = '_') || (c = '-'))
                  wordStart
            then
               (String.sub wordStart 0 1)
               ^ (String.capitalize_ascii (String.sub wordStart 1 1))
            else
               wordStart
         | _ ->
            (* regex should not allow this *)
            wordStart
      in
      Str.global_substitute finder substituter input

   let toInt ?(zeroPadded:(bool * int) option) ?(widthMaxed:int option)
      ?(signed:bool option) (input:string) : int option =

      (* input-string analysis:
         [prefix: non-digits] [digits: digits only] [suffix: any left] *)
      let digitsPos   = indexpl Char_.isDigit input in
      let suffixPos   =
         (indexpl (Fun.negate Char_.isDigit) ~start:digitsPos input)
      in
      let digitsWidth = suffixPos - digitsPos in

      if
         (* is string basically viable ? *)
         (notEmpty input)
         &&
         (* is prefix (sign) valid: 0 or 1 width, and - or + ? *)
         (  let isSignedValid   = (digitsPos = 1) && (Char_.isSign input.[0])
            and isUnsignedValid = digitsPos = 0
            in
            match signed with
            | Some false -> isUnsignedValid
            | Some true  -> isSignedValid
            | None       -> isUnsignedValid || isSignedValid )
         &&
         (* is digits width minimally valid ? *)
         ( digitsWidth >= 1 )
         &&
         (* is there no (non-digit) suffix ? *)
         ( suffixPos == String.length input )
         &&
         (* is digits width not too large ? *)
         (  match widthMaxed with
            | Some width -> digitsWidth <= (max 1 width)
            | None       -> true )
         &&
         (* is digits correctly zero-padded (to minimum width) ? *)
         (  match zeroPadded with
            | Some (false , _)     -> input.[digitsPos] <> '0'
            | Some (true  , width) -> digitsWidth = (max 1 width)
            | None                 -> true )
      then
         (* Scan %d handles decimal, optional sign, and leading zeros -- OK.
            Scan %d also allows embedded '_'s -- not OK, but these have been
            prohibited in the above checks. *)
         Option_.fromExc
            (fun () -> Scanf.sscanf (lead input suffixPos) "%d" Fun.id)
      else
         None

   let ofOpt (so:string option) : string =
      Option.value ~default:"" so

   let toOpt (s:string) : string option =
      Option_.classify notEmpty s
end


module List_ :
sig
   val isEmpty       : 'a list -> bool
   val notEmpty      : 'a list -> bool
   val hd            : 'a list -> 'a option
   val first         : 'a list -> 'a option
   val ft            : 'a list -> 'a option
   val last          : 'a list -> 'a option
   val tlSafe        : 'a list -> 'a list
   val nth           : int -> 'a list -> 'a option
   val hdft          : 'a list -> 'a list
   val bisect        : 'a list -> int -> ('a list * 'a list)
   val find          : ('a -> bool) -> 'a list -> 'a option
   val filtmap       : ('a -> 'b option) -> 'a list -> 'b list
   val partmap       : ('a -> ('o, 'e) result) -> 'a list -> ('o list * 'e list)
   val findmap       : ('a -> 'b option) -> 'a list -> 'b option
   val deduplicate : 'a list -> 'a list
   val optAnd        : ('a option) list -> ('a list) option
   val optOr         : ('a option) list -> ('a list) option
   val resAnd        : (('o,'e) result list) -> ('o list , 'e list) result
   val unfoldl       : (int->'a) -> int -> 'a list
   val unfoldo       : (int->'a option) -> 'a list
   val equalenTruncate : 'a list -> 'b list -> ('a list * 'b list)
   val equalenExtend   : 'a -> 'b -> 'a list -> 'b list -> ('a list * 'b list)
   val ofStringAscii : string -> char list
   val toStringAscii : (char list) -> string
   val ofOpt         : 'a option -> 'a list
   val toOpt         : 'a list -> 'a option
end
=
struct
   let isEmpty (l:'a list) : bool =
      (List.length l = 0)

   let notEmpty (l:'a list) : bool =
      not (isEmpty l)

   let hd (l:'a list) : 'a option =
      match l with
      | hd :: _ -> Some hd
      | []      -> None

   let first = hd

   let rec ft (l:'a list) : 'a option =
      match l with
      | ft :: [] -> Some ft
      | _  :: tl -> ft tl
      | []       -> None

   let last = ft

   let tlSafe (l:'a list) : 'a list =
      match l with
      | _ :: tail -> tail
      | []        -> []

   let nth (index:int) (l:'a list) : 'a option =
      try Some (List.nth l index) with Failure _ -> None

   let hdft (l:'a list) : 'a list =
      match l with
      | []         -> []
      | [single]   -> [single]
      | hd :: tail -> [ hd ; (List.rev tail) |> List.hd ]

   let bisect (l:'a list) (m:int) : ('a list * 'a list) =
      let rec recur (i:int) (l:'a list) (body:'a list) : ('a list * 'a list) =
         if i > 0
         then recur (i - 1) (List.tl l) ((List.hd l) :: body)
         else ( (List.rev body) , l )
      in
      recur (min m (List.length l)) l []

   let find = List.find_opt
   (*let findo (f:'a -> bool) (l:'a list) : 'a option =
      try Some (List.find f l) with Not_found -> None*)

   (*
   ?

   let classify2 (preds:('a -> bool) list) (l:'a list) : 'a list list =
      List.fold_left
         (fun (classes:'a list list) (item:'a) : 'a list list ->
            List.rev_map2
               (fun (pred:('a -> bool)) (class_:'a list) : 'a list ->
                   if (pred item) then (item :: class_) else class_)
               preds classes)
         (List.unfoldl (Fun.const []) (List.length preds))
         l

   let classify (f:'out list array -> 'inp -> 'out list array) (l:'inp list)
      : 'out list array =
      List.fold_left f [||] (List.rev l)

   let clsf1 (last:string list array) (item:char) : string list array =
      let last = if last = [||] then [|[];[];[]|] else last in
      let index =
         match item with
         | 'a'..'z' -> 0
         | '0'..'9' -> 1
         | _        -> 2
      in
      Array.set last index ((string_of_char item) :: last.(index)) ;
      last
   *)

   let filtmap = List.filter_map
   (*let filtmap (f:'a -> 'b option) (l:'a list) : 'b list =
      List.fold_right (fun a out ->
         match f a with | Some b -> b :: out | None -> out) l []*)

   let partmap (f:'a -> ('ok,'er) result) (l:'a list) : ('ok list * 'er list) =
      List.fold_right
         (fun a (oOut , eOut) ->
            match f a with
            | Ok    o -> (o :: oOut ,      eOut)
            | Error e -> (     oOut , e :: eOut))
         l ([] , [])

   let deduplicate (l:'a list) : 'a list =
      let dict = Hashtbl.create (List.length l) in
      (List.fold_left
         (fun (dict , uniques) value ->
            if not (Hashtbl.mem dict value)
            then
               let () = Hashtbl.add dict value true in
               (dict , value :: uniques)
            else
               (dict , uniques) )
         (dict , [])
         l )
      |> snd
      |> List.rev

   let findmap = List.find_map
   (*let rec findmap (predmap:'a -> 'b option) (l:'a list) : 'b option =
      match l with
      | a :: tail ->
         begin match predmap a with
         | None        -> findmap predmap tail
         | Some _ as b -> b
         end
      | [] -> None*)

   let optAnd (lo:('a option) list) : ('a list) option =
      let la    = filtmap Fun.id lo in
      let laLen = List.length la in
      (* Some if: all Some, or empty *)
      if (laLen = (List.length lo))
      then Some la else None

   let optOr (lo:('a option) list) : ('a list) option =
      match filtmap Fun.id lo with
      | [] -> None
      | la -> Some la

   let resAnd (listRes:(('ok,'er) result) list) : ('ok list , 'er list) result =
      let listOk , listErr = partmap Fun.id listRes in
      let listOkLen        = List.length listOk
      and listResLen       = List.length listRes in
      (* Ok if: all Ok, or empty *)
      if (listOkLen = listResLen)
      then (Ok listOk) else (Error listErr)

   let unfoldl (f:int->'a) (size:int) : 'a list =
      let rec recur (list:'a list) (f:int->'a) (size:int) : 'a list =
         if size > 0
         then recur ((f (size - 1)) :: list) f (size - 1)
         else list
      in
      recur [] f size

   let unfoldo (f:int->'a option) : 'a list =
      let rec recur (list:'a list) (index:int) (f:int->'a option) : 'a list =
         match (f index) with
         | Some element -> recur (element :: list) (index + 1) f
         | None         -> List.rev list
      in
      recur [] 0 f

   let equalenTruncate (la:'a list) (lb:'b list) : ('a list * 'b list) =
      let aLen , bLen = List.length la , List.length lb in
      match compare aLen bLen with
      | +1 -> (fst (bisect la bLen) , lb)
      | -1 -> (la , fst (bisect lb aLen))
      |  _ -> (la , lb)

   let equalenExtend (da:'a) (db:'b) (la:'a list) (lb:'b list)
      : ('a list * 'b list) =
      let aLen , bLen = List.length la , List.length lb in
      match compare aLen bLen with
      | -1 -> (la @ (unfoldl (Fun.const da) (bLen - aLen)) , lb)
      | +1 -> (la , lb @ (unfoldl (Fun.const db) (aLen - bLen)))
      |  _ -> (la , lb)

   let ofStringAscii (s:string) : char list =
      unfoldo (fun i -> try Some s.[i] with | Invalid_argument _ -> None)

   (*let rec ofStringAscii ?(lc:char list = []) (s:string) : char list =
      let len = String.length s in
      if len > 0
      then ofStringAscii ~lc:(s.[len - 1] :: lc) (String.sub s 0 (len - 1))
      else lc*)

   let toStringAscii (lc:char list) : string =
      String.concat "" (List.map string_of_char lc)

   let ofOpt (o:'a option) : 'a list =
      match o with
      | Some a -> [a]
      | None   -> []

   let toOpt = hd
end


module Array_ :
sig
   val isEmpty   : 'a array -> bool
   val lead      : int -> 'a array -> 'a array
   val trail     : int -> 'a array -> 'a array
   val leadTrail : int -> 'a array -> ('a array * 'a array)
   val bisect    : 'a array -> int -> ('a array * 'a array)
   val bisecto   : 'a array -> int -> ('a array * 'a array) option
   val partition : ('a -> bool) -> 'a array -> ('a array * 'a array)
   val printc_x  : ('a -> out_channel -> unit) -> 'a array -> out_channel ->
                   unit
   val ofOpt     : 'a option -> 'a array
   val toOpt     : 'a array  -> 'a option
   (*
   val printks_x : ('a -> unit -> string) -> 'a array -> unit -> string
   *)
end
=
struct
   let isEmpty (a:'a array) : bool =
      Array.length a = 0

   let lead (pos:int) (a:'a array) : 'a array =
      let posc = clamp ~lo:0 ~up:(Array.length a) pos in
      Array.sub a 0 posc

   let trail (pos:int) (a:'a array) : 'a array =
      let posc = clamp ~lo:0 ~up:(Array.length a) pos in
      Array.sub a posc ((Array.length a) - posc)

   let leadTrail (pos:int) (a:'a array) : ('a array * 'a array) =
      ( lead pos a , trail pos a )

   (* exceptioning (same as Array.sub) *)
   (*let bisect (a:'a array) (i:int) : ('a array * 'a array) =
      let len = Array.length a in
      (Array.sub a 0 i) , (Array.sub a i (len - i))*)

   let bisect (a:'a array) (i:int) : ('a array * 'a array) =
      let len = Array.length a in
      let i   = min (max 0 i) len in
      (Array.sub a 0 i) , (Array.sub a i (len - i))

   let bisecto (a:'a array) (i:int) : ('a array * 'a array) option =
      let len = Array.length a in
      try
         Some ( (Array.sub a 0 i) , (Array.sub a i (len - i)) )
      with Invalid_argument _ -> None

   let partition (pred:'a -> bool) (a:'a array) : ('a array * 'a array) =
      let l0      = Array.to_list a in
      (* (refman says List.partition preserves order) *)
      let l1 , l2 = List.partition pred l0 in
      ( Array.of_list l1 , Array.of_list l2 )

   let printc_x (printer:('a -> out_channel -> unit)) (a:'a array)
      (out:out_channel) : unit =
      Array.iter (fun e -> Printf.fprintf out "%t " (printer e)) a

   let ofOpt (o:'a option) : 'a array =
      match o with
      | Some a -> [|a|]
      | None   -> [||]

   let toOpt (a:'a array) : 'a option =
      Option_.fromExc (fun () -> Array.get a 0)

   (*
   let printks_x (p:('a -> unit -> string)) (a:'a array) () : string =
      (* maybe *)
   *)
end


module Scanf_ :
sig
   val scanExnUnify_x : (unit -> 'a) -> 'a
   val kscanfErrFn    : Scanf.Scanning.scanbuf -> exn -> ('o , string) result
   val skipBlank      : Scanf.Scanning.scanbuf -> unit
end
=
struct
   let scanExnUnify_x (f:unit -> 'a) : 'a =
      try f () with
      | Scanf.Scan_failure s | Failure s | Invalid_argument s ->
         raise (Scanf.Scan_failure s)
      | End_of_file ->
         raise (Scanf.Scan_failure "unexpected end-of-file")

      (*match Result_.fromExc (f ()) with
      | Ok o    -> o
      | Error e ->
         let s =
            match e with
            | Scanf.Scan_failure s | Failure s | Invalid_argument s -> s
            | End_of_file -> "unexpected end-of-file"
            | _           -> "unspecified exception"
         in
         raise (Scanf.Scan_failure s)*)

   let (kscanfErrFn : Scanf.Scanning.scanbuf -> exn -> ('o , string) result) =
      function _ -> function
      | Scanf.Scan_failure s -> Error ("Scanf.Scan_failure: " ^ s)
      | Failure s            -> Error ("Number conversion failure: " ^ s)
      | Invalid_argument s   -> Error ("Invalid format string: " ^ s)
      | End_of_file          -> Error "Unexpected end-of-file"
      | _                    -> Error "Unknown exception"

   let skipBlank (inBuffer:Scanf.Scanning.scanbuf) : unit =
      try
         Scanf.bscanf inBuffer " " ()
      with
      | Scanf.Scan_failure _ | Failure _ | Invalid_argument _
      | End_of_file                                           -> ()
end




(* ---- modules ---- *)

module Blanks :
sig
   val blankSpacyCtrlChars : string -> string
   val blankNewlines       : string -> string
   val unifyNonNewlines    : string -> string
   val unifySpaces         : string -> string
   val squashSpaces        : string -> string
end
=
struct
   let blankSpacyCtrlChars : (string -> string) =
      String.map
         (function
         | '\x09' | '\x0A' | '\x0B' | '\x0C' | '\x0D' -> ' '
         | c                                          -> c)

   let blankNewlines : (string -> string) =
      String.map (function | '\n' | '\r' -> ' ' | c -> c)

   let unifyNonNewlines (s:string) : string =
      let rx = Str.regexp
         "\x09\\|\x0B\\|\x0C\\|\x20\\|\
         \xC2\x85\\|\xC2\xA0\\|\
         \xE1\x9A\x80\\|\xE1\xA0\x8E\\|\
         \xE2\x80\x80\\|\xE2\x80\x81\\|\xE2\x80\x82\\|\xE2\x80\x83\\|\
         \xE2\x80\x84\\|\xE2\x80\x85\\|\xE2\x80\x86\\|\
         \xE2\x80\x87\\|\xE2\x80\x88\\|\
         \xE2\x80\x89\\|\xE2\x80\x8A\\|\
         \xE2\x80\x8B\\|\xE2\x80\x8C\\|\xE2\x80\x8D\\|\
         \xE2\x80\xA8\\|\xE2\x80\xA9\\|\
         \xE2\x80\xAF\\|\
         \xE2\x81\x9F\\|\
         \xE2\x81\xA0\\|\
         \xE3\x80\x80\\|\
         \xEF\xBB\xBF"
      in
      Str.global_replace rx " " s

   let unifySpaces (s:string) : string =
      (unifyNonNewlines s)
      |> blankNewlines

   let squashSpaces (s:string) : string =
      let rx = Str.regexp "  +" in
      Str.global_replace rx " " s
end


module FileName :
sig
   val splitExt  : string -> (string * string)
   val getMain   : string -> string
   val getExt    : string -> string
   val splitPath : string -> (string * string)
   val getName   : string -> string
   val getPath   : string -> string
end
=
struct
   let splitExt (nameExt:string) : (string * string) =
      try
         let extPos = (String.rindex nameExt '.') in
         let extLen = (String.length nameExt) - extPos in
         ( String.sub nameExt 0 extPos , String.sub nameExt extPos extLen )
      with
      | Not_found -> (nameExt , "")

   let getMain (nameExt:string) : string =
      fst (splitExt nameExt)

   let getExt (nameExt:string) : string =
      snd (splitExt nameExt)

   let splitPath (pathName:string) : (string * string) =
      match (String.rindex_opt pathName '/') with
      | Some pos -> String_.leadTrail pathName (pos + 1)
      | None     -> ("" , pathName)

   let getPath (pathName:string) : string =
      fst (splitPath pathName)

   let getName (pathName:string) : string =
      snd (splitPath pathName)
end


module Rx :
sig
   type rx
   type rxmatch

   val compile    : ?caseInsens:bool -> string -> rx
   val apply      : rx     -> ?pos:int -> string -> rxmatch option
   val regexApply : string -> ?pos:int -> ?caseInsens:bool -> string ->
                    rxmatch option
   val seek       : rx -> ?pos:int -> string -> rxmatch option
   val regexSeek  : string -> ?pos:int -> ?caseInsens:bool -> string ->
                    rxmatch option
   val allMatches : rx -> string -> string list
   val allMatchesPos : rx -> string -> (string * int) list
   val wholeFound : rxmatch -> string
   val wholePos   : rxmatch -> (int * int)
   val groupFound : rxmatch -> int -> (string option)
end
=
struct
   type rx      = Str.regexp
   type rxmatch =
      {  whole  : string ;
         pos    : (int * int) ;
         groups : (string option) array ; }
   (*type rx      = Re.re
   type rxmatch = string array option*)

   (* private *)
   let expressOneMatch (query:bool) (content:string) : rxmatch option =
      if query
      then
         match
            (* should not fail, since the matching query succeeded
               (but if it does, abort the whole result) *)
            try Some (Str.matched_string content) with
            | Not_found | Invalid_argument _ -> None
         with
         | Some wholeMatch ->
            let groups : (string option) list =
               List_.unfoldo
                  (fun index : (string option) option ->
                     try Some (Some (Str.matched_group index content)) with
                     | Not_found          -> Some None
                     | Invalid_argument _ -> None)
            in
            Some
               { whole  = wholeMatch ;
                 pos    = ( Str.match_beginning () , Str.match_end () ) ;
                 groups = Array.of_list groups }
         | None -> None
      else
         None
      (*try Ok (Re.get_all (Re.exec rx content)) with
      | Not_found -> None*)

   let compile ?(caseInsens:bool=false) (rxs:string) : rx =
      if not caseInsens
      then Str.regexp rxs
      else Str.regexp_case_fold rxs

   let apply (rx:rx) ?(pos:int=0) (content:string) : rxmatch option =
      let query = Str.string_match rx content pos in
      expressOneMatch query content

   let regexApply (rxs:string) ?(pos:int=0) ?(caseInsens:bool=false)
      (content:string)
      : rxmatch option =
      apply (compile ~caseInsens rxs) ~pos content

   let seek (rx:rx) ?(pos:int=0) (content:string) : rxmatch option =
      let query =
         try ignore(Str.search_forward rx content pos) ; true with
         | Not_found -> false
      in
      expressOneMatch query content

   let regexSeek (rxs:string) ?(pos:int=0) ?(caseInsens:bool=false)
      (content:string)
      : rxmatch option =
      seek (compile ~caseInsens rxs) ~pos content

   let allMatches (rx:rx) (content:string) : string list =
      (Str.full_split rx content)
      |>
      (List_.filtmap (function
         | Str.Delim d -> Some d
         | Str.Text  _ -> None ))

   let allMatchesPos (rx:rx) (content:string) : (string * int) list =
      (* get all matches : Str.split_result list *)
      (Str.full_split rx content)
      |>
      (* calc positions : (Str.split_result , int) list *)
      ((List.fold_left
         (fun (all,pos) part ->
            let srVal part = Str.(match part with | Delim s | Text s -> s) in
            (  (part , pos) :: all ,
               pos + (String.length (srVal part)) ))
         ( [] , 0 ) )
         %>
         (fun (lst , _) -> List.rev lst))
      |>
      (* drop non-sought parts : (string , int) list *)
      (List_.filtmap
         (function
            | (Str.Delim str , pos) -> Some (str , pos)
            | (Str.Text  _   , _  ) -> None ))

   let wholeFound (rxmatch:rxmatch) : string =
      rxmatch.whole

   let wholePos (rxmatch:rxmatch) : (int * int) =
      rxmatch.pos

   let groupFound (rxmatch:rxmatch) (index:int) : string option =
      excToDefault None (fun () -> rxmatch.groups.(index))
end




(* ---- functions ---- *)

(* -- error-handling pipeline/monad -- *)

let bypass (f:'a -> unit) (a:'a) : 'a =
   (f a) ; a

(*let ( |>=& ) (r0:('o0,string) result) (r1:('o1,string) result)
   : (('o1 * 'o0) , string) result =

   match (r0, r1) with
   | (Ok r0    , Ok r1   ) -> Ok (r1, r0)
   | (Error r0 , Ok _    ) -> Error r0
   | (Ok _     , Error r1) -> Error r1
   | (Error r0 , Error r1) -> Error (r0 ^ ", " ^ r1)*)


(*let resAnd (ab:('o0,'e0) result * ('o1,'e1) result)
   : (('o0 * 'o1) , ('e0 option * 'e1 option)) result =

   match ab with
   | Ok o0    , Ok o1    -> Ok (o0 , o1)
   | Error e0 , Ok _     -> Error (Some e0 , None   )
   | Ok _     , Error e1 -> Error (None    , Some e1)
   | Error e0 , Error e1 -> Error (Some e0 , Some e1)*)


(*let ( &&= ) (a:('o0,'e) result) (b:('o1,'e) result)
   : (('o1 * 'o0) , 'e list) result =

   match (a,b) with
   | (Ok o0    , Ok o1   ) -> Ok (o1 , o0)
   | (Ok _     , Error e1) -> Error [e1]
   | (Error e0 , Ok _    ) -> Error [e0]
   | (Error e0 , Error e1) -> Error [e1 ; e0]*)


let ( ||> ) (o:'a option) (f:unit -> 'a option) : 'a option =
   match o with
   | Some _ as a -> a
   | None        -> f ()


let ( &&> ) (o:'a option) (f:unit -> 'a option) : 'a option =
   match o with
   | Some _ -> f ()
   | None   -> None


let ( |>- ) = Option.bind
(*let ( |>- ) (o1:'o1 option) (f:'o1 -> 'o2 option)
   : 'o2 option =

   match o1 with
   | Some o -> f o
   | None   -> None*)

(*let ( let|>- ) = ( |>- )*)


let ( |>= ) = Result.bind
(*let ( |>= ) (r1:('o1,'e) result) (f:'o1 -> ('o2,'e) result)
   : ('o2,'e) result =

   match r1 with
   | Ok    o      -> f o
   | Error _ as e -> e*)

let ( let|>= ) = ( |>= )


(*let ( |>>= ) (r1:('o1,'e) result) (f:'o1 -> ('o2,'e) result)
   : (('o1 * 'o2) , 'e) result =

   r1 |>= (fun o1 ->
      match f o1 with
      | Ok    o2     -> Ok (o1 , o2)
      | Error _ as e -> e)*)


(* dependent on List, so moved below that

let ( |^= ) (r:('o1,'e) result) (lf:('o1 -> ('o2,'e) result) list)
   : ('o2 list , 'e list) result =

   (* : (('o2,'e) result) list *)
   (List.map ((|>=) r) lf)
   |>
   List.resAnd
*)


let optAnd2p
   (f0:'s0 -> 'o1 option)
   (f1:'s0 -> 'o2 option)
   (s0:'s0)
   : ('o1 * 'o2) option =

   Option_.and2 (f0 s0) (f1 s0)


let ressOr2p
   (joiner:string)
   (defaults:('o1 * 'o2))
   (f0:'o0 -> ('o1,string) result)
   (f1:'o0 -> ('o2,string) result)
   (o0:'o0)
   : (('o1 * 'o2) , string) result =

   Result_.ressOr2 joiner defaults ((f0 o0) , (f1 o0))


let ressAnd2p
   (joiner:string)
   (f0:'o0 -> ('o1,string) result)
   (f1:'o0 -> ('o2,string) result)
   (o0:'o0)
   : (('o1 * 'o2) , string) result =

   Result_.ressAnd2 joiner ((f0 o0) , (f1 o0))


let ( |^^- )
   (o1:'o1 option)
   (  (f0:'o1 -> 'o2 option) ,
      (f1:'o1 -> 'o3 option) )
   : ('o2 * 'o3) option =

   o1 |>- (optAnd2p f0 f1)


let ( |^^= )
   (r1:'o1 ress)
   (  (f0:'o1 -> 'o2 ress) ,
      (f1:'o1 -> 'o3 ress) )
   : ('o2 * 'o3) ress =

   r1 |>= (fun o1 ->
      match ((f0 o1) , (f1 o1)) with
      | (Ok o2    , Ok o3   ) -> Ok (o2 , o3)
      | (Ok _     , Error e1) -> Error e1
      | (Error e0 , Ok _    ) -> Error e0
      | (Error e0 , Error e1) -> Error (e0 ^ " && " ^ e1))


let ressAnd3p
   (joiner:string)
   (f0:'o0 -> ('o1,string) result)
   (f1:'o0 -> ('o2,string) result)
   (f2:'o0 -> ('o3,string) result)
   (o0:'o0)
   : (('o1 * 'o2 * 'o3) , string) result =

   Result_.ressAnd3 joiner ((f0 o0) , (f1 o0) , (f2 o0))

   (*
   (ressAnd2 joiner
      ( (ressAnd2 joiner
         ( (f0 o0)
         , (f1 o0)))
      , (f2 o0)))
   |>
   (okMap (fun ((o1,o2),o3) -> (o1,o2,o3)))

   (*let f12 = (fun o0 -> ressAnd2p joiner f1 f2 (Ok o0)) in
   match (ressAnd2p joiner f0 f12 r) with
   | Ok (o1 , (o2 , o3)) -> Ok (o1 , o2 , o3)
   | Error e             -> Error e*)

   (*let f12 = (fun o0 -> ressAnd2p joiner f1 f2 (Ok o0)) in
   match (ressAnd2p joiner f0 f12 r) with
   | Ok (o1 , (o2 , o3)) -> Ok (o1 , o2 , o3)
   | Error e             -> Error e*)
   *)


let ( |^^^= )
   (r1:('o1,string) result)
   (  (f0:'o1 -> 'o2 ress) ,
      (f1:'o1 -> 'o3 ress) ,
      (f2:'o1 -> 'o4 ress) )
   : ('o2 * 'o3 * 'o4) ress =

   let f12 = (fun o1 -> (Ok o1) |^^= (f1 , f2)) in
   match r1 |^^= (f0 , f12) with
   | Ok (o2 , (o3 , o4)) -> Ok (o2 , o3 , o4)
   | Error e             -> Error e

   (*r1 |>= (fun o1 ->
      match ((f0 o1) , (f1 o1) , (f2 o1)) with
      | (Ok o2 , Ok o3 , Ok o4)          -> Ok (o2 , o3 , o4)
      | (Error e0 , Ok _ , Ok _)         -> Error e0
      | (Ok _ , Error e1 , Ok _)         -> Error e1
      | (Ok _ , Ok _ , Error e2)         -> Error e2
      | (Error e0 , Error e1 , Ok _)     -> Error (e0 ^ " && " ^ e1)
      | (Error e0 , Ok _ , Error e2)     -> Error (e0 ^ " && " ^ e2)
      | (Ok _ , Error e1 , Error e2)     -> Error (e1 ^ " && " ^ e2)
      | (Error e0 , Error e1 , Error e2) ->
         Error (e0 ^ " && " ^ e1 ^ " && " ^ e2))*)


let ( |>=? ) (r1:('o,'e) result) (f:'e -> ('o,'e) result)
   : ('o,'e) result =

   match r1 with
   | Ok _ as o -> o
   | Error e   -> f e


let ( |>=+ ) (o1:'o1) (f:'o1 -> ('o2,'e) result) : ('o2,'e) result =
   (Ok o1) |>= f


let ( |>=- ) (r1:('o1,'e) result) (f:'o1 -> 'o2) : ('o2,'e) result =
   r1 |>= (fun o1 -> Ok (f o1))


let ( |>=> )
   (f:'o1 -> ('o2,'e) result)
   (g:'o2 -> ('o3,'e) result)
   (r:('o1,'e) result)
   : ('o3,'e) result =

   r |>= f |>= g


(* -- file read/write -- *)

let useFile (filePathname:string)
   (opener:string -> 'c) (closer:'c -> unit) (use:'c -> 'i)
   : ('i , exn) result =

   Result_.fromExc (fun () : 'c -> opener filePathname)
   |>=
   (fun (channel:'c) : ('i , exn) result ->
      let result = Result_.fromExc (fun () : 'i -> use channel) in
      closer channel ;
      result)


let fileRead ~(filePathname:string)
   : (string , exn) result =

   useFile filePathname open_in_bin close_in_noerr
      (fun c ->
         let len = in_channel_length c in
         really_input_string c len
         (*if len <= ...
         then really_input_string c len
         else failwith ("file too large (" ^ (string_of_int len) ^ ")")*))


let fileWrite ~(filePathname:string) ~(stuffToWrite:string)
   : (unit , exn) result =

   useFile filePathname open_out_bin close_out_noerr
      (fun c ->
         output_string c stuffToWrite ;
         flush c ;)


let inputString (inChannel:in_channel) (expectedLength:int) : string =

   let buf = Buffer.create expectedLength in
   (* just keep sucking chars until EOF exception *)
   begin
      try
         while true do
            let c = input_char inChannel in
            Buffer.add_char buf c ;
         done ;
      with
      | End_of_file -> () ;
   end ;
   Buffer.contents buf


(* -- command-line invoke -- *)

let quoteShellPathname (pathname:string) : string =

   let quotesRemoved     = String.split_on_char '\'' pathname in
   let reinsertedEscaped = String.concat {|'\''|} quotesRemoved in
   "'" ^ reinsertedEscaped ^ "'"


let commandLineInvoke2 (command:string) (environ:string list) (input:string)
   (expectedLength:int)
   : ((string * string * int) , (Unix.process_status * Unix.error)) result =

   try
      let stdOut , stdIn , stdErr =
         Unix.open_process_full command (Array.of_list environ)
      in
      output_string stdIn input ;
      close_out stdIn ;
      let toolOut    = inputString stdOut expectedLength in
      let toolErr    = inputString stdErr 32 in
      let exitStatus = Unix.close_process_full (stdOut , stdIn , stdErr) in

      match exitStatus with
      | Unix.WEXITED exitCode       -> Ok (toolOut , toolErr , exitCode)
      | Unix.WSIGNALED _ as sigCode -> Error (sigCode , Unix.EUNKNOWNERR 0)
      | Unix.WSTOPPED  _ as sigCode -> Error (sigCode , Unix.EUNKNOWNERR 0)

   with
   | Unix.Unix_error (errorCode , _ , _) -> Error (Unix.WEXITED 0 , errorCode)


let commandLineInvoke (command:string) (environ:string list) (input:string)
   (expectedLength:int)
   : string ress =

   match commandLineInvoke2 command environ input expectedLength with

   (* normal execution *)
   | Ok (output , error , exitCode) ->
      begin match exitCode with
      | 0        -> Ok output
      | exitCode -> Error ("(" ^ (string_of_int exitCode) ^ ") " ^ error)
      end

   (* invocation failure *)
   | Error (sigCode , unixError) ->
      begin match sigCode with
      | Unix.WSIGNALED _
      | Unix.WSTOPPED  _ ->
         Error "invocation did not exit, due to signal"
      | Unix.WEXITED _   ->
         Error (Unix.error_message unixError)
      end
