(*------------------------------------------------------------------------------

   HXA General Library (OCaml 4.12)
   Harrison Ainsworth / HXA7241 : 2015, 2021

   http://www.hxa.name/tools/

   License: AGPL -- https://www.gnu.org/licenses/agpl-3.0.html

------------------------------------------------------------------------------*)




(**
   General useful little pieces to fill-in or extend the stdlib.

   No larger, overall, structure or dependencies, merely disparate items, with
   a few delegations here and there.

   <h3>types</h3>
   - specialised Results
   - list longer than 0
   - sysexits

   <h3>functions</h3>
   - data
   - print, exception-default, assert, exit
   - string, numerical, timer
   - function combinators
   - heterogenous (product) map

   <h3>stdlib module augmentations</h3>
   - Option
   - Result
   - Int
   - Char
   - String
   - List
   - Array
   - Scanf

   <h3>other modules</h3>
   - Blanks
   - FileName
   - Rx

   <h3>functions</h3>
   - error-handling pipeliner/monad
   - file read/write
   - command-line invoke
*)




(** <h1> types </h1> *)

type 'a ress  = ('a , string) result
type 'a resx  = ('a , exn)    result

(** List of at least one item. *)
type 'a list1 = ('a * 'a list)

(** The standard exit codes defined in sysexits.h. *)
type sysExit =
   | EXIT_OK
   | EXIT_USAGE       | EXIT_DATAERR     | EXIT_NOINPUT    | EXIT_NOUSER
   | EXIT_NOHOST      | EXIT_UNAVAILABLE | EXIT_SOFTWARE   | EXIT_OSERR
   | EXIT_OSFILE      | EXIT_CANTCREAT   | EXIT_IOERR      | EXIT_TEMPFAIL
   | EXIT_PROTOCOL    | EXIT_NOPERM      | EXIT_CONFIG
   | EXIT_UNSPECIFIED

(** General exception, of exit code, message, and hint. *)
exception Intolerable of (sysExit * string * string)




(* -------------------------------------------------------------------------- *)
(** <h1> functions </h1> *)


(** <h2> data </h2> *)

(** Convert list1 to std list. *)
val ofList1  : 'a list1 -> 'a list

(** Convert list1 option to std list. *)
val ofList1o : 'a list1 option -> 'a list

(** Convert std list to list1 option. *)
val toList1 : 'a list -> 'a list1 option

(** Reverse cons -- append at foot. *)
val ( @< ) : 'a list -> 'a -> 'a list



(** <h2> print, exception-default, assert, exit </h2> *)

(** Like print_string, but with a flush. *)
val print_string_flush : string -> unit

(** Print log/trace heading (module.function, title) conditionally. *)
val traceHead : bool -> string -> string -> unit

(** Print log/trace label and content conditionally. *)
val traceString : bool -> string -> string -> unit

(** Print log/trace label and converted content conditionally. *)
val traceRess : bool -> string -> ('a -> string) -> ('a,string) result -> unit

(** Convert (by catching) exception to defaulter function
    (except: Out_of_memory, Stack_overflow, Sys.Break). *)
val excToDefaultf : default:(unit -> 'a) -> f:(unit -> 'a) -> 'a

(** Convert (by catching) exception to default value
    (except: Out_of_memory, Stack_overflow, Sys.Break). *)
val excToDefault : 'a -> (unit -> 'a) -> 'a

(** If boolean false, call outputter with string.
    @raise Unknown whatever can be raised by Printf.ksprintf *)
val assertLog_x : (string->unit) -> bool -> string -> bool

(** Print a message (main, detail, hint), and exit with a particular code. *)
val exitPrint : int -> string -> string -> string -> 'a

(** Print a message and exit, according to the sysexits.h convention. *)
val exitSysPrint : sysExit -> string -> string -> 'a

(**
   Print a message and raise an Intolerable exception.

   @param first  to print or not
   @param second sysExit
   @param third  code location
   @param fourth message
   @param fifth  hint
   @param sixth  supplemental message
 *)
val raiseTrace_x : bool -> sysExit -> string -> string -> string -> string -> 'a



(** <h2> string, numerical, timer </h2> *)

(** Convert a char to a string. *)
val string_of_char : char -> string

(** Convert a char-code (clamped to 0-FF) to a string. *)
val string_of_byte : int -> string


(** Clamp number to: lower <= num <= upper. *)
val clamp : lo:'a -> up:'a -> 'a -> 'a

(** The NaN aspect of Pervasives.classify_float. *)
val isNan : float -> bool

(** Basic stats of a float list. *)
val minMaxMean : float list -> (float * float * float)


(**
   Wrap a function in a timer (wall-clock, sub-second resolution).

   @param first  function
   @param second input for function
   @return function output, and duration
 *)
val timerWall : ('a -> 'b) -> 'a -> ('b * float)



(** <h2> function combinators </h2> *)

(** Identity: id x = x -- same as Fun.id. *)
val id : 'a -> 'a

(** Constant: (ko c) x = c -- same as Fun.const. *)
val ko : 'a -> 'b -> 'a

(** Transpose: (tr f) x y = f y x -- same as Fun.flip. *)
val tr : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

(** Compose: (f % g)(x) = g(f(x)) *)
val ( %  ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

(** Compose: (f %> g)(x) = g(f(x)) *)
val ( %> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

(** Negate: (ne p) x = not (p x) -- same as Fun.negate. *)
val ne : ('a -> bool) -> 'a -> bool



(** <h2> heterogenous (product) map </h2> *)

(**
   Heterogenous 2-product map.
   Applies a pair of functions to a pair of values.
 *)
val hemap2 : (('a0 -> 'b0) * ('a1 -> 'b1)) -> ('a0 * 'a1) -> ('b0 * 'b1)




(* -------------------------------------------------------------------------- *)
(** <h1> stdlib module augmentations </h1> *)

(**
   Augmentation of stdlib Option module.
 *)
module Option_ :
sig
   (** <h2> map </h2> *)

   (** Unify an option with a default for None.
      Conciser synonym for Option.value. *)
   val default  : 'a           -> 'a option -> 'a

   (** Unify an option with a defaulter function for None. *)
   val valuef   : (unit -> 'a) -> 'a option -> 'a

   (** Unify an option with a defaulter function for None.
      Synonym for valuef. *)
   val defaultf : (unit -> 'a) -> 'a option -> 'a

   (** Diverge an option to a tuple of options (non-case -> None). *)
   val diverge : 'a option -> ('a option * unit option)

   (** Map Some, and default (by function) None, for an option. *)
   val foldf : ('a -> 'b) -> (unit -> 'b) -> 'a option -> 'b


   (** <h2> convert </h2> *)

   (** Convert option -> bool. *)
   val toBool : 'a option -> bool

   (** Convert bool -> option. *)
   val fromBool : bool -> unit option

   (** Convert value to option, according to predicate. *)
   val classify : ('a -> bool) -> 'a -> 'a option

   (** Convert option to result (by replacing None with Error). *)
   val toRes : 'er -> 'ok option -> ('ok,'er) result

   (** Convert option to result (by replacing None with unit Error). *)
   val toResU : 'ok option -> ('ok, unit) result

   (** Transpose res-opt to opt-res.
   <code>
   opt res -> res opt
   ------------------
   some ok | ok some (value)
   some er | er (value)
   none -  | ok none
   none -  | ok none</code> *)
   val resoptToOptres : (('ok, 'er) result) option -> ('ok option, 'er) result

   (** Convert option to value or raised exception.
      @raise Abstract the input exception *)
   val toExc_x : (unit -> exn) -> 'a option -> 'a

   (** Convert exception (by catching) to option.
      (not caught: Out_of_memory, Stack_overflow, Sys.Break). *)
   val fromExc : (unit -> 'a) -> 'a option


   (** <h2> logical </h2> *)

   (** And-merge two heterogeneous options.
      Some a and Some b -> Some (a , b), otherwise None. *)
   val and2 : ('a option) -> ('b option) -> (('a * 'b) option)

   (** Or-short-circuit two homogeneous options.
      Some a, else Some b, else None. *)
   val or2 : ('a option) -> ('a option) -> ('a option)

   (*(** Not sure about this ... .
      @param first  null-form of type
      @param second sum function for type *)
   val merge2 : 'a -> ('a -> 'a -> 'a) -> ('a option * 'a option) -> 'a*)
end


(**
   Augmentation of stdlib Result module.
 *)
module Result_ :
sig
   (** <h2> map </h2> *)

   (** Unify a result with a default for Error.
      Conciser synonym for Result.value. *)
   val default  : 'o         -> ('o,'e) result -> 'o

   (** Unify a result with a defaulter function for Error. *)
   val valuef   : ('e -> 'o) -> ('o,'e) result -> 'o

   (** Unify a result with a defaulter function for Error.
      Synonym for valuef. *)
   val defaultf : ('e -> 'o) -> ('o,'e) result -> 'o

   (** Map both Ok and Error: (o , e) result -> ((fo o) , (fe e)) result. *)
   val map : (('a -> 'c) * ('b -> 'd)) -> ('a,'b) result -> ('c,'d) result

   (*(** Map (o,e) result -> ((fo o),(fe e)) result.
      (Type-inference implemtation seems to make default args not work.). *)
   val resMap_ : ?ok:('a -> 'c) -> ?er:('b -> 'd) -> ('a,'b) result ->
      ('c,'d) result*)

   (** Map Ok: (o , e) result -> ((fo o) , e) result.
      Synonym for Result.map. *)
   val okMap    : ('a -> 'b) -> ('a,'er) result -> ('b,'er) result

   (** Map Error: (o , e) result -> (o , (fe e)) result.
      Synonym for Result.map_error. *)
   val errorMap : ('a -> 'b) -> ('o,'a) result -> ('o,'b) result

   (** Diverge a result to a tuple of options (non-case -> None). *)
   val diverge : ('o,'e) result -> ('o option * 'e option)


   (** <h2> convert </h2> *)

   (** Convert result -> option (by discarding error).
      Synonym for Result.to_option. *)
   val toOpt : ('o,'e) result -> 'o option

   (** Transpose opt-res to res-opt.
   <code>
   res opt -> opt res
   ------------------
   ok some | some ok (value)
   ok none | none
   er -    | some er
   er -    | some er</code> *)
   val optresToResopt : ('o option, 'e) result -> (('o, 'e) result) option

   (** Convert result to value or raised exception.
      @raise Abstract the input exception *)
   val toExc_x : ('e -> exn) -> ('o,'e) result -> 'o

   (** Convert exception (by catching) to result (with Error of exn).
      (not caught: Out_of_memory, Stack_overflow, Sys.Break). *)
   val fromExc : (unit -> 'o) -> ('o , exn) result

   (** Convert exception (by catching) to result (with Error of input e).
      (not caught: Out_of_memory, Stack_overflow, Sys.Break). *)
   val fromExc2 : 'e -> (unit -> 'o) -> ('o,'e) result


   (** <h2> logical </h2> *)

   (** Or-merge two heterogeneous results.
      Error a and Error b -> Error (a ^ joiner ^ b),
      otherwise, Ok ( value|default , value|default ). *)
   val ressOr2 :
      string ->
      ('a * 'b) ->
      ((('a,string) result) * (('b,string) result)) ->
      ((('a * 'b) , string) result)

   (** And-merge two heterogeneous results.
      Ok a and Ok b -> Ok (a , b), otherwise Error:
   <code>
   res a res b -> res ab
   -----------------------
   Ok a , Ok b | Ok (a , b)
   Er a , Ok _ | Er a
   Ok _ , Er b | Er b
   Er a , Er b | Er (a ^ joiner ^ b)</code> *)
   val ressAnd2 :
      string ->
      ((('a,string) result) * (('b,string) result)) ->
      ((('a * 'b) , string) result)

   (** And-merge three heterogeneous results.
      Ok a and Ok b and Ok c -> Ok (a , b , c), otherwise Error
      (like ressAnd2 but for 3). *)
   val ressAnd3 :
      string ->
      ((('a,string) result) * (('b,string) result) * (('c,string) result)) ->
      ((('a * 'b * 'c) , string) result)
end


(**
   Augmentation of stdlib Int module.
 *)
module Int_ :
sig
   (** Number of decimal digits a number has. *)
   val digitsDec : int -> int

   (**
      Wrap-around modulo.

      E.g.:
      - usual mod: <code>[-3 -2 -1 0 1 2 3] mod  3 = [-0 -2 -1 0 1 2 0]</code>
      - this modw: <code>[-3 -2 -1 0 1 2 3] modw 3 = [ 0  1  2 0 1 2 0]</code>
    *)
   val modw : int -> int -> int
end


(**
   Augmentation of stdlib Char module.
 *)
module Char_ :
sig
   (** <h2> alpha </h2> *)

   (** Is a-z or A-Z. *)
   val isAlpha   : char -> bool

   (** Is a-z. *)
   val isLowercase : char -> bool

   (** Is A-Z. *)
   val isUppercase : char -> bool


   (** <h2> numeric </h2> *)

   (** Is 0-9. *)
   val isDigit   : char -> bool

   (** Is 0-7. *)
   val isDigitOct : char -> bool

   (** Is 0-9 or A-F. *)
   val isDigitHex : char -> bool

   (** Is - or +. *)
   val isSign    : char -> bool


   (** <h2> ascii </h2> *)

   (** Is char-code <= 127. *)
   val isAscii   : char -> bool

   (** Is any of: space \n \r \t \v \f. *)
   val isBlank   : char -> bool

   (** Is \n. *)
   val isNewline : char -> bool

   (** Is \r or \n. *)
   val isCrOrLf : char -> bool

   (** Is \x00-\x1F or \x7F. *)
   val isCtrl   : char -> bool
end


(**
   Augmentation of stdlib String module.
 *)
module String_ :
sig
   (** <h1> query </h1> *)

   (** Is string empty. *)
   val isEmpty  : string -> bool

   (** Is string not empty. *)
   val notEmpty : string -> bool

   (** Length - 1 (will be -1 if empty). *)
   val lastPos : string -> int

   (** Test first char (empty gives false). *)
   val isFirstChar : (char -> bool) -> string -> bool

   (** Like index_from, but return option instead of exception. *)
   val index : char -> ?start:int -> string -> int option

   (** Like index_from, but with predicate instead of char, and return option
      instead of exception. *)
   val indexp : (char -> bool) -> ?start:int -> string -> int option

   (** Like index_from, but return length instead of exception. *)
   val indexl : char -> ?start:int -> string -> int

   (** Like index_from, but with predicate instead of char, and return length
      instead of exception. *)
   val indexpl : (char -> bool) -> ?start:int -> string -> int

   (** Like rindex, but with predicate instead of char, and option instead of
      exception. *)
   val rindexp : (char -> bool) -> string -> int option

   (** Like contains, but with predicate instead of char. *)
   val containsp : (char -> bool) -> string -> bool

   (** Check predicate is true for all chars (like a kind of &&).
      (Empty string returns true.) *)
   val check : (char -> bool) -> string -> bool


   (** <h1> construct / modify / extract </h1> *)

   (** <h2> construct </h2> *)

   (** Concatenation of n copies of a string. *)
   val repeat : int -> string -> string


   (** <h2> modify </h2> *)

   (** Remove false-predicate chars -- filter out falses. *)
   val filter : (char -> bool) -> string -> string

   (** Remove non-Ascii (code > 127) chars. *)
   val filterAscii : string -> string

   (** Trim, and check if too long (byte length). *)
   val trimTrunc : (string * int) -> (string , string) result

   (** Truncate to max length (byte length). *)
   val truncate : int -> string -> string

   (** Like capitalize_ascii, but all words, not only first letter of string.
      (Uppercase all alpha chars that follow ascii non-alpha chars.) *)
   val capitaliseAll : string -> string


   (** <h2> extract </h2> *)

   (** Last char.
      @raise Invalid_argument "index out of bounds" on empty string *)
   val last_x : string -> char

   (** First n chars, with clamped n. *)
   val lead : int -> string -> string

   (** Rest of chars from pos, with clamped pos. *)
   val trail : int -> string -> string

   (** First n chars, and rest of chars from pos, with clamped pos. *)
   val leadTrail : int -> string -> (string * string)

   (** Like sub, but return option instead of exception. *)
   val subo : int -> int -> string -> string option

   (** Like sub, but clamped params. *)
   val subc : int -> int -> string -> string

   (** Like sub, but 2nd int is pos not len.
      @raise Invalid_argument for invalid positions *)
   val subp_x : int -> int -> string -> string

   (** Like sub, but 2nd int is pos not len, and clamped params. *)
   val subpc : int -> int -> string -> string

   (** Break into two fragments by finding first instance of char, also
      output position. *)
   val halvep : char -> string -> (string * string * int) option

   (** Break into two fragments by finding first instance of char. *)
   val halve : char -> string -> (string * string) option

   (** Break into fragments by char predicate, also output positions. *)
   val splitp : ?ls:((string * int) list) -> (char -> bool) -> string ->
      (string * int) list

   (** Break into fragments by char predicate. *)
   val split : (char -> bool) -> string -> string list


   (** <h1> convert </h1> *)

   (** Parse int, with some optional restrictions.
      (Optional bools express:
      allowed/forbidden/required for absent/false/true.) *)
   val toInt : ?zeroPadded:(bool * int) -> ?widthMaxed:int -> ?signed:bool ->
      string -> int option

   (** Convert by defaulting string option -> empty string. *)
   val ofOpt : string option -> string

   (** Convert non-empty/empty string -> option. *)
   val toOpt : string -> string option
end


(**
   Augmentation of stdlib List module.
 *)
module List_ :
sig
   (** <h1> query </h1> *)

   (** Is list empty. *)
   val isEmpty  : 'a list -> bool

   (** Is list not empty. *)
   val notEmpty : 'a list -> bool

   (** Like List.find, but with option instead of exception.
      (synomym for List.find_opt) *)
   val find : ('a -> bool) -> 'a list -> 'a option

   (** Some kind of find (synomym for List.find_map). *)
   val findmap : ('a -> 'b option) -> 'a list -> 'b option


   (** <h1> construct / extract / modify </h1> *)

   (** <h2> construct </h2> *)

   (** Make a list by calling a function for each index, with explicit length.
      Length is specified up-front as a parameter. *)
   val unfoldl : (int -> 'a) -> int -> 'a list

   (** Make a list by calling a function for each index, with implicit length.
      Length is extended until generator function returns None. *)
   val unfoldo : (int -> 'a option) -> 'a list


   (** <h2> extract </h2> *)

   (** Like hd, but with option instead of exception. *)
   val hd    : 'a list -> 'a option

   (** Like hd, but with option instead of exception (synonym for hd). *)
   val first : 'a list -> 'a option

   (** Last element (foot) - opposite end to head. *)
   val ft    : 'a list -> 'a option

   (** Last element (foot) - opposite end to head (synonym for ft). *)
   val last  : 'a list -> 'a option

   (** Like nth, but with option instead of exception.
      (Synonym for List.nth_opt, but with swapped params.) *)
   val nth : int -> 'a list -> 'a option

   (** Like tl, but return empty list if list is empty. *)
   val tlSafe : 'a list -> 'a list

   (** Get first and last only, else single item, else empty. *)
   val hdft : 'a list -> 'a list

   (** Break into two: first n and last (len - n) elements.
      (Out-of-range bisect point is clamped.) *)
   val bisect : int -> 'a list -> ('a list * 'a list)

   (** First n elements, with clamped n. *)
   val lead   : int -> 'a list -> 'a list

   (** Rest of elements from pos, with clamped pos. *)
   val trail  : int -> 'a list -> 'a list

   (** Gather options with an 'and': yield Some when all Some, else None.
      (A 'transpose' (by &&): list of options -> option of list.).
      (Empty list yields Some.). *)
   val optAnd : ('a option) list -> ('a list) option

   (** Gather options with an 'or': yield Some when >=1 Some, else None.
      (A 'transpose' (by ||): list of options -> option of list.).
      (Empty list yields None.). *)
   val optOr : ('a option) list -> ('a list) option

   (** Gather results with an 'and': yield Ok when all Ok, else Error.
      (A 'transpose' (by &&): result list -> list result.).
      (Empty list yields Ok.). *)
   val resAnd : (('ok,'er) result) list -> ('ok list , 'er list) result


   (** <h2> modify </h2> *)

   (** Filter to a different type -- by predicate returning option.
      (Synonym for List.filter_map.) *)
   val filtmap : ('a -> 'b option) -> 'a list -> 'b list

   (** Partition to different types -- by predicate returning either.
      (Synonym for List.partition_map.) *)
   val partmap : ('a -> ('b, 'c) Either.t) -> 'a list -> ('b list * 'c list)

   (** Remove duplicate elements. *)
   val deduplicate : 'a list -> 'a list

   (** Equalize the length of two lists, by truncating the longer one. *)
   val equalenTruncate : 'a list -> 'b list -> ('a list * 'b list)

   (** Equalize the length of two lists, by extending the shorter one with
      default values. *)
   val equalenExtend : 'a -> 'b -> 'a list -> 'b list -> ('a list * 'b list)


   (** <h1> convert </h1> *)

   (** Convert string (of bytes) -> list-of-chars. *)
   val ofStringAscii : string -> char list

   (** Convert list-of-chars -> string (of bytes). *)
   val toStringAscii : (char list) -> string

   (** Convert option -> singletonian/empty list. *)
   val ofOpt : 'a option -> 'a list

   (** Convert singletonian/empty list -> option. *)
   val toOpt : 'a list -> 'a option
end


(**
   Augmentation of stdlib Array module.
 *)
module Array_ :
sig
   (** <h1> query </h1> *)

   (** Is array empty. *)
   val isEmpty : 'a array -> bool


   (** <h1> construct / modify / extract </h1> *)

   (** First n elements, with clamped n. *)
   val lead      : int -> 'a array -> 'a array

   (** Rest of elements from pos, with clamped pos. *)
   val trail     : int -> 'a array -> 'a array

   (** First n elements, and rest of elements from pos, with clamped pos. *)
   val leadTrail : int -> 'a array -> ('a array * 'a array)

   (** Break into two: first n and last (len - n) elements.
      (Out-of-range bisect point is clamped.) *)
   val bisect  : int -> 'a array -> ('a array * 'a array)

   (** Break into two: first n and last (len - n) elements, or None. *)
   val bisecto : int -> 'a array -> ('a array * 'a array) option

   (** Filter into two parts (like List.partition) (order preserving). *)
   val partition : ('a -> bool) -> 'a array -> ('a array * 'a array)


   (** <h1> convert </h1> *)

   (** Convert option -> singletonian/empty array. *)
   val ofOpt : 'a option -> 'a array

   (** Convert singletonian/empty array -> option. *)
   val toOpt : 'a array -> 'a option

   (** For use (curried) with _printf %t .
      @raise Unknown whatever can be raised by Printf._printf *)
   val printc_x : ('a -> out_channel -> unit) -> 'a array -> out_channel
      -> unit

   (* (** For use (curried) with ksprintf %t .
      @raise Unknown whatever can be raised by Printf.sprintf *)
   val printks_x : ('a -> unit -> string) -> 'a array -> unit -> string *)
end


(**
   Augmentation of stdlib Scanf module.
 *)
module Scanf_ :
sig
   (** Catches the various Scanf exceptions, and re-raises them all as
      Scanf.Scan_failure. *)
   val scanExnUnify_x : (unit -> 'a) -> 'a

   (** For calling Scanf.kscanf, converts the various Scanf exceptions into
      Results. *)
   val kscanfErrFn : Scanf.Scanning.scanbuf -> exn -> ('o , string) result

   (** Skips blanks in a Scanf.bscanf call, and ignores exceptions. *)
   val skipBlank : Scanf.Scanning.scanbuf -> unit
end




(* -------------------------------------------------------------------------- *)
(** <h1> other modules </h1> *)

(**
   Functionality for handling blank chars in strings.
 *)
module Blanks :
sig
   (** Replace \n, \r, \t, \v, \f with space. *)
   val blankSpacyCtrlChars : string -> string

   (** Replace \n, \r with space. *)
   val blankNewlines : string -> string

   (**
      Replace all UTF-8 blank chars with spaces.

      - 09       -- U+0009 -- HT horizontal tab
      - 0B       -- U+000B -- VT vertical tab
      - 0C       -- U+000C -- FF form feed
      - 20       -- U+0020 -- SP space
      - C2 85    -- U+0085 -- NEL next line
      - C2 A0    -- U+00A0 -- no-break space
      - E1 9A 80 -- U+1680 -- ogham space mark
      - E1 A0 8E -- U+180E -- mongolian vowel separator
      - E2 80 80 -- U+2000 -- en quad
      - E2 80 81 -- U+2001 -- em quad
      - E2 80 82 -- U+2002 -- en space
      - E2 80 83 -- U+2003 -- em space
      - E2 80 84 -- U+2004 -- three-per-em space
      - E2 80 85 -- U+2005 -- four-per-em space
      - E2 80 86 -- U+2006 -- six-per-em space
      - E2 80 87 -- U+2007 -- figure space
      - E2 80 88 -- U+2008 -- punctuation space
      - E2 80 89 -- U+2009 -- thin space
      - E2 80 8A -- U+200A -- hair space
      - E2 80 8B -- U+200B -- zero-width space
      - E2 80 8C -- U+200C -- zero-width non-joiner
      - E2 80 8D -- U+200D -- zero-width joiner
      - E2 80 A8 -- U+2028 -- line separator
      - E2 80 A9 -- U+2029 -- paragraph separator
      - E2 80 AF -- U+202F -- narrow no-break space
      - E2 81 9F -- U+205F -- medium mathematical space
      - E2 81 A0 -- U+2060 -- word joiner
      - E3 80 80 -- U+3000 -- ideographic space
      - EF BB BF -- U+FEFF -- zero width no-break space
    *)
   val unifyNonNewlines : string -> string

   (** Like unifyNonNewlines, but including \n and \r too. *)
   val unifySpaces : string -> string

   (** Replace each run of spaces with a single space char. *)
   val squashSpaces : string -> string
end


(**
   Functionality for manipulating file names.
 *)
module FileName :
sig
   (** <h2> name . ext </h2> *)

   (** Return "name" and ".ext". *)
   val splitExt : string -> (string * string)

   (** Return "name" but not ".ext". *)
   val getMain : string -> string

   (** Return ".ext" but not "name". *)
   val getExt : string -> string


   (** <h2> path / name </h2> *)

   (** Return "dir/path/" and "filename". *)
   val splitPath : string -> (string * string)

   (** Return "dir/path/" but not "filename". *)
   val getPath : string -> string

   (** Return "filename" but not "dir/path/". *)
   val getName : string -> string
end


(**
   Wrapper for std Str module regex functionality.
 *)
module Rx :
sig
   (** <h2> types </h2> *)

   (** The regular expression. *)
   type rx

   (** The matched text. *)
   type rxmatch


   (** <h2> prepare </h2> *)

   (** Prepare a regex. *)
   val compile : ?caseInsens:bool -> string -> rx


   (** <h2> apply </h2> *)

   (** Check if START of string matches regex.
      @param first  compiled regex
      @param second position
      @param third  string to inspect *)
   val apply : rx -> ?pos:int -> string -> rxmatch option

   (** Compile and apply regex, at START of string only.
      @param first  regex string
      @param second position
      @param third  case insensitivity flag
      @param fourth string to inspect *)
   val regexApply : string -> ?pos:int -> ?caseInsens:bool -> string ->
      rxmatch option

   (** Find first substring in string that matches regex.
      @param first  compiled regex
      @param second position
      @param third  string to inspect *)
   val seek : rx -> ?pos:int -> string -> rxmatch option

   (** Find first substring in string that matches regex.
      @param first  regex string
      @param second position
      @param third  case insensitivity flag
      @param fourth string to inspect *)
   val regexSeek : string -> ?pos:int -> ?caseInsens:bool -> string ->
      rxmatch option

   (** Find all substrings in string that match regex.
      (But of overlapping matches, only the first is found --
      search continues after the *end* of each match.)
      @param first  compiled regex
      @param second string to inspect *)
   val allMatches : rx -> string -> string list

   (** Find all substrings in string that match regex, with their positions.
      (But of overlapping matches, only the first is found --
      search continues after the *end* of each match.)
      @param first  compiled regex
      @param second string to inspect *)
   val allMatchesPos : rx -> string -> (string * int) list


   (** <h2> extract </h2> *)

   (** Get whole of a match. *)
   val wholeFound : rxmatch -> string

   (** Get position bounds (first, last+1) of whole of a match. *)
   val wholePos   : rxmatch -> (int * int)

   (** Get a group (index starts at 1) from a match. *)
   val groupFound : int -> rxmatch -> string option
end




(* -------------------------------------------------------------------------- *)
(** <h1> functions </h1> *)


(** <h2> error-handling pipeliner/monad </h2> *)

(** <h3> sequential </h3> *)

(** Option-handling pipeliner (option monad 'bind').
   Pipeline form. *)
val ( |>- )    : 'a option -> ('a -> 'b option) -> 'b option

(** Option-handling pipeliner (option monad 'bind').
   Scope-stack form. *)
val ( let|>- ) : 'a option -> ('a -> 'b option) -> 'b option

(** Result-handling pipeliner (result monad 'bind').
   Pipeline form. *)
val ( |>= )    : ('a,'e) result -> ('a -> ('b,'e) result) -> ('b,'e) result

(** Result-handling pipeliner (result monad 'bind').
   Scope-stack form. *)
val ( let|>= ) : ('a,'e) result -> ('a -> ('b,'e) result) -> ('b,'e) result

(* (** let|>= alternative, by tuple pass-through instead of scope stack.

   Example use:

   <code>
   (* read three vectors, in order *)
   (vRead inBuffer)
   |>>= (fun _ -> vRead inBuffer)
   |>>= (fun _ -> vRead inBuffer)
   (* read two light values, in order *)
   |>>= (fun _ -> Light.iRead inBuffer)
   |>>= (fun _ -> Light.iRead inBuffer)

   (* assemble datastruct *)
   |>- (fun ((((v0 , v1) , v2) , reflectivity) , emitivity) ->
      {  vertexs      = { v0 ;
                          v1 ;
                          v2 ; } ;
         reflectivity = Light.iClamp01  reflectivity ;
         emitivity    = Light.iClampPos emitivity ;  })</code> *)
val ( |>>= ) : ('a,'e) result -> ('a -> ('b,'e) result) ->
   (('a * 'b) , 'e) result *)

(** <h3> parallel </h3> *)

(* (** Result-handling pipeliner, parallel and-merge homogeneous list. *)
val ( |^= ) : ('o1,'e) result -> (('o1 -> ('o2,'e) result) list) ->
   ('o2 list,'e list) result *)

(** Option-handling pipeliner: parallel and-merge two heterogeneous.
   Some (f x) and Some (g x) -> Some ((f x) , (g x)), otherwise None.
   @param first  function to apply to value
   @param second function to apply to value
   @param third  input value *)
val optAnd2p :
   ('x -> 'a option) ->
   ('x -> 'b option) ->
   'x ->
   ('a * 'b) option

(** Option-handling pipeliner: parallel and-merge two heterogeneous.
   Infix operator version of optAnd2p.
   @param first  (left) input value
   @param second (right) pair of functions to apply to value *)
val ( |^^- ) :
   'x option ->
   (  ('x -> 'a option) *
      ('x -> 'b option) ) ->
   ('a * 'b) option

(** Result-handling pipeliner: parallel or-merge two heterogeneous.
   Error (f x) and Error (g x) -> Error ((f x) ^ joiner ^ (g x)), otherwise,
   Ok ( value|default , value|default ).
   @param first  error message joiner
   @param second defaults
   @param third  function to apply to value
   @param fourth function to apply to value
   @param fifth  input value *)
val ressOr2p :
   string ->
   ('a * 'b) ->
   ('x -> ('a,string) result) ->
   ('x -> ('b,string) result) ->
   'x ->
   ((('a * 'b) , string) result)

(** Result-handling pipeliner: parallel and-merge two heterogeneous.
   Ok (f x) and Ok (g x) -> Ok ((f x) , (g x)), otherwise Error:
   <code>
   res a res b -> res ab
   -----------------------
   Ok a , Ok b | Ok (a , b)
   Er a , Ok _ | Er a
   Ok _ , Er b | Er b
   Er a , Er b | Er (a ^ joiner ^ b)</code>
   @param first  error message joiner
   @param second function to apply to value
   @param third  function to apply to value
   @param fourth input value *)
val ressAnd2p :
   string ->
   ('x -> ('a,string) result) ->
   ('x -> ('b,string) result) ->
   'x ->
   ((('a * 'b) , string) result)

(** Result-handling pipeliner: parallel and-merge two heterogeneous.
   Infix operator version of ressAnd2p.
   @param first  (left) input value
   @param second (right) pair of functions to apply to value *)
val ( |^^= ) :
   ('x,string) result ->
   (  ('x -> ('a,string) result) *
      ('x -> ('b,string) result) ) ->
   (('a * 'b) , string) result

(** Result-handling pipeliner: parallel and-merge three heterogeneous.
   Ok (f x) and Ok (g x) and Ok (h x) -> Ok ((f x) , (g x) , (h x)), otherwise
   Error (like ressAnd2p but for 3). *)
val ressAnd3p :
   string ->
   ('x -> ('a,string) result) ->
   ('x -> ('b,string) result) ->
   ('x -> ('c,string) result) ->
   'x ->
   ((('a * 'b * 'c) , string) result)

(** Result-handling pipeliner: parallel and-merge three heterogeneous.
   Infix operator version of ressAnd3p.
   @param first  (left) input value
   @param second (right) triple of functions to apply to value *)
val ( |^^^= ) :
   ('x,string) result ->
   (  ('x -> ('a,string) result) *
      ('x -> ('b,string) result) *
      ('x -> ('c,string) result) ) ->
   (('a * 'b * 'c) , string) result

(** <h3> other </h3> *)

(* val ( and|&= ) :
   ('a,'e) result ->
   ('b,'e) result ->
   (('a,'e) * ('b,'e)) result *)

(** Result-handling pipeliner, else-if structure.
   Like |>=, but instead of mapping Ok, maps Error (value, but not type). *)
val ( |>=? ) : ('o,'e) result -> ('e -> ('o,'e) result) -> ('o,'e) result

(** Result-handling pipeliner, no-error input form. *)
val ( |>=+ ) : 'a -> ('a -> ('b,'e) result) -> ('b,'e) result

(** Result-handling pipeliner, no-error output form. *)
val ( |>=- ) : ('a,'e) result -> ('a -> 'b) -> ('b,'e) result

(** Result-handling pipeliner compose (result monad 'compose'). *)
val ( |>=> ) :
   ('a -> ('b,'e) result) ->
   ('b -> ('c,'e) result) ->
   ('a,'e) result ->
   ('c,'e) result

(** Impurity pipeliner: insert a side-effect, and pass through a value. *)
val bypass : ('a -> unit) -> 'a -> 'a


(** <h3> logical </h3> *)

(** Option-or: returns first option if some, else second option. *)
val ( ||> ) : 'a option -> (unit -> 'a option) -> 'a option

(** Option-and: returns second option if first some, else none. *)
val ( &&> ) : 'a option -> (unit -> 'a option) -> 'a option



(** <h2> file read/write </h2> *)

(**
   Open and close a file (from name) around the given callback.

   @param first  filePathname
   @param second opener
   @param third  closer
   @param fourth action
 *)
val useFile : string -> (string -> 'c) -> ('c -> unit) -> ('c -> 'i) ->
   ('i , exn) result

(** Read a file (from name) into a string. *)
val fileRead  : filePathname:string ->
   (string , exn) result

(** Write a file (from name) from a string. *)
val fileWrite : filePathname:string -> stuffToWrite:string ->
   (unit , exn) result

(**
   Read an in_channel into a string.

   @param first  in_channel
   @param second expected length
 *)
val inputString : in_channel -> int -> string



(** <h2> command-line invoke </h2> *)

(**
   Protect special chars in a pathname.

   A two part recipe:
   - replace each ' with '\'' (exit quote, escape ', enter quote)
   - surround all with ' like so: '...'.
 *)
val quoteShellPathname : string -> string

(**
   Run the command (on the shell), and return its results.

   @param first  command and options etc
   @param second environment (item eg: "LANG=en_GB.UTF-8")
   @param third  string to feed to std input
   @param fourth expected length of output
   @return std output, std error, and return code from the invocation; or
           signal and error code
 *)
val commandLineInvoke2 : string -> string list -> string -> int ->
   ((string * string * int) , (Unix.process_status * Unix.error)) result

(**
   Run the command (on the shell), and return its results (with simplified
   errors).

   @param first  command and options etc
   @param second environment (item eg: "LANG=en_GB.UTF-8")
   @param third  string to feed to std input
   @param fourth expected length of output
   @return std output or error message
 *)
val commandLineInvoke : string -> string list -> string -> int ->
   (string , string) result
