(*------------------------------------------------------------------------------

   HXA General lib (OCaml 4.02)
   Harrison Ainsworth / HXA7241 : 2015

   http://www.hxa.name/tools/

   License: CC0 -- http://creativecommons.org/publicdomain/zero/1.0/

------------------------------------------------------------------------------*)




(* ---- types ---- *)

(** Like option, but instead of None has a string for messages. *)
type 'a eoption = Ok of 'a | Err of string




(* ---- functions ---- *)

(** Print message and exit 1. *)
val fail : string -> 'a

(** Return "dir/path/" and "filename". *)
val splitFilePathName : string -> (string * string)

(** Return "dir/path/" but not "filename". *)
val getFilePath : string -> string

(** Return "filename" but not "dir/path/". *)
val getFileName : string -> string

(** Return the inverted predicate. *)
val fNot : ('a -> bool) -> ('a -> bool)

(** The identity function. *)
val id : 'a -> 'a

(** Convert eoption to option (by discarding error message). *)
val eopToOpt : 'a eoption -> 'a option

(** Convert option to eoption (by replacing None with error message). *)
val optToEop : 'a option -> string -> 'a eoption

(** Map Some a -> Some (f a). *)
val mapOpt : ('a -> 'b) -> 'a option -> 'b option

(** Map Err a -> Err (f a). *)
val mapErr : (string -> string) -> 'a eoption -> 'a eoption

(** Compose functions (be aware of value restriction). *)
val ( % ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

(** Augmented or: short-circuiting 'or' that returns more than a bool. *)
val ( ||> ) : 'b option -> (unit -> 'b option) -> 'b option
(*val ( ||> ) : 'a option -> (unit -> 'b option) -> 'b option*)

(** Augmented and: short-circuiting 'and' that returns more than a bool. *)
val ( &&> ) : 'b option -> (unit -> 'b option) -> 'b option
(*val ( &&> ) : 'a option -> (unit -> 'b option) -> 'b option*)

(** Error-handling pipeline. *)
val ( |>= ) : 'a eoption -> ('a -> 'b eoption) -> 'b eoption

(** Error-handling pipeline, merge double-parallel. *)
val ( |^^= ) : 'a eoption -> (('a -> 'b eoption) * ('a -> 'c eoption))
   -> ('b * 'c) eoption

(** Error-handling pipeline, merge triple-parallel. *)
val ( |^^^= ) : 'a eoption ->
   (('a -> 'b eoption) * ('a -> 'c eoption) * ('a -> 'd eoption))
   -> ('b * 'c * 'd) eoption

(** Error-handling pipeline, no-error input form. *)
val ( |>+ ) : 'a -> ('a -> 'b eoption) -> 'b eoption

(** Error-handling pipeline, no-error output form. *)
val ( |>- ) : 'a eoption -> ('a -> 'b) -> 'b eoption

(** Convert a char to a string. *)
val string_of_char : char -> string

(** Convert a char-code (clamped to 0-FF) to a string. *)
val string_of_byte : int -> string

(**
 * Replace all UTF-8 blank chars with spaces.
 *
 * [ ["0x09" ],                 "U+0009  HT horizontal tab"         ],
 * [ ["0x0A" ],                 "U+000A  LF line feed"              ],
 * [ ["0x0B" ],                 "U+000B  VT vertical tab"           ],
 * [ ["0x0C" ],                 "U+000C  FF form feed"              ],
 * [ ["0x0D" ],                 "U+000D  CR carriage return"        ],
 * [ ["0x20" ],                 "U+0020  SP space"                  ],
 * [ ["0xC2", "0x85" ],         "U+0085  NEL next line"             ],
 * [ ["0xC2", "0xA0" ],         "U+00A0  no-break space"            ],
 * [ ["0xE1", "0x9A", "0x80"],  "U+1680  ogham space mark"          ],
 * [ ["0xE1", "0xA0", "0x8E"],  "U+180E  mongolian vowel separator" ],
 * [ ["0xE2", "0x80", "0x80"],  "U+2000  en quad"                   ],
 * [ ["0xE2", "0x80", "0x81"],  "U+2001  em quad"                   ],
 * [ ["0xE2", "0x80", "0x82"],  "U+2002  en space"                  ],
 * [ ["0xE2", "0x80", "0x83"],  "U+2003  em space"                  ],
 * [ ["0xE2", "0x80", "0x84"],  "U+2004  three-per-em space"        ],
 * [ ["0xE2", "0x80", "0x85"],  "U+2005  four-per-em space"         ],
 * [ ["0xE2", "0x80", "0x86"],  "U+2006  six-per-em space"          ],
 * [ ["0xE2", "0x80", "0x87"],  "U+2007  figure space"              ],
 * [ ["0xE2", "0x80", "0x88"],  "U+2008  punctuation space"         ],
 * [ ["0xE2", "0x80", "0x89"],  "U+2009  thin space"                ],
 * [ ["0xE2", "0x80", "0x8A"],  "U+200A  hair space"                ],
 * [ ["0xE2", "0x80", "0x8B"],  "U+200B  zero-width space"          ],
 * [ ["0xE2", "0x80", "0x8C"],  "U+200C  zero-width non-joiner"     ],
 * [ ["0xE2", "0x80", "0x8D"],  "U+200D  zero-width joiner"         ],
 * [ ["0xE2", "0x80", "0xA8"],  "U+2028  line separator"            ],
 * [ ["0xE2", "0x80", "0xA9"],  "U+2029  paragraph separator"       ],
 * [ ["0xE2", "0x80", "0xAF"],  "U+202F  narrow no-break space"     ],
 * [ ["0xE2", "0x81", "0x9F"],  "U+205F  medium mathematical space" ],
 * [ ["0xE2", "0x81", "0xA0"],  "U+2060  word joiner"               ],
 * [ ["0xE3", "0x80", "0x80"],  "U+3000  ideographic space"         ],
 * [ ["0xEF", "0xBB", "0xBF"]   "U+FEFF  zero width no-break space" ]
 *)
val unifySpaces : string -> string

(** Replace \n, \r, \t, \v, \f with space. *)
val blankSpacyCtrlChars : string -> string

(** Replace \n, \r with space. *)
val blankNewlines : string -> string




(* ---- std lib module augmentations ---- *)

module Char :
sig
   include module type of Char

   (** Is 0-9 ?. *)
   val isDigit : char -> bool
end


module String :
sig
   include module type of String

   (** Is string empty?. *)
   val isEmpty : string -> bool

   (** Like index, but with option instead of exception. *)
   val index_o : char -> string -> int option

   (** Like index, but return length instead of exception. *)
   val indexl : char -> string -> int

   (** Remove false-predicate chars. *)
   val filter : (char -> bool) -> string -> string

   (** Remove non-Ascii (> 127) chars. *)
   val filterAscii : string -> string

   (** Check predicate is true for all chars (like a kind of &&). *)
   val check : (char -> bool) -> string -> bool

   (** Split by char. *)
   val split : ?ls:(string list) -> char -> string -> string list

   (** Trim, and check if too long (bytewise length). *)
   val trimTrunc : (string * int) -> string eoption

   (** Truncate to max length. *)
   val truncate : int -> string -> string
end


module List :
sig
   include module type of List

   (** Like nth, but with option instead of exception. *)
   val nth_o : int -> 'a list -> 'a option

   (** Like find, but with option instead of exception. *)
   val find_o : ('a -> bool) -> 'a list -> 'a option

   (** Filter and change type -- by predicate returning option. *)
   val filtmap : ('a -> 'b option) -> 'a list -> 'b list

   (*val findmap_o : ('a -> 'b option) -> 'a list -> 'b option*)

   (*val unfold : ?list:('a list) -> (int->'a) -> int -> 'a list*)

   (*val ofStringAscii : ?lc:(char list) -> string -> char list*)
end
