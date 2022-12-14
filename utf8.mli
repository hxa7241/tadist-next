(*------------------------------------------------------------------------------

   UTF-8 lib (OCaml 4.12)
   Harrison Ainsworth / HXA7241 : 2015, 2021

   http://www.hxa.name/tools/

   License: AGPL -- https://www.gnu.org/licenses/agpl-3.0.html

------------------------------------------------------------------------------*)




(* --- values --- *)

val _REPLACEMENT_CHAR_UTF8 : string




(* --- modules --- *)

module Codec :
sig

   (* --- functions --- *)

   (**
    * Translates codepoint to UTF-8.
    * According to: RFC-3629 -- http://tools.ietf.org/html/rfc3629
    * returns empty string for an invalid codepoint
    * (valid codepoints: 0x0000..0xD7FF and 0xE000..0x10FFFF)
    *)
   val ofCode : int -> string

   (** Translates UTF-8 byte group into a codepoint. *)
   (*val toCode : string -> Uchar.t*)

   (**
    * Translates any '\uXXXX' escaped UTF-16 codes into UTF-8.
    * @param  false: leave invalids untranslated; true: replacement-char them
    * @param  string containing escapes
    * @return string translated to UTF-8
    *)
   val ofU16Esc : bool -> string -> string

end




module Filter :
sig

   (* --- types --- *)

   type charResult = Char of string | Bad of string | EOF of string


   (* --- functions --- *)

   (* primary / low-level *)

   (**
    * Read next valid UTF-8 char or invalid bytes from open file.
    *
    * Effectively partitions the file into valid and invalid parts: so
    * if you just output the Char and Bad strings again, interleaved
    * in order (and the EOF string at the end), you get exactly the
    * original file.
    *
    * According to: RFC-3629 -- http://tools.ietf.org/html/rfc3629
    * and: Unicode 7.0 -- http://www.unicode.org/versions/Unicode7.0.0/
    *
    * @param byte stream to read from
    * @return valid UTF-8 Char | Bad | EOF
    *)
   val readChar : (char Stream.t) -> charResult


   (* secondary, for streams *)

   (** Check if all valid. *)
   val checkStream   : (char Stream.t) -> bool

   (** Remove all invalid byte sequences. *)
   val filterStream  : (char Stream.t) -> (string->unit) -> unit

   (** Replace all invalid byte sequences with replacement chars. *)
   val replaceStream : (char Stream.t) -> (string->unit) -> unit


   (* secondary, for strings *)

   (** Check if all valid. *)
   val check   : string -> bool

   (** Remove all invalid byte sequences. *)
   val filter  : string -> string

   (** Replace all invalid byte sequences with replacement chars. *)
   val replace : string -> string

end




(* --- functions --- *)

val removeReplacementChars : string -> string




(* --- tests --- *)

(**
 * Test Codec.ofCode.
 *
 * @param  whether to print messages/stuff
 * @return true for success, false for failure
 *)
val test_Codec_ofCode : bool -> bool


(**
 * Test Codec.ofU16Esc.
 *
 * @param  whether to print messages/stuff
 * @return true for success, false for failure
 *)
val test_Codec_ofU16Esc : bool -> bool
