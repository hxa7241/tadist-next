(*------------------------------------------------------------------------------

   UTF-8 lib (OCaml 4.10)
   Harrison Ainsworth / HXA7241 : 2015, 2021

   http://www.hxa.name/tools/

   License: AGPL -- https://www.gnu.org/licenses/agpl-3.0.html

------------------------------------------------------------------------------*)




open HxaGeneral




(* --- values --- *)

let _REPLACEMENT_CHAR_UTF8 = "\xEF\xBF\xBD"




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

   (* Translates any '\uXXXX' escaped UTF-16 codes into UTF-8.
    * @param  false: leave invalids untranslated; true: replacement-char them
    * @param  string with escapes
    * @return string translated to UTF-8
    *)
   val ofU16Esc : bool -> string -> string

end
=
struct

   (* --- functions --- *)

   let ofCode (codeInt:int) : string =

      (*
      (0x0000..0xD7FF and 0xE000..0x10FFFF) -> UTF-8

      RFC-3629 -- http://tools.ietf.org/html/rfc3629

       Char. number range   |        UTF-8 octet sequence
          (hexadecimal)     |              (binary)
      ----------------------+-------------------------------------------
      0000 0000 - 0000 007F | 0xxxxxxx
      0000 0080 - 0000 07FF | 110xxxxx 10xxxxxx
      0000 0800 - 0000 FFFF | 1110xxxx 10xxxxxx 10xxxxxx
      0001 0000 - 0010 FFFF | 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx

      but not:  D800 - DFFF (UTF-16 surrogate pairs)
      *)

      (* prefix , mask , shift -- constants for each possible UTF-8 form *)
      let oneOctetsForm   =
         [  (0b00000000 , 0b000000000000001111111 ,  0) ]
      and twoOctetsForm   =
         [  (0b11000000 , 0b000000000011111000000 ,  6)
         ;  (0b10000000 , 0b000000000000000111111 ,  0) ]
      and threeOctetsForm =
         [  (0b11100000 , 0b000001111000000000000 , 12)
         ;  (0b10000000 , 0b000000000111111000000 ,  6)
         ;  (0b10000000 , 0b000000000000000111111 ,  0) ]
      and fourOctetsForm  =
         [  (0b11110000 , 0b111000000000000000000 , 18)
         ;  (0b10000000 , 0b000111111000000000000 , 12)
         ;  (0b10000000 , 0b000000000111111000000 ,  6)
         ;  (0b10000000 , 0b000000000000000111111 ,  0) ]
      in

      let dueOctetForm : (int * int * int) list =
         if      codeInt <       0x0 then [] (* too small *)
         else if codeInt <=   0x007F then oneOctetsForm
         else if codeInt <=   0x07FF then twoOctetsForm
         else if codeInt <=   0xD7FF then threeOctetsForm
         else if codeInt <=   0xDFFF then [] (* surrogate pair *)
         else if codeInt <=   0xFFFF then threeOctetsForm
         else if codeInt <= 0x10FFFF then fourOctetsForm
         else                             [] (* too large *)
      in
      (* chop code into List.len pieces, and convert each to a char *)
      let octets : string list =
         List.map
            (fun (prefix , mask , shift) : string ->
               let octet : int = ((codeInt land mask) lsr shift) lor prefix in
               String.make 1 (char_of_int octet))
            dueOctetForm
      in
      String.concat "" octets


   let ofU16Esc (replace:bool) (escs:string) : string =
      (* TODO *)
      ""

      (*
      general: [0-9A-F][0-9A-F][0-9A-F][0-9A-F]

      exhaustive, mutually exclusive:
         norm 1: [0-9A-C][0-9A-F][0-9A-F][0-9A-F]  \|
                 D[0-7][0-9A-F][0-9A-F]
                 [E-F][0-9A-F][0-9A-F][0-9A-F]
         surr p: D[8-9A-F][0-9A-F][0-9A-F]D[8-9A-F][0-9A-F][0-9A-F]

      | n | ss
      | sn | s

      rx match single or surrogate pair: u \| uu
      if pair is partial match, so invalid, and ultimately non-matching, then
      just leave it

      1:
      - input escs string
      - make labelled segmentation with/like Str.full_split
      - map labelled esc chunks to utf8 chunks
      *)

end




module Filter :
sig
   (* --- types --- *)

   type charResult = Char of string | Bad of string | EOF of string


   (* --- values --- *)

   val _REPLACEMENT_CHAR_UTF8 : string


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
=
struct
   (* --- types --- *)

   type charResult = Char of string | Bad of string | EOF of string

   type validness = Invalid | Valid
   type condition = Incomplete of validness | Complete of validness


   (* --- values --- *)

   (* standard Unicode 'replacement char' U+FFFD UTF-8:EFBFBD *)
   let _REPLACEMENT_CHAR_UTF8 = "\xEF\xBF\xBD"


   (* --- functions --- *)

   (* implementation *)

   let string_of_char (c:char) : string = String.make 1 c
   (*
   let string_of_byte (b:int)  : string = String.make 1 (char_of_int (b land 0xFF))
   *)


   (**
    * Somewhat like a function passed to a fold: it is called repeatedly on a
    * sequence, the params state and octets are ongoing state, and nexto is the
    * next element to process.
    *
    * Invalid sequences end at the next head-byte found -- which therefore must be
    * put back into the stream by the caller, so it can be read again as the start
    * of the next sequence.
    *
    * References:
    * * "Unicode Standard 7.0" ; TheUnicodeConsortium ; 2014 / ISBN-9781936213092 /
    *   book .
    *    * sect 3.9, D92: p124-p126
    * * "RFC 3629" ; TheInternetSociety ; 2003 / txt .
    *
    * Validity as expressed by the unicode standard:
    *
    * byte 0:  00 - 7F (0???????) | C2 - F4
    *    110????? && >= C2
    *    1110????
    *    11110??? && <= F4
    * byte 1:  80 - BF (10??????) unless:
    *    byte 0 = E0 : byte 1 = A0 - BF (101?????)
    *    byte 0 = ED : byte 1 = 80 - 9F (100?????)
    *    byte 0 = F0 : byte 1 = 90 - BF
    *    byte 0 = F4 : byte 1 = 80 - 8F (1000????)
    * byte 2:  80 - BF (10??????)
    * byte 3:  80 - BF (10??????)
    *
    * Validity as expressed by the rfc:
    *
    * UTF8-octets = *( UTF8-char )
    * UTF8-char   = UTF8-1 / UTF8-2 / UTF8-3 / UTF8-4
    * UTF8-1      = %x00-7F
    * UTF8-2      = %xC2-DF UTF8-tail
    * UTF8-3      = %xE0 %xA0-BF UTF8-tail / %xE1-EC 2( UTF8-tail ) /
    *               %xED %x80-9F UTF8-tail / %xEE-EF 2( UTF8-tail )
    * UTF8-4      = %xF0 %x90-BF 2( UTF8-tail ) / %xF1-F3 3( UTF8-tail ) /
    *               %xF4 %x80-8F 2( UTF8-tail )
    * UTF8-tail   = %x80-BF
    *)
   let classify (state:validness) (octets:string) (nexto:char) : condition =

      let index = String.length octets
      and nextb = int_of_char nexto
      in

      let validness =
         if (match state with
            (* previously invalid stays invalid *)
            | Invalid when index <> 0 -> false
            | Invalid | Valid ->
               (* is next octet valid ? *)
               begin match index with
               | 0 ->
                  (* head-byte (including ASCII) *)
                  (nexto <= '\x7F') || ((nexto >= '\xC2') && (nexto <= '\xF4'))
               | 1 ->
                  (* first tail-byte has extra constraints *)
                  begin match octets.[0] with
                  | '\xE0' -> (nextb land 0b11100000) = 0b10100000
                  | '\xED' -> (nextb land 0b11100000) = 0b10000000
                  | '\xF0' -> (nextb >= 0x90) && (nextb <= 0xBF)
                  | '\xF4' -> (nextb land 0b11110000) = 0b10000000
                  | _      -> (nextb land 0b11000000) = 0b10000000
                  end
               | 2
               | 3 ->
                  (* other, simple, tail-bytes *)
                  (nextb land 0b11000000) = 0b10000000
               | _ -> false
               end)
         then Valid else Invalid
      in

      let isComplete =
         match validness with
         | Valid ->
            (* length according to head octet has been reached *)
            begin match index with
            | 0 -> nexto      <= '\x7F'
            | 1 -> octets.[0] <  '\xE0'
            | 2 -> octets.[0] <  '\xF0'
            | _ -> true
            end
         | Invalid ->
            (* invalid sequences end at the next head-byte found
               (which must be put back by caller) *)
            (index > 0) && ((nextb land 0b11000000) <> 0b10000000)
      in

      if isComplete then Complete validness else Incomplete validness


   (* primary / low-level *)

   let readChar (inStream:char Stream.t) : charResult =

      (* accumulate bytes into a chunk *)
      let rec readBytes (state:validness) (bytes:Buffer.t) : charResult =
         (* peek at next byte *)
         match Stream.peek inStream with
         | None      -> EOF (Buffer.contents bytes)
         | Some next ->
            (* consume next byte, and add to chunk *)
            let accumulate () : unit =
               Stream.junk inStream ;
               Buffer.add_string bytes (string_of_char next)
            in
            match classify state (Buffer.contents bytes) next with
            | Incomplete state -> readBytes state (accumulate () ; bytes)
            | Complete Valid   -> Char (accumulate () ; Buffer.contents bytes)
            | Complete Invalid -> Bad (Buffer.contents bytes)
      in

      readBytes Invalid (Buffer.create 8)


   (* secondary, for streams *)

   let rec checkStream (input:char Stream.t) : bool =

      match readChar input with
      | Char _ -> checkStream input
      | Bad  _ -> false
      | EOF  s -> (String.length s) = 0


   let rec scanStream (replacement:string)
      (input:char Stream.t) (output:string->unit) : unit =

      match readChar input with
      | Char s -> (output s           ; scanStream replacement input output)
      | Bad  _ -> (output replacement ; scanStream replacement input output)
      | EOF  s -> output (if (String.length s) = 0 then "" else replacement)


   let filterStream : (char Stream.t) -> (string->unit) -> unit =
      scanStream ""


   let replaceStream : (char Stream.t) -> (string->unit) -> unit =
      scanStream _REPLACEMENT_CHAR_UTF8


   (* secondary, for strings *)

   let check (s:string) : bool = checkStream (Stream.of_string s)


   let scanString (replacement:string) (s:string) : string =

      let buf = Buffer.create (String.length s) in
      scanStream replacement (Stream.of_string s) (Buffer.add_string buf) ;
      Buffer.contents buf


   let filter : string -> string  = scanString ""


   let replace : string -> string = scanString _REPLACEMENT_CHAR_UTF8

end
