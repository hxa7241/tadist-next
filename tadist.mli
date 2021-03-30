(*------------------------------------------------------------------------------

   TADIST tool (OCaml 4.10)
   Harrison Ainsworth / HXA7241 : 2015, 2020

   http://www.hxa.name/tools/
   http://www.hxa.name/notes/note-hxa7241-20141231T1101Z.html

   License: AGPL -- https://www.gnu.org/licenses/agpl-3.0.html

------------------------------------------------------------------------------*)




(* using HxaGeneral *)




(*

Original document:
   "TADIST file naming pattern" ; HXA7241 ; 2014-12-31 / html .
   http://www.hxa.name/notes/note-hxa7241-20141231T1101Z.html



Filename Grammar
----------------

   (filename (, plain meta))

   (plain   (, title (? (, "_" author (? (, "_" date))))))
   (title   (+ alphnum "-"))
   (author  (+ alphnum "-"))
   (date    (+ (, year (? (, month (? day)))) "-"))

   (meta    (, (? (, "." id)) (? (, "." subtype)) "." type))
   (id      (, alphnum "-" alphnum))
   (subtype alphnum)
   (type    alphnum)

   (year    (, (? "-")
               (| (=4 digit)
                  (, (=3 digit) "X")
                  (, (=2 digit) "XX")
                  (, digit "XXX"))))
   (month   (| "01" "02" "03" "04" "05" "06"
               "07" "08" "09" "10" "11" "12"))
   (day     (| (, "0" (| "1" "2" "3" "4" "5" "6" "7" "8" "9"))
               (, (| "1" "2") digit)
               (, "3" (| "0" "1"))))
   (alphnum (+ (| letter digit)))

   (digit   (| "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

Assuming:
* letter -- at least ASCII

Comments:
* title   -- a string of words
* author  -- an array of names
* date    -- an IS0-8601 date in compact (no-space) form
* id      -- following a label-value convention, e.g.: ISBN-1852338210
* subtype -- understandable from the type, e.g.: 100x60.png, c320.mp3
* type    -- from a commonly known vocabulary, e.g.: jpg, epub, mp3
* year    -- does use leading '-'; for BCE, and has a non-standard augmentation: 'X'; for unknown



Text-Form Grammar
-----------------

   (textform (, plain meta " ."))

   (plain   (, title (? (, " ; " author (? (, " ; " date))))))
   (title   (, "\"" chars-no-dquos "\""))
   (author  (+ chars-no-commas-or-semicolons-or-slashes ", "))
   (date    (+ (, year (? (, "-" month (? "-" day)))) ", "))

   (meta    (, (? (, " / " id)) (? (, " / " subtype)) " / " type))
   (id      (, alphnum "-" alphnum))
   (subtype alphnum)
   (type    alphnum)

   (year    (, (? "-")
               (| (=4 digit)
                  (, (=3 digit) "X")
                  (, (=2 digit) "XX")
                  (, digit "XXX"))))
   (month   (| "01" "02" "03" "04" "05" "06"
               "07" "08" "09" "10" "11" "12"))
   (day     (| (, "0" (| "1" "2" "3" "4" "5" "6" "7" "8" "9"))
               (, (| "1" "2") digit)
               (, "3" (| "0" "1"))))
   (alphnum (+ (| letter digit)))

   (digit   (| "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

Assuming:
* chars-no-dquos
* chars-no-commas-or-semicolons-or-slashes
* letter -- at least ASCII

Comments:
* title   -- a string of words
* author  -- an array of names
* date    -- an IS0-8601 date
* id      -- following a label-value convention, e.g.: ISBN-1852338210
* subtype -- understandable from the type, e.g.: 100x60 / png, c320 / mp3
* type    -- from a commonly known vocabulary, e.g.: jpg, epub, mp3
* year    -- does use leading '-'; for BCE, and has a non-standard augmentation: 'X'; for unknown

*)




(* ---- modules ---- *)

(**
 * A constrained string: only TADIST-allowed chars, non-zero length.
 *)
module StringT :
sig
   type t

   val isAllowedChar : char -> bool

   (** Remove invalid chars from string. *)
   val filter : string -> string

   (** Make from string, if valid. *)
   val make : string -> t HxaGeneral.ress

   (** Make from string, by discarding invalid chars. *)
   val makef : string -> t HxaGeneral.ress

   (** Get string from. *)
   val toString : t -> string

   (** Std comparison *)
   val compare : t -> t -> int
end


(**
 * A constrained Array: non-zero length.
 *)
module ArrayNe :
sig
   type 'a t

   (** Make from array, if valid. *)
   val make : 'a array -> 'a t HxaGeneral.ress

   (** Get array from. *)
   val toArray : 'a t -> 'a array
end


(**
 * Date ISO8601, with extended features.
 *
 * year[-month[-day]]
 * [-]00XX[[-]00[[-]00]]
 * * optional leading hyphen
 * * optional separator hyphens -- all or none
 * * year can have least-significant 'X's -- one or more digits, then all
 *   digits or all 'X'
 *)
module DateIso8601e :
sig
   type t

   (** Make date from string of iso8601, if valid. *)
   val make : string -> t HxaGeneral.ress

   (** Get string of iso8601 from date. *)
   val toString : bool -> t -> string

   (** Reduce to just the year. *)
   val yearOnly : t -> t

   (** As for standard compare. *)
   val compare : t -> t -> int
end


(**
 * A 13 digit or 10 digit/X string -- the basic syntax only.
 *)
module Isbn :
sig
   type t

   (** Induct bare digits, or with "ISBN" prefix and spaces/hyphens. *)
   val make : string -> t HxaGeneral.ress

   (** Check bare digits, or with "ISBN" prefix and spaces/hyphens. *)
   val check : string -> bool

   (** ISBN type: only 13 or 10. *)
   val length       : t -> int

   (** Calc checksum digit. *)
   val makeChecksum  : t -> char

   (** Check checksum digit. *)
   val checkChecksum : t -> bool

   (** String of digits only (possibly last one an 'X'). *)
   val toString     : t -> string

   (** String of digits only (possibly last one an 'X'). *)
   val toStringBare : t -> string

   (** String of form 'ISBN 012-345678901-2' or 'ISBN 012345678X'. *)
   val toStringFull : t -> string

   (**
    * Search for first bare isbn number, by format.
    *
    * @params start pos, length to search, text to search in
    *)
   val search : int -> int -> string -> string option

   (**
    * Search for all bare isbn numbers, by length and checksum.
    * (return all matches, including overlapping ones)
    * Not very efficient, so maybe not for large data.
    *
    * @params start pos, length to search, text to search in
    *)
   val searchByChecksum : int -> ?len:int -> string -> string list

   (**
    * Find and extract ISBNs from book text.
    *
    * @params trace, maximum count, pages
    *)
   val extractIsbnsFromText : bool -> int -> string list -> string list
end




(* ---- types ---- *)

type nameStruct = {
   title  : StringT.t ArrayNe.t ;
   author : StringT.t array ;
   date   : DateIso8601e.t array ;
   id     : (StringT.t * StringT.t) option ;
   subtyp : StringT.t option ;
   typ    : StringT.t ;
}

type nameStructLax = {
   titleLax  : StringT.t array ;
   authorLax : StringT.t array ;
   dateLax   : DateIso8601e.t array ;
   idLax     : (StringT.t * StringT.t) array ;
   subtypLax : StringT.t option ;
   typLax    : StringT.t option ;
}

type nameStructRaw = {
   titleRaw  : string list ;
   authorRaw : string list ;
   dateRaw   : string list ;
   idRaw     : string list ;
   subtypRaw : string ;
   typRaw    : string ;
}




(* ---- values ---- *)

val _MAX_NAME_LEN : int




(* ---- normalisers ---- *)

(** Remove empty strings. *)
val nonEmpties : string list -> string list

(**
 * Split a string of ;|and&, -separated names.
 *
 * This will parse lists of names separated by ; | & 'and' , .
 *  Eg:
 *  * First Last; Last, First Other; etc...
 *  * First Last, First Other Otheretc last, and Other Name (etc...)
 *  But if names themselves use commas to put last first,
 *  eg:
 *  * Connor, Sarah
 *  commas cannot also be used as the list separator.
 *)
val parseNamelist : string -> string list

(** Ie as from: "First Others Last" or "Last, Others First". *)
val getLastName : string -> string

(** Truncate word list to a maximum.
 *  NB: truncates according to byte-length, not necessarily char-length *)
val truncateWords : int -> string list -> string list

val normaliseTitle : string list -> StringT.t array

val normaliseAuthor : string list -> StringT.t array

val normaliseDate : string list -> DateIso8601e.t array

val normaliseIsbn : string list -> (StringT.t * StringT.t) array

(**
 * Subtyp taken to be page-count.
 *)
val normaliseSubtyp : string -> StringT.t option

val normaliseString : string -> StringT.t option

val normaliseMetadataLax : nameStructRaw -> nameStructLax

val normaliseMetadata : nameStructLax -> nameStruct HxaGeneral.ress




(* ---- functions ---- *)

(** Translate \""  \\  \/  \b  \f  \n  \r  \t  into actual chars. *)
val unescapeJsonString : string -> string




(* ---- outer functions ---- *)

(* nameStruct related *)

(** Decide if TADIST string is in text-form or filename-form. *)
val isTextform : string -> bool

(** Make name-struct from TADIST string. *)
val makeNameStruct : string -> nameStruct HxaGeneral.ress

(** Render name-struct to string, in filename style (filename grammar). *)
val toStringName : nameStruct -> string

(** Render name-struct to string, in text-form style (text-form grammar). *)
val toStringText : nameStruct -> string
