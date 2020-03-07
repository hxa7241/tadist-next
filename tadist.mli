(*------------------------------------------------------------------------------

   TADIST lib (OCaml 4.02)
   Harrison Ainsworth / HXA7241 : 2015

   http://www.hxa.name/tools/
   http://www.hxa.name/notes/note-hxa7241-20141231T1101Z.html

   License: CC0 -- http://creativecommons.org/publicdomain/zero/1.0/

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
   val make : string -> t HxaGeneral.eoption

   (** Make from string, by discarding invalid chars. *)
   val makef : string -> t HxaGeneral.eoption

   (** Get string from. *)
   val toString : t -> string
end


(**
 * A constrained Array: non-zero length.
 *)
module ArrayNe :
sig
   type 'a t

   (** Make from array, if valid. *)
   val make : 'a array -> 'a t HxaGeneral.eoption

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
   val make : string -> t HxaGeneral.eoption

   (** Get string of iso8601 from date. *)
   val toString : bool -> t -> string

   (** Reduce to just the year. *)
   val yearOnly : t -> t

   (** As for standard compare. *)
   val compare : t -> t -> int
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




(* ---- functions ---- *)

(* nameStruct related *)

(** Decide if TADIST string is in text-form or filename-form. *)
val isTextform : string -> bool

(** Make name-struct from TADIST string. *)
val makeNameStruct : string -> nameStruct HxaGeneral.eoption

(** Render name-struct to string, in filename style (filename grammar). *)
val toStringName : nameStruct -> string

(** Render name-struct to string, in text-form style (text-form grammar). *)
val toStringText : nameStruct -> string
