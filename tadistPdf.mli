(*------------------------------------------------------------------------------

   PDF lib (OCaml 4.02)
   Harrison Ainsworth / HXA7241 : 2015

   http://www.hxa.name/tools/

   License: CC0 -- http://creativecommons.org/publicdomain/zero/1.0/

------------------------------------------------------------------------------*)




(* using HxaGeneral *)
(* using Tadist *)




(* ---- values ---- *)

val _TYPE : string




(* ---- functions ---- *)

(**
 * Extract fairly raw TADIST data from a PDF file (from a filepathname).
 *
 * (PDF has sufficient metadata, but sadly it is usually unfilled.
 * An ISBN often must be sought in the body of the text, then used to
 * lookup the metadata remotely.).
 *
 * @param bool whether to print extra tracing
 * @param string PDF filepathname
 *)
val extractTadist : bool -> string ->
   (Tadist.nameStructRaw option) HxaGeneral.ress
