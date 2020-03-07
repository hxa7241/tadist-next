(*------------------------------------------------------------------------------

   Epub lib (OCaml 4.02)
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
 * Extract fairly raw TADIST data from an Epub file (from a filepathname).
 *
 * (Epub has quite good metadata, but data can always be a bit messy.
 * ISBN often must be sought in the body of the text.).
 *
 * @param bool whether to print extra tracing
 * @param string Epub filepathname
 *)
val extractTadist : bool -> string ->
   (Tadist.nameStructRaw option) HxaGeneral.eoption
