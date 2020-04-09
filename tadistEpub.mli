(*------------------------------------------------------------------------------

   Epub lib (OCaml 4.10)
   Harrison Ainsworth / HXA7241 : 2015, 2020

   http://www.hxa.name/tools/

   License: AGPL -- https://www.gnu.org/licenses/agpl-3.0.html

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
 * @param string Epub filepathname
 *)
val extractTadist : string ->
   (Tadist.nameStructRaw option) HxaGeneral.ress
