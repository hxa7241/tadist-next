(*------------------------------------------------------------------------------

   PDF lib (OCaml 4.10)
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
