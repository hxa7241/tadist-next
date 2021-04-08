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
 * @param bool verbose trace
 * @param string Epub filepathname
 * @exception HxaGeneral.Intolerable
 *)
val extractTadist_x : bool -> string -> Tadist.nameStructRaw option
