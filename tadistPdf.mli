(*------------------------------------------------------------------------------

   TADIST tool (OCaml 4.12)
   Harrison Ainsworth / HXA7241 : 2015, 2021

   http://www.hxa.name/tools/
   http://www.hxa.name/notes/note-hxa7241-20141231T1101Z.html

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
 * @param bool verbose trace
 * @param string PDF filepathname
 * @exception HxaGeneral.Intolerable
 *)
val extractTadist_x : bool -> string -> Tadist.nameStructRaw option
