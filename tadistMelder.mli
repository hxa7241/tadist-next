(*------------------------------------------------------------------------------

   TADIST tool (OCaml 4.12)
   Harrison Ainsworth / HXA7241 : 2015, 2021

   http://www.hxa.name/tools/
   http://www.hxa.name/notes/note-hxa7241-20141231T1101Z.html

   License: AGPL -- https://www.gnu.org/licenses/agpl-3.0.html

------------------------------------------------------------------------------*)




(* using HxaGeneral *)
(* using Tadist *)




(* ---- functions ---- *)

(**
 * Extract TADIST data from a file (from a filepathname).
 *
 * (Recognises and handles the filetype (delegating to another module), then
 * normalises the harvested data.).
 *
 * @param bool whether to print extra tracing
 * @param string filepathname
 * @exception HxaGeneral.Intolerable
 *)
val makeMetadataFromFilename_x : bool -> string -> Tadist.nameStructRaw


(**
 * Extract TADIST data from a file (from a filepathname).
 *
 * (Recognises and handles the filetype (delegating to another module), then
 * normalises the harvested data.).
 *
 * @param bool whether to print extra tracing
 * @param string filepathname
 * @exception HxaGeneral.Intolerable
 *)
val makeNameStructFromFileName_x : bool -> string -> Tadist.nameStruct
