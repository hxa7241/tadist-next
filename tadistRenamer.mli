(*------------------------------------------------------------------------------

   TADIST renamer (OCaml 4.02)
   Harrison Ainsworth / HXA7241 : 2015

   http://www.hxa.name/tools/
   http://www.hxa.name/notes/note-hxa7241-20141231T1101Z.html

   License: CC0 -- http://creativecommons.org/publicdomain/zero/1.0/

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
 *)
val makeNameStructFromFileName : bool -> string ->
   Tadist.nameStruct HxaGeneral.eoption
