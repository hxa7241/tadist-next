(*------------------------------------------------------------------------------

   TADIST tool (OCaml 4.10)
   Harrison Ainsworth / HXA7241 : 2015, 2020

   http://www.hxa.name/tools/
   http://www.hxa.name/notes/note-hxa7241-20141231T1101Z.html

   License: AGPL -- https://www.gnu.org/licenses/agpl-3.0.html

------------------------------------------------------------------------------*)




(* using HxaGeneral *)
(* using Tadist *)




(* ---- functions ---- *)

(**
 * Get basic TADIST data (title, author, date) from an internet ISBN query.
 *
 * @param  bool whether to print extra tracing
 * @param  isbn
 * @return title, authors, date
 *)
val getBasicTadForIsbn : bool -> Tadist.Isbn.t ->
   Tadist.nameStructRaw HxaGeneral.ress
