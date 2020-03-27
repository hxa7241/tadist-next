(*------------------------------------------------------------------------------

   TADIST tool (OCaml 4.10)
   Harrison Ainsworth / HXA7241 : 2015, 2020

   http://www.hxa.name/tools/
   http://www.hxa.name/notes/note-hxa7241-20141231T1101Z.html

   License: AGPL -- https://www.gnu.org/licenses/agpl-3.0.html

------------------------------------------------------------------------------*)




open HxaGeneral
open Tadist




(* ---- functions ---- *)




(* ---- public functions ---- *)

let getBasicTadForIsbn (trace:bool) (isbn:Isbn.t)
   : (StringT.t option * StringT.t array * DateIso8601e.t array) =

   failwith "not implemented yet"

   (*(  Result_.toOpt (StringT.makef "[dummy]") ,
      [||] ,
      [||] )*)
