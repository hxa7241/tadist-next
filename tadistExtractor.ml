(*------------------------------------------------------------------------------

   TADIST tool (OCaml 4.10)
   Harrison Ainsworth / HXA7241 : 2015, 2020

   http://www.hxa.name/tools/
   http://www.hxa.name/notes/note-hxa7241-20141231T1101Z.html

   License: AGPL -- https://www.gnu.org/licenses/agpl-3.0.html

------------------------------------------------------------------------------*)




open HxaGeneral
open Tadist
(* using TadistEpub *)
(* using TadistPdf *)
(* (using Djvu) *)




(* ---- public functions ---- *)

let makeNameStructFromFileName (trace:bool) (filePathname:string)
   : nameStruct ress =

   (* redundant with recogniseEpub ... and the others?
   (* check file exists *)
   match (try close_in (open_in_bin filePathname) ; Ok true
      with _ -> Error "cannot open file")
   with
   | Error _ as e -> e
   | Ok _         ->
   *)

   let rec fileTryer (filePathname:string)
      (lf:(bool -> string -> (Tadist.nameStructRaw option) ress) list)
      : nameStruct ress =
      match lf with
      | f :: rest ->
         begin match f trace filePathname with
         | Ok None      -> fileTryer filePathname rest
         | Ok Some nsr  -> Tadist.normaliseMetadata trace nsr
         | Error _ as e -> e
         end
      | [] -> Error "unrecognised file type"
   in

   fileTryer filePathname [
      TadistEpub.extractTadist ;
      TadistPdf.extractTadist ;
      (*Djvu.extractTadist*) ]
