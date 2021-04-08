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
(* using TadistQuerier *)




let extractMetadata (trace:bool) (filePathname:string) : nameStructRaw ress =

   let rec fileTryer (filePathname:string)
      (extractors:(bool -> string -> (Tadist.nameStructRaw option) ress) list)
      : nameStructRaw ress =
      match extractors with
      | extractor :: rest ->
         begin match (extractor trace filePathname) with
         (* recurse to try next extractor *)
         | Ok None      -> fileTryer filePathname rest
         (* return the extracted data *)
         | Ok Some nsr  -> Ok nsr
         | Error _ as e -> e
         end
      | [] -> Error "unrecognised file type"
   in

   fileTryer filePathname [
      TadistEpub.extractTadist ;
      TadistPdf.extractTadist ; ]


let meldExtractedAndQueried (metadata:nameStructLax) (querydata:nameStructLax)
   : nameStructLax =

   let titlePriority =
      (* query has priority, unless empty, in which case default to meta *)
      match querydata.titleLax with
      | [||]    -> metadata.titleLax
      | qdTitle -> qdTitle

   and authorSetUnion =
      (* Build a set, by alternately adding the next item from each list.
       * (So the merge is an interleaving of two ordered lists,
       * but for any duplicate, only the first is kept.)
       * (Using asymptotically slow algo, but the lists are very small.) *)
      let rec merge (result:StringT.t list) (a:StringT.t list)
         (b:StringT.t list)
         : StringT.t list =
         (* append only if not already there -- like a set *)
         let setAppend (l:StringT.t list) (x:StringT.t) : StringT.t list =
            if (List.mem x l) then l else x :: l
         in
         (* interleave two unequal length lists *)
         match (a , b) with
         | ([]         , []        ) -> result
         | (hda :: tla , []        ) -> merge (setAppend result hda) tla []
         | ([]         , hdb :: tlb) -> merge (setAppend result hdb) [] tlb
         | (hda :: tla , hdb :: tlb) ->
            merge (setAppend (setAppend result hda) hdb) tla tlb
      in
      let interleavedSet =
         merge
            []
            (Array.to_list metadata.authorLax)
            (Array.to_list querydata.authorLax)
      in
      interleavedSet |> List.rev |> Array.of_list

   and datePriorityEnds =
      (* prioritise query *)
      (if not (Array_.isEmpty querydata.dateLax)
      then querydata.dateLax
      else metadata.dateLax)
      |> Array.to_list
      (* sort *)
      |> (List.sort_uniq DateIso8601e.compare)
      (* take first and last only *)
      |> List_.hdft
      |> Array.of_list

   and subtypPriority =
      (* prioritise metadata *)
      Option_.or2 metadata.subtypLax querydata.subtypLax
   in

   {  titleLax  = titlePriority ;
      authorLax = authorSetUnion ;
      dateLax   = datePriorityEnds ;
      idLax     = metadata.idLax ;
      subtypLax = subtypPriority ;
      typLax    = metadata.typLax    ;  }


let queryForIsbn (trace:bool) (nsLax:nameStructLax) : nameStructRaw ress =

   (* only try first few : 'a array *)
   (Array_.lead 2 nsLax.idLax)
   |>
   (* try successive ISBNs until good result : nameStructRaw ress *)
   (Array.fold_left
      (fun lastResult (thisItem:(StringT.t * StringT.t)) ->
         match lastResult with
         | Error _ ->
            thisItem
            |>  (snd %> StringT.toString %> Isbn.make)
            |>= (TadistQuerier.getBasicTadForIsbn trace)
            |>= (fun nsr ->
               (* fail if query result is (nigh) empty *)
               if
                  List_.isEmpty nsr.titleRaw
                  || String_.isEmpty (List.hd nsr.titleRaw)
               then Error "inadequate info from ISBN query"
               else Ok nsr)
         | Ok nsr -> Ok nsr)
      (Error "no ISBN to use"))




(* ---- public functions ---- *)

let makeNameStructFromFileName_x (trace:bool) (filePathname:string)
   : nameStruct =

   (* : nameStructRaw ress *)
   (extractMetadata trace filePathname)
   |>=
   (* : nameStruct ress *)
   (fun (metadataRaw:nameStructRaw) ->
      metadataRaw
      |>
      Tadist.normaliseMetadataLax
      |>
      (* : nameStructLax *)
      (fun (metadataLax:nameStructLax) ->
         (Ok metadataLax)
         |>=
         (queryForIsbn trace)
         |>=-
         Tadist.normaliseMetadataLax
         |>=-
         (meldExtractedAndQueried metadataLax)
         |>
         (* any query failure defaults meld to just internal metadata *)
         (Result.value ~default:metadataLax) )
      |>
      (* : nameStruct ress *)
      (Tadist.normaliseMetadata_x %> Result.ok) )
   |>
   (bypass
      (fun value ->
         traceHead trace __MODULE__ "makeNameStructFromFileName_x" "" ;
         traceRess trace "" (ko "") value ; ))
   |>
   (Result_.toExc_x (fun s -> Intolerable (EXIT_UNSPECIFIED , s)))
