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




let extractMetadata_x (trace:bool) (filePathname:string) : nameStructRaw =

   let rec fileTryer
      (filePathname:string)
      (fExtractors:(bool -> string -> Tadist.nameStructRaw option) list)
      : nameStructRaw =

      match fExtractors with
      | fExtractor :: rest ->
         begin match (fExtractor trace filePathname) with
         (* recognised: return the extracted data *)
         | Some nsr -> nsr
         (* unrecognised: recurse to try next extractor *)
         | None     -> fileTryer filePathname rest
         end
      | [] ->
         (* all tries at recognition failed *)
         let __MODULE_FUNCTION__ = __MODULE__ ^ ".extractMetadata_x"
         and message = "unrecognised file type" in
         traceHead trace __MODULE_FUNCTION__ "" ;
         raiseTrace
            trace EXIT_DATAERR __MODULE_FUNCTION__
            message
            "Only the file types mentioned in the documentation are supported. \
            You can either pre-filter the files you feed in, or accept \
            occurrences of this error message."
            ""
   in

   fileTryer
      filePathname
      [  TadistEpub.extractTadist_x ;
         TadistPdf.extractTadist_x ;  ]


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


let queryForIsbn (trace:bool) (nsLax:nameStructLax) : nameStructRaw option =

   (* only try first few : 'a array *)
   (Array_.lead 2 nsLax.idLax)
   |>
   (* try successive ISBNs until good result : nameStructRaw ress *)
   (Array.fold_left
      (fun lastResult (thisItem:(StringT.t * StringT.t)) ->
         match lastResult with
         (* try again *)
         | Error _ ->
            thisItem
            |>  (snd %> StringT.toString %> Isbn.make)
            |>= (TadistQuerier.getBasicTadForIsbn trace)
            |>= (fun nsr ->
               (* fail if query result is (nigh) empty *)
               if
                  List_.isEmpty nsr.titleRaw
                     || String_.isEmpty (List.hd nsr.titleRaw)
               then
                  (Error "inadequate info from ISBN query")
                  |>
                  (bypass (traceRess trace "" (ko "")))
               else
                  Ok nsr)
         (* pass through success *)
         | Ok nsr ->
            Ok nsr)
      (Error "no ISBN to use"))
   |>
   (bypass
      (fun nsrRess ->
         let __MODULE_FUNCTION__ = __MODULE__ ^ ".queryForIsbn" in
         traceHead trace __MODULE_FUNCTION__ "" ;
         traceRess trace "" (ko "successful ISBN query") nsrRess ; ))
   |>
   Result_.toOpt




(* ---- public functions ---- *)

let makeNameStructFromFileName_x (trace:bool) (filePathname:string)
   : nameStruct =

   (* get internal metadata *)
   let metadata : nameStructLax =
      (* : nameStructRaw *)
      (extractMetadata_x trace filePathname)
      |>
      Tadist.normaliseMetadataLax
   in

   (* mix in remotely queried metadata, if available *)
   let melded : nameStructLax =
      (* : nameStructRaw option *)
      (queryForIsbn trace metadata)
      |>-
      (fun querydata : nameStructLax option ->
         (Tadist.normaliseMetadataLax querydata)
         |>
         (meldExtractedAndQueried metadata)
         |>
         Option.some)
      |>
      (* any query failure defaults meld to just internal metadata *)
      (Option_.default metadata)
   in

   Tadist.normaliseMetadata_x trace melded
