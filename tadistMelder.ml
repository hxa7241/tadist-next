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
         begin match extractor trace filePathname with
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


let meldExtractedAndQueried
   (trace:bool) (metadata:nameStruct) (querydata:nameStruct)
   : nameStruct ress =

   let authorSetUnion =
      (* Build a set, by alternately adding the next item from each list.
       * (So the merge is an interleaving of two ordered lists,
       * but for any duplicate, only the first is kept.)
       * (Using asymptotically slow algo, but the lists are very small.) *)
      let rec merge (result:StringT.t list) (a:StringT.t list) (b:StringT.t list)
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
            (Array.to_list metadata.author)
            (Array.to_list querydata.author)
      in
      interleavedSet |> List.rev |> Array.of_list

   and dateSetUnionEnds =
      (* lump them together *)
      ( (Array.append metadata.date querydata.date) |> Array.to_list )
      |>
      (* sort all *)
      (List.sort_uniq DateIso8601e.compare)
      |>
      (* take first and last only *)
      List_.hdft
      |>
      Array.of_list
   in

   (* DEBUG *)
   if trace
   then begin
      print_endline "" ;
      print_endline ("* M-title:   " ^
         (  querydata.title
            |> ArrayNe.toArray |> Array.to_list
            |> (List.map StringT.toString)
            |> (String.concat " ")  )) ;
      print_endline ("* M-authors: " ^
         (  authorSetUnion
            |> Array.to_list
            |> (List.map StringT.toString)
            |> (String.concat " | ")  )) ;
      print_endline ("* M-date:    " ^
         (  dateSetUnionEnds
            |> Array.to_list
            |> (List.map (DateIso8601e.toString false))
            |> (String.concat " | ")  )) ;
      end ;

   Ok {
      title  = querydata.title ;
      author = authorSetUnion ;
      date   = dateSetUnionEnds ;
      id     = metadata.id ;
      subtyp = metadata.subtyp ;
      typ    = metadata.typ    ; }


let getIsbn (ns:nameStruct) : Isbn.t ress =

   (Option_.toRes "no isbn" ns.id)
   |>=
   (snd %> StringT.toString %> Isbn.make)

   (*
   match ns.id with
   | None          -> Error "no isbn"
   | Some (_ , id) -> id |> StringT.toString |> Isbn.make
   *)




(* ---- public functions ---- *)

let makeNameStructFromFileName (trace:bool) (filePathname:string)
   : nameStruct ress =

   (* get basic metadata *)
   (* : nameStructRaw ress *)
   (extractMetadata trace filePathname)
   |>=
   (* : nameStruct ress *)
   (Tadist.normaliseMetadata trace)
   |>=

   (* get and meld isbn query data *)
   (* : nameStruct ress *)
   (fun metadata ->
      (* : Isbn.t ress *)
      (getIsbn metadata)
      |>=
      (* : nameStructRaw ress *)
      (TadistQuerier.getBasicTadForIsbn trace)
      |>=
      (* : nameStruct ress *)
      (Tadist.normaliseMetadata trace)
      |>=
      (* : nameStruct ress *)
      (meldExtractedAndQueried trace metadata))
