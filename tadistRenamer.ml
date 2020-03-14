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




(* ---- functions ---- *)

let nonEmpties (ls:string list) : (string list) =

   ls
   |> (List.map StringT.filter)
   |> (List.filter (Fun.negate String_.isEmpty))


(* NB: truncates according to byte-length, not necessarily char-length *)
let truncateWords (max:int) (words:string list) : string list =

   words
   (* word lengths, with spaces added *)
   |> (List.map String.length)
   |> (function
      | head :: tail -> head :: List.map succ tail
      | _            -> [])
   (* successive sums *)
   |> (List.fold_left (fun ls l -> (l + List.hd ls) :: ls) [0])
   |> List.rev
   |> (function | _ :: t -> t | _ -> [])
   (* truncate after max length *)
   |> (List.combine words)
   |> (List.filter (fun (_,i) -> i <= max))
   |> List.split |> fst


let normaliseTitle (titles:string list) : StringT.t ArrayNe.t ress =

   (* use only first *)
   match titles with
   | []         -> Error "no title found"
   | first :: _ ->
      first
      |> Utf8filter.replace |> Blanks.blankSpacyCtrlChars
      (* truncate after ':', if more than 7 chars before *)
      |> (fun title ->
         try
            let i = String.index title ':' in
            if i > 8 then String.sub title 0 i else title
         with Not_found -> title)
      (* tokenise *)
      |> String.trim |> (String_.split ((=) ' ')) |> (List.map String.trim)
      (* constrain (non-empties, max length, StringT) *)
      |> nonEmpties |> (truncateWords 48)
      |> (List_.filtmap (StringT.make % Result_.toOpt))
      (* convert to (non-empty) array *)
      |> Array.of_list |> ArrayNe.make
      |> Result_.errorMap (fun _ -> "no valid title")


let normaliseAuthor (trace:bool) (authors:string list) : StringT.t array =

   authors
   |> (List.map (Utf8filter.replace % Blanks.blankSpacyCtrlChars))
   (* unified list of all names *)
   |> (fun authors ->
      authors
      (* first, split by ';'s *)
      |> (List.map (fun s -> String_.split ((=) ';') s))
      |> List.flatten
      (* then, split those by 'and' and ','s *)
      |> (
         let rx1 = Str.regexp_string_case_fold " and "
         and rx2 = Str.regexp_case_fold " and \\|,"
         in
         List.map (fun s ->
            if
               (try ignore(Str.search_forward rx1 s 0) ; true
               with Not_found -> false)
            then
               let ss = Str.global_replace rx2 " ; " s in
               String_.split ((=) ';') ss
            else [s]) )
      |> List.flatten
      (* clean-up *)
      |> (List.map String.trim)
      |> (List.filter (Fun.negate String_.isEmpty)))
   |> (fun names ->
      if trace
      then print_endline ("* names:      " ^ (String.concat " | " names)) ;
      names)
   (* extract last names *)
   |> (List.map (fun s ->
         String.trim
            (* try "last, others first" *)
            (try
               let i = String.index s ',' in
               String.sub s 0 i
            (* otherwise "first others last" *)
            with Not_found ->
               let i = try String.rindex s ' ' with Not_found -> 0 in
               String.sub s i ((String.length s) - i))))
   |> (fun lastNames ->
      if trace
      then print_endline ("* last-names: " ^ (String.concat " | " lastNames)) ;
      lastNames)
   (* constrain *)
   |> nonEmpties |> (truncateWords 32)
   |> (List_.filtmap (StringT.make % Result_.toOpt))
   |> Array.of_list


let normaliseDate (dates:string list) : DateIso8601e.t array =

   dates
   |> List.map (Utf8filter.replace % Blanks.unifySpaces)
   (* truncate time from presumed iso8601 dateTtime *)
   |> (List.map (fun s ->
      let s = String.trim s in
      let i =
         try String.index s 'T' with
         | Not_found ->
            try String.index s ' ' with
            | Not_found -> String.length s
      in
      String.sub s 0 i))
   (* check, and just keep OK, sorted, unique years *)
   |> (List.map DateIso8601e.make)
   |> (List_.filtmap Result_.toOpt)
   |> (List.map DateIso8601e.yearOnly)
   |> (List.sort_uniq DateIso8601e.compare)
   (* first and last only *)
   |> (function
      | []            -> [||]
      | [single]      -> [| single |]
      | first :: tail ->
         let last = List.hd (List.rev tail) in
         [| first ; last |])


let normaliseIsbn (isbns:string list) : (StringT.t * StringT.t) option =

   isbns
   |> List.map (Utf8filter.filter % Blanks.unifySpaces)
   (* to machine-readable form *)
   |> List.map (String_.filter (function | ' ' | '-' -> false | _ -> true))
   (* remove bad ones *)
   |> List.filter (fun mForm ->
      match String.length mForm with
      | 13 ->
         String_.check Char_.isDigit mForm
      | 10 ->
         let isDigitOrX = function | '0'..'9' | 'X' -> true | _ -> false
         and main,last =
            let len = String.length mForm in
            (String.sub mForm 0 (len - 1) , String.sub mForm (len - 1) 1)
         in
         (String_.check Char_.isDigit main) && (String_.check isDigitOrX last)
      | _ -> false)
   (* choose first 13-form one *)
   |> List.sort (fun a b -> compare (String.length b) (String.length a))
   |> (function
      | first :: _ ->
         (Ok first)
         |^^= ( (fun _ -> StringT.make "ISBN") , StringT.make )
         |> Result_.toOpt
      | _ -> None)


let normaliseString (s:string) : StringT.t option =

   s
   |> Utf8filter.replace |> Blanks.blankSpacyCtrlChars |> StringT.filter
   (* truncate utf8 chars to max byte length *)
   |> (fun st ->
      st
      |> (let _MAXLEN = 24 in String_.truncate _MAXLEN)
      |> Utf8filter.filter)
   |> StringT.make |> Result_.toOpt


let normaliseMetadata (trace:bool) (titles:string list) (authors:string list)
   (dates:string list) (isbns:string list) (subtyp:string) (typ:string)
   : nameStruct ress =

   (* title is mandatory *)
   match normaliseTitle titles with
   | Error _ as e -> e
   | Ok title     ->
      (* type is mandatory *)
      match normaliseString typ with
      | None     -> Error "invalid type"
      | Some typ ->
         (* the rest are optional *)
         Ok {
            title  = title ;
            author = normaliseAuthor trace authors ;
            date   = normaliseDate dates ;
            id     = normaliseIsbn isbns ;
            subtyp = normaliseString subtyp ;
            typ    = typ }




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
         | Ok Some nsr  ->
            normaliseMetadata trace nsr.titleRaw nsr.authorRaw nsr.dateRaw
               nsr.idRaw nsr.subtypRaw nsr.typRaw
         | Error _ as e -> e
         end
      | [] -> Error "unrecognised file type"
   in

   fileTryer filePathname [
      TadistEpub.extractTadist ;
      TadistPdf.extractTadist ;
      (*Djvu.extractTadist*) ]
