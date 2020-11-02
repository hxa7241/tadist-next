(*------------------------------------------------------------------------------

   Epub lib (OCaml 4.10)
   Harrison Ainsworth / HXA7241 : 2015, 2020

   http://www.hxa.name/tools/

   License: AGPL -- https://www.gnu.org/licenses/agpl-3.0.html

------------------------------------------------------------------------------*)




open HxaGeneral
(* using Tadist *)
(* using Zip *)




(* ---- values ---- *)

let _TYPE = "epub"




(* ---- lib module augmentations ---- *)

module Str_ =
struct

   (* (f as either (Str.matched_string) or (Str.matched_group <int>)) *)
   let allMatches (rx:Str.regexp) (toSearch:string) (f:string -> string)
      : string list =
      let rec lister pos l : string list =
         match
            if pos >= 0
            then
               try
                  let p = Str.search_backward rx toSearch pos in
                  let m = f toSearch in
                  Some ( p , m )
               with
               | Not_found -> None
            else None
         with
         | Some (pos , mtch) -> lister (pos - 1) (mtch :: l)
         | None -> l
      in
      lister (String.length toSearch) []

end


module Zip =
struct

   include Zip

   let withZipfile (filePathname:string) (f:Zip.in_file -> 'a ress)
      : 'a ress =
      (* open file *)
      match
         try Ok (Zip.open_in filePathname) with
         | Sys_error s                             -> Result.Error s
         | Zip.Error (zipName, entryName, message) ->
            Result.Error (Printf.sprintf "%s %s: %s" zipName entryName message)
      with
      | Result.Error _ as e -> e
      | Ok zipfile   ->
         try
            let ao = f zipfile in
            (* close file *)
            Zip.close_in zipfile ;
            ao

         (* close file *)
         with x -> Zip.close_in zipfile ; raise x

   let readZippedItem (zipfile:Zip.in_file) (pathname:string)
      : string ress =
      try
         let a = Zip.find_entry zipfile pathname in
         Ok (Zip.read_entry zipfile a)
      with
      | Not_found -> Result.Error "zip entry not found"
      | Zip.Error (zipName, entryName, message) ->
         Result.Error (Printf.sprintf "%s %s: %s" zipName entryName message)

end




(* ---- functions ---- *)

let recogniseEpub (epubPathname:string) : bool ress =

   try
      (* (assuming this can fail) *)
      let file = open_in_bin epubPathname in

      let recognised =
         let readString file pos len =
            (* (assuming these cannot fail, in a deeper IO sense) *)
            try seek_in file pos ; really_input_string file len with _ -> ""
         in
         (* check zip id then epub id *)
         ((readString file 0 4) = "\x50\x4B\x03\x04") &&
            ((readString file 30 28) = "mimetypeapplication/epub+zip")
      in

      close_in_noerr file ;
      Ok recognised
   with
   | _ -> Error ("cannot open/read file: " ^ epubPathname)


let getContentOpf (epubPathname:string) : (string * string) ress =

   Zip.withZipfile epubPathname
      (fun zipfile ->

         (* get raw metadata etc *)
         match
            (* get pathname of metadata zipped-file, from epub-root
               zipped file *)
            match Zip.readZippedItem zipfile "META-INF/container.xml" with
            | Error _ as e    -> e
            | Ok containerxml ->
               (* find the filepathname string *)
               try
                  (* remove line-ends for easier regexps *)
                  let containerxml = Blanks.blankNewlines containerxml
                  and rx = Str.regexp "<rootfile[ \t]+\\(.+\\)?\
                     full-path=[\"']\\([^\"']*\\)[\"']"
                  in
                  let _ = Str.search_forward rx containerxml 0 in
                  Ok (Str.matched_group 2 containerxml)
               with
               | Not_found -> Error "content.opf FilePathname not found"
         with
         | Error _ as e              -> e
         | Ok contentopfFilepathname ->

            (* read metadata zipped-file *)
            match Zip.readZippedItem zipfile contentopfFilepathname with
            | Error _ as e  -> e
            | Ok contentopf ->
               Ok ( FileName.getPath contentopfFilepathname , contentopf )
      )


let getContentopfMetadata (contentopf:string)
   : (string list * string list * string list * string list) =

   (* remove line-ends for easier regexps *)
   let contentopf = Blanks.blankNewlines contentopf

   and matcher (tag:string) (attr:string) (prefix:string) (text:string)
      : string list =
      let rx =
         Str.regexp_case_fold
            ("<dc:" ^ tag ^ "[^>]*\
            " ^ attr ^ "[^>]*\
            >\
            " ^ prefix ^ "\
            \\([^<]*\\)\
            </dc:" ^ tag ^ ">")
      in
      Str_.allMatches rx text (Str.matched_group 1)
   in

   let matcherSimple (tag:string) (text:string) : string list =
      matcher tag "" "" text

   and matcherIsbn (text:string) : string list =
      let matcher tag attr prefix text =
         HxaGeneral.toList1 (matcher tag attr prefix text)
      and searcher (tag:string) (text:string)
         : string list1 option =
         (matcher tag "" "" text)
         |> (List_.filtmap (Tadist.Isbn.search 0 (String.length text)))
         |> HxaGeneral.toList1
      in

      (* define the various matching forms *)
      let identifier2  text () =
         matcher "identifier" "opf:scheme=[\"']ISBN[\"']" "" text
      and identifier3  text () =
         matcher "identifier" "" "urn:isbn:" text
      and source2 text () =
         matcher "source" "opf:scheme=[\"']ISBN[\"']" "" text
      and source3 text () =
         matcher "source" "" "urn:isbn:" text

      and bareIdentifier text () = searcher "identifier" text
      and bareSource     text () = searcher "source"     text in

      (* take the first successful match, in this priority of alternatives *)
      (identifier3 text ())
      ||> (identifier2 text)
      ||> (source3 text)
      ||> (source2 text)
      ||> (bareIdentifier text)
      ||> (bareSource text)
      |> ofList1o
   in

   (  matcherSimple "title"   contentopf ,
      matcherSimple "creator" contentopf ,
      matcherSimple "date"    contentopf ,
      matcherIsbn             contentopf )


let getHtmlPathnames (contentopf:string) : string list =

   let manifest =
      (* remove line-ends for easier regexps *)
      let contentopf = Blanks.blankNewlines contentopf in
      try
         let rx = Str.regexp "<manifest[^<>]*>\\(.+\\)</manifest>" in
         let _ = Str.search_forward rx contentopf 0 in
         Str.matched_group 1 contentopf
      with
      | Not_found -> ""
   and head , foot = "<item[ \t\n]+[^<>]*" , "[^<>]*/>"
   in

   let htmlItems =
      let rx = Str.regexp (head ^
         "media-type=[\"']application/xhtml\\+xml[\"']" ^ foot)
      in
      let l = Str_.allMatches rx manifest (Str.matched_string) in
      String.concat "\n" l
   and rx = Str.regexp (head ^ "href=[\"']\\([^\"']*\\)[\"']" ^ foot)
   in

   Str_.allMatches rx htmlItems (Str.matched_group 1)


let findFirstIsbn (text:string) : (string option) =

   match
      (* find 'ISBN' (pos of last char) *)
      try
         let rx = Str.regexp_string_case_fold "isbn" in
         let _  = Str.search_forward rx text 0 in
         Some ((Str.match_end ()) - 1)
      with
         Not_found -> None
   with
   | Some pos ->
      (* find number nearby *)
      (*print_endline ("   'isbn' pos: " ^ (string_of_int pos)) ;*)
      Tadist.Isbn.search pos 15 text
   | None     -> None


let getIsbns (epubPathname:string) (contentopfpath:string)
   (htmlPathnames:string list)
   : string list =

   (* Very usually, there is an obvious ISBN: there is a publishing details
      page (often called the copyright page), near the beginning, containing
      Library of Congress cataloging data, copyright data, and ISBN(s). The
      ISBNs are fairly conformant and quite easily recognised and extracted.

      Otherwise, not much more is tried: if there are multiple pages
      containing ISBNs, the one with "Library of Congress" is chosen; if
      there are multiple pages of *that* case, then give up. *)
   (* get HTML files *)
   let htmls =
      match
         Zip.withZipfile epubPathname
            (fun zipfile ->
               Ok (List.map (fun htmlPathname ->
                  Zip.readZippedItem zipfile (contentopfpath ^ htmlPathname))
                  htmlPathnames))
      with
      | Ok a    -> a
      | Error _ -> []
   in

   (*print_endline ("html files: " ^ (string_of_int (List.length htmls))) ;*)

   (* filter for ISBN presence *)
   let isbnFiles =
      List_.filtmap (function
         | Error _  -> None
         | Ok html  ->
            (* for easier searching: coerce to UTF-8; blank-out line-ends,
               tabs, markup; translate en-dashs to hyphens *)
            let text = html
               |> Utf8filter.filter
               |> Blanks.unifySpaces
               |> (Str.global_replace (Str.regexp "<[^>]+>") " ")
               |> (Str.global_replace (Str.regexp_string "\xE2\x80\x93") "-")
            in
            Option.map (fun isbn -> (text,isbn)) (findFirstIsbn text)
         ) htmls
   in

   (*print_endline ("isbn files: " ^ (string_of_int (List.length isbnFiles))) ;*)

   match isbnFiles with
   | [] as empty        -> empty
   | (_ , unique) :: [] -> [unique]
   | ambiguous          ->
      (* filter for "library of congress" presence *)
      let loc = List.filter (fun (text,_) ->
         try
            let rx = Str.regexp_string_case_fold "library of congress" in
            let _  = Str.search_forward rx text 0 in
            true
         with Not_found -> false
         ) ambiguous
      in

      (*begin
         print_endline ("isbn ambiguous (" ^
         (string_of_int (List.length loc)) ^ ")") ;
         List.iter (fun (_,isbn) -> print_endline ("   loc: " ^ isbn)) loc
      end ;*)

      begin match loc with
      | (_ , unique) :: [] -> [unique]
      (* give up *)
      | _                  -> []
      end




(* ---- public functions ---- *)

let extractTadist (epubPathname:string)
   : (Tadist.nameStructRaw option) ress =

   match recogniseEpub epubPathname with
   | Error _ as e -> e
   | Ok false     -> Ok None
   | Ok true      ->

      match getContentOpf epubPathname with
      | Error _ as e                     -> e
      | Ok (contentopfpath , contentopf) ->

         let titles , authors , dates , isbns =
            getContentopfMetadata contentopf
         in

         (* maybe look for ISBN elsewhere *)
         let isbns = if isbns <> []
            then isbns
            else let htmlPathnames = getHtmlPathnames contentopf in
               let isbns = getIsbns epubPathname contentopfpath
                  htmlPathnames in
               isbns
         in

         Ok (Some Tadist.( {
            titleRaw  = titles ;
            authorRaw = authors ;
            dateRaw   = dates ;
            idRaw     = isbns ;
            subtypRaw = "" ;
            typRaw    = _TYPE } ) )
