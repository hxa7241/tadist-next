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
         with
         | x ->
            Zip.close_in zipfile ;
            raise x

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

let recogniseEpub_x (trace:bool) (epubPathname:string) : bool =

   let __MODULE_FUNCTION__ = __MODULE__ ^ ".recogniseEpub_x" in
   traceHead trace __MODULE_FUNCTION__ "" ;

   try
      (* exceptions: and if so, everything is indeed in vain --
         cannot open file => the program can do nothing *)
      let file = open_in_bin epubPathname in

      let recognised =
         let readString (file:in_channel) (pos:int) (len:int) : string =
            try
               (* exceptions: and if so, everything is indeed in vain --
                  cannot read file => the program can do nothing *)
               seek_in file pos ;
               really_input_string file len
            with
            | x ->
               close_in_noerr file ;
               raise x
         and checkMimetypeFile (epubPathname:string) : bool =
            (* open zip file : bool ress *)
            (Zip.withZipfile
               epubPathname
               (* is the file recognisable ? *)
               (fun zipfile : bool ress ->
                  (* extract mimetype file *)
                  (Zip.readZippedItem zipfile "mimetype")
                  |>=
                  (* check contents *)
                  (fun mimetypeFileContent : bool ress ->
                     let contentNormalised =
                        mimetypeFileContent
                        |> Blanks.unifySpaces |> String.trim
                        |> String.lowercase_ascii
                     in
                     Ok (contentNormalised = "application/epub+zip"))))
            |>
            (bypass (traceRess trace "mimetype file: " string_of_bool))
            |>
            (* any errors count as 'unrecognised' *)
            (Result_.default false)
         in

         let checkZipId (file:in_channel) : bool =
            let b = (readString file 0 4) = "\x50\x4B\x03\x04" in
            traceString trace "zip id: " (string_of_bool b) ;
            b
         and checkMimetypeId (file:in_channel) : bool =
            let b = (readString file 30 28) = "mimetypeapplication/epub+zip" in
            traceString trace "epub mimetype: " (string_of_bool b) ;
            b
         in

         (* must have zip id, then check epub mimetype ... *)
         (checkZipId file) &&
            (* if the archive was packaged properly, the mimetype is visible,
               otherwise, check the mimetype file and its contents *)
            ((checkMimetypeId file) || (checkMimetypeFile epubPathname))
      in

      close_in_noerr file ;
      recognised

   with
   | Sys_error extraMsg ->
      let message = "cannot open/read file: " ^ epubPathname in
      raiseTrace
         trace EXIT_NOINPUT __MODULE_FUNCTION__
         message
         "Either there is no file of that name, or it is not openable. Maybe \
         you need to correct the file name or path."
         extraMsg


let getContentOpf_x (trace:bool) (epubPathname:string) : (string * string) =

   let __MODULE_FUNCTION__ = __MODULE__ ^ ".getContentOpf_x" in
   traceHead trace __MODULE_FUNCTION__ "" ;

   (Zip.withZipfile epubPathname
      (fun zipfile ->

         (* get pathname of metadata zipped-file, from epub-root
            zipped file : string ress *)
         (match Zip.readZippedItem zipfile "META-INF/container.xml" with
         | Error msg ->
            Error msg
         | Ok containerXml ->
            (* find the filepathname string *)
            try
               (* remove line-ends for easier regexps *)
               let containerXml = Blanks.blankNewlines containerXml
               and rx = Str.regexp "<rootfile[ \t]+\\(.+\\)?\
                  full-path=[\"']\\([^\"']*\\)[\"']"
               in
               let _ = Str.search_forward rx containerXml 0 in
               Ok (Str.matched_group 2 containerXml)
            with
            | Not_found ->
               Error "content.opf FilePathname not found")
         |>=
         (* get raw metadata etc : (string * string) ress *)
         (fun contentOpfFilePathname ->
            (* read metadata zipped-file *)
            match Zip.readZippedItem zipfile contentOpfFilePathname with
            | Error msg ->
               Error msg
            | Ok contentOpf ->
               traceString
                  trace
                  ((FileName.getPath contentOpfFilePathname) ^ "\n")
                  contentOpf ;
               Ok
                  (  FileName.getPath contentOpfFilePathname
                  ,  Utf8.Filter.replace contentOpf )) ) )
   |>
   (Result_.defaultf
      (fun message ->
         (raiseTrace
            trace EXIT_DATAERR __MODULE_FUNCTION__
            message
            "The file seems to missing important parts. You probably need to \
            know about the epub format and perform some manual surgery."
            "")))


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


let getHtmlPathnames (trace:bool) (contentopf:string) : string list =

   let __MODULE_FUNCTION__ = __MODULE__ ^ ".getHtmlPathnames" in
   traceHead trace __MODULE_FUNCTION__ "" ;

   let manifest =
      (* remove line-ends for easier regexps *)
      let contentopf = Blanks.blankNewlines contentopf in
      try
         let rx = Str.regexp "<manifest[^<>]*>\\(.+\\)</manifest>" in
         let _ = Str.search_forward rx contentopf 0 in
         Str.matched_group 1 contentopf
      with
      | Not_found -> ""
   and head , foot =
      "<item[ \t\n]+[^<>]*" , "[^<>]*/>"
   in

   let htmlItems =
      let rx = Str.regexp (head ^
         "media-type=[\"']application/xhtml\\+xml[\"']" ^ foot)
      in
      let l = Str_.allMatches rx manifest (Str.matched_string) in
      String.concat "\n" l
   and rx =
      Str.regexp (head ^ "href=[\"']\\([^\"']*\\)[\"']" ^ foot)
   in

   let htmlPathnames = Str_.allMatches rx htmlItems (Str.matched_group 1) in

   traceString
      trace
      ((string_of_int (List.length htmlPathnames)) ^ "\n\n")
      (String.concat "\n" htmlPathnames) ;

   htmlPathnames


let getTextPages (trace:bool) (epubPathname:string) (contentopfpath:string)
   (htmlPathnames:string list)
   : string list =

   let __MODULE_FUNCTION__ = __MODULE__ ^ ".getTextPages" in
   traceHead trace __MODULE_FUNCTION__ "" ;

   (* get HTML texts *)
   let htmls : string list =
      let strResList : (string ress) list =
         (Zip.withZipfile
            epubPathname
            (fun zipfile : (string ress) list ress ->
               Ok
                  (List.map
                     (fun htmlPathname ->
                        Zip.readZippedItem
                           zipfile (contentopfpath ^ htmlPathname))
                     htmlPathnames)))
         |>
         (bypass (traceRess trace "count: " (List.length %> string_of_int)))
         |>
         (Result.value ~default:[])
      in

      List.iter
         (traceRess
            trace "" (Blanks.blankNewlines %> (String_.lead 80)))
         strResList ;

      List_.filtmap Result_.toOpt strResList
   in

   traceString trace "count: " (string_of_int (List.length htmls)) ;

   (* remove tags *)
   List.map
      (fun html ->
         let rx = Str.regexp {|<[^>]*>|} in
         Str.global_replace rx "" html)
      htmls


let getTextIsbns (trace:bool) (epubPathname:string) (contentopfpath:string)
   (htmlPathnames:string list)
   : string list =

   let pages : string list =
      (* only inspect first 10 and last 5 pages *)
      let leadAndtrail =
         let lead , rest  = List_.bisect htmlPathnames 10 in
         let restLen = List.length rest in
         let _ , trail = List_.bisect rest (restLen - 5) in
         List.append lead trail
      in
      getTextPages trace epubPathname contentopfpath leadAndtrail
   in

   Tadist.Isbn.extractIsbnsFromText trace 2 pages




(* ---- public functions ---- *)

let extractTadist_x (trace:bool) (epubPathname:string)
   : Tadist.nameStructRaw option =

   let __MODULE_FUNCTION__ = __MODULE__ ^ ".extractTadist_x" in

   if recogniseEpub_x trace epubPathname

   (* recognised as epub *)
   then
      let contentOpfPath , contentopf = getContentOpf_x trace epubPathname in

      (* get metadata *)
      let titles , authors , dates , isbns =
         getContentopfMetadata contentopf
      in

      traceHead trace __MODULE_FUNCTION__ "ISBNs in metadata" ;
      traceString trace "" (String.concat " | " isbns) ;

      (* get list of html sections *)
      let htmlPathnames = getHtmlPathnames trace contentopf in
      let sectionCount = string_of_int (List.length htmlPathnames) in

      (* add ISBNs found in text *)
      let isbns =
         (getTextIsbns trace epubPathname contentOpfPath htmlPathnames)
         |> (List.append isbns)
         |> List_.deduplicate
      in

      let nsr =
         Tadist.{
            titleRaw  = titles ;
            authorRaw = authors ;
            dateRaw   = dates ;
            idRaw     = isbns ;
            subtypRaw = sectionCount ;
            typRaw    = _TYPE }
      in

      traceHead trace __MODULE_FUNCTION__ "raw metadata" ;
      traceString trace "" (Tadist.rawToString nsr) ;

      Some nsr

   (* not recognised as epub *)
   else
      None
