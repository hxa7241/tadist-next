(*------------------------------------------------------------------------------

   Epub lib (OCaml 4.02)
   Harrison Ainsworth / HXA7241 : 2015

   http://www.hxa.name/tools/

   License: CC0 -- http://creativecommons.org/publicdomain/zero/1.0/

------------------------------------------------------------------------------*)




open HxaGeneral
(* using Tadist *)
(* using Zip *)




(* ---- values ---- *)

let _TYPE = "epub"




(* ---- lib module augmentations ---- *)

module Str =
struct

   include Str

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

   let withZipfile (filePathname:string) (f:Zip.in_file -> 'a eoption)
      : 'a eoption =
      (* open file *)
      match
         try Ok (Zip.open_in filePathname) with
         | Sys_error s -> Err s
         | Zip.Error (zipName, entryName, message) ->
            Err (Printf.sprintf "%s %s: %s" zipName entryName message)
      with
      | Err _ as e -> e
      | Ok zipfile ->
         try
            let ao = f zipfile in
            (* close file *)
            Zip.close_in zipfile ;
            ao

         (* close file *)
         with x -> Zip.close_in zipfile ; raise x

   let readZippedItem (zipfile:Zip.in_file) (pathname:string)
      : string eoption =
      try
         let a = Zip.find_entry zipfile pathname in
         Ok (Zip.read_entry zipfile a)
      with
      | Not_found -> Err "zip entry not found"
      | Zip.Error (zipName, entryName, message) ->
         Err (Printf.sprintf "%s %s: %s" zipName entryName message)

end




(* ---- functions ---- *)

let recogniseEpub (epubPathname:string) : bool eoption =

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
   | _ -> Err ("cannot open/read file: " ^ epubPathname)


let getContentOpf (epubPathname:string) : (string * string) eoption =

   Zip.withZipfile epubPathname
      (fun zipfile ->

         (* get raw metadata etc *)
         match
            (* get pathname of metadata zipped-file, from epub-root
               zipped file *)
            match Zip.readZippedItem zipfile "META-INF/container.xml" with
            | Err _ as e      -> e
            | Ok containerxml ->
               (* find the filepathname string *)
               try
                  (* remove line-ends for easier regexps *)
                  let containerxml = blankNewlines containerxml
                  and rx = Str.regexp "<rootfile[ \t]+\\(.+\\)?\
                     full-path=[\"']\\([^\"']*\\)[\"']"
                  in
                  let _ = Str.search_forward rx containerxml 0 in
                  Ok (Str.matched_group 2 containerxml)
               with
               | Not_found -> Err "content.opf FilePathname not found"
         with
         | Err _ as e                -> e
         | Ok contentopfFilepathname ->

            (* read metadata zipped-file *)
            match Zip.readZippedItem zipfile contentopfFilepathname with
            | Err _ as e    -> e
            | Ok contentopf ->
               Ok ( getFilePath contentopfFilepathname , contentopf )
      )


let getContentopfMetadata (contentopf:string)
   : (string list * string list * string list * string list) =

   (* remove line-ends for easier regexps *)
   let contentopf = blankNewlines contentopf in
   let matcher tag attr : string list =
      let rx = Str.regexp_case_fold ("<dc:" ^ tag ^ attr ^
         "[^>]*>\\([^<]*\\)</dc:" ^ tag ^ ">")
      in
      Str.allMatches rx contentopf (Str.matched_group 1)
   in

   (  matcher "title"      "" ,
      matcher "creator"    "" ,
      matcher "date"       "" ,
      matcher "identifier" "[^>]*opf:scheme=[\"']ISBN[\"']" )


let getHtmlPathnames (contentopf:string) : string list =

   let manifest =
      (* remove line-ends for easier regexps *)
      let contentopf = blankNewlines contentopf in
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
      let l = Str.allMatches rx manifest (Str.matched_string) in
      String.concat "\n" l
   and rx = Str.regexp (head ^ "href=[\"']\\([^\"']*\\)[\"']" ^ foot)
   in

   Str.allMatches rx htmlItems (Str.matched_group 1)


let findFirstIsbn (trace:bool) (text:string) : (string option) =

   match
      (* find 'ISBN' (yield pos of last char) *)
      try
         let rx = Str.regexp_string_case_fold "isbn" in
         let _  = Str.search_forward rx text 0 in
         Some ((Str.match_end ()) - 1)
      with Not_found -> None
   with
   | None     -> None
   | Some pos ->
      (* find number *)

      if trace then print_endline ("   'isbn' pos: " ^ (string_of_int pos)) ;

      let matchIsbnNum (txt:string) (pos:int) : (string option) =
         let matchIsbnH13 , matchIsbnH10 , matchIsbnM13 , matchIsbnM10 =
            let matchG1 (rx:Str.regexp) (len:int) (txt:string) (pos:int)
               : string option =
               if Str.string_match rx txt pos
               then
                  let isbn = Str.matched_group 1 txt in
                  if String.length isbn = len then Some isbn else None
               else None
            in
            (  matchG1 (Str.regexp "[^0-9]\\([0-9]+\\([- ]\\)\
                  [0-9]+\\2[0-9]+\\2[0-9]+\\2[0-9]+\\)[^0-9]") 17
            ,  matchG1 (Str.regexp "[^0-9]\\([0-9]+\\([- ]\\)\
                  [0-9]+\\2[0-9]+\\2[0-9]*[0-9X]\\)[^0-9]") 13
            ,  matchG1 (Str.regexp "[^0-9]\\([0-9]+\\)[^0-9]") 13
            ,  matchG1 (Str.regexp "[^0-9]\\([0-9]+\\)[^0-9]") 10 )
         in
         None
         ||> (fun () -> matchIsbnH13 txt pos)
         ||> (fun () -> matchIsbnH10 txt pos)
         ||> (fun () -> matchIsbnM13 txt pos)
         ||> (fun () -> matchIsbnM10 txt pos)
      in

      let rec searchForward (i:int) (iend:int) : string option =
         if i < iend
         then
            match matchIsbnNum text (pos + i) with
            | None           -> searchForward (i + 1) iend
            | Some _ as isbn -> isbn
         else None
      in
      searchForward 0 (min 15 ((String.length text) - pos))


let getIsbns (trace:bool) (epubPathname:string) (contentopfpath:string)
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
      | Ok a  -> a
      | Err _ -> []
   in

   if trace
   then print_endline ("html files: " ^ (string_of_int (List.length htmls))) ;

   (* filter for ISBN presence *)
   let isbnFiles =
      List.filtmap (function
         | Err _    -> None
         | Ok html  ->
            (* for easier searching: coerce to UTF-8; blank-out line-ends,
               tabs, markup; translate en-dashs to hyphens *)
            let text = html
               |> Utf8filter.filter
               |> unifySpaces
               |> (Str.global_replace (Str.regexp "<[^>]+>") " ")
               |> (Str.global_replace (Str.regexp_string "\xE2\x80\x93") "-")
            in
            mapOpt (fun isbn -> (text,isbn)) (findFirstIsbn trace text)
         ) htmls
   in

   if trace
   then print_endline ("isbn files: " ^
      (string_of_int (List.length isbnFiles))) ;

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

      if trace
      then begin
         print_endline ("isbn ambiguous (" ^
         (string_of_int (List.length loc)) ^ ")") ;
         List.iter (fun (_,isbn) -> print_endline ("   loc: " ^ isbn)) loc
         end ;

      begin match loc with
      | (_ , unique) :: [] -> [unique]
      (* give up *)
      | _                  -> []
      end




(* ---- public functions ---- *)

let extractTadist (trace:bool) (epubPathname:string)
   : (Tadist.nameStructRaw option) eoption =

   match recogniseEpub epubPathname with
   | Err _ as e -> e
   | Ok false   -> Ok None
   | Ok true    ->

      match getContentOpf epubPathname with
      | Err _ as e    -> e
      | Ok (contentopfpath , contentopf) ->

         let titles , authors , dates , isbns =
            getContentopfMetadata contentopf
         in

         if trace
         then begin
            print_endline "" ;
            print_endline ("* titles:  " ^ (String.concat " | " titles)) ;
            print_endline ("* authors: " ^ (String.concat " | " authors)) ;
            print_endline ("* dates:   " ^ (String.concat " | " dates)) ;
            print_endline ("* isbns:   " ^ (String.concat " | " isbns))
            end ;

         (* maybe look for ISBN elsewhere *)
         let isbns = if isbns <> []
            then isbns
            else let htmlPathnames = getHtmlPathnames contentopf in
               let isbns = getIsbns trace epubPathname contentopfpath
                  htmlPathnames in
               if trace then print_endline ("* isbns (b): " ^
                  (String.concat " | " isbns)) ;
               isbns
         in

         Ok (Some Tadist.( {
            titleRaw  = titles ;
            authorRaw = authors ;
            dateRaw   = dates ;
            idRaw     = isbns ;
            subtypRaw = "" ;
            typRaw    = _TYPE } ) )
