(*------------------------------------------------------------------------------

   PDF lib (OCaml 4.10)
   Harrison Ainsworth / HXA7241 : 2015, 2020

   http://www.hxa.name/tools/

   License: AGPL -- https://www.gnu.org/licenses/agpl-3.0.html

------------------------------------------------------------------------------*)




open HxaGeneral
(* using Tadist *)




(* ---- values ---- *)

let _TYPE = "pdf"




(* ---- functions ---- *)

let recognisePdf (pdfPathname:string) : bool ress =

   try
      (* (assuming this can fail) *)
      let file = open_in_bin pdfPathname in

      let recognised =
         let pdfId = "\x25\x50\x44\x46\x2D"
         and bom   = "\xEF\xBB\xBF" in
         let readString file pos len =
            (* (assuming these cannot fail, in a deeper IO sense) *)
            try seek_in file pos ; really_input_string file len with _ -> ""
         in
         let first8Bytes = (readString file 0 8) in
         (* check pdf id as first, or BOM first then pdf id *)
         ((String_.lead first8Bytes 5) = pdfId) || (first8Bytes = bom ^ pdfId)
      in

      close_in_noerr file ;
      Ok recognised
   with
   | _ -> Error ("cannot open/read file: " ^ pdfPathname)


let toolInvoke (command:string) : string ress =

   let environ : string list =
      (* use path and this dir so tool can be found *)
      let path = try Unix.getenv "PATH" with | _ -> "" in
      [ "PATH=." ^ (if path <> "" then (":" ^ path) else "") ]
   in

   (commandLineInvoke command environ "" 1024)
   |>
   (* shorten and preface the error output *)
   (Result.map_error
      (fun msg ->
         let msg = String_.lead msg (min 40 (String_.indexl '\n' msg)) in
         "tool failure" ^ (if msg <> "" then (": " ^ msg) else "")))


let getMetadata (pdfPathname:string) : (string * string) ress =

   (*
      using xpdf pdftotext tool:
      * https://www.xpdfreader.com/pdfinfo-man.html

      with the command:
      $ pdfinfo -enc UTF-8 -rawdates -meta somedocument.pdf

      ie:
      * UTF-8 encoding
      * non human-readable-ised dates
      * print xml metadata
      * input file somedocument.pdf
   *)

   let invoke (options:string) : string ress =
      toolInvoke ("pdfinfo -enc UTF-8 -rawdates " ^ options ^ " " ^ pdfPathname)
   in

   let infoRaw : string ress =
      (invoke "")
      |>
      (Result.map Blanks.unifyNonNewlines)
   (*
   let infoRaw : (string * string) list ress =
      (invoke "")
      (* parse into assoc list *)
      |> (Result.map (fun str ->
         str
         |> Blanks.unifyNonNewlines
         (* separate lines : string list *)
         |> (String_.split ((=) '\n'))
         (* extract name-value pairs : (string * string) option list *)
         |> (String_.halve ':')
         (* discard invalid : (string * string) list *)
         |> (List.filtmap id)
         (* trim all : (string * string) list *)
         |> (List.map
            (fun (name,value) -> (String.trim name , String.trim value)))
         (* remove duplicates : (string * string) list *)
         |> List_.deduplicate))
   *)
   and xmpRaw  : string ress =
      (invoke "-meta ")
      (* extract all xml : rxmatch option *)
      |>
      (Result.map (fun str ->
         (* find open and close xml tags : int option , int option *)
         let openTagPos_o =
            (Rx.regexSeek {|<rdf:RDF[^>]*>|} str)
            |>-
            (Rx.wholePos %> fst %> Option.some)
         and closeTagEnd_o =
            (Rx.regexSeek {|</rdf:RDF>|} str)
            |>-
            (Rx.wholePos %> snd %> Option.some)
         in
         (* both or nothing : (int * int) option *)
         (Option_.and2 openTagPos_o closeTagEnd_o)
         |>-
         (* extract that substring : string option *)
         (fun (openTagPos , closeTagEnd) ->
            Some (String_.subpc str openTagPos closeTagEnd))
         |>
         (* default to empty string *)
         String_.ofOpt ))
   in

   let xmpNoNewlines = Result.map Blanks.unifySpaces xmpRaw in

   Result_.ressOr2 "\n" ("","") (infoRaw , xmpNoNewlines)


let lookupInfoValue (info:string) (key:string) : string =

   let rxs = {|^|} ^ key ^ {| *: *\(.*\)$|} in

   (* : rxmatch option *)
   (Rx.regexSeek rxs info)
   |>-
   (* : string option *)
   ((Fun.flip Rx.groupFound) 1)
   |>
   (* : string *)
   String_.ofOpt

   (*
   (List.assoc_opt key info)
   |>
   String_.ofOpt
   *)


let lookupXmlValue (xmp:string) (regex:string) (group:int) : string =

   (Rx.regexSeek regex ~caseInsens:true ~pos:0 xmp)
   |>-
   (Fun.flip Rx.groupFound group)
   |>
   String_.ofOpt


let lookupMetadataValue (metadata:string*string) (key:string) : string =

   let info , _ = metadata in

   lookupInfoValue info key


let getDate (metadata:string*string) : string =

   (* format:
    * D:YYYYMMDDHHmmSSOHH'mm'
    * with optionalness: D:YYYY[MM[DD[HH[mm[SS[O[HH'[mm']]]]]]]] *)
   let s = lookupMetadataValue metadata "CreationDate" in

   (* take yearmonthday, or just year *)
   if (String.length s) >= 10
   then
      String.sub s 2 8
   else if (String.length s) >= 6
   then
      String.sub s 2 4
   else
      ""


let getPagecount (metadata:string*string) : string =

   lookupMetadataValue metadata "Pages"


let getIsbnsFromMetadata (metadata:string*string) : string list =

   (* main metadata fields: XMP rdf/xml: *)

   let identifierDc (xmp:string) : string list =
      let tag =
         (* <dc:identifier ...>[content]</dc:identifier> *)
         lookupXmlValue
            xmp
            {|<dc:identifier\([^>]*\)?>\([^<]*\)</dc:identifier>|}
            2
      in
      Tadist.Isbn.searchByChecksum 0 tag

   and identifierXmp (xmp:string) : string list =
      (* TODO: check and test extract array elements *)
      (* <xmp:Identifier ...>
            <rdf:Bag ...>
               <rdf:li ...>
                  <xmpidq:Scheme>mobi-asin</xmpidq:Scheme>
                  <rdf:value>B00BR40XJ6</rdf:value>
               </rdf:li>
               ...
            </rdf:Bag>
         </xmp:Identifier> *)
      (* find element containing value array : string *)
      (lookupXmlValue
         xmp
         ({|<xmp:Identifier\([^>]*\)?>|}
         ^ {|[^<]*<rdf:\(Alt\|Bag\|Seq\)\([^>]*\)?>|}
         ^ {|\(.*\)|}
         ^ {|</rdf:\(Alt\|Bag\|Seq\)>|})
         4)
      |>
      (* find all <rdf:li>s : string list *)
      (Rx.allMatches (Rx.compile {|<rdf:li\([^>]*\)?>.</rdf:li>|}))
      |>
      (* map to label and value : (string * string) list *)
      (List.map
         (fun (item : string) ->
            let seekGroup1 (rx:string) (s:string) : string =
               (Rx.regexSeek rx s)
               |>-
               ((Fun.flip (Rx.groupFound)) 1)
               |>
               String_.ofOpt
            in
            ( (seekGroup1 {|<xmpidq:Scheme>\([^<]*\)</xmpidq:Scheme>|} item)
            , (seekGroup1 {|<rdf:value>\([^<]*\)</rdf:value>|} item) ) ))
      |>
      (* filter-in by checksum validity : (string * string) list *)
      List_.filtmap
         (fun ((label , value) : (string * string)) ->
            (Tadist.Isbn.searchByChecksum 0 value)
            |> List_.toOpt
            |> (Option.map (fun isbn -> label , isbn)))
      |>
      (* sort by ISBN labelled before not : (string * string) list *)
      (List.stable_sort
         (fun (lbl0 , _) (lbl1 , _) ->
            compare
               (Option_.toBool (Rx.regexSeek ~caseInsens:true "isbn" lbl0))
               (Option_.toBool (Rx.regexSeek ~caseInsens:true "isbn" lbl1))
               ))
      |>
      (* discard labels : string list *)
      (List.split %> snd)

   (* other inappropriate metadata fields (unlikely but possible) *)

   and subject (metadata:string*string) : string list =
      let field = lookupMetadataValue metadata "Subject" in
      Tadist.Isbn.searchByChecksum 0 field

   and keywords (metadata:string*string) : string list =
      let field = lookupMetadataValue metadata "Keywords" in
      Tadist.Isbn.searchByChecksum 0 field
   in

   let _ , xmp = metadata in

   (* concat all, in this priority *)
   (identifierDc xmp)
   @ (identifierXmp xmp)
   @ (subject metadata)
   @ (keywords metadata)


let getTextPages (pdfPathname:string) : (string list) ress =

   (*
      using xpdf pdftotext tool:
      * https://www.xpdfreader.com/pdftotext-man.html

      with the command:
      $ pdftotext -q -enc UTF-8 -eol unix -l 10 somedocument.pdf -

      ie:
      * no error messages
      * UTF-8 encoding
      * unix line-endings
      * first 10 pages
      * input file somedocument.pdf
      * output to stdout
   *)

   let text : string ress =
      let _NUMBER_OF_PAGES_TO_INSPECT = 10 (* 7 *) in
      toolInvoke
         ("pdftotext -q -enc UTF-8 -eol unix -l "
         ^ (string_of_int _NUMBER_OF_PAGES_TO_INSPECT) ^ " "
         ^ pdfPathname ^ " -")
   in

   text
   |>=
   (* split into pages by ascii form-feed char : (string list) ress *)
   ((String.split_on_char '\x0C') %> Result.ok)


let regulariseDashs (replacement:string) (text:string) : string =

   (* just a list of plausible variations, not a complete countermeasure to
      every possible villainy and madness *)

   (* HYPHEN-MINUS (ordinary '-') *)
   (* SOFT HYPHEN *)
   (* MINUS SIGN *)
   (* HYPHEN *)
   (* NON-BREAKING HYPHEN *)
   (* FIGURE DASH *)
   (* EN DASH *)
   (* EM DASH *)
   (* HORIZONTAL BAR *)
   (* SMALL EM DASH *)
   (* SMALL HYPHEN-MINUS *)
   (* FULLWIDTH HYPHEN-MINUS *)
   let rx = Str.regexp
      "-\\|\
      \xC2\xAD\\|\
      \xE2\x88\x92\\|\
      \xE2\x80\x90\\|\
      \xE2\x80\x91\\|\
      \xE2\x80\x92\\|\
      \xE2\x80\x93\\|\
      \xE2\x80\x94\\|\
      \xE2\x80\x95\\|\
      \xEF\xB9\x98\\|\
      \xEF\xB9\xA3\\|\
      \xEF\xBC\x8D"
   in

   Str.global_replace rx replacement text


let getIsbnsFromText (texts:string list) : string list =

   (* map texts to isbns *)
   let isbnsAll : string list list =
      (List.map
         (fun (text : string) : string list ->
            (*
               example stream fragment:
                  0.0287 Tc 9.3 0 0 9.3 151.14 89.04 Tm
                  (ISBN-10: )Tj
                  0.0429 Tc 9.5 0 0 9.5 176.06 89.04 Tm
                  (0-674-53751-3 )Tj
                  0.0142 Tc -5.931 -3.586 Td

               extracted text:
                  ISBN-10:  0-674-53751-3

               * strings can be fragmented (or even reordered, but ignore that)
               * spaces may be absent, or spurious, hence are meaningless
               * hyphens may be some other similar looking char

               handles (with/without preceding/following digits/etc):
               * ISBN 555555555X
               * ISBN: 555555555X
               * ISBN-10: 555555555X
               * ISBN-10 555555555X
               * ISBN13: 5555555555555
               * ISBNs 555555555X...555555555X...555555555X...
               * ISBN-10 ISBN-13 e-ISBN 555555555X...555555555X...555555555X...

               but not:
               * ISBN 10 555555555X
            *)

            text
            (* remove all spaces *)
            |> Blanks.unifySpaces |> (String_.filter ((<>)' '))
            (* remove '-10' '-13' ISBN suffixs, and any chars up to digits *)
            |> (Str.global_replace
               (Str.regexp_case_fold {|ISBN\([^0-9]1[03]:?\|1[03]:\)?[^0-9]*|})
               "ISBN")
            (* remove dashes *)
            |> (regulariseDashs "")
            (* extract all label+number by regex *)
            |> (Rx.allMatches
               (Rx.compile ~caseInsens:true
               ({|ISBN[^0-9]*|} ^
               {|\(97[89][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]\||} ^
               {|[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][xX0-9]\)|} )))
            (* remove 'ISBN' labels *)
            |> (List.map ((Fun.flip String_.trail) 4))
            (* disambiguate 10/13 by checksum *)
            |> (List.map
               (fun number ->
                  if String.length number < 13
                  then number
                  else
                     if Tadist.Isbn.check number
                     then number
                     else String_.lead number 10) )
            )
         texts)
   in

   (* select priority isbns: first few on copyright page *)
   let isbns : string list =
      (* check for 'library of congress', indicating copyright page *)
      let isCopyrightPages : bool list =
         (List.map
            (
               Blanks.unifySpaces
               (* remove all spaces *)
               %> (String_.filter ((<>)' '))
               (* search for marker *)
               %> (Rx.regexSeek "libraryofcongress" ~pos:0 ~caseInsens:true)
               %> Option_.toBool
            )
            texts)
      in
      (List.combine isCopyrightPages isbnsAll)
      (* move copyright pages to the front *)
      |> (List.stable_sort (fun (b0 , _) (b1 , _) -> ~-(compare b0 b1)))
      |> (List.split %> snd)
      |> List.flatten
      |> List_.deduplicate
      (* take first 2 *)
      |> (((Fun.flip List_.bisect) 2) %> fst)
   in

   isbns


let getIsbns (metadata:string*string) (texts:string list) : string list =

   (List.append
      (getIsbnsFromText texts)
      (getIsbnsFromMetadata metadata))
   |>
   List_.deduplicate




(* ---- public functions ---- *)

let extractTadist (pdfPathname:string) : (Tadist.nameStructRaw option) ress =

   match recognisePdf pdfPathname with
   | Error _ as e -> e
   | Ok false     -> Ok None
   | Ok true      ->

      (* try to get basic data *)
      (getMetadata pdfPathname , getTextPages pdfPathname)
      |>
      (* if either data source failed, make the best of it with one,
         unless both failed, then concede defeat *)
      (Result_.ressOr2 "\n" (("",""),[]))
      |>=
      (fun (metadata , text) ->
         (* extract and package chosen data *)
         Ok (Some Tadist.{
            titleRaw  = [ lookupMetadataValue metadata "Title" ] ;
            authorRaw = [ lookupMetadataValue metadata "Author" ] ;
            dateRaw   = [ getDate metadata ] ;
            idRaw     = getIsbns metadata text ;
            subtypRaw = getPagecount metadata ;
            typRaw    = _TYPE ;
         } ) )
