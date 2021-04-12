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

let recognisePdf_x (trace:bool) (pdfPathname:string) : bool =

   traceHead trace __MODULE__ "recognisePdf_x" "" ;

   try
      (* exception raiser: and if so, everything is indeed in vain --
         cannot open file => the program can do nothing *)
      let file = open_in_bin pdfPathname in

      let recognised =
         let pdfId = "\x25\x50\x44\x46\x2D"
         and bom   = "\xEF\xBB\xBF" in
         let readString (file:in_channel) (pos:int) (len:int) : string =
            (* exception raisers: and if so, everything is indeed in vain --
               cannot read file => the program can do nothing *)
            try
               seek_in file pos ;
               really_input_string file len
            with
            | x ->
               close_in_noerr file ;
               raise x
         in
         let first8Bytes = (readString file 0 8) in
         (* check pdf id as first, or BOM first then pdf id *)
         let isPdfId =
            ((String_.lead first8Bytes 5) = pdfId) ||
            (first8Bytes                  = bom ^ pdfId)
         in

         traceString trace "pdf id: " (if isPdfId then "true" else "false") ;

         isPdfId
      in

      close_in_noerr file ;
      recognised

   with
   | Sys_error msg ->
      let message =
         Printf.sprintf "cannot open/read file: %s (%s)" pdfPathname msg
      in
      traceString trace "*** Error: " message ;
      raise (Intolerable (EXIT_NOINPUT , message))


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


let getMetadata (trace:bool) (pdfPathname:string) : (string * string) ress =

   (*
      using xpdf pdfinfo tool:
      * https://www.xpdfreader.com/pdfinfo-man.html

      with the command:
      $ pdfinfo -enc UTF-8 -rawdates -meta somedocument.pdf

      ie:
      * UTF-8 encoding
      * non human-readable-ised dates
      * print xml metadata
      * input file somedocument.pdf
   *)

   traceHead trace __MODULE__ "getMetadata" "" ;

   let invoke (options:string) : string ress =
      (toolInvoke
         ("pdfinfo -enc UTF-8 -rawdates "
         ^ options ^ " "
         ^ (quoteShellPathname pdfPathname)))
      |>
      (Result.map Utf8.Filter.replace)
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

   traceRess trace ""   id infoRaw ;
   traceRess trace "\n" id xmpRaw ;

   let xmpNoNewlines = Result.map Blanks.unifySpaces xmpRaw in

   Result_.ressOr2 "; " ("","") (infoRaw , xmpNoNewlines)


let lookupInfoValue (info:string) (key:string) : string =

   (* for lines of: "key: value\n" *)
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


(* A flattened sequence of all matched tags -- so they must not be nesting. *)
let extractXmlTagsFlat (tagName:string) (xml:string) : string list =

   (* expecting successive pairs of open and close (no nesting) *)

   let opens : (string * int) list =
      Rx.allMatchesPos (Rx.compile ("<"  ^ tagName ^ "[ >]")) xml
   and closes : (string * int) list =
      Rx.allMatchesPos (Rx.compile ("</" ^ tagName ^ "[ >]")) xml
   in
   (* : (string * int) list * (string * int) list *)
   (List_.equalenTruncate opens closes)
   |>
   (* : ((string * int) * (string * int)) list *)
   (fun (opens , closes) -> (List.combine opens closes))
   |>
   (* get contents only, not enclosing tags : string list *)
   (List.map
      (fun ((_ , openPos) , (_, closePos)) ->
         let openAndContent = String_.subpc xml openPos closePos in
         match (String_.halve '>' openAndContent) with
         | Some (_ , second) -> second
         | None              -> openAndContent ))


(* First matched tag -- must not be nestable. *)
let extractXmlOneTagFlat (tagName:string) (xml:string) : string =

   (extractXmlTagsFlat tagName xml)
   |>
   (List_.hd %> String_.ofOpt)


(* Array inside a single matched tag -- must not be nestable. *)
let extractXmlArrayFlat (tagName:string) (xml:string) : string list =

   (extractXmlOneTagFlat tagName xml)
   |>
   (extractXmlTagsFlat "rdf:li")


(* First of array inside a single matched tag -- must not be nestable. *)
let extractXmlArrayFirstFlat (tagName:string) (xml:string) : string =

   (extractXmlOneTagFlat tagName xml)
   |>
   (extractXmlOneTagFlat "rdf:li")


let lookupMetadataValues (metadata:string*string) (key:string) : string list =

   (* merge info and xmp items by choosing one, not accumulating both *)

   let info , xmp = metadata in

   (* first, look in info dictionary *)
   match (lookupInfoValue info key) with
   | ""    ->
      (* if nothing found, try equivalents in xmp *)
      begin match key with
      | "Title" ->
         (* <dc:title>
              <rdf:Alt>
                <rdf:li xml:lang="x-default">Six Easy Pieces</rdf:li>
              </rdf:Alt>
            </dc:title> *)
         (extractXmlArrayFirstFlat "dc:title" xmp)
         |> (fun s -> [ s ])
      | "Author" ->
         (* <dc:creator>
              <rdf:Seq>
                <rdf:li>Richard Feynman</rdf:li>
              </rdf:Seq>
            </dc:creator> *)
         extractXmlArrayFlat "dc:creator" xmp
      | "CreationDate" ->
         let dcDate =
            (* <dc:date>
                 <rdf:Seq>
                   <rdf:li>YYYY-MM-DDThh:mm:ss.sTZD</rdf:li>
                 </rdf:Seq>
               </dc:date> *)
            extractXmlArrayFlat "dc:date" xmp
         and xmpDate =
            (* <xmp:CreateDate>YYYY-MM-DDThh:mm:ss.sTZD</xmp:CreateDate> *)
            [ extractXmlOneTagFlat "xmp:CreateDate" xmp ]
         in
         (dcDate @ xmpDate)
         |>
         (List.map
            (fun date ->
               (* convert from "YYYY-MM-DDThh:mm:ss.sTZD" to "D:YYYYMMDD" *)
               (String_.lead date 10)
               |>
               (String_.filter ((<>)'-'))
               |>
               ((^) "D:") ))
      | "Subject"->
         (* <dc:description>
              <rdf:Alt>
                <rdf:li xml:lang="x-default"> ... </rdf:li>
              </rdf:Alt>
            </dc:description> *)
         (extractXmlArrayFirstFlat "dc:description" xmp)
         |> (fun s -> [ s ])
      | "Keywords" ->
         let subject =
            (* <dc:subject>
                 <rdf:Bag>
                   <rdf:li> ... </rdf:li>
                 </rdf:Bag>
               </dc:subject> *)
            extractXmlArrayFlat "dc:subject" xmp
         and keywords =
            (* <pdf:keywords> ... </pdf:keywords> *)
            [ extractXmlOneTagFlat "pdf:keywords" xmp ]
         in
         (subject @ keywords)
         |> (List.filter String_.notEmpty)
         |> (String.concat ", ")
         |> (fun s -> [ s ])
      | _ -> [ "" ]
      end
   | value ->
      (* use the value in info dictionary, and ignore xmp *)
      [ value ]


let lookupMetadataValue (metadata:string*string) (key:string) : string =

   (* take the first only, defaulting none to an empty string *)

   (lookupMetadataValues metadata key)
   |>
   (List_.hd %> String_.ofOpt)


let getDates (metadata:string*string) : string list =

   (* format:
    * D:YYYYMMDDHHmmSSOHH'mm'
    * with optionalness: D:YYYY[MM[DD[HH[mm[SS[O[HH'[mm']]]]]]]] *)

   (lookupMetadataValues metadata "CreationDate")
   |>
   List.map
      (fun (s:string) ->
         (* take yearmonthday, or just year *)
         if (String.length s) >= 10
         then
            String.sub s 2 8
         else if (String.length s) >= 6
         then
            String.sub s 2 4
         else
            "" )


let getIsbnsFromMetadata (metadata:string*string) : string list =

   (* main metadata fields: XMP rdf/xml: *)

   let identifierDc (xmp:string) : string list =
      (extractXmlOneTagFlat "dc:identifier" xmp)
      |>
      (Tadist.Isbn.searchByChecksum 0 ~len:(-1))

   and identifierXmp (xmp:string) : string list =
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
      (* : string list *)
      (extractXmlTagsFlat "xmp:Identifier" xmp)
      |>
      (* there should be only 1 *)
      (List_.hd %> String_.ofOpt)
      |>
      (* find all <rdf:li>s : string list *)
      (extractXmlTagsFlat "rdf:li")
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
            ~-(compare
               (Option_.toBool (Rx.regexSeek ~caseInsens:true "isbn" lbl0))
               (Option_.toBool (Rx.regexSeek ~caseInsens:true "isbn" lbl1)))))
      |>
      (* discard labels : string list *)
      (List.split %> snd)

   (* other inappropriate metadata fields (unlikely but possible) *)

   and subject (metadata:string*string) : string list =
      (lookupMetadataValue metadata "Subject")
      |>
      (Tadist.Isbn.searchByChecksum 0 ~len:(-1))

   and keywords (metadata:string*string) : string list =
      (lookupMetadataValue metadata "Keywords")
      |>
      (Tadist.Isbn.searchByChecksum 0 ~len:(-1))
   in

   let _ , xmp = metadata in

   (* concat all, in this priority *)
   (identifierDc xmp)
   @ (identifierXmp xmp)
   @ (subject metadata)
   @ (keywords metadata)


let getTextPages (trace:bool) (pdfPathname:string) : (string list) ress =

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

   traceHead trace __MODULE__ "getTextPages" "" ;

   let text : string ress =
      let _NUMBER_OF_PAGES_TO_INSPECT = 10 (* 7 *) in
      (toolInvoke
         ("pdftotext -q -enc UTF-8 -eol unix -l "
         ^ (string_of_int _NUMBER_OF_PAGES_TO_INSPECT) ^ " "
         ^ (quoteShellPathname pdfPathname)
         ^ " -"))
      |>
      (Result.map Utf8.Filter.replace)
   in

   traceRess trace "" (String.length %> string_of_int) text ;

   text
   |>=
   (* split into pages by ascii form-feed char : (string list) ress *)
   ((String.split_on_char '\x0C') %> Result.ok)




(* ---- public functions ---- *)

let extractTadist_x (trace:bool) (pdfPathname:string)
   : Tadist.nameStructRaw option =

   if recognisePdf_x trace pdfPathname

   (* recognised as pdf *)
   then

      (* try to get basic data *)
      (getMetadata trace pdfPathname
         , getTextPages trace pdfPathname)
      |>
      (* if either data source failed, make the best of it with one,
         unless both failed, then concede defeat *)
      (Result_.ressOr2 "\n" (("","") , []))
      |>
      (Result_.toExc_x (fun s -> Intolerable (EXIT_DATAERR , s)))
      |>
      (fun (metadata , text) ->
         (* get chosen metadata *)
         let titles  = [ lookupMetadataValue metadata "Title" ]
         and authors = lookupMetadataValues metadata "Author"
         and dates   = getDates metadata
         and isbns   = getIsbnsFromMetadata metadata
         and pages   = lookupMetadataValue metadata "Pages" in

         traceHead trace __MODULE__ "extractTadist" "ISBNs in metadata" ;
         traceString trace "" (String.concat " | " isbns) ;

         (* add ISBNs found in text *)
         let isbns =
            (List.append
               (Tadist.Isbn.extractIsbnsFromText trace 2 text)
               isbns)
            |>
            List_.deduplicate
         in

         let nsr =
            Tadist.{
               titleRaw  = titles ;
               authorRaw = authors ;
               dateRaw   = dates ;
               idRaw     = isbns ;
               subtypRaw = pages ;
               typRaw    = _TYPE }
         in

         traceHead trace __MODULE__ "extractTadist" "raw metadata" ;
         traceString trace "" (Tadist.rawToString nsr) ;

         Some nsr )

   (* not recognised as pdf *)
   else
      None
