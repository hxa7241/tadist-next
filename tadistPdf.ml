(*------------------------------------------------------------------------------

   PDF lib (OCaml 4.10)
   Harrison Ainsworth / HXA7241 : 2015, 2020

   http://www.hxa.name/tools/

   License: AGPL -- https://www.gnu.org/licenses/agpl-3.0.html

------------------------------------------------------------------------------*)




open HxaGeneral
(* using Tadist *)
(* using Camlpdf: Pdf, Pdfpage, Pdfread, Pdftext *)




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


(*
 * Lookup an entry from the /Info dictionary, returning it as a UTF8 string
 *)
let lookupInfoUtf8 (pdf:Pdf.t) (key:string) : string =

   let infodict =
      match Pdf.lookup_direct pdf "/Info" pdf.Pdf.trailerdict with
      | Some infodict -> infodict
      | None          -> Pdf.Dictionary []
   in

   match Pdf.lookup_direct pdf key infodict with
   | Some (Pdf.String s) -> Pdftext.utf8_of_pdfdocstring s
   | Some
      Pdf.(Null | Boolean _ | Integer _ |
         Real _ | Name _ | Array _ |
         Dictionary _ | Stream _ | Indirect _) -> ""
   | None -> ""


let getXmpXml (pdf:Pdf.t) : string =

   (Some pdf.Pdf.trailerdict)
   |>-
   (Pdf.lookup_direct pdf "/Root")
   |>-
   (Pdf.lookup_direct pdf "/Metadata")
   |>-
   (* : ((Pdf.pdfobject * Pdf.stream) Stdlib.ref) option *)
   (fun (pdfo:Pdf.pdfobject) ->
      match pdfo with
      | Pdf.Null    | Pdf.Boolean _    | Pdf.Integer _
      | Pdf.Real _  | Pdf.String _     | Pdf.Name _
      | Pdf.Array _ | Pdf.Dictionary _ | Pdf.Indirect _ -> None
      | Pdf.Stream stream                               -> Some stream)
   |>-
   (fun stream ->
      Pdfcodec.decode_pdfstream pdf (Pdf.Stream stream) ;
      Some stream)
   |>-
   (* : Pdfio.bytes option *)
   (fun stream ->
      match stream with
      | {contents = (_, Pdf.Got data)} -> Some data
      | {contents = (_, Pdf.ToGet _ )} -> None)
   |>-
   (fun b -> Some (Pdfio.string_of_bytes b))
   |>
   (Option.value ~default:"")


let lookupXmlTag (xmp:string) (regex:string) (group:int) : string =

   (Rx.regexFirst regex ~caseInsens:true ~pos:0 xmp)
   |>-
   (Fun.flip Rx.groupFound group)
   |>
   (Option.value ~default:"")


let getDate (pdf:Pdf.t) : string =

   (* format:
    * D:YYYYMMDDHHmmSSOHH'mm'
    * with optionalness: D:YYYY[MM[DD[HH[mm[SS[O[HH'[mm']]]]]]]] *)
   let s = lookupInfoUtf8 pdf "/CreationDate" in

   (* take yearmonthday, or just year *)
   if (String.length s) >= 10
   then
      String.sub s 2 8
   else if (String.length s) >= 6
   then
      String.sub s 2 4
   else
      ""


let getPagecount (pdf:Pdf.t) : int =

   Pdfpage.endpage pdf ;;


let getIsbnsFromMetadata (pdf:Pdf.t) : string list =

   (* define the various matching forms *)
   (* XMP rdf/xml: *)
   let identifierDc (xmp:string) () : string option =
      let tag =
         (* <dc:identifier ...>[content]</dc:identifier> *)
         lookupXmlTag
            xmp
            "<dc:identifier\\( [^>]*\\)?>\\([^<]*\\)</dc:identifier>"
            2
      in
      Tadist.Isbn.search 0 (String.length tag) tag
   and identifierXmp (xmp:string) () : string option =
      let tag =
         (* <xmp.Identifer ...>
               <rdf:Bag ...>
                  <rdf:li ...>[content]</rdf:li>
                  ...
               </rdf:Bag>
            </xmp.Identifer> *)
         lookupXmlTag
            xmp
            "<xmp.Identifer\\( [^>]*\\)?>[^<]*\
               <rdf:\\(Alt\\|Bag\\|Seq\\)\\( [^>]*\\)?>[^<]*\
               <rdf:li[^>]*>\\([^<]*\\)</rdf:li>"
            4
      in
      Tadist.Isbn.search 0 (String.length tag) tag
   (* other inappropriate metadata (unlikely but possible) *)
   and subject (pdf:Pdf.t) () : string option =
      let field = lookupInfoUtf8 pdf "/Subject" in
      Tadist.Isbn.search 0 (String.length field) field
   and keywords (pdf:Pdf.t) () : string option =
      let field = lookupInfoUtf8 pdf "/Keywords" in
      Tadist.Isbn.search 0 (String.length field) field
   in

   let xmp = (getXmpXml pdf) |> Blanks.blankSpacyCtrlChars in

   (* take the first successful match, in this priority of alternatives *)
   (identifierDc xmp ())
   ||> (identifierXmp xmp)
   ||> (subject pdf)
   ||> (keywords pdf)
   |> List_.ofOpt


let unescapeChars (text:string) : string =

   (*
      the various things to do:
      * unescape: \n \r \t \b \f \( \) \\
         * \f -> 0C
      * unencode: \000 (1-3 octal digits, 0-255)
      * remove:   \\n (linewrap: \ and immediately following actual \n)
      * leave:    \ with any other following char
   *)

   let translate (whole:string) : string =
      let found = Str.matched_string whole in
      match found.[1] with
      (* unencode *)
      | '0' .. '7' ->
         begin try
            let code = Scanf.sscanf (String_.trail found 1) "%o" Fun.id
            (* add back extraneous char picked up by regex *)
            and nextChar =
               let last = String_.last found in
               if Char_.isDigitOct last then "" else string_of_char last
            in
            (string_of_char (Char.chr code)) ^ nextChar
         with
         (* failed to read number, so leave it untranslated *)
         | Scanf.Scan_failure _ | Failure _ | End_of_file -> found
         end
      (* unescape *)
      | 'n'  -> "\n"
      | 'r'  -> "\r"
      | 't'  -> "\t"
      | 'b'  -> "\b"
      | 'f'  -> "\x0C"
      | '('  -> "("
      | ')'  -> ")"
      | '\\' -> "\\"
      (* remove *)
      | '\n' -> ""
      (* leave *)
      | _    -> found

   and rx = Str.regexp
      (  {|\\[0-7][0-7][0-7]\||} ^
         {|\\[0-7][0-7][^0-7]\||} ^
         {|\\[0-7][0-7]$\||} ^
         {|\\[0-7][^0-7]\||} ^
         {|\\[0-7]$\||} ^
         {|\\[^0-7]|}  )
   in

   Str.global_substitute rx translate text


let extractTextFromPdfStream (pdfStream:string) : string =

   (*
   let extractHexString (pdfStream:string) (openPos:int) : (string * int) =
      (* PDF 1.3 ref: 3.2.3
         <1A70D5...>
         * unspaced pairs of hex digits, each meaning a byte
         * ignore any blanks
         * if uneven total count, then assume the missing last is one 0 *)
      (* seek closing '>' *)
      let closePos =
         (String_.index '>' ~start:(openPos + 1) pdfStream)
         |> (Option.value ~default:(String.length pdfStream))
      in
      (* extract sub-string *)
      (String_.subp pdfStream (openPos + 1) closePos)
      (* remove non hex-digits *)
      |> (String_.filter Char_.isDigitHex)
      (* if odd length, add trailing 0 *)
      |> (fun s -> if (String.length s) mod 2 = 1 then s ^ "0" else s)
      (* translate digit pairs to bytes *)
      |> (Rx.allMatches (Rx.compile ".."))
      |> (List.map
         (fun hexPair ->
            let code = Scanf.sscanf hexPair "%x" Fun.id in
            (string_of_char (Char.chr code))) )
      (*|> (Str.global_substitute (Rx.compile "..")
         (fun whole ->
            let found = Str.matched_string whole in
            let code  = Scanf.sscanf found "%x" Fun.id
            (string_of_char (Char.chr code))))*)
      |> (String.concat "")
      (* return byte-string and end pos *)
      |> (fun str -> (str , closePos))
   *)

   let extractParenString (pdfStream:string) (startPos:int) : (string * int) =
      (* strings are in ()s,
         but those strings can include inner unescaped ()s if balanced *)
      let isParen (c:char) : bool = (c = '(') || (c = ')') in
      let rec seekParen (pdfStream:string) (seekPos:int)
         (nestDepth:int) (openPos:int)
         : (string * int) =
         let foundPosOpt = String_.indexp isParen ~start:seekPos pdfStream in
         match foundPosOpt with
         | Some foundPos ->
            let foundChar =
               let isUnEscaped =
                  (foundPos = 0) || (pdfStream.[foundPos - 1] <> '\\')
               in
               if isUnEscaped then pdfStream.[foundPos] else ' '
            and foundEnd = foundPos + 1
            in
            begin match foundChar with
            | '(' ->
               (* only set open pos at bottom-level open (others are ignored) *)
               let openPos = if nestDepth = 0 then foundEnd else openPos
               (* (nesting cannot overflow because max string len < max int) *)
               and nestDepth = (min nestDepth (Int.max_int - 1)) + 1 in
               seekParen pdfStream foundEnd nestDepth openPos
            | ')' ->
               (* only recurse if inside nested parens *)
               if nestDepth > 1
               then
                  let nestDepth = nestDepth - 1 in
                  seekParen pdfStream foundEnd nestDepth openPos
               else
                  (* extract parenthised string *)
                  (String_.subp pdfStream openPos foundPos , foundPos)
            (* it was escaped, so ignorable *)
            | _ ->
               seekParen pdfStream foundEnd nestDepth openPos
            end
         (* end of string reached *)
         | None ->
            (* extract string to end *)
            let streamEndPos = String.length pdfStream in
            (String_.subp pdfStream openPos streamEndPos , streamEndPos)
      in
      let str , endPos = seekParen pdfStream startPos 0 0 in
      (unescapeChars str) , endPos
   in

   let rec seekStrings (pdfStream:string) (seekPos:int) (accum:string list)
      : string list =
      let isOpen (c:char) : bool = (c = '(') (*|| (c = '<')*) in
      let foundPosOpt = String_.indexp isOpen ~start:seekPos pdfStream in
      match foundPosOpt with
      | Some openPos ->
         let foundString , closePos =
            match pdfStream.[openPos] with
            | '(' -> extractParenString pdfStream openPos
            (*| '<' -> extractHexString   pdfStream openPos*)
            (* impossible *)
            | _   -> ("" , String.length pdfStream)
         in
         seekStrings pdfStream (closePos + 1) (foundString :: accum)
      | None ->
         accum
   in

   (seekStrings pdfStream 0 [])
   |> List.rev
   (* remove empties *)
   |> (List_.filtmap (Option_.classify String_.notEmpty))
   (* put spaces between each chunk
      (PDF text content is often broken into words, or more, per ()) *)
   |> (String.concat " ")


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


let rec getPageStreamString (pdf:Pdf.t) (obj:Pdf.pdfobject) : string =

   try

      match obj with

      (* ignore *)
      | Pdf.Null   | Pdf.Boolean _    | Pdf.Integer _  | Pdf.Real _
      | Pdf.Name _ | Pdf.Dictionary _ | Pdf.String _ ->
         ""

      (* merge a 'sub-tree' *)
      | Pdf.Array objs ->
         let strs = List.map (getPageStreamString pdf) objs in
         String.concat "\n" strs

      (* follow 'pointer' *)
      | Pdf.Indirect _ ->
         getPageStreamString pdf (Pdf.direct pdf obj)

      (* this is the main thing *)
      | Pdf.Stream stream ->
         Pdfcodec.decode_pdfstream pdf (Pdf.Stream stream) ;
         begin match stream with
         | {contents = (_, Pdf.Got data)} -> Pdfio.string_of_bytes data
         | {contents = (_, Pdf.ToGet _ )} -> ""
         end

   with
   | _ -> ""


let getTextPages (pdf:Pdf.t) (pageCount:int) : string list =

   (* get first few pages *)
   let pages : Pdfpage.t list =
      try
         let pages = Pdfpage.pages_of_pagetree pdf in
         fst (List_.bisect pages (max 0 pageCount))
      with
      | _ -> []
   in

   (* map pages to streams *)
   let streams : string list =
      List.map
         (fun (page : Pdfpage.t) : string ->
            let objs : Pdf.pdfobject list = page.Pdfpage.content in
            let strings : string list =
               List.map
                  (fun (obj : Pdf.pdfobject) -> getPageStreamString pdf obj)
                  objs
            in
            String.concat "\n" strings )
         pages
   in

   (* map streams to texts *)
   List.map extractTextFromPdfStream streams


let getTextPagesExternal (pdfPathname:string) (pageCount:int) : string list =

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

   let text : string =
      try
         let toolOutChannel : in_channel =
            let command : string =
               let _TOOL_AND_OPTIONS =
                  "pdftotext -q -enc UTF-8 -eol unix -l " ^
                  (string_of_int (max 0 pageCount))
               and _FILES_IO = pdfPathname ^ " -" in
               _TOOL_AND_OPTIONS ^ " " ^ _FILES_IO
            in
            (* exceptions: Unix.Unix_error *)
            Unix.open_process_in command
         in

         let toolOutput : string = HxaGeneral.inputString toolOutChannel 512 in

         (* exceptions: Unix.Unix_error *)
         let _ = Unix.close_process_in toolOutChannel in

         toolOutput
      with
      | Unix.Unix_error _ ->
         ""
   in

   (* split into pages by ascii form-feed char *)
   String.split_on_char '\x0C' text


let getIsbnsFromTextPages (texts:string list) : string list =

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
               %> (Rx.regexFirst "libraryofcongress" ~pos:0 ~caseInsens:true)
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


let getIsbns (pdf:Pdf.t) (pdfPathname:string) : string list =

   let isbnsFromTextPages =
      let _NUMBER_OF_PAGES_TO_INSPECT = 10 (* 7 *) in
      let internalSourced =
         getIsbnsFromTextPages (getTextPages pdf _NUMBER_OF_PAGES_TO_INSPECT)
      in
      (* if text extraction fails, try external method *)
      if List_.notEmpty internalSourced
      then
         internalSourced
      else
         getIsbnsFromTextPages
            (getTextPagesExternal pdfPathname _NUMBER_OF_PAGES_TO_INSPECT)
   in

   (List.append isbnsFromTextPages (getIsbnsFromMetadata pdf))
   |> List_.deduplicate




(* ---- public functions ---- *)

let extractTadist (pdfPathname:string) : (Tadist.nameStructRaw option) ress =

   match recognisePdf pdfPathname with
   | Error _ as e -> e
   | Ok false     -> Ok None
   | Ok true      ->
      try
         let pdf = Pdfread.pdf_of_file None None pdfPathname in

         Ok (Some Tadist.{
            titleRaw  = [ lookupInfoUtf8 pdf "/Title" ] ;
            authorRaw = [ lookupInfoUtf8 pdf "/Author" ] ;
            dateRaw   = [ getDate pdf ] ;
            idRaw     = getIsbns pdf pdfPathname ;
            subtypRaw = string_of_int (getPagecount pdf) ;
            typRaw    = _TYPE ;
         } )

      with
      | _ -> Error "PDF read failed"
