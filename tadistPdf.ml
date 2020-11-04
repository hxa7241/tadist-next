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
         let readString file pos len =
            (* (assuming these cannot fail, in a deeper IO sense) *)
            try seek_in file pos ; really_input_string file len with _ -> ""
         in
         (* check pdf id ("%PDF-") *)
         (readString file 0 5) = "\x25\x50\x44\x46\x2D"
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


let extractTextFromPdfStream (pdfStream:string) : string =

   let extractParenthised (pdfStream:string) : string =
      (* strings are in ()s *)
      (* BUT those strings can include inner unescaped ()s if balanced *)
      let isParen (c:char) : bool = (c = '(') || (c = ')') in
      let rec seek (pdfStream:string) (seekpos:int)
         (nestdepth:int) (openpos:int) (accum:string list)
         : string list =
         let foundPosOpt = String_.indexp isParen ~start:seekpos pdfStream in
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
               let openpos = if nestdepth = 0 then foundEnd else openpos
               (* (nesting cannot overflow because max string len < max int) *)
               and nestdepth = (min nestdepth (Int.max_int - 1)) + 1 in
               seek pdfStream foundEnd nestdepth openpos accum
            | ')' ->
               (* only accum at bottom close (which must follow a bottom open) *)
               let accum =
                  if nestdepth = 1
                  then (String_.subp pdfStream openpos foundPos) :: accum
                  else accum
               (* disallow negative nestdepths *)
               and nestdepth = max 0 (nestdepth - 1) in
               seek pdfStream foundEnd nestdepth openpos accum
            | _ ->
               (* it was escaped, so ignorable *)
               seek pdfStream foundEnd nestdepth openpos accum
            end
         | None ->
            (* if inside bottom paren, accum from openpos to end *)
            if nestdepth > 0
            then
               let streamEnd = String.length pdfStream in
               (String_.subp pdfStream openpos streamEnd) :: accum
            else
               accum
      in
      (* apply to entire stream *)
      (seek pdfStream 0 0 0 [])
      |> List.rev
      (* put spaces between each () chunk
         (PDF text content is often broken into words, or more, per ()) *)
      |> (String.concat " ")

   and unescapeChars (text:string) : string =
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
         (* (to match a single '\', the escaping here requires 4) *)
         "\\\\[0-7][0-7][0-7]\\|\
          \\\\[0-7][0-7][^0-7]\\|\
          \\\\[0-7][0-7]$\\|\
          \\\\[0-7][^0-7]\\|\
          \\\\[0-7]$\\|\
          \\\\[^0-7]"
      in
      Str.global_substitute rx translate text
   in

   pdfStream |> extractParenthised |> unescapeChars


let regulariseDashs (text:string) : string =

   (* just a list of plausible variations, not a complete countermeasure to
      every possible villainy and madness *)

   (* (SOFT HYPHEN) *)
   (* (MINUS SIGN) *)
   (* (HYPHEN) *)
   (* (NON-BREAKING HYPHEN) *)
   (* (FIGURE DASH) *)
   (* (EN DASH) *)
   (* (EM DASH) *)
   (* (HORIZONTAL BAR) *)
   (* (SMALL EM DASH) *)
   (* (SMALL HYPHEN-MINUS) *)
   (* (FULLWIDTH HYPHEN-MINUS) *)
   let rx = Str.regexp
      "\xC2\xAD\\|\
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

   (* replace all with ordinary '-' *)
   Str.global_replace rx "-" text


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


let getIsbnsFromText (pdf:Pdf.t) : string list =

   (* get first few pages *)
   let pages : Pdfpage.t list =
      try
         let pages = Pdfpage.pages_of_pagetree pdf
         and _NUMBER_OF_PAGES_TO_INSPECT = 10 (* 7 *) in
         fst (List_.bisect pages _NUMBER_OF_PAGES_TO_INSPECT)
      with
      | _ -> []
   in

   (* map pages to streams *)
   let streams : string list =
      let streamss : string list list =
         List.map
            (fun (page : Pdfpage.t) ->
               let objs : Pdf.pdfobject list = page.Pdfpage.content in
               List.map
                  (fun (obj : Pdf.pdfobject) -> getPageStreamString pdf obj)
                  objs )
            pages
      in
      List.flatten streamss
   in

   (* map streams to texts *)
   let texts : string list = List.map extractTextFromPdfStream streams in

   (* map texts to isbns *)
   let isbns : string list =
      List_.filtmap
         (fun (text : string) ->
            (*
               example stream fragment:
                  0.0287 Tc 9.3 0 0 9.3 151.14 89.04 Tm
                  (ISBN )Tj
                  0.0429 Tc 9.5 0 0 9.5 176.06 89.04 Tm
                  (0-674-53751-3 )Tj
                  0.0142 Tc -5.931 -3.586 Td

               extracted text:
                  ISBN  0-674-53751-3

               possible eccentricities:
                  * ISBN:
                  * eISBN
                  * ISBN-13
                  * hyphens not actual '-' chars
            *)
            (* robust against atomised chars and shuffled 'words' *)

            (* remove all spaces *)
            let text =
               text |> Blanks.unifySpaces |> (String_.filter ((<>)' '))
            in
            (* check for any 'ISBN' labels *)
            if Option_.toBool (Rx.regexFirst "isbn" ~caseInsens:true text)
            then
               (* extract first conforming number in text *)
               text
               |> regulariseDashs
               (* delete '-10' '-13' ISBN suffixs *)
               |> (Str.global_replace (Str.regexp_case_fold "ISBN-1[03]") "ISBN")
               |> Tadist.Isbn.search 0 (String.length text)
            else
               None )
         texts
   in

   isbns


let getIsbns (pdf:Pdf.t) : string list =

   let meta = getIsbnsFromMetadata pdf in

   if meta <> []
   then meta
   else getIsbnsFromText pdf




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
            idRaw     = getIsbns pdf ;
            subtypRaw = string_of_int (getPagecount pdf) ;
            typRaw    = _TYPE ;
         } )

      with
      | _ -> Error "PDF read failed"
