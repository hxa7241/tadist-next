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


let getIsbns (pdf:Pdf.t) : string list =

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
      Tadist.Isbn.search tag 0 (String.length tag)
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
      Tadist.Isbn.search tag 0 (String.length tag)
   (* other inappropriate metadata (unlikely but possible) *)
   and subject (pdf:Pdf.t) () : string option =
      let field = lookupInfoUtf8 pdf "/Subject" in
      Tadist.Isbn.search field 0 (String.length field)
   and keywords (pdf:Pdf.t) () : string option =
      let field = lookupInfoUtf8 pdf "/Keywords" in
      Tadist.Isbn.search field 0 (String.length field)
   in

   let xmp = (getXmpXml pdf) |> Blanks.blankSpacyCtrlChars in

   (* take the first successful match, in this priority of alternatives *)
   (identifierDc xmp ())
   ||> (identifierXmp xmp)
   ||> (subject pdf)
   ||> (keywords pdf)
   |> List_.ofOpt




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
