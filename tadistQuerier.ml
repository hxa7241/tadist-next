(*------------------------------------------------------------------------------

   TADIST tool (OCaml 4.10)
   Harrison Ainsworth / HXA7241 : 2015, 2020

   http://www.hxa.name/tools/
   http://www.hxa.name/notes/note-hxa7241-20141231T1101Z.html

   License: AGPL -- https://www.gnu.org/licenses/agpl-3.0.html

------------------------------------------------------------------------------*)




open HxaGeneral
open Tadist
(* using Unix *)




(* ---- functions ---- *)

let httpResponseParts (response:string) : string list =

   match Rx.regexFirst "\r\n\r\n\\|\n\n" response with
   | Some rxmatch ->
      let posEnd      = snd (Rx.wholePos rxmatch) in
      let head , body = String_.leadTrail response posEnd in
      [ head ; body ]
   | None ->
      [ response ]


let httpStatusLine (head:string) : string =

   let newlinePos = String_.indexpl Char_.isCrOrLf head in
   String_.lead head newlinePos
   |> String_.truncate 50


let httpStatusCode (statusLine:string) : string =

   let tokens =
      let rx = Str.regexp "[ \t]+" in
      Str.split rx statusLine
   in
   Option.value ~default:"" (List_.nth 1 tokens)


let httpResponseBody (head:string) (bodyRaw:string) : string =

   (* to handle header-field: "Transfer-Encoding: chunked"
      https://tools.ietf.org/html/rfc2616#section-3.6.1
      * each chunk is:
         * preceded by:
            * its byte length, in hex
            * (maybe other stuff)
            * \r\n
         * and followed by:
            * \r\n
      * last chunk has zero length, but has the same pre/post-fixes
         * extra postfix: maybe other stuff \r\n, \r\n *)

   (* DEBUG *)
   (*print_endline ("bodyRaw:\n" ^ bodyRaw ^ "<<<") ;*)

   let isChunked =
      let rx =
         Rx.compile ~caseInsens:true " *Transfer-Encoding: +chunked *[\r\n]"
      in
      Option_.toBool (Rx.seekFirst rx head)
   in

   if not isChunked
   then
      bodyRaw
   else
      let chunks : string list =
         let rec extractChunks (bodyRaw:string) (pos:int) (chunks:string list)
            : string list =

            (* DEBUG *)
            (*print_endline ("\n* pos: " ^ (string_of_int pos)) ;*)

            (* parse length from prefix -- len[;stuff]\r\n : int option *)
            let parseLength ((bodyRaw , pos):string * int) : int option =
               (Rx.regex "[0-9a-fA-F]+" ~pos bodyRaw)
               |>- (fun rxmatch -> Some (Rx.wholeFound rxmatch))
               (* DEBUG *)
               (*|>- (fun s -> print_endline ("* len-str: " ^ s) ; Some s)*)
               |>- (fun hex     -> int_of_string_opt ("0x" ^ hex))
            (* parse start from prefix -- len[;stuff]\r\n : int option *)
            and parseStart ((bodyRaw , pos):string * int) : int option =
               (Rx.regex "[^\r\n]*\r?\n" ~pos bodyRaw)
               (*(Rx.compile "[^\r\n]*\r?\n")
               |>  (fun rx      -> Rx.seekFirst rx ~pos bodyRaw)*)
               (* DEBUG *)
               (*|>- (fun rxmatch -> print_endline ("* fch: " ^
                  (String.sub bodyRaw (snd (Rx.wholePos rxmatch)) 1) ) ;
                  Some rxmatch)*)
               |>- (fun rxmatch -> Some (snd (Rx.wholePos rxmatch)))

            (* assemble chunks : string list *)
            and handleChunk ((length , start) : int * int) : string list =
               (* DEBUG *)
               (*print_endline ("* start , length: " ^
                  (string_of_int start) ^ " , " ^ (string_of_int length)) ;*)

               (* is not last chunk: recurse *)
               if length <> 0
               then
                  let thisChunk = String.sub bodyRaw start length
                  and posNext   =
                     let posEnd = start + length in
                     (Rx.regex "\r?\n" ~pos:posEnd bodyRaw)
                     |>- (Rx.wholePos %> snd %> Option.some)
                     |> (Option.value ~default:posEnd)
                  in
                  (* DEBUG *)
                  (*print_endline ("* chunk:\n" ^ thisChunk) ;*)
                  extractChunks bodyRaw posNext (thisChunk :: chunks)
               else
                  chunks
            (* failure: just dump the rest as the last chunk *)
            and dumpRest () : string list =
               (* DEBUG *)
               (*print_endline "dump" ;*)
               (String_.trail bodyRaw pos) :: chunks
            in

            (Some (bodyRaw , pos))
            |^^-
            (parseLength , parseStart)
            |>
            (Option_.mapUnify handleChunk dumpRest)
            |>
            List.rev

         and prefixPos = String_.indexp (Fun.negate Char_.isBlank) bodyRaw in

         match prefixPos with
         | Some pos -> extractChunks bodyRaw pos []
         | None     -> []
      in

      String.concat "" chunks


(**
 * Get JSON from OpenLibrary http query of an ISBN.
 *
 * Reference docs:
 *
 * * https://ocaml.org/releases/4.10/htmlman/libref/Unix.html
 * * https://tools.ietf.org/html/rfc2616  #section-5  #section-6
 * * http://openlibrary.org/developers
 * * https://openlibrary.org/dev/docs/api/books
 * * https://openlibrary.org/dev/docs/restful_api
 *)
let requestOpenLib (isbn:Isbn.t) : string ress =

   let requestHost = "openlibrary.org" in
   (* constant except for 'isbn' *)
   let request     =
      let urlPathAndQuery =
         "/api/books?bibkeys=ISBN:"
         ^ (Isbn.toStringBare isbn)
         ^ "&format=json&jscmd=data"
      in
      "GET " ^ urlPathAndQuery ^ " HTTP/1.1\r\n\
      Host: " ^ requestHost ^ "\r\n\
      User-Agent: Tadist tool 1.1\r\n\
      Connection: close\r\n\
      \r\n"
   in

   try
      let socket = Unix.(socket PF_INET SOCK_STREAM 0) in
      try
         (* connect *)
         begin
            let address =
               let ip = Unix.((gethostbyname requestHost).h_addr_list.(0)) in
               Unix.ADDR_INET (ip, 80)
            in
            Unix.connect socket address
         end ;

         (* request *)
         let _ (*outCount*) =
            Unix.write_substring socket request 0 (String.length request)
         in

         (* receive *)
         let response :string =
            (* imperative mutable stuff: read into byte buffer *)
            let bufLen  = 65536 in (* expecting <8KB *)
            let byteBuf = Bytes.make bufLen ' ' in
            let readLen =
               let rec readMore (offset:int) : int =
                  match Unix.read socket byteBuf offset (bufLen - offset) with
                  | 0       -> offset
                  | inCount -> readMore (offset + inCount)
               in
               readMore 0
            in
            Bytes.sub_string byteBuf 0 readLen
         in
         (* DEBUG *)
         (*print_endline ("\n* ISBN query response length: " ^
            (string_of_int (String.length response))) ;
         print_endline ("* ISBN query response:\n" ^ response ^ "<<<") ;*)

         (* basic parse of http response *)
         let body =
            let head , bodyRaw =
               let maybeHeadAndBody = httpResponseParts response in
               match maybeHeadAndBody with
               | [] ->
                  failwith "bad response"
               | head :: body ->
                  let statusLine = httpStatusLine head in
                  let statusCode = httpStatusCode statusLine in
                  if (body = []) || (statusCode <> "200")
                  then failwith ("bad response: " ^ statusLine)
                  else (head , List.hd body)
            in
            httpResponseBody head bodyRaw
         in

         Unix.close socket ;

         Ok body

      with
      | _  as error -> Unix.close socket ; raise error

   with
   | _ as error ->
      let message =
         match error with
         | Unix.Unix_error (code,fn,param) ->
            (Unix.error_message code) ^
            " (" ^ fn
            ^ (if String_.notEmpty param then " - " ^ param else "")
            ^ ")"
         | Not_found -> "cannot find IP address of query host"
         | Failure s -> s
         | _         -> "failed inscrutably"
      in
      Error (message ^ ".")


(**
 * Parse JSON from OpenLibrary http query of an ISBN.
 *
 * Reference docs:
 *
 * * http://openlibrary.org/developers
 * * https://openlibrary.org/dev/docs/api/books
 * * https://openlibrary.org/dev/docs/restful_api
 *
 * Easy but non-robust approach: don't bother to parse, but simply search for:
 *
 * * "title" : "___"
 * * "authors" [ ... "name" : "___" ... "name" : "___" ... ]
 *    eg:
 *    "authors": [ {
 *          "url": "http://openlibrary.org/authors/OL2662614A/Timothy_Gowers",
 *          "name": "Timothy Gowers"
 *       }
 *    ],
 * * "publish_date" : "___"
 *
 * Example request and response body:

$ curl 'http://openlibrary.org/api/books?bibkeys=ISBN:9780691118802&format=json&jscmd=data'

{
   "ISBN:9780691118802": {
      "publishers": [
         { "name": "Princeton University Press" }
      ],
      "identifiers": {
         "isbn_13": ["9780691118802"],
         "openlibrary": ["OL11182845M"],
         "isbn_10": ["0691118809"],
         "goodreads": ["1471873"],
         "librarything": ["6238370"]
      },
      "title": "The Princeton Companion to Mathematics",
      "url": "http://openlibrary.org/books/OL11182845M/The_Princeton_Companion_to_Mathematics",
      "number_of_pages": 1008,
      "cover": {
         "small": "https://covers.openlibrary.org/b/id/7423173-S.jpg",
         "large": "https://covers.openlibrary.org/b/id/7423173-L.jpg",
         "medium": "https://covers.openlibrary.org/b/id/7423173-M.jpg"
      },
      "subjects": [ {
            "url": "https://openlibrary.org/subjects/mathematics",
            "name": "Mathematics"
         }
      ],
      "publish_date": "September 11, 2008",
      "key": "/books/OL11182845M",
      "authors": [ {
            "url": "http://openlibrary.org/authors/OL2662614A/Timothy_Gowers",
            "name": "Timothy Gowers"
         }
      ],
      "ebooks": [ {
            "formats": {},
            "preview_url": "https://archive.org/details/princetoncompani00gowe_360",
            "availability": "restricted"
         }
      ]
   }
}
 *)
let parseOpenLib (json:string)
   : (string option * string list * string option * string option) =

   let stringValueRx = "\"\\([^\"]*\\)\""
   and numberValueRx = "\\([^ ,]+\\),"
   and extractElement (json:string) (name:string) (form:string)
      : string option =
      let rx = Rx.compile ("\"" ^ name ^ "\" *: *" ^ form) in
      (Rx.seekFirst rx json)
      |>-
      (fun rxmatch -> Rx.groupFound rxmatch 1)
   in

   let extractTitle (json:string) : string option =
      (* find second '{' (the main object) : int option *)
      (String_.index '{' json)
      |>- (fun pos -> (String_.index '{' ~start:pos json))
      (* therefrom, delete (leaf) sub-objects : string option *)
      |>- (fun pos -> Some (String_.trail json pos))
      |>- (fun json ->
         let rx = Str.regexp "{[^{}]*}" in
         Some (Str.global_replace rx "" json))
      (* now, seek "title" (without ambiguities) : string option *)
      |>- (fun json -> extractElement json "title" stringValueRx)
      (* regex version *)
      (*let rx =
         Rx.compile
            "[^{}]*{[^{}]*{[^{}]*\({[^{}]*}[^{}]*\)*\"title\" *: *\"\([^\"]*\)\""
      in
      (Rx.seekFirst rx json)
      |> Option.map ((Fun.flip Rx.groupFound) 2)*)
   and extractAuthors (json:string) : string list =
      let rx = Rx.compile "\"name\" *: *\"\\([^\"]*\\)\"" in
      (* get authors json array : string option *)
      (extractElement json "authors" "\\[\\([^]]*\\)\\]" )
      |>-
      (*(fun s ->
         print_endline ("* Q-authsjson: " ^ s) ;
         Some s)
      |>-*)
      (* get all 'name' sub elements (name-value pairs) : string list1 option *)
      (fun s -> (Rx.allMatches rx s) |> toList1)
      |>-
      (*(fun l1s ->
         print_endline ("* Q-authsnames: "
            ^ (String.concat " | " (ofList1 l1s))) ;
         Some l1s)
      |>-*)
      (* from the name-value pairs, extract just the value strings *)
      (ofList1
         %>
         (List_.filtmap
            (fun s -> (Rx.apply rx s) |>- ((Fun.flip Rx.groupFound) 1)))
         %>
         toList1)
      |>
      ofList1o
   in

   let json = Blanks.blankSpacyCtrlChars json in

   let titleo  = extractTitle json
   and authors = extractAuthors json
   and dateo   = extractElement json "publish_date" stringValueRx
   and pageso  = extractElement json "number_of_pages" numberValueRx in

   ( titleo , authors, dateo, pageso )




(* ---- public functions ---- *)

let getBasicTadForIsbn (isbn:Isbn.t) : nameStructRaw ress =

   (* : string ress *)
   (requestOpenLib isbn)
   |>=-
   (* : TAD tuple *)
   parseOpenLib
   |>=-
   (* : nameStructRaw *)
   (fun (titleo , authors , dateo , pageso) ->
      {  titleRaw  = Option.to_list titleo ;
         authorRaw = authors ;
         dateRaw   = Option.to_list dateo ;
         idRaw     = [ Isbn.toStringBare isbn ] ;
         subtypRaw = (Option.value ~default:"" pageso);
         typRaw    = "openlibrary.org" ; } )
