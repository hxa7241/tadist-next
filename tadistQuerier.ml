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

   let rx = Str.regexp "\r\n\r\n\\|\r\r\\|\n\n" in
   Str.split rx response


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
         let response =
            let bufLen  = 16384 in (* expecting ~ 1-2KB *)
            let byteBuf = Bytes.make bufLen ' ' in
            let inCount = Unix.read socket byteBuf 0 bufLen in
            Bytes.sub_string byteBuf 0 inCount
         in
         (*print_endline ("* ISBN query response:\n" ^ response) ;*)

         (* basic check of http response *)
         let body =
            let maybeHeadAndBody = httpResponseParts response in
            match maybeHeadAndBody with
            | [] ->
               failwith "bad response"
            | head :: body ->
               let statusLine = httpStatusLine head in
               let statusCode = httpStatusCode statusLine in
               if (body = []) || (statusCode <> "200")
               then
                  failwith ("bad response: " ^ statusLine)
               else
                  begin
                     let body = List.hd body in
                     (*print_endline ("* ISBN query body:\n" ^ body) ;*)

                     body
                  end
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
   : (string option * string list * string option) =

   let extractElement (json:string) (name:string) (form:string)
      : string option =
      let rx = Rx.compile ("\"" ^ name ^ "\" *: *" ^ form) in
      (Rx.seekFirst rx json)
      |>-
      (fun rxmatch -> Rx.groupFound rxmatch 1)
   in
   let extractAuthors (json:string) : string list =
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
   let stringValueRx = "\"\\([^\"]*\\)\""
   and json = Blanks.blankSpacyCtrlChars json in

   let titleo  = extractElement json "title" stringValueRx
   and authors = extractAuthors json
   and dateo   = extractElement json "publish_date" stringValueRx in

   ( titleo , authors, dateo )




(* ---- public functions ---- *)

let getBasicTadForIsbn (isbn:Isbn.t) : nameStructRaw ress =

   (* : string ress *)
   (requestOpenLib isbn)
   |>=-
   (* : TAD tuple *)
   parseOpenLib
   |>=-
   (* : nameStructRaw *)
   (fun (titleo , authors , dateo) ->
      {  titleRaw  = Option_.toList titleo ;
         authorRaw = authors ;
         dateRaw   = Option_.toList dateo ;
         idRaw     = [ Isbn.toStringBare isbn ] ;
         subtypRaw = "" ;
         typRaw    = "openlibrary.org" ; } )
