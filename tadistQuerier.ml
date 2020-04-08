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
 *
 * Example response body:
 *
 * {"ISBN:9781786635167": {"publishers": [{"name": "Verso"}], "identifiers": {"isbn_13": ["9781786635167"], "openlibrary": ["OL27613422M"]}, "subtitle": "How the World's Biggest Corporations are Laying the Foundation for Socialism", "weight": "9.9 ounces", "title": "The People's Republic of Walmart", "url": "http://openlibrary.org/books/OL27613422M/The_People's_Republic_of_Walmart", "number_of_pages": 256, "cover": {"small": "https://covers.openlibrary.org/b/id/9063092-S.jpg", "large": "https://covers.openlibrary.org/b/id/9063092-L.jpg", "medium": "https://covers.openlibrary.org/b/id/9063092-M.jpg"}, "publish_date": "1819", "key": "/books/OL27613422M", "authors": [{"url": "blah", "name": "Foo Bar"}, {"url": "http://openlibrary.org/authors/OL7730842A/Leigh_Phillips", "name": "Leigh Phillips"}]}}
 *)
let requestOpenLib (trace:bool) (isbn:Isbn.t) : string ress =

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
               (* DEBUG *)
               (*let ip = Unix.inet_addr_of_string "207.241.232.201" in*)
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
         (* DEBUG *)
         if trace then print_endline ("* ISBN query response:\n" ^ response) ;

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
                     (* DEBUG *)
                     if trace then print_endline ("* ISBN query body:\n" ^ body) ;

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
      Error ("ISBN query: " ^ message ^ ".")


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
 *)
let parseOpenLib (trace:bool) (json:string)
   : (string option * string list * string option) ress =

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
      (extractElement json "authors" "\\[\\(.*\\)\\]" )
      |>-
      (* DEBUG *)
      (fun s ->
         if trace then print_endline ("* Q-authsjson: " ^ s) ;
         Some s)
      |>-
      (* get all 'name' sub elements (name-value pairs) : string list1 option *)
      (fun s -> (Rx.allMatches rx s) |> toList1)
      |>-
      (* DEBUG *)
      (fun l1s ->
         if trace
         then print_endline ("* Q-authsnames: "
            ^ (String.concat " | " (ofList1 l1s))) ;
         Some l1s)
      |>-
      (* from the name-value pairs, extract just the value strings *)
      (ofList1
         %>
         (List_.filtmap
            (fun s ->
               (Rx.apply rx s)
               |>-
               ((Fun.flip Rx.groupFound) 1)))
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

   (* DEBUG *)
   if trace
   then begin
      print_endline ("* Q-title:   " ^(Option.value ~default:"[empty]" titleo)) ;
      print_endline ("* Q-authors: " ^ (String.concat " | " authors)) ;
      print_endline ("* Q-date:    " ^ (Option.value ~default:"[empty]" dateo)) ;
      end ;

   Ok ( titleo , authors, dateo )




(* ---- public functions ---- *)

let getBasicTadForIsbn (trace:bool) (isbn:Isbn.t) : nameStructRaw ress =

   (* DEBUG *)
   if trace then print_endline "" ;

   (* : string ress *)
   (requestOpenLib trace isbn)
   |>=
   (* : TAD tuple ress *)
   (parseOpenLib trace)
   |>=
   (* : nameStructRaw ress *)
   (fun (titleo , authors , dateo) ->
      Ok {
         titleRaw  = Option_.toList titleo ;
         authorRaw = authors ;
         dateRaw   = Option_.toList dateo ;
         idRaw     = [ Isbn.toStringBare isbn ] ;
         subtypRaw = "" ;
         typRaw    = "isbn-query" ; } )
