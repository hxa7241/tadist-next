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

let requestOpenLib (trace:bool) (isbn:Isbn.t) : string ress =

   (*
   * http://openlibrary.org/developers
   * https://openlibrary.org/dev/docs/api/books
   * https://openlibrary.org/dev/docs/restful_api
   *)

   let requestHost = "openlibrary.org" in
   let request     =
      let urlPathAndQuery =
         "/api/books?bibkeys=ISBN:9780691118802&format=json&jscmd=data"
      in
      "GET " ^ urlPathAndQuery ^ " HTTP/1.1\\r\\n \
      Host: " ^ requestHost ^ "\\r\\n \
      User-Agent: Tadist tool 1.1\\r\\n \
      Connection: close\\r\\n \
      \\r\\n"
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

         Unix.close socket ;

         Ok response
      with
      | _  as e -> Unix.close socket ; raise e

   with
   | _  as e ->
      let message =
         match e with
         | Unix.Unix_error (code,fn,param) ->
            (Unix.error_message code) ^ ": " ^ fn ^ " - " ^ param
         | Not_found -> "cannot find IP address of query host."
         | _         -> "failed inscrutably."
      in
      Error ("ISBN query: " ^ message)


let parseOpenLib (trace:bool) (json:string)
   : (string option * string list * string option) ress =

   Error "not implemented yet"



(* ---- public functions ---- *)

let getBasicTadForIsbn (trace:bool) (isbn:Isbn.t)
   : (string option * string list * string option) ress =

   failwith "not implemented yet"

   (* : string ress *)
   (requestOpenLib trace isbn)
   |>=
   (* : TAD tuple ress *)
   (parseOpenLib trace)
