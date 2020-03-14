(*------------------------------------------------------------------------------

   PDF lib (OCaml 4.10)
   Harrison Ainsworth / HXA7241 : 2015, 2020

   http://www.hxa.name/tools/

   License: AGPL -- https://www.gnu.org/licenses/agpl-3.0.html

------------------------------------------------------------------------------*)




open HxaGeneral
(* using Tadist *)
(* using Pdf, Pdfread, Pdfpage *)




(* ---- values ---- *)

let _TYPE = "pdf"




(* ---- public functions ---- *)

let extractTadist (_(*trace*):bool) (_(*pdfPathname*):string)
   : (Tadist.nameStructRaw option) ress =

   Error "not implemented yet"

   (*
   Ok None -- not recognised
   Ok Some ... -- recognised, data

   ? recognised as a PDF
      - no: Ok None
      - yes:
         ? read PDF
            - fail: Error ...
            - ok:
               - get TAD and pagecount (or not/empty)
                  - get TAD
                     ? get /Info
                        - fail: "" , "" , ""
                        - ok:
                           - get /Title , /Author , /CreationDate
                  - get pagecount
               - get isbn from body (or not/empty)
               ? any TAD empty
                  - yes:
                     - lookup data from isbn
                        - openlibrary.org ...
               - Ok (Some Tadist.( {
                  titleRaw  = title ;
                  authorRaw = author ;
                  dateRaw   = date ;
                  idRaw     = isbn ;
                  subtypRaw = "p" ^ (string_of_int pagecount) ;
                  typRaw    = _TYPE } ) )

   todo:
   * recognisePdf
      * first bytes: %PDF–1.x(endline)
   *)
