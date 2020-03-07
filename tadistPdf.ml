(*------------------------------------------------------------------------------

   PDF lib (OCaml 4.02)
   Harrison Ainsworth / HXA7241 : 2015

   http://www.hxa.name/tools/

   License: CC0 -- http://creativecommons.org/publicdomain/zero/1.0/

------------------------------------------------------------------------------*)




open HxaGeneral
(* using Tadist *)
(* using Pdf, Pdfread, Pdfpage *)




(* ---- values ---- *)

let _TYPE = "pdf"




(* ---- public functions ---- *)

let extractTadist (trace:bool) (pdfPathname:string)
   : (Tadist.nameStructRaw option) eoption =

   Err "not implemented yet"

   (*
   Ok None -- not recognised
   Ok Some ... -- recognised, data

   ? recognised as a PDF
      - no: Ok None
      - yes:
         ? read PDF
            - fail: Err ...
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
