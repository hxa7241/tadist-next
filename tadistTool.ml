(*------------------------------------------------------------------------------

   TADIST tool (OCaml 4.10)
   Harrison Ainsworth / HXA7241 : 2015, 2020

   http://www.hxa.name/tools/
   http://www.hxa.name/notes/note-hxa7241-20141231T1101Z.html

   License: AGPL -- https://www.gnu.org/licenses/agpl-3.0.html

------------------------------------------------------------------------------*)




open HxaGeneral
(* using Tadist *)
(* using TadistMelder *)




(* ---- values ---- *)

let _HELP =
{|
  TADIST tool 1.1 (OCaml 4.10) ***UNFINISHED***
  Harrison Ainsworth / HXA7241 : 2015, 2020
  http://www.hxa.name

Does one of three things:
* suggests a name for a file, from file metadata and internet query
* renames a file, from examined file metadata and internet query
* converts a string between 'name-form' and 'text-form'

... according to the TADIST format definition:
http://www.hxa.name/notes/note-hxa7241-20141231T1101Z.html

File-types supported: Epub.

Usage:
  tadist [-?|--help]
  tadist (-??|--doc)
  tadist [-(s|S|r|R)] (-|<filename>)
  tadist -c (-|<string>)

Options:
-?  | --help  help
-?? | --doc   more doc
-s  suggest: print inferred name (default)
-S  suggest: verbosely print inferred name
-r  rename: ask to rename file to inferred name
-R  rename: go ahead and rename file to inferred name
-c  convert: between name and text form
-   take filename/string from stdin
<string>  (use single quotes to quote)
|} ;;


let _DOC =
{|
Original document:

"TADIST file naming pattern" ; HXA7241 ; 2014-12-31 / html .
http://www.hxa.name/notes/note-hxa7241-20141231T1101Z.html



Examples
--------

These give most of the idea:
   The-Practice-Of-Programming_Kernighan-Pike_1999.ISBN-020161586X.djvu
   Concerto-2-E-min-RV279-1-Allegro_Vivaldi_1713-1995.ISRC-GBFO77341004.v245.mp3
   Missa-Papae-Marcelli-Kyrie_Palestrina-TheTallisScholars_1562-1999.ISRC-GBADM9400034.v245.mp3
   Sparrow-On-The-Crabapple-Tree_Anderson_2007.flickr-734188511.1024x683.jpg



Description
-----------

‘TADIST’ is an acronym: the format is basically:
   title _ author _ date . id . subtype . type

But some parts can be omitted, so then the schema is roughly (with [] meaning optional):
   title [_ author [_ date]] [. id] [. subtype] . type

Furthermore, each part (except the type) can have sub-parts separated by ‘-’, so (with ... meaning possible repetition):
   ttt-tt-... [_ aaa-aa-... [_ ddd-dd-...]] [. iii-iiii] [. sss ] . type

The semantics of sub-parts vary though:
* title is a string of words (‘-’ means space)
* author and date are arrays of items (‘-’ means ‘,’)
* id is a label-value pair (‘-’ separates those two)

And more detail on some elements:
* date is an ISO-8601 date in compact (no-space) form
* id should follow some standard or convention, e.g.: ISBN-9780631128014
* sub-type should be understandable from the type, e.g.: 100x60.png, c320.mp3
* type should be from a commonly known vocabulary, e.g.: jpg, epub, mp3

These are all assembled only from three character classes which are easy to handle in file names:
* letters: (at least ASCII)
* digits: 0123456789
* separators: - _ .



Filename Grammar
----------------

   (filename (, plain meta))

   (plain   (, title (? (, "_" author (? (, "_" date))))))
   (title   (+ alphnum "-"))
   (author  (+ alphnum "-"))
   (date    (+ (, year (? (, month (? day)))) "-"))

   (meta    (, (? (, "." id)) (? (, "." subtype)) "." type))
   (id      (, alphnum "-" alphnum))
   (subtype alphnum)
   (type    alphnum)

   (year    (, (? "-")
               (| (=4 digit)
                  (, (=3 digit) "X")
                  (, (=2 digit) "XX")
                  (, digit "XXX"))))
   (month   (| "01" "02" "03" "04" "05" "06"
               "07" "08" "09" "10" "11" "12"))
   (day     (| (, "0" (| "1" "2" "3" "4" "5" "6" "7" "8" "9"))
               (, (| "1" "2") digit)
               (, "3" (| "0" "1"))))
   (alphnum (+ (| letter digit)))

   (digit   (| "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

Assuming:
* letter -- at least ASCII

Comments:
* title   -- a string of words
* author  -- an array of names
* date    -- an IS0-8601 date in compact (no-space) form
* id      -- following a label-value convention, e.g.: ISBN-1852338210
* subtype -- understandable from the type, e.g.: 100x60.png, c320.mp3
* type    -- from a commonly known vocabulary, e.g.: jpg, epub, mp3
* year    -- does use leading '-'; for BCE, and has a non-standard augmentation: 'X'; for unknown



Text-Form Grammar
-----------------

   (textform (, plain meta " ."))

   (plain   (, title (? (, " ; " author (? (, " ; " date))))))
   (title   (, "\"" chars-no-dquos "\""))
   (author  (+ chars-no-commas-or-semicolons-or-slashes ", "))
   (date    (+ (, year (? (, "-" month (? "-" day)))) ", "))

   (meta    (, (? (, " / " id)) (? (, " / " subtype)) " / " type))
   (id      (, alphnum "-" alphnum))
   (subtype alphnum)
   (type    alphnum)

   (year    (, (? "-")
               (| (=4 digit)
                  (, (=3 digit) "X")
                  (, (=2 digit) "XX")
                  (, digit "XXX"))))
   (month   (| "01" "02" "03" "04" "05" "06"
               "07" "08" "09" "10" "11" "12"))
   (day     (| (, "0" (| "1" "2" "3" "4" "5" "6" "7" "8" "9"))
               (, (| "1" "2") digit)
               (, "3" (| "0" "1"))))
   (alphnum (+ (| letter digit)))

   (digit   (| "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

Assuming:
* chars-no-dquos
* chars-no-commas-or-semicolons-or-slashes
* letter -- at least ASCII

Comments:
* title   -- a string of words
* author  -- an array of names
* date    -- an IS0-8601 date
* id      -- following a label-value convention, e.g.: ISBN-1852338210
* subtype -- understandable from the type, e.g.: 100x60 / png, c320 / mp3
* type    -- from a commonly known vocabulary, e.g.: jpg, epub, mp3
* year    -- does use leading '-'; for BCE, and has a non-standard augmentation: 'X'; for unknown

|} ;;




(* ---- functions ---- *)

let readInput (s:string) : string =

   if s <> "-"
   then
      s
   else
      try read_line () with
      | Sys_error s -> fail ("input failure: " ^ s)
      | End_of_file
      | _           -> fail "input failure"


let printNamestructData (trace:bool) (ns:Tadist.nameStruct) : unit =

   if trace
   then begin
      print_endline "\nTadist form (merged data)" ;

      let arrayPrinter (label:string) (sep:string) (sa:string array) : unit =
         let content = sa |> Array.to_list |> (String.concat sep) in
         print_endline (label ^ "   " ^ content) ;
      in

      let open Tadist in

      arrayPrinter "* title: " " "
         ((ns.title |> ArrayNe.toArray) |> (Array.map StringT.toString)) ;
      arrayPrinter "* author:" " | "
         (ns.author |> (Array.map StringT.toString)) ;
      arrayPrinter "* date:  " " | "
         (ns.date |> (Array.map (DateIso8601e.toString false))) ;

      print_endline ("* id:       " ^
         (Option_.mapUnify
            (fun (il,ic) -> (StringT.toString il) ^ "-" ^ (StringT.toString ic))
            (Fun.const "") ns.id) ) ;
      print_endline ("* subtyp:   " ^
         (Option_.mapUnify StringT.toString (Fun.const "") ns.subtyp) ) ;
      print_endline ("* typ:      " ^
         (StringT.toString ns.typ) ) ;
   end


let suggestRename ~(rename:bool) ?(quiet:bool = false) ?(verbose:bool = false)
   (input:string) : unit =

   (* get old name *)
   let filePathnameOld = readInput input in
   let path , nameOld = FileName.splitPath filePathnameOld in

   (* get new name *)
   let nameNew , textNew =
      match
         TadistMelder.makeNameStructFromFileName verbose filePathnameOld
      with
      | Ok ns ->
         begin
            printNamestructData verbose ns ;
            ( Tadist.toStringName ns , Tadist.toStringText ns )
         end
      | Error s -> fail s
   in
   let filePathnameNew = path ^ nameNew in

   (* only act if name different *)
   if nameOld <> nameNew
   then
      if not rename

      (* suggest name *)
      then
         Printf.printf "\n%s\n%s\n%!" nameNew textNew

      (* rename file *)
      else begin
         let ask () : bool =
            Printf.printf
               "Do you want to rename:\n  %s\nto:\n  %s\n(Y or N) ?\n%!"
               nameOld nameNew ;
            let response = input_char stdin in
            response = 'Y'
         in
         if quiet || (ask ())
         then
            try
               Unix.rename filePathnameOld filePathnameNew ;
               Printf.printf "\n%s\n%s\n%!" nameNew textNew
            with
            | Unix.Unix_error (e , sF , sP) ->
               ignore (fail ((Unix.error_message e) ^ ": " ^ sF ^ " " ^ sP))
         end


let convert (input:string) : unit =

   let input = readInput input in

   let output =
      match Tadist.makeNameStruct input with
      | Ok ns   ->
         if Tadist.isTextform input
         then Tadist.toStringName ns
         else Tadist.toStringText ns
      | Error s -> fail ("bad input: " ^ s)
   in

   print_endline output

;;




(* ---- entry point ---- *)

try

   match Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1) with

   (* print help / doc *)
   | [||]
   | [|"-?"|]  | [|"--help"|] -> print_endline _HELP
   | [|"-??"|] | [|"--doc"|]  -> print_endline _DOC

   (* execute *)
   | _ as _argv ->

      set_binary_mode_in  stdin  true ;
      set_binary_mode_out stdout true ;
      set_binary_mode_out stderr true ;

      match _argv with
      | [| input |]
      | [| "-s" ; input |] -> suggestRename ~rename:false input
      | [| "-S" ; input |] -> suggestRename ~rename:false ~verbose:true input
      | [| "-r" ; input |] -> suggestRename ~rename:true  input
      | [| "-R" ; input |] -> suggestRename ~rename:true  ~quiet:true input
      | [| "-c" ; input |] -> convert input
      | _ as av            ->
         let first = if (Array.length av) > 0 then av.(0) else "" in
         fail ("unrecognised command: " ^ first ^ " ...")

with
| e -> prerr_string "*** General failure: " ; raise e
