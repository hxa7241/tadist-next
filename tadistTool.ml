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

let _VERSION =
{|
TADIST tool (OCaml 4.10) 1.2
Harrison Ainsworth / HXA7241 : 2015, 2021 : http://www.hxa.name

License GNU AGPLv3 : http://www.gnu.org/licenses/agpl-3.0.html
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
|} ;;


let _HINT =
{|
TADIST tool 1.2 (OCaml 4.10) : 2021 : http://www.hxa.name

$ tadist -m <filename>   : for metadata (epub/pdf)
$ tadist -r <filename>   : to rename (epub/pdf)
$ tadist -?              : for help
|} ;;


let _HELP =
{|
  TADIST tool 1.2 (OCaml 4.10) ***UNFINISHED***
  Harrison Ainsworth / HXA7241 : 2015, 2021
  http://www.hxa.name

Description:
  TADIST Tool is a small command-line tool that does one of three things:
  * prints metadata for an ebook, from file and internet query
  * suggests a file name for an ebook, from file metadata and internet query
  * renames an ebook file, from file metadata and internet query

  According to the TADIST format definition:
  http://www.hxa.name/notes/note-hxa7241-20141231T1101Z.html

  Ebook file-types supported:
  * Epub
  * PDF

Examples:
  print metadata:
    $ tadist -m An-Ebook.pdf
  suggest a file name:
    $ tadist -s An-Ebook.pdf
  rename an ebook file:
    $ tadist -r An-Ebook.pdf

Usage:
  $ tadist [-h|-?|--help]
  $ tadist -v | (-??|--doc)
  $ tadist [-(m|j|s|r|R)] (-|<filename>)
  $ tadist -c (-|<string>)

Options:
  -h | -? | --help     help
  -??     | --doc      more doc (144 lines)
  -v | --version       version info
  -m | --metadata      print: output metadata as INI
  -j | --json          print: output metadata as JSON
  -s | --suggest       suggest: print inferred name
  -r | --rename        rename: ask to rename file to inferred name
  -R | --rename-quiet  rename: immediately rename file to inferred name
  -c | --convert       convert: between name and text form
  -                    take filename/string from stdin
  <string>             (use single quotes to quote)
|} ;;


let _DOC =
{|

TADIST file naming pattern
==========================


2014-12-31T11:01Z

(490 words)


Original document:

   "TADIST file naming pattern"; HXA7241; 2014-12-31 / html.
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

   (textform (, plain meta "."))

   (plain   (, title (? (, "; " author (? (, "; " date))))))
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

(* secondary *)

let readInput (s:string) : string =

   if s <> "-"
   then
      s
   else
      try read_line () with
      | Sys_error s -> raise (Intolerable (EXIT_IOERR , s))
      | End_of_file
      | _           -> raise (Intolerable (EXIT_IOERR , ""))


let getOldAndNewFilenames (input:string) : (string * string * string) =

   (* get old name *)
   let filePathnameOld = readInput input in
   let path , nameOld = FileName.splitPath filePathnameOld in

   (* get new name *)
   let nameNew =
      let nameStruct =
         TadistMelder.makeNameStructFromFileName_x false filePathnameOld
      in
      Tadist.toStringName nameStruct
   in

   ( path , nameOld , nameNew )


(* primary *)

let printMetadata ?(trace:bool = false) (json:bool) (input:string) : unit =

   let nameStruct =
      let filePathname = readInput input in
      TadistMelder.makeNameStructFromFileName_x trace filePathname
   in

   let open Tadist in

   let stringToString (str:string) : string =
      if not json then str else "\"" ^ str ^ "\""
   in
   let arrayToString (json:bool) (sep:string) (sa:string array) : string =
      let opn , cls =
         if not json then "" , "" else "[ " , " ]"
      and concatenation =
         sa
         |> Array.to_list |> (List.map stringToString) |> (String.concat sep)
      in
      opn ^ concatenation ^ cls
   in

   let title =
      nameStruct.title |> ArrayNe.toArray |> (Array.map StringT.toString)
      |> Array.to_list |> (String.concat " ") |> stringToString
   and authors =
      arrayToString json ", "
         (nameStruct.author |> (Array.map StringT.toString))
   and dates =
      arrayToString json ", "
         (nameStruct.date |> (Array.map (DateIso8601e.toString false))) ;
   and isbn =
      (Option_.mapUnify
         (snd %> StringT.toString)
         (Fun.const "") nameStruct.id)
       |> stringToString
   and pages =
      (Option_.mapUnify
         (StringT.toString %> (String_.filter Char_.isDigit))
         (Fun.const "0") nameStruct.subtyp)
   and filetype =
      (StringT.toString nameStruct.typ) |> stringToString
   in

   if not json
   then
      Printf.printf
{|
[metadata]
; string, csv, csv, string, number, string
title    = %s
authors  = %s
dates    = %s
isbn     = %s
pages    = %s
filetype = %s

|}
         title authors dates isbn pages filetype
   else
      Printf.printf
{|
{
   "metadata" :
      {
         "title"    : %s ,
         "authors"  : %s ,
         "dates"    : %s ,
         "isbn"     : %s ,
         "pages"    : %s ,
         "filetype" : %s
      }
}

|}
         title authors dates isbn pages filetype


let suggest (input:string) : unit =

   let _ , nameOld , nameNew = getOldAndNewFilenames input in

   if nameOld <> nameNew
   then
      Printf.printf "%s\n%!" nameNew
   else
      Printf.printf "(%s  -- already properly named)\n%!" nameOld


let rename (quiet:bool) (input:string) : unit =

   let path , nameOld , nameNew = getOldAndNewFilenames input in

   if nameOld <> nameNew
   then
      let ask () : bool =
         Printf.printf
            "Do you want to rename:\n  %s\nto:\n  %s\n(Y or N) ?\n%!"
            nameOld nameNew ;
         let response = input_char stdin in
         (response = 'y') || (response = 'Y')
      in

      begin if quiet || (ask ())
      then
         try
            Unix.rename (path ^ nameOld) (path ^ nameNew) ;
            Printf.printf "%s  --renamed-to->  %s\n%!" nameOld nameNew
         with
         | Unix.Unix_error (code , funct , param) ->
            raise (Intolerable
               (EXIT_CANTCREAT ,
                  (Printf.sprintf "%s (%s: %s)"
                     (Unix.error_message code) funct param)))
      end
   else
      Printf.printf "(%s  -- already properly named)\n%!" nameOld


let convert (input:string) : unit =

   let input = readInput input in

   let output =
      match Tadist.makeNameStruct input with
      | Ok ns   ->
         if Tadist.isTextform input
         then Tadist.toStringName ns
         else Tadist.toStringText ns
      | Error s ->
         raise (Intolerable (EXIT_DATAERR , s))
   in

   print_endline output

;;




(* ---- entry point ---- *)

try

   match Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1) with

   (* print help (0 or 1 param) *)
   | [||]                                -> print_endline _HINT
   | [|"-h"|]  | [|"-?"|] | [|"--help"|] -> print_endline _HELP
   | [|"-??"|] | [|"--doc"|]             -> print_endline _DOC
   | [|"-v"|]  | [|"--version"|]         -> print_endline _VERSION

   (* execute (2 params) *)
   | _ as _argv ->

      set_binary_mode_in  stdin  true ;
      set_binary_mode_out stdout true ;
      set_binary_mode_out stderr true ;

      match _argv with
      | [| flag ; arg |] ->
         begin match flag with
         | "-m" | "--metadata"     -> printMetadata false arg
         | "-j" | "--json"         -> printMetadata true arg
         | "-s" | "--suggest"      -> suggest arg
         | "-r" | "--rename"       -> rename false arg
         | "-R" | "--rename-quiet" -> rename true  arg
         | "-c" | "--convert"      -> convert arg
         | "-!"                    -> printMetadata ~trace:true false arg
         | _                       ->
            raise (Intolerable (EXIT_USAGE , ("unrecognised command: " ^ flag)))
         end
      | [| str |] ->
         let msg =
            if String_.isFirstChar ((=)'-') str
            then "missing filename/string"
            else "missing command"
         in
         raise (Intolerable (EXIT_USAGE , msg))
      | [||] -> raise (Intolerable (EXIT_USAGE , "missing command"))
      | _    -> raise (Intolerable (EXIT_USAGE , "too many params"))

with

| Intolerable (sysexit , message) -> exite sysexit message
| _                               -> exite EXIT_UNSPECIFIED ""
