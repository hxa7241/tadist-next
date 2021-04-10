(*------------------------------------------------------------------------------

   TADIST tool (OCaml 4.10)
   Harrison Ainsworth / HXA7241 : 2015, 2020

   http://www.hxa.name/tools/
   http://www.hxa.name/notes/note-hxa7241-20141231T1101Z.html

   License: AGPL -- https://www.gnu.org/licenses/agpl-3.0.html

------------------------------------------------------------------------------*)




open HxaGeneral




(* ---- modules ---- *)

module StringT :
sig
   type t

   val isAllowedChar : char -> bool
   val filter        : string -> string

   val make     : string -> t ress
   val makef    : string -> t ress
   val toString : t -> string
   val compare  : t -> t -> int
end
=
struct
   type t = string

   let isAllowedChar (c:char) : bool =
      (* disallow: TADIST format chars, unix bad filename chars (and '\') *)
      match c with
      | '-' | '_' | '.'
      | ' ' | ',' | ';' | '/' | '"' | '\\'
      | '\x00'..'\x1F' | '\x7F'            -> false
      | _                                  -> true

   let filter (s:string) : string =
      String_.filter isAllowedChar s

   let checkf (s:string) : string ress =
      if String_.check isAllowedChar s
      then Ok s else Error "StringT chars invalid"

   let checke (s:string) : string ress =
      if not (String_.isEmpty s)
      then Ok s else Error "StringT emptiness invalid"

   let make (s:string) : t ress =
      s |> checkf |>= checke

   let makef (s:string) : t ress =
      s |> filter |> checke

   let toString (st:t) : string =
      st

   let compare (a:t) (b:t) : int =
      compare (toString a) (toString b)
end


module ArrayNe :
sig
   type 'a t

   val make    : 'a array -> 'a t ress
   val toArray : 'a t -> 'a array
end
=
struct
   type 'a t = 'a array

   let make (a:'a array) : 'a t ress =
      if (Array.length a) > 0 then Ok a else Error "ArrayNe emptiness invalid"

   let toArray (a:'a t) : 'a array =
      a
end


module DateIso8601e :
sig
   type t

   val make     : string -> t ress
   val toString : bool -> t -> string

   val yearOnly : t -> t
   val compare  : t -> t -> int
end
=
struct
   type t = string * (string * string option) option


   let make (s:string) : t ress =

      (Result_.errorMap (fun e -> "date " ^ e) (String_.trimTrunc (s, 11)))

      |>=

      (* check non-zero length *)
      (fun (st:string) ->
         if String_.isEmpty st then Error "date is empty" else Ok st)

      |>=-

      (* separate first char *)
      (fun (st:string) ->
         let len = String.length st in
         if (len > 0) && (st.[0] = '-')
         then ("-" , String.sub st 1 (len - 1))
         else ("" , st))

      |>=

      (* convert non-compact to compact *)
      (fun ((bce:string) , (sr:string)) ->
         let sc = String_.filter (fun c -> c <> '-') sr in
         (* detect if compact format or not (containing hyphens) *)
         let len  = (String.length sr) in
         let diff = len - (String.length sc) in
         if diff = 0
         then Ok (bce , sr)
         else
            (* check: hyphens valid (presence then absence) *)
            if ((len >  4) && (sr.[4] <> '-'))  ||
               ((len >  7) && (sr.[7] <> '-'))  ||
               ((len <= 7) && (diff > 1))       ||
               ((len >  7) && (diff > 2))
            then Error "date hyphens invalid"
            else Ok (bce , sc))

      |>=

      (* extract parts to build datastruct *)
      (fun ((bce:string) , (sc:string)) ->
         let checkDigitsX (s:string) (b:bool) : string ress =
            let dd , xb =
               if b
               (* one or more digits, then all digits or all 'X' *)
               then
                  let len = String.length s
                  and px  = String_.indexl 'X' s in
                  let dx  = String.sub s px (len - px) in
                  (  String.sub s 0 px ,
                     (px = 0) || not (String_.check (fun c -> c = 'X') dx) )
               else
                  (s , false)
            in
            if xb || not (String_.check Char_.isDigit dd)
            then Error "date digits invalid"
            else Ok s
         in

         (* check length *)
         let len = String.length sc in
         if (len <> 4) && (len <> 6) && (len <> 8)
         then Error "date length invalid"
         else
            (* build *)
            match checkDigitsX (String.sub sc 0 4) true with
            | Error _ as e -> e
            | Ok year      ->
               if len = 4
               then Ok (bce ^ year , None)
               else
                  match checkDigitsX (String.sub sc 4 2) false with
                  | Error _ as e -> e
                  | Ok month     ->
                     if len = 6
                     then Ok (bce ^ year , Some (month, None))
                     else
                        match checkDigitsX (String.sub sc 6 2) false with
                        | Error _ as e -> e
                        | Ok day       ->
                           Ok (bce ^ year , Some (month, Some day)))


   let toString (isCompact:bool) (d:t) : string =

      let sep = if isCompact then "" else "-" in

      match d with
      | (year , None)                    -> year
      | (year , Some (month , None))     -> year ^ sep ^ month
      | (year , Some (month , Some day)) -> year ^ sep ^ month ^ sep ^ day


   let yearOnly (d:t) : t =
      match d with | (year , _) -> (year , None)


   let compare (d0:t) (d1:t) : int =
      compare (toString true d0) (toString true d1)
end


module Isbn :
sig
   type t

   val make  : string -> t ress
   val check : string -> bool

   val length           : t -> int
   val makeChecksum     : t -> char
   val checkChecksum    : t -> bool
   val toString         : t -> string
   val toStringBare     : t -> string
   val toStringFull     : t -> string
   val search           : int -> int -> string -> string option
   val searchByChecksum : int -> ?len:int -> string -> string list
   val extractIsbnsFromText : bool -> int -> string list -> string list
end
=
struct
   (* module invariant: length = 13 or 10 *)
   type t = string

   (* private *)
   let checkChars (s:string) : string ress =
      (* 13: all must be digits *)
      if (String_.check Char_.isDigit s)
         ||
         (* 10: last digit may be 'X' *)
         (  ((String.length s) = 10) &&
            ((String_.last s) = 'X') &&
            (String_.check Char_.isDigit (String_.lead s 9))  )
      then Ok s
      else Error "ISBN chars invalid"

   (* private *)
   let checkLength (s:string) : string ress =
      match String.length s with
      | 13 | 10 -> Ok s
      | _       -> Error "ISBN length invalid"

   let make (s:string) : t ress =
      (* trim possible "ISBN" prefix *)
      let s =
         if (String_.lead s 4) = "ISBN"
         then (String_.trail s 4)
         else s
      in
      (* remove any spaces or hyphens *)
      let s = String_.filter (fun c -> not ((c = ' ') || (c = '-'))) s in
      (* check content *)
      s |> checkChars |>= checkLength

   let length (isbn:t) : int =
      String.length isbn


   let makeChecksum (isbn:t) : char =

      let calcChecksum (digits:string) (weightGen:int -> int) (modFactor:int)
         (toChar:int -> char) : char =
         let digits : int list =
            (List_.ofStringAscii digits)
            |> (List_.filtmap (string_of_char %> int_of_string_opt))
         in
         let weights  = List_.unfoldl weightGen (List.length digits) in
         let products = List.map2 ( * ) digits weights in
         let sum      = List.fold_left (+) 0 products in
         toChar ((modFactor - (sum mod modFactor)) mod modFactor)
      in

      let firstDigits = String_.lead isbn (String_.lastPos isbn) in
      match String.length firstDigits with
      | 12 ->
         calcChecksum firstDigits (fun i -> if (i land 1) = 0 then 1 else 3) 10
            (fun i -> (string_of_int i).[0])
      |  9 ->
         calcChecksum firstDigits (fun i -> 10 - i) 11
            (fun i -> if i < 10 then (string_of_int i).[0] else 'X')
      | _ -> ' ' (* not possible: module invariant *)


   let checkChecksum (isbn:t) : bool =
      (String_.notEmpty isbn) &&
         ((String_.last isbn) = (makeChecksum isbn))

   let check (s:string) : bool =
      match make s with
      | Ok i    -> checkChecksum i
      | Error _ -> false

   let toString (isbn:t) : string =
      isbn

   let toStringBare = toString

   let toStringFull (isbn:t) : string =
      match length isbn with
      (* 13: ISBN 3 digits - 9 digits - 1 digits *)
      | 13 ->
         "ISBN "
         ^ (String.sub isbn  0 3) ^ "-"
         ^ (String.sub isbn  3 9) ^ "-"
         ^ (String.sub isbn 12 1)
      (* 10: ISBN 9 digits digit/X *)
      | _  -> "ISBN " ^ isbn


   let search (pos:int) (len:int) (text:string) : string option =

      (* precondition params *)
      let pos = clamp ~lo:0 ~up:(String.length text) pos in
      let len =
         if len >= 0
         then min len ((String.length text) - pos)
         else String.length text
      in

      let matchIsbnNum (txt:string) (pos:int) : (string option) =
         let matchIsbnH13 , matchIsbnH10 , matchIsbnM13 , matchIsbnM10 =
            let matchG1 (rx:Str.regexp) (len:int) (txt:string) (pos:int)
               : string option =
               if Str.string_match rx txt pos
               then
                  let isbn = Str.matched_group 2 txt in
                  if String.length isbn = len then Some isbn else None
               else
                  None
            and front  = "\\(^\\|[^0-9]\\)"
            and back   = "\\($\\|[^0-9]\\)"
            and hPart1 = "\\([0-9]+\\([- ]\\)[0-9]+\\3[0-9]+\\3"
            in
            (* 'human form' containing spaces or hyphens *)
            (  matchG1
                  (Str.regexp (front ^ hPart1 ^ "[0-9]+\\3[0-9]+\\)" ^ back)) 17
            ,  matchG1
                  (Str.regexp (front ^ hPart1 ^ "[0-9]*[0-9X]\\)"    ^ back)) 13
            (* 'machine form' containing only digits (maybe last X) *)
            ,  matchG1
                  (Str.regexp (front ^          "\\([0-9]+\\)"       ^ back)) 13
            ,  matchG1
                  (Str.regexp (front ^         "\\([0-9]+[0-9X]\\)" ^ back)) 10)
         in
         None
         ||> (fun () -> matchIsbnH13 txt pos)
         ||> (fun () -> matchIsbnH10 txt pos)
         ||> (fun () -> matchIsbnM13 txt pos)
         ||> (fun () -> matchIsbnM10 txt pos)
      in

      let rec searchForward (i:int) (iend:int) : string option =
         if i < iend
         then
            match matchIsbnNum text (pos + i) with
            | None           -> searchForward (i + 1) iend
            | Some _ as isbn -> isbn
         else None
      in

      searchForward 0 (min len ((String.length text) - pos))


   let searchByChecksum (pos:int) ?(len:int = -1) (text:string) : string list =

      (* precondition params *)
      let pos = clamp ~lo:0 ~up:(String.length text) pos in
      let len =
         if len >= 0
         then min len ((String.length text) - pos)
         else String.length text
      in

      (* to match contiguous 10 and 13 digit number
         (allowing last 'X' for 10, and filtering by lead '978'/'979' for 13) *)
      let rx =
         Rx.compile
         ( {|\(97[89][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]\)\||} ^
           {|\([0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][xX0-9]\)|} )
      in

      (* match number with correct checksum *)
      let matchIsbnNum (txt:string) (pos:int) : (string option) =
         (* seek basic match *)
         (Rx.apply rx ~pos txt)
         |>- (Rx.wholeFound %> Option.some)
         (* reject if invalid checksum *)
         |>- (Option_.classify checkChecksum)
      in

      (* apply matching throughout string
         (inefficiently, but probably not handling large data) *)
      let rec searchForward (i:int) (iend:int) (ls:string list) : string list =
         if i < iend
         then
            let ls =
               match matchIsbnNum text (pos + i) with
               | None      -> ls
               | Some isbn -> (isbn :: ls)
            in
            (* only advance by one, thus find all overlapping matches *)
            searchForward (i + 1) iend ls
         else
            List.rev ls
      in

      searchForward 0 len []


   let regulariseDashs (replacement:string) (text:string) : string =

      (* just a list of plausible variations, not a complete countermeasure to
         every possible villainy and madness *)

      (* HYPHEN-MINUS (ordinary '-') *)
      (* SOFT HYPHEN *)
      (* MINUS SIGN *)
      (* HYPHEN *)
      (* NON-BREAKING HYPHEN *)
      (* FIGURE DASH *)
      (* EN DASH *)
      (* EM DASH *)
      (* HORIZONTAL BAR *)
      (* SMALL EM DASH *)
      (* SMALL HYPHEN-MINUS *)
      (* FULLWIDTH HYPHEN-MINUS *)
      let rx = Str.regexp
         "-\\|\
         \xC2\xAD\\|\
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

      Str.global_replace rx replacement text


   let extractIsbnsFromText (trace:bool) (maxCount:int) (texts:string list)
      : string list =

      (* map page texts to isbns *)
      let isbnsAll : string list list =
         List.map
            (fun (text : string) : string list ->
               (*
                  example stream fragment:
                     0.0287 Tc 9.3 0 0 9.3 151.14 89.04 Tm
                     (ISBN-10: )Tj
                     0.0429 Tc 9.5 0 0 9.5 176.06 89.04 Tm
                     (0-674-53751-3 )Tj
                     0.0142 Tc -5.931 -3.586 Td

                  extracted text:
                     ISBN-10:  0-674-53751-3

                  * strings can be fragmented (or even reordered, but ignore
                    that)
                  * spaces may be absent, or spurious, hence are meaningless
                  * hyphens may be some other similar looking char

                  handles (with/without preceding/following digits/etc):
                  * ISBN 555555555X
                  * ISBN: 555555555X
                  * ISBN-10: 555555555X
                  * ISBN-10 555555555X
                  * ISBN13: 5555555555555
                  * ISBNs 555555555X...555555555X...555555555X...
                  * ISBN-10 ISBN-13 e-ISBN 555555555X...555555555X...
                    555555555X...

                  but not:
                  * ISBN 10 555555555X
               *)

               text
               (* remove all spaces *)
               |> Blanks.unifySpaces |> (String_.filter ((<>)' '))
               (* remove '-10' '-13' ISBN suffixs, and any chars up to digits *)
               |> (Str.global_replace
                  (Str.regexp_case_fold
                     {|ISBN\([^0-9]1[03]:?\|1[03]:\)?[^0-9]*|})
                  "ISBN")
               (* replace eccentric dashes *)
               |> (regulariseDashs "")
               (* extract all label+number by regex *)
               |> (Rx.allMatches
                  (Rx.compile ~caseInsens:true
                  ({|ISBN[^0-9]*|} ^
              {|\(97[89][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]\||} ^
                  {|[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][xX0-9]\)|} )))
               (* remove 'ISBN' labels *)
               |> (List.map ((Fun.flip String_.trail) 4))
               (* disambiguate 10/13 by checksum *)
               |> (List.map
                  (fun number ->
                     if String.length number < 13
                     then
                        number
                     else
                        if check number
                        then number
                        else String_.lead number 10) )
               )
            texts
      in

      (* select priority isbns: first few on copyright page *)
      let isbns : string list =
         (* check for 'library of congress', indicating copyright page *)
         let isCopyrightPages : bool list =
            (List.map
               (
                  Blanks.unifySpaces
                  (* remove all spaces *)
                  %> (String_.filter ((<>)' '))
                  (* search for marker *)
                  %> (Rx.regexSeek "libraryofcongress" ~pos:0 ~caseInsens:true)
                  %> Option_.toBool
               )
               texts)
         in
         (List.combine isCopyrightPages isbnsAll)
         (* move copyright pages to the front *)
         |> (List.stable_sort (fun (b0 , _) (b1 , _) -> ~-(compare b0 b1)))
         |> (List.split %> snd)
         |> List.flatten
         |> List_.deduplicate
      in

      traceHead trace __MODULE__ "extractIsbnsFromText" "found ISBNs" ;
      traceString trace "" (String.concat "\n" isbns) ;

      (* truncate *)
      fst (List_.bisect isbns maxCount)
end




(* ---- types ---- *)

type nameStruct = {
   title  : StringT.t ArrayNe.t ;
   author : StringT.t array ;
   date   : DateIso8601e.t array ;
   id     : (StringT.t * StringT.t) option ;
   subtyp : StringT.t option ;
   typ    : StringT.t ;
}

type nameStructLax = {
   titleLax  : StringT.t array ;
   authorLax : StringT.t array ;
   dateLax   : DateIso8601e.t array ;
   idLax     : (StringT.t * StringT.t) array ;
   subtypLax : StringT.t option ;
   typLax    : StringT.t option ;
}

type nameStructRaw = {
   titleRaw  : string list ;
   authorRaw : string list ;
   dateRaw   : string list ;
   idRaw     : string list ;
   subtypRaw : string ;
   typRaw    : string ;
}




(* ---- values ---- *)

let _MAX_NAME_LEN = 255




(* ---- normalisers ---- *)

let nonEmpties (ls:string list) : string list =

   ls
   |> (List.map StringT.filter)
   |> (List.filter (Fun.negate String_.isEmpty))


let parseNamelist (names:string) : string list =

   let replaceAmp (s:string) : string =
      Str.global_replace (Str.regexp_case_fold "&amp;") "&" s

   and isListSepChar (c:char) : bool =
      match c with
      | ';' | '|' -> true
      | _         -> false

   and splitAnds (andStr:string) : string list =
      Str.split (Str.regexp "&\\| and ") andStr

   and splitCommas (commaStr:string) : string list =
      let isMultiWord (s:string) : bool =
         String.contains (String.trim s) ' '
      in
      let items = String.split_on_char ',' commaStr in
      match List.length items with
      (* is this two normal names, or one reversed name ? *)
      (* (multiple words before comma is only a probable indicator
         -- it fails with single-word names, but those are unlikely, and where
         they might occur (eg: Plato, Cicero) they would likely not be in a
         list of authors -- instead, the editor would be given) *)
      | 2 -> if isMultiWord (List.hd items) then items else [commaStr]
      | 1 -> items
      | _ -> items
   in

   names

   (* clean a little *)
   |> Utf8.Filter.replace |> Blanks.blankSpacyCtrlChars
   |> replaceAmp

   (* split into semicolon segments *)
   |> (String_.split isListSepChar)

   (* split into 'and' segments *)
   |> (List.map splitAnds)
   |> List.flatten

   (* split into comma segments *)
   |> (List.map splitCommas)
   |> List.flatten

   (* clean a little *)
   |> (List.map String.trim)
   |> (List.filter String_.notEmpty)


let getLastName (name:string) : string =

   let name = String.trim name in

   (* is there a comma ? *)
   begin match String_.index ',' name with
   | Some pos ->
      (* Last, Others First *)
      String.sub name 0 pos
   | None     ->
      (* First Others Last *)
      name
      |> (String_.rindexp Char_.isBlank) |> (Option.value ~default:0)
      |> (String_.trail name)
   end

   |> String.trim


(* NB: truncates according to byte-length, not necessarily char-length *)
let truncateWords (max:int) (words:string list) : string list =

   words
   (* word lengths, with spaces added *)
   |> (List.map String.length)
   |> (function
      | head :: tail -> head :: List.map succ tail
      | _            -> [])
   (* successive sums *)
   |> (List.fold_left (fun ls l -> (l + List.hd ls) :: ls) [0])
   |> List.rev
   |> (function | _ :: t -> t | _ -> [])
   (* truncate after max length *)
   |> (List.combine words)
   |> (List.filter (fun (_,i) -> i <= max))
   |> List.split |> fst


let normaliseTitle (titles:string list) : StringT.t array =

   let abbrevEdition (title:string) : string =
      let editions =
         [  ( "(?\\(first\\|1st\\) edition)?"        , "1ed"  ) ;
            ( "(?\\(second\\|2nd\\) edition)?"       , "2ed"  ) ;
            ( "(?\\(third\\|3rd\\) edition)?"        , "3ed"  ) ;
            ( "(?\\(fourth\\|4th\\) edition)?"       , "4ed"  ) ;
            ( "(?\\(fifth\\|5th\\) edition)?"        , "5ed"  ) ;
            ( "(?\\(sixth\\|6th\\) edition)?"        , "6ed"  ) ;
            ( "(?\\(seventh\\|7th\\) edition)?"      , "7ed"  ) ;
            ( "(?\\(eighth\\|8th\\) edition)?"       , "8ed"  ) ;
            ( "(?\\(ninth\\|9th\\) edition)?"        , "9ed"  ) ;
            ( "(?\\(tenth\\|10th\\) edition)?"       , "10ed" ) ;
            ( "(?\\(eleventh\\|11th\\) edition)?"    , "11ed" ) ;
            ( "(?\\(twelfth\\|12th\\) edition)?"     , "12ed" ) ;
            ( "(?\\(thirteenth\\|13th\\) edition)?"  , "13ed" ) ;
            ( "(?\\(fourteenth\\|14th\\) edition)?"  , "14ed" ) ;
            ( "(?\\(fifteenth\\|15th\\) edition)?"   , "15ed" ) ;
            ( "(?\\(sixteenth\\|16th\\) edition)?"   , "16ed" ) ;
            ( "(?\\(seventeenth\\|17th\\) edition)?" , "17ed" ) ;
            ( "(?\\(eighteenth\\|18th\\) edition)?"  , "18ed" ) ;
            ( "(?\\(nineteenth\\|19th\\) edition)?"  , "19ed" ) ; ]
      and replacer (s:string) (ed:(string * string)) : string =
         Str.global_replace (Str.regexp_case_fold (fst ed)) (snd ed) s
      in
      List.fold_left replacer title editions
   in

   (* use only first *)
   match titles with
   | []         -> [||]
   | first :: _ ->
      first
      |> Utf8.Filter.replace
      |> Blanks.blankSpacyCtrlChars
      |> Blanks.unifySpaces
      |> String.trim
      (* truncate any parenthised suffix *)
      |> (fun title ->
         title
         |> (Rx.regexApply {|^\(.....+\)(.*) *$|} ~pos:0 ~caseInsens:false)
         |>- ((Fun.flip Rx.groupFound) 1)
         |> (Option.value ~default:title) )
      (* truncate after ':', if more than 7 chars before *)
      |> (fun title ->
         try
            let i = String.index title ':' in
            if i > 8 then String.sub title 0 i else title
         with Not_found -> title)
      (* abbreviate edition *)
      |> abbrevEdition
      (* replace hyphens with spaces *)
      |> (String.map (function | '-' -> ' ' | c -> c))
      (* lowercase, if all uppercase, and more than one word *)
      |> (fun title ->
         if String.contains title ' '
         then String.lowercase_ascii title
         else title)
      (* tokenise *)
      |> (String_.split ((=) ' ')) |> (List.map String.trim)
      (* constrain (non-empties, max length) *)
      |> nonEmpties |> (truncateWords 48)
      (* title-case *)
      |> (List.map String.capitalize_ascii)
      (* convert to StringT.t array *)
      |> (List_.filtmap (StringT.make % Result_.toOpt))
      |> Array.of_list


let normaliseAuthor (authors:string list) : StringT.t array =

   authors

   (* split any in-string name lists, and flatten all *)
   |> (List.map parseNamelist) |> List.flatten
   |> (List.filter String_.notEmpty)

   (* remove parenthised *)
   |> (List.map (Str.global_replace (Str.regexp "([^)]*)") ""))

   (* extract and regularise last names *)
   |> (List.map getLastName)
   |> (List.map (String.lowercase_ascii %> String_.capitaliseAll))

   (* constrain *)
   |> nonEmpties |> (truncateWords 32)
   |> (List_.filtmap (StringT.make % Result_.toOpt))
   |> Array.of_list


let normaliseDate (dates:string list) : DateIso8601e.t array =

   (* (does not recognise negative/BC or 5+ digit years) *)

   dates
   (* extract years : (DateIso8601e.t option) list *)
   |> List.map
      (fun rawDateString ->
         rawDateString
         (* : string *)
         |> (Utf8.Filter.replace % Blanks.unifySpaces)
         |> (fun s -> " " ^ s ^ " ")
         (* : string option *)
         |> (fun searchableDateString ->
            (* first 4-digit chunk (general conventional year) *)
            (  (Rx.regexSeek
                  {|[^0-9]\([0-9][0-9][0-9][0-9]\)[^0-9]|}
                  searchableDateString)
               |>-
               ((Fun.flip Rx.groupFound) 1)  )
            ||>
            (* or, first 4-digits of first 8-digit chunk (iso8601 compact) *)
            (fun () ->
               (Rx.regexSeek
                  {|[^0-9]\([0-9][0-9][0-9][0-9]\)[0-9][0-9][0-9][0-9][^0-9]|}
                  searchableDateString)
               |>-
               ((Fun.flip Rx.groupFound) 1) ) )
         (* check : DateIso8601e.t option *)
         |>- (DateIso8601e.make %> Result_.toOpt) )
   (* keep sorted, unique years *)
   |> (List_.filtmap id)
   |> (List.map DateIso8601e.yearOnly)
   |> (List.sort_uniq DateIso8601e.compare)
   (* take first and last only *)
   |> List_.hdft
   |> Array.of_list


let normaliseIsbn (isbns:string list) : (StringT.t * StringT.t) array =

   isbns
   |> List.map
      (Utf8.Filter.filter % Utf8.removeReplacementChars % Blanks.unifySpaces)
   (* to machine-readable form *)
   |> List.map (String_.filter (function | ' ' | '-' -> false | _ -> true))
   (* remove bad ones *)
   |> List.filter (fun mForm ->
      match String.length mForm with
      | 13 ->
         String_.check Char_.isDigit mForm
      | 10 ->
         let isDigitOrX = function | '0'..'9' | 'X' -> true | _ -> false
         and main,last =
            let len = String.length mForm in
            (String.sub mForm 0 (len - 1) , String.sub mForm (len - 1) 1)
         in
         (String_.check Char_.isDigit main) && (String_.check isDigitOrX last)
      | _ -> false)
   (* prioritise 13-form ones *)
   |> List.sort (fun a b -> compare (String.length b) (String.length a))
   (* pair with label *)
   |> List.map
         (fun isbn ->
            (Ok isbn)
            |^^= ( (fun _ -> StringT.make "ISBN") , StringT.make )
            |> Result_.toOpt)
   |> List_.filtmap id
   |> Array.of_list


let normaliseSubtyp (s:string) : StringT.t option =

   s
   |> Utf8.Filter.filter |> Utf8.removeReplacementChars
   |> Blanks.blankSpacyCtrlChars |> String.trim

   (* drop if contains invalid chars or is negative *)
   |> (fun s ->
      let notValid (c:char) : bool =
         match c with
         | '0'..'9' | '+' | '-' | '.' | 'e' | 'E' -> false
         | _ -> true
      in
      if String_.containsp notValid s then "" else s)
   |> (fun s -> if String_.isFirstChar ((=) '-') s then "" else s)

   (* symbols to alphas *)
   |> (String.map (function | '-' -> 'm' | '.' -> 'p' | c -> c))
   |> (String_.filter ((<>) '+'))

   (* suffix 'p' *)
   |> (function | "" -> "" | s -> s ^ "p")

   |> StringT.make |> Result_.toOpt


let normaliseString (s:string) : StringT.t option =

   s
   |> Utf8.Filter.replace |> Blanks.blankSpacyCtrlChars |> StringT.filter
   (* truncate utf8 chars to max byte length *)
   |> (fun st ->
      st
      |> (let _MAXLEN = 24 in String_.truncate _MAXLEN)
      |> Utf8.Filter.filter
      |> Utf8.removeReplacementChars)
   |> StringT.make |> Result_.toOpt


let normaliseMetadataLax (nsr:nameStructRaw) : nameStructLax =

   {  titleLax  = normaliseTitle nsr.titleRaw ;
      authorLax = normaliseAuthor nsr.authorRaw ;
      dateLax   = normaliseDate nsr.dateRaw ;
      idLax     = normaliseIsbn nsr.idRaw ;
      subtypLax = normaliseSubtyp nsr.subtypRaw ;
      typLax    = normaliseString nsr.typRaw ;  }


let normaliseMetadata_x (trace:bool) (nsl:nameStructLax) : nameStruct =

   (* title and type are mandatory *)
   let title =
      (ArrayNe.make nsl.titleLax)
      |>
      (Result_.toExc_x
         (fun msg ->
            traceHead trace __MODULE__ "normaliseMetadata_x" "" ;
            traceString trace "" msg ;
            Intolerable (EXIT_DATAERR , "no valid title found")))
   and typ =
      nsl.typLax
      |>
      (Option_.toExc_x
         (fun () ->
            let message = "no valid type" in
            traceHead trace __MODULE__ "normaliseMetadata_x" "" ;
            traceString trace "" message ;
            Intolerable (EXIT_DATAERR , message)))
   in

   {  title  = title ;
      author = nsl.authorLax ;
      date   = nsl.dateLax ;
      id     = Array_.toOpt nsl.idLax ;
      subtyp = nsl.subtypLax ;
      typ    = typ ;  }




(* ---- functions ---- *)

let unescapeJsonString (json:string) : string =

   let unescapeSimples (json:string) : string =
      (*  \""  \\  \/  \b  \f  \n  \r  \t  *)
      let rx = Str.regexp_case_fold {|\\["\/bfnrt]|} in
      Str.global_substitute
         rx
         (fun wholeString ->
            let esc = Str.matched_string wholeString in
            match esc with
            | "\\\""       -> "\""
            | "\\\\"       -> "\\"
            | "\\/"        -> "/"
            | "\\f"        -> "\x0C"
            | "\\b"        -> "\b"
            | "\\n"        -> "\n"
            | "\\r"        -> "\r"
            | "\\t"        -> "\t"
            | unrecognised -> unrecognised )
         json
   in

   json
   |>
   unescapeSimples
   |>
   (Utf8.Codec.ofU16Esc true)




(* ---- other functions ---- *)

(* nameStruct related *)

let isTextform (s:string) : bool =
   String.contains s '/'


let extractNameHalfs (name:string) (isText:bool) (metaSep:char) :
   (string * string) ress =

   match String_.index metaSep name with
   | Some pos ->
      Ok (
         let p , m = (String.sub name 0 pos ,
            String.sub name (pos + 1) ((String.length name) - (pos + 1)))
         in
         if isText
         then (String.trim p , String.trim m)
         else (p , m) )
   | None -> Error "no plain/meta divider"


let splitHalfIntoParts (half:string) (sep:char) (isText:bool)
   (halfName:string) : (string list) ress =

   let lp = String_.split ((=) sep) half in
   if (List.length lp < 1) || (List.length lp > 3)
   then Error ("wrong number of " ^ halfName ^ " parts")
   else
      let lp = if isText then List.map String.trim lp else lp in
      if (List.exists String_.isEmpty lp)
      then Error ("empty " ^ halfName ^ " parts")
      else Ok lp


let extractPlainParts (plain:string) (isText:bool) (partSep:char)
   (oQuo:char option) (subSep:char array)
   : (StringT.t ArrayNe.t * StringT.t array * DateIso8601e.t array) ress =

   (* split plain into parts *)
   (splitHalfIntoParts plain partSep isText "plain")

   |>=

   (fun (parts:string list) ->

      (* differentially handle parts *)
      let f parts index delQuotes subpartSep filterEmpty trimSubparts negLead
         maker =

         match List_.nth index parts with
         | Some part ->
            (* remove quotes *)
            begin match delQuotes with
            | Some q ->
               let len = String.length part in
               if (len < 2) || (part.[0] <> q) || (part.[len - 1] <> q)
               then Error "bad title quotes"
               else Ok (String.sub part 1 (len - 2))
            | None -> Ok part
            end

            |>=-

            (* split into subparts *)
            (fun (part:string) ->
               String_.split ((=) subpartSep) part)

            |>=-

            (* interpret empty date as leading '-' *)
            (fun (subparts:string list) ->
               if negLead
               then
                  (* prepare parallel shifted tuple-list *)
                  let lt =
                     let a = List.map (fun e -> Some e) subparts in
                     let b = [None ; None] @ a
                     and c = [None] @ a @ [None]
                     and d = a @ [None ; None]
                     in
                     List.combine b (List.combine c d)
                  in
                  (* apply modification kernel *)
                  let lm = List.map
                     (function
                        | (Some "", (Some s, _)) when (s <> "") ->
                           Some ("-" ^ s)
                        | (_, (Some "", Some s)) when (s <> "") -> None
                        | (_, (e, _))                           -> e)
                     lt
                  in
                  (* remove padding/blanks *)
                  List_.filtmap Fun.id lm
               else subparts)

            |>=-

            (fun (subparts:string list) ->
               if filterEmpty
               then List.filter (Fun.negate String_.isEmpty) subparts
               else subparts)

            |>=-

            (fun (subparts:string list) ->
               if trimSubparts then List.map String.trim subparts else subparts)

            |>=

            (fun (subparts:string list) ->
               if (List.exists String_.isEmpty subparts)
               then Error "empty plain subparts"
               else Ok subparts)

            |>=

            (fun (subparts:string list) ->
               (* make subparts *)
               let lmo = List.map maker subparts in
               (* if any errors, just get first one *)
               match
                  List_.find (function | Error _ -> true | Ok _ -> false) lmo
               with
               | Some (Error _ as e)  -> e
               | Some (Ok _) | None   ->
                  let lm = List_.filtmap Result_.toOpt lmo in
                  Ok (Array.of_list lm))

         | None -> Ok [||]
      in

      (Ok parts)

      |^^^=

      begin
         (fun (parts:string list) ->
            (f parts 0 oQuo subSep.(0) isText false false StringT.make)
            |>= ArrayNe.make
            |> Result_.errorMap (fun e -> ("title " ^ e)))
         ,
         (fun (parts:string list) ->
            (f parts 1 None subSep.(1) false isText false StringT.make)
            |> Result_.errorMap (fun e -> ("author " ^ e)))
         ,
         (fun (parts:string list) ->
            f parts 2 None subSep.(2) false isText (not isText)
               DateIso8601e.make)
      end )


let extractMetaParts (meta:string) (isText:bool) (metaSep:char)
   : ((StringT.t * StringT.t) option * StringT.t option * StringT.t) ress =

   begin
      (* maybe remove end-char *)
      begin if isText
      then
         let len = String.length meta in
         if meta.[len - 1] <> '.'
         then Error "no meta terminator"
         else Ok (String.sub meta 0 (len - 1))
      else Ok meta
      end

      |>=

      (* split meta into parts *)
      (fun (meta:string) ->
         splitHalfIntoParts meta metaSep isText "meta")

      |>=

      (* line-up/disambiguate optional parts *)
      (fun (parts:string list) ->
         match parts with
         | typ :: []                 -> Ok (None, None, typ)
         | idOrSubtyp :: typ :: []   ->
            if String.contains idOrSubtyp '-'
            then Ok (Some idOrSubtyp, None, typ)
            else Ok (None, Some idOrSubtyp, typ)
         | id :: subtyp :: typ :: [] -> Ok (Some id, Some subtyp, typ)
         | _ -> Error "unrecognised meta parts")
   end

   |^^^=

   (* differentially handle parts *)
   begin
      (fun ((id:string option) , (_:string option) , (_:string)) ->
         match id with
         | Some s ->
            begin match String_.split ((=) '-') s with
            | label :: code :: [] ->
               (let lco =
                  Ok (label , code)
                  |^^=
                  (  (fun ((label:string) , (_:string)) ->
                        StringT.make label)
                     ,
                     (fun ((_:string) , (code:string))  ->
                        StringT.make code) )
               in
               match lco with
               | Ok lc   -> Ok (Some lc)
               | Error e -> Error ("id " ^ e))
            | _  -> Error "bad id"
            end
         | None -> Ok None)
      ,
      (fun ((_:string option) , (subtyp:string option) , (_:string)) ->
         match subtyp with
         | Some s ->
            begin match StringT.make s with
            | Ok o    -> Ok (Some o)
            | Error e -> Error ("subtype " ^ e)
            end
         | None   -> Ok None
      )
      ,
      (fun ((_:string option) , (_:string option) , (typ:string)) ->
         Result_.errorMap (fun e -> "type " ^ e) (StringT.make typ))
   end


let makeNameStruct_x (s:string) : nameStruct =

   (* set parameters for text or name forms *)
   let isText , plainSep , metaSep , oQuo , subSep =
      if isTextform s
      then true  , ';' , '/' , Some '"' , [| ' ' ; ',' ; ',' |]
      else false , '_' , '.' , None     , [| '-' ; '-' ; '-' |]
   in

   (s, _MAX_NAME_LEN)
   |>
   String_.trimTrunc
   |>=
   (* split into halves *)
   (fun (s:string) ->
      (extractNameHalfs s isText metaSep)
      |>=
      (* check not empty *)
      (fun ((plain:string) , (meta:string)) ->
         if (String_.isEmpty plain) || (String_.isEmpty meta)
         then Error "empty plain or meta part"
         else Ok (plain , meta)))
   |^^=
   (* extract parts *)
   (  (fun ((plain:string) , _) ->
         extractPlainParts plain isText plainSep oQuo subSep)
      ,
      (fun (_ , (meta:string)) ->
         extractMetaParts meta isText metaSep) )
   |>=-
   (* build *)
   (fun (tad , ist) ->
      let (title , author , date) , (id , subtyp , typ) = tad , ist in
      { title ; author ; date ; id ; subtyp ; typ })

   |>
   (Result_.toExc_x (fun msg -> Intolerable (EXIT_DATAERR , msg)))


let toStringNameStruct (name:nameStruct) sq st s0 s1 s2 se : string =

   let toStrings = Array.map StringT.toString in

   (sq ^ (String.concat st (Array.to_list
      (toStrings (ArrayNe.toArray name.title)))) ^ sq)
   ^
   begin if Array.length name.author > 0
   then s0 ^ (String.concat s2 (Array.to_list (toStrings name.author)))
      ^
      if Array.length name.date > 0
      then s0 ^ (String.concat s2 (Array.to_list
         (Array.map (fun e -> DateIso8601e.toString (s2 = "-") e) name.date)))
      else ""
   else ""
   end
   ^
   begin match name.id with
   | Some (label,code) ->
      s1 ^ (StringT.toString label) ^ "-" ^ (StringT.toString code)
   | None              -> ""
   end
   ^
   begin match name.subtyp with
   | Some subtype -> s1 ^ (StringT.toString subtype)
   | None         -> ""
   end
   ^
   (s1 ^ (StringT.toString name.typ) ^ se)


let toStringName (name:nameStruct) : string =
   toStringNameStruct name ""  "-"  "_"  "."  "-"  ""


let toStringText (name:nameStruct) : string =
   toStringNameStruct name "\""  " "  "; "  " / "  ", "  "."


let rawToString (nsr:nameStructRaw) : string =
   [  ("titles:  " , nsr.titleRaw     ) ;
      ("authors: " , nsr.authorRaw    ) ;
      ("dates:   " , nsr.dateRaw      ) ;
      ("isbns:   " , nsr.idRaw        ) ;
      ("pages:   " , [ nsr.subtypRaw ]) ;
      ("type:    " , [ nsr.typRaw    ]) ]
   |>
   (List.map (fun (label , value) -> label ^ (String.concat " | " value)))
   |>
   (String.concat "\n")
