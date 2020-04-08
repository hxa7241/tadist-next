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
      (* whitelist: alpha numeric *)
      match c with
      | 'a'..'z' | 'A'..'Z' | '0'..'9' -> true
      | _                              -> false
      (* blacklist: TADIST format chars, unix bad filename chars, (and '\') *)
      (*match c with
      | '-' | '_' | '.' | ' ' | ',' | ';' | '/' | '"'
      | (*'/' | '\x00' |*) '\x00'..'\x1F' | '\x7F' | '\\' -> false
      | _                                                 -> true*)

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

   val make : string -> t ress

   val length       : t -> int
   val toString     : t -> string
   val toStringBare : t -> string
   val toStringFull : t -> string
end
=
struct
   type t = string

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


let normaliseTitle (titles:string list) : StringT.t ArrayNe.t ress =

   (* use only first *)
   match titles with
   | []         -> Error "no title found"
   | first :: _ ->
      first
      |> Utf8filter.replace |> Blanks.blankSpacyCtrlChars
      (* truncate after ':', if more than 7 chars before *)
      |> (fun title ->
         try
            let i = String.index title ':' in
            if i > 8 then String.sub title 0 i else title
         with Not_found -> title)
      (* tokenise *)
      |> String.trim |> (String_.split ((=) ' ')) |> (List.map String.trim)
      (* constrain (non-empties, max length, StringT) *)
      |> nonEmpties |> (truncateWords 48)
      |> (List_.filtmap (StringT.make % Result_.toOpt))
      (* convert to (non-empty) array *)
      |> Array.of_list |> ArrayNe.make
      |> Result_.errorMap (fun _ -> "no valid title")


let normaliseAuthor (trace:bool) (authors:string list) : StringT.t array =

   authors
   |> (List.map (Utf8filter.replace % Blanks.blankSpacyCtrlChars))
   (* unified list of all names *)
   |> (fun authors ->
      authors
      (* first, split by ';'s *)
      |> (List.map (fun s -> String_.split ((=) ';') s))
      |> List.flatten
      (* then, split those by 'and' and ','s *)
      |> (
         let rx1 = Str.regexp_string_case_fold " and "
         and rx2 = Str.regexp_case_fold " and \\|,"
         in
         List.map (fun s ->
            if
               (try ignore(Str.search_forward rx1 s 0) ; true
               with Not_found -> false)
            then
               let ss = Str.global_replace rx2 " ; " s in
               String_.split ((=) ';') ss
            else [s]) )
      |> List.flatten
      (* clean-up *)
      |> (List.map String.trim)
      |> (List.filter (Fun.negate String_.isEmpty)))
   |> (fun names ->
      if trace
      then print_endline ("* names:      " ^ (String.concat " | " names)) ;
      names)
   (* extract last names *)
   |> (List.map (fun s ->
         String.trim
            (* try "last, others first" *)
            (try
               let i = String.index s ',' in
               String.sub s 0 i
            (* otherwise "first others last" *)
            with Not_found ->
               let i = try String.rindex s ' ' with Not_found -> 0 in
               String.sub s i ((String.length s) - i))))
   |> (fun lastNames ->
      if trace
      then print_endline ("* last-names: " ^ (String.concat " | " lastNames)) ;
      lastNames)
   (* constrain *)
   |> nonEmpties |> (truncateWords 32)
   |> (List_.filtmap (StringT.make % Result_.toOpt))
   |> Array.of_list


let normaliseDate (dates:string list) : DateIso8601e.t array =

   dates
   |> List.map (Utf8filter.replace % Blanks.unifySpaces)
   (* truncate time from presumed iso8601 dateTtime *)
   |> (List.map (fun s ->
      let s = String.trim s in
      let i =
         try String.index s 'T' with
         | Not_found ->
            try String.index s ' ' with
            | Not_found -> String.length s
      in
      String.sub s 0 i))
   (* check, and just keep OK, sorted, unique years *)
   |> (List.map DateIso8601e.make)
   |> (List_.filtmap Result_.toOpt)
   |> (List.map DateIso8601e.yearOnly)
   |> (List.sort_uniq DateIso8601e.compare)
   (* first and last only *)
   |> (function
      | []            -> [||]
      | [single]      -> [| single |]
      | first :: tail ->
         let last = List.hd (List.rev tail) in
         [| first ; last |])


let normaliseIsbn (isbns:string list) : (StringT.t * StringT.t) option =

   isbns
   |> List.map (Utf8filter.filter % Blanks.unifySpaces)
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
   (* choose first 13-form one *)
   |> List.sort (fun a b -> compare (String.length b) (String.length a))
   |> (function
      | first :: _ ->
         (Ok first)
         |^^= ( (fun _ -> StringT.make "ISBN") , StringT.make )
         |> Result_.toOpt
      | _ -> None)


let normaliseString (s:string) : StringT.t option =

   s
   |> Utf8filter.replace |> Blanks.blankSpacyCtrlChars |> StringT.filter
   (* truncate utf8 chars to max byte length *)
   |> (fun st ->
      st
      |> (let _MAXLEN = 24 in String_.truncate _MAXLEN)
      |> Utf8filter.filter)
   |> StringT.make |> Result_.toOpt


let normaliseMetadata (trace:bool) (nsr:nameStructRaw) : nameStruct ress =

   (* title is mandatory *)
   match normaliseTitle nsr.titleRaw with
   | Error _ as e -> e
   | Ok title     ->
      (* type is mandatory *)
      match normaliseString nsr.typRaw with
      | None     -> Error "invalid type"
      | Some typ ->
         (* the rest are optional *)
         Ok {
            title  = title ;
            author = normaliseAuthor trace nsr.authorRaw ;
            date   = normaliseDate nsr.dateRaw ;
            id     = normaliseIsbn nsr.idRaw ;
            subtyp = normaliseString nsr.subtypRaw ;
            typ    = typ }




(* ---- outer functions ---- *)

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
   (oQuo:char option) (subSep:char array) :
   (StringT.t ArrayNe.t * StringT.t array * DateIso8601e.t array) ress =

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
      end
   )


let extractMetaParts (meta:string) (isText:bool) (metaSep:char) :
   ((StringT.t * StringT.t) option * StringT.t option * StringT.t)
   ress =

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


let makeNameStruct (s:string) : nameStruct ress =

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
   begin
      (fun ((plain:string) , _) ->
         extractPlainParts plain isText plainSep oQuo subSep)
      ,
      (fun (_ , (meta:string)) ->
         extractMetaParts meta isText metaSep)
   end

   |>=-

   (* build *)
   (fun (tad , ist) ->
      let (title , author , date) , (id , subtyp , typ) = tad , ist in
      { title ; author ; date ; id ; subtyp ; typ })


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
   toStringNameStruct name "\""  " "  " ; "  " / "  ", "  " ."
