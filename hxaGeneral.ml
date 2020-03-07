(*------------------------------------------------------------------------------

   HXA General lib (OCaml 4.02)
   Harrison Ainsworth / HXA7241 : 2015

   http://www.hxa.name/tools/

   License: CC0 -- http://creativecommons.org/publicdomain/zero/1.0/

------------------------------------------------------------------------------*)




(* ---- types ---- *)

type 'a eoption = Ok of 'a | Err of string




(* ---- functions ---- *)

let fail (message:string) : 'a =
   begin
      prerr_endline ("*** Failed: " ^ message ^ ".") ;
      exit 1
   end


let splitFilePathName (pathName:string) : (string * string) =
   try
      let namePos = (String.rindex pathName '/') + 1 in
      let nameLen = (String.length pathName) - namePos in
      ( String.sub pathName 0 namePos , String.sub pathName namePos nameLen )
   with
   | Not_found -> ("" , "")


let getFilePath (pathName:string) : string =
   fst (splitFilePathName pathName)


let getFileName (pathName:string) : string =
   snd (splitFilePathName pathName)


let fNot (f:'a -> bool) : ('a -> bool) =
   fun p -> not (f p)


let id (v:'a) : 'a =
   v


let eopToOpt (veo:'a eoption) : 'a option =
   match veo with
   | Ok v  -> Some v
   | Err _ -> None


let optToEop (vo:'a option) (s:string) : 'a eoption =
   match vo with
   | Some v -> Ok v
   | None   -> Err s


let mapOpt (f:'a -> 'b) (o:'a option) : 'b option =
   match o with
   | Some a -> Some (f a)
   | None   -> None


let mapErr (f:string -> string) (eo:'a eoption) : 'a eoption =
   match eo with
   | Ok _ as o -> o
   | Err e     -> Err (f e)


let ( % ) f g x =
   g (f x)


let ( ||> ) (o:'a option) (f:unit -> 'b option) : 'b option =
   match o with
   | Some _ as a -> a
   | None        -> f ()


let ( &&> ) (o:'a option) (f:unit -> 'b option) : 'b option =
   match o with
   | Some _ -> f ()
   | None   -> None


let ( |>= ) (eo:'a eoption) (f:'a -> 'b eoption) : 'b eoption =
   match eo with
   | Ok v       -> f v
   | Err _ as e -> e


let ( |^^= ) (eo:'a eoption) ((f0:'a -> 'b eoption) , (f1:'a -> 'c eoption))
   : ('b * 'c) eoption =

   eo |>= (fun v ->
      match ((f0 v) , (f1 v)) with
      | (Ok b , Ok c)          -> Ok (b , c)
      | ((Err _ as e0) , Ok _) -> e0
      | (Ok _ , (Err _ as e1)) -> e1
      | (Err s0 , Err s1)      -> Err (s0 ^ " && " ^ s1))


let ( |^^^= ) (eo:'a eoption)
   ((f0:'a -> 'b eoption) , (f1:'a -> 'c eoption) , (f2:'a -> 'd eoption))
   : ('b * 'c * 'd) eoption =

   let f12 = (fun v -> (Ok v) |^^= (f1 , f2)) in
   match eo |^^= (f0 , f12) with
   | Ok (b , (c , d)) -> Ok (b , c , d)
   | Err _ as e       -> e

   (*eo |>= (fun v ->
      match ((f0 v) , (f1 v) , (f2 v)) with
      | (Ok b , Ok c , Ok d)          -> Ok (b , c , d)
      | ((Err _ as e0) , Ok _ , Ok _) -> e0
      | (Ok _ , (Err _ as e1) , Ok _) -> e1
      | (Ok _ , Ok _ , (Err _ as e2)) -> e2
      | (Err s0 , Err s1 , Ok _)      -> Err (s0 ^ " && " ^ s1)
      | (Err s0 , Ok _ , Err s2)      -> Err (s0 ^ " && " ^ s2)
      | (Ok _ , Err s1 , Err s2)      -> Err (s1 ^ " && " ^ s2)
      | (Err s0 , Err s1 , Err s2)    -> Err (s0 ^ " && " ^ s1 ^ " && " ^ s2))*)


let ( |>+ ) (v:'a) (f:'a -> 'b eoption) : 'b eoption =
   (Ok v) |>= f


let ( |>- ) (eo:'a eoption) (f:'a -> 'b) : 'b eoption =
   eo |>= (fun v -> Ok (f v))


let string_of_char (c:char) : string =
   String.make 1 c


let string_of_byte (b:int)  : string =
   String.make 1 (char_of_int (b land 0xFF))


let unifySpaces (s:string) : string =
   let rx = Str.regexp
      "\x09\\|\x0A\\|\x0B\\|\x0C\\|\x0D\\|\x20\\|\
      \xC2\x85\\|\xC2\xA0\\|\
      \xE1\x9A\x80\\|\xE1\xA0\x8E\\|\
      \xE2\x80\x80\\|\xE2\x80\x81\\|\xE2\x80\x82\\|\xE2\x80\x83\\|\
      \xE2\x80\x84\\|\xE2\x80\x85\\|\xE2\x80\x86\\|\
      \xE2\x80\x87\\|\xE2\x80\x88\\|\
      \xE2\x80\x89\\|\xE2\x80\x8A\\|\
      \xE2\x80\x8B\\|\xE2\x80\x8C\\|\xE2\x80\x8D\\|\
      \xE2\x80\xA8\\|\xE2\x80\xA9\\|\
      \xE2\x80\xAF\\|\
      \xE2\x81\x9F\\|\
      \xE2\x81\xA0\\|\
      \xE3\x80\x80\\|\
      \xEF\xBB\xBF"
   in
   Str.global_replace rx " " s


let blankSpacyCtrlChars : (string -> string) =
   String.map (function
      | '\x09' | '\x0A' | '\x0B' | '\x0C' | '\x0D' -> ' ' | c -> c)


let blankNewlines : (string -> string) =
   String.map (function | '\n' | '\r' -> ' ' | c -> c)




(* ---- std lib module augmentations ---- *)

module Char :
sig
   include module type of Char

   val isDigit : char -> bool
end
=
struct
   include Char

   let isDigit (c:char) : bool =
      match c with | '0'..'9' -> true | _ -> false
end


module String :
sig
   include module type of String

   val isEmpty     : string -> bool
   val index_o     : char -> string -> int option
   val indexl      : char -> string -> int
   val filter      : (char -> bool) -> string -> string
   val filterAscii : string -> string
   val check       : (char -> bool) -> string -> bool
   val split       : ?ls:(string list) -> char -> string -> string list
   val trimTrunc   : (string * int) -> string eoption
   val truncate    : int -> string -> string
end
=
struct
   include String

   let isEmpty (s:string) : bool =
      (String.length s = 0)

   let index_o (c:char) (s:string) : int option =
      try Some (String.index s c) with Not_found -> None

   let indexl (c:char) (s:string) : int =
      try (String.index s c) with Not_found -> String.length s

   let filter (pred: char -> bool) (s:string) : string =
      let iEnd = String.length s in
      let rec r (i:int) (sf:string) : string =
         if i < iEnd
         then r (i + 1) (sf ^ (if pred s.[i] then string_of_char s.[i] else ""))
         else sf
      in
      r 0 ""

   let filterAscii : (string -> string) =
      filter (fun c -> (int_of_char c) <= 127)

   let check (pred: char -> bool) (s:string) : bool =
      (String.length (filter pred s)) = (String.length s)

   let rec split ?(ls:string list = []) (div:char) (s:string) : string list =
      try
         let pos = String.rindex s div in
         let half1 = String.sub s 0 pos
         and half2 = String.sub s (pos + 1) ((String.length s) - (pos + 1))
         in
         split ~ls:(half2 :: ls) div half1
      with
      | Not_found -> (s :: ls)

   let trimTrunc ((s:string) , (max:int)) : string eoption =
      let st = String.trim s in
      if (String.length st) <= max
      then Ok st
      else Err ("too long (> " ^ (string_of_int max) ^ ")")

   let truncate (max:int) (s:string) : string =
      if String.length s <= max then s else String.sub s 0 max
end


module List :
sig
   include module type of List

   val nth_o   : int -> 'a list -> 'a option
   val find_o  : ('a -> bool) -> 'a list -> 'a option
   val filtmap   : ('a -> 'b option) -> 'a list -> 'b list
   (*val findmap_o : ('a -> 'b option) -> 'a list -> 'b option*)
   (*val unfold    : ?list:('a list) -> (int->'a) -> int -> 'a list*)
   (*val ofStringAscii : ?lc:(char list) -> string -> char list*)
end
=
struct
   include List

   let nth_o (index:int) (l:'a list) : 'a option =
      try Some (List.nth l index) with Failure _ -> None

   let find_o (f:'a -> bool) (l:'a list) : 'a option =
      try Some (List.find f l) with Not_found -> None

   let filtmap (f:'a -> 'b option) (l:'a list) : 'b list =
      List.fold_right (fun ea out ->
         match f ea with | Some eb -> eb :: out | None -> out) l []

   (*let rec findmap_o (predmap:'a -> 'b option) (l:'a list) : 'b option =
      match l with
      | a :: tail ->
         begin match predmap a with
         | None        -> findmap predmap tail
         | Some _ as b -> b
         end
      | [] -> None*)

   (*let rec unfold ?(list = []) (f:int->'a) (size:int) : 'a list =
      if size > 0
      then unfold ~list:((f (size - 1)) :: list) f (size - 1)
      else list*)

   (*let rec ofStringAscii ?(lc:char list = []) (s:string) : char list =
      let len = String.length s in
      if len > 0
      then ofStringAscii ~lc:(s.[len - 1] :: lc) (String.sub s 0 (len - 1))
      else lc*)
end
