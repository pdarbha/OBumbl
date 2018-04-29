open Yojson.Basic
open Yojson.Basic.Util

(* Limitations -- ** sanitize/escape all inputs before pushing to server **
* Name - 64 char (full name) -- alphanumeric + spaces
* set a reasonable image size limit (2mb?) and try to compress - upload as base 64 encoding
* school - 64 char (alphanumeric + spaces)
* groups - cannot join more than 100 groups -- g1;g2;g3;... for integer ids
* description - 800 characters (alphanumeric + spaces + *basic* punctuation)
* interests - 15 char limit per interest, 40 interests max (alphanumeric + spaces) -- i0;i1;i2;...;i39
* experience - "BEG" or "INT" or "ADV"
* role - 32 char (alphanumeric + spaces)
* looking_for - "BEG" or "INT" or "ADV" for exp, 32 for role (alphanumeric + spaces) -- exp;role
* Github URL - 60 char (URL encoded then decoded -- example: https://www.urlencoder.org)
*)

(* will store all information about each person including id, name, photo, description, etc.*)
type profile = {user_id:int; name:string; photo:string; school:string; group_id_list: int list;
                description: string; interest_list: string list; experience : [ `BEG | `INT | `ADV ];
                role: string; looking_for: ([ `BEG | `INT | `ADV ]*string);
                github_url : string}

let encode_url u = Netencoding.Url.encode u

let decode_url u = Netencoding.Url.decode u

let string_to_exp s =
  if s = "BEG" then `BEG
  else if s = "INT" then `INT
  else if s = "ADV" then `ADV
  else failwith "Experience must be \"BEG\", \"INT\", or \"ADV\""

let string_to_looking_for s =
  (string_to_exp (String.sub s 0 3), String.sub s 4 ((String.length s)-4))
(*
Used to remove escaped quotation marks when that is what the JSON has,
not necessary though.
let to_string_trimmed j field=
  let s = j|>member field|>to_string in
  if String.length s <2 then "" else
  (String.sub s 1 (String.length s -2))
*)

(* will take in Json file and parse it and store that information in an object of type profile *)
let init j =
  let id = j|>member "user_id"|>to_string|>int_of_string in
  let name = j|>member "name"|>to_string in
  let photo = j|>member "photo"|>to_string in
  let school = j|>member "school"|>to_string in
  let groups = let s = j|>member "group_list"|>to_string in
      if s = "" then [] else s|>String.split_on_char ';'|>List.map int_of_string in
  let desc = j|>member "description"|>to_string in
  let interests = j|>member "interest_list"|>to_string|>String.split_on_char ';' in
  let exp = j|>member "experience"|>to_string|>string_to_exp in
  let role = j|>member "role"|>to_string in
  let look_for = j|>member "looking_for"|>to_string|>string_to_looking_for in
  let github = j|>member "github_url"|>to_string |> decode_url in
  {user_id = id; name = name; photo = photo; school = school; group_id_list = groups;
   description = desc; interest_list = interests; experience = exp; role = role;
   looking_for = look_for; github_url = github}


(* will return the unique user id associated with a profile *)
let user_id p = p.user_id

(* will return the name of a user *)
let name p = p.name

(* will return the photo of a user, encoded as a Base64 string *)
let photo p = p.photo

(* will return the school of a user *)
let school p = p.school

(* will return a list of a user’s tags *)
let groups p = p.group_id_list

(* will return a user’s description *)
let description p = p.description

(* will return a list of keyword interests for a user *)
let interests p = p.interest_list

(* will return the experience of a user as a variant *)
let experience p = p.experience

(* will return the role of a user *)
let role p = p.role

(* will return a tuple of experience as variant and role as string that user is looking for *)
let looking_for p = p.looking_for

 (* will return a string to a github.com profile URL *)
let github p = p.github_url

let list_to_string l=
  match l with
  | [] -> ""
  | _ -> let s = List.fold_left (fun s1 s2 -> s1^";"^s2) "" l in
         String.sub s 1 ((String.length s)-1)

let int_list_to_string l =
  let l' = List.map (string_of_int) l in
  list_to_string l'

let exp_to_string e =
  match e with
  |`BEG -> "BEG"
  |`INT -> "INT"
  |`ADV -> "ADV"

let looking_for_to_string lf = (exp_to_string (fst lf)) ^ ";" ^ (snd lf)

let to_json (p:profile) : json =
  let photo = match p.photo with None -> `Null | Some s -> `String s in
  `Assoc [("user_id", `Int p.user_id);("name", `String p.name);("photo", photo);
          ("school",`String p.school);("group_list", `String (int_list_to_string (p.group_id_list)));
          ("description", `String p.description);
          ("interest_list", `String (list_to_string (p.interest_list)));
          ("experience", `String (exp_to_string (p.experience))); ("role", `String (p.role));
          ("looking_for", `String ((exp_to_string (fst p.looking_for))^";"^(snd p.looking_for)));
          ("github_url", `String (p.github_url))]

(* will accept a profile and two strings (field, value) to be updated and returned in new profile *)
let edit p field new_val =
  match field with
  |"user_id" -> (try let i = int_of_string new_val in {p with user_id = i}
                with _ -> failwith "Tried to set user_id to a non integer value")
  |"name" -> {p with name = new_val}
  |"photo" -> if new_val = "null" then {p with photo = None} else {p with photo = Some new_val}
  |"school" -> {p with school = new_val}
  |"group_id_list" -> (try {p with group_id_list = List.map (int_of_string) (String.split_on_char ';' new_val)}
                with _ -> failwith "Tried to give group_id non integer value")
  |"description" -> {p with description = new_val}
  |"interest_list" -> {p with interest_list = String.split_on_char ';' new_val}
  |"experience" -> {p with experience = (string_to_exp new_val)}
  |"role" -> {p with role = new_val}
  |"looking_for" -> {p with looking_for = string_to_looking_for new_val}
  |"github_url" -> {p with github_url = new_val}
  | _ -> failwith "Must enter a valid field of profile to edit"

(* Query server and pull profile json from server, convert to profile type *)
let lookup_profile id =
  let jsonProfileString = Nethttp_client.Convenience.http_get ("http://18.204.146.26/obumbl/get_profile.php?user_id=" ^ (string_of_int id)) in
  init (from_string jsonProfileString)

(* will take in a profile and uploads it to the server and returns true if it is uploaded
 * successfully. Has the side effect of changing information in the server. *)
let update_server p =
  let params = [("user_id", string_of_int (p.user_id));("name", (p.name));("photo", (p.photo));("school", (p.school));("group_id_list", int_list_to_string (p.group_id_list));("description", (p.description));("interest_list", list_to_string (p.interest_list));("experience", exp_to_string (p.experience));("role", (p.role));("looking_for", looking_for_to_string (p.looking_for));("github_url", encode_url (p.github_url))] in
  let update = (Nethttp_client.Convenience.http_post "http://18.204.146.26/obumbl/insert_profile.php" params) in
  if update = "1" then true
  else false