open Helper
open Yojson.Basic
open Yojson.Basic.Util
open Netencoding.Url
open Nethttp_client.Convenience

(* Limitations -- ** sanitize/escape all inputs before pushing to server **
* Name - 64 char (full name) -- alphanumeric + spaces
* set a reasonable image size limit (2mb?) and try to compress - upload as base 64 encoding
* school - 64 char (alphanumeric + spaces)
* groups - cannot join more than 100 groups -- g1;g2;g3;... for integer ids
* description - 800 characters (alphanumeric + spaces + *basic* punctuation)
* interests - 15 char limit per interest, 40 interests max (alphanumeric + spaces) -- i0;i1;i2;...;i39
* experience - "BEG" or "INT" or "ADV"
* role - 32 char (alphanumeric + spaces)
* looking_for list - "BEG" or "INT" or "ADV" for exp, 32 for role (alphanumeric + spaces) -- exp1 role1;exp2 role2
* Github URL - 60 char (URL encoded then decoded -- example: https://www.urlencoder.org)
*)

(* will store all information about each person including id, name, photo, description, etc.*)
type profile = {user_id:int; name:string; photo:string ref; school:string;
                group_id_list: int list; description: string;
                interest_list: string list; experience : [ `BEG | `INT | `ADV ];
                role: string; looking_for: ([ `BEG | `INT | `ADV ]*string) list;
                github_url : string}

let string_to_exp s =
  if s = "INT" then `INT
  else if s = "ADV" then `ADV
  else `BEG

let string_to_looking_for s =
  if s = "" then []
  else let lfList = split_string_to_list (String.sub s 0 ((String.length s)-1)) in
  let validLfList = List.filter (fun s -> (String.length s) > 4) lfList in
  List.map (fun el -> (string_to_exp (String.sub el 0 3),
                       String.sub el 4 ((String.length el)-4))) validLfList

(* will take in Json file and parse it and store that information in an object of type profile *)
let init_profile j =
  let id = j|>member "user_id"|>to_string|>int_of_string in
  let name = j|>member "name"|>to_string in
  let photo = ref (j|>member "photo"|>to_string) in
  let school = j|>member "school"|>to_string in
  let groups = let s = j|>member "group_list"|>to_string in
      if s = "" then [] else s|>split_string_to_list|>List.map int_of_string in
  let desc = j|>member "description"|>to_string in
  let interests = j|>member "interest_list"|>to_string|>split_string_to_list in
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
let photo p = !(p.photo)

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

let exp_to_string e =
  match e with
  |`BEG -> "BEG"
  |`INT -> "INT"
  |`ADV -> "ADV"

let exp_print_string e =
  match e with
  |`BEG -> "Beginner"
  |`INT -> "Intermediate"
  |`ADV -> "Advanced"

let rec looking_for_to_string lf =
  match lf with
  | [] -> ""
  | h::t -> (exp_to_string (fst h)) ^ " " ^ (snd h) ^ ";" ^ (looking_for_to_string t)

(* will accept a profile and two strings (field, value) to be updated and returned in new profile *)
let edit p field new_val =
  match field with
  |"user_id" -> (try let i = int_of_string new_val in {p with user_id = i}
                with _ -> failwith "Tried to set user_id to a non integer value")
  |"name" -> {p with name = new_val}
  |"photo" -> {p with photo = ref (new_val)}
  |"school" -> {p with school = new_val}
  |"group_id_list" when new_val = "" -> {p with group_id_list = []}
  |"group_id_list" -> (try
                        {p with group_id_list =
                            List.map (int_of_string) (split_string_to_list new_val)}
                      with _ -> failwith "Tried to give group_id non integer value")
  |"description" -> {p with description = new_val}
  |"interest_list" -> {p with interest_list = split_string_to_list new_val}
  |"experience" -> {p with experience = (string_to_exp new_val)}
  |"role" -> {p with role = new_val}
  |"looking_for" -> {p with looking_for = string_to_looking_for new_val}
  |"github_url" -> {p with github_url = new_val}
  | _ -> failwith "Must enter a valid field of profile to edit"

let add_group p group_id =
  edit p "group_id_list" (int_list_to_string (group_id::p.group_id_list))

let remove_group p group_id =
  edit p "group_id_list"
    (int_list_to_string (List.filter (fun i -> i <> group_id) p.group_id_list))

(* will take in a profile and uploads it to the server and returns true if it is uploaded
 * successfully. Has the side effect of changing information in the server. *)
let update_server p =
  let params = [("user_id", string_of_int (p.user_id));("name", (p.name));
                ("photo", (!(p.photo)));("school", (p.school));
                ("group_list", (int_list_to_string (p.group_id_list)));
                ("description", (p.description));
                ("interest_list", list_to_string (p.interest_list));
                ("experience", exp_to_string (p.experience));("role", (p.role));
                ("looking_for", looking_for_to_string (p.looking_for));
                ("github_url", encode_url (p.github_url))] in
  let update = (http_post insert_prof_url params) in
  if update = "1" then true else false

let rec cp_looking_for ()=
  let lf_role = print_read ("Please enter a role you are looking for on your team"^
                           " or type \"done\": ") in
  if String.lowercase_ascii (String.trim lf_role) = "done"
    then []
  else let lf_exp = print_read ("Are you looking for a beginner (BEG), intermediate"^
                                  " (INT), or advanced (ADV) " ^ lf_role ^ " ? ") in
  (string_to_exp lf_exp, lf_role)::(cp_looking_for ())

let rec cp_interests () =
  let interest = print_read "Please enter one of your interests or type \"done\": " in
  if String.lowercase_ascii (String.trim interest) = "done" || String.trim interest = ""
    then []
  else if (String.contains interest ';')
    then (print_endline ("\nYour interests may not contain \';\'."^
                        " Please enter it again.\n");
          cp_interests ())
  else
    let interests_tail = cp_interests () in
    if List.mem interest interests_tail
      then interests_tail
    else interest::interests_tail

let rec create_profile id =
  let n = print_read "\nEnter your full name: " in
  let s = print_read "Enter your school: " in
  let d = print_read "Enter your description: " in
  let interests = cp_interests () in
  let exp = print_read ("Are you a beginner (BEG), intermediate (INT), or "^
                       "advanced (ADV) computer scientist? ") in
  let r = print_read "What is your typical role on a team? " in
  let lf = cp_looking_for () in
  let github = print_read "What's your github URL? " in
  let prof = {user_id = id; name = n; photo = ref ""; school = s; group_id_list = [];
              description = d; interest_list = interests;
              experience = (string_to_exp exp); role = r; looking_for = lf;
              github_url = github} in
  if (update_server prof) && n <> ""
    then ()
  else
    (print_string "Invalid information, please enter your details again.\n";
    create_profile id)

(* Query server and pull profile json from server, convert to profile type *)
let rec lookup_profile id =
  let jProfileString = http_get (get_prof_url ^ (string_of_int id)) in
  if jProfileString = "-1"
    then (create_profile id; lookup_profile id)
  else init_profile (from_string jProfileString)

let about_profile p =
  print_string ("\n  " ^ p.name);
  if p.school <> ""
    then print_endline (" (" ^ p.school ^ ")")
    else print_string "\n";
  if p.description <> ""
    then print_endline ("  - Description: " ^ p.description);
  if p.interest_list <> []
    then print_endline ("  - Interested in: " ^
         List.fold_right (fun a b -> a ^ "; " ^ b) p.interest_list "");
  print_endline ("  - Experience: " ^ (exp_print_string p.experience));
  if p.role <> "" then print_endline ("  - Role: " ^ p.role);
  if p.github_url <> "" then print_endline ("  - Github: " ^ p.github_url)
