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
* email - 254 char limit
*)

(* [type profile] stores all information about each person including id, name,
   photo, description, etc. Record entries are immutable, and each entry must be
   filled to complete a profile
 * requires inputs from user to match declared types *)
type profile = {user_id:int; name:string; photo:string ref; school:string;
                group_id_list: int list; description: string;
                interest_list: string list; experience : [ `BEG | `INT | `ADV ];
                role: string; looking_for: ([ `BEG | `INT | `ADV ]*string) list;
                github_url : string; email: string}

(* [string_to_exp s] takes in a string s and matches that string to the
   experience variant types.
 * requires: a string*)
let string_to_exp s =
  if s = "INT" then `INT
  else if s = "ADV" then `ADV
  else `BEG


(* [string_to_looking_for s] takes in a string s and convers it to list form, to
   represent the looking for category from profile.
 * requires: a string*)
let string_to_looking_for s =
  if s = "" then []
  else let lfList = split_string_to_list (String.sub s 0 ((String.length s)-1)) in
  let validLfList = List.filter (fun s -> (String.length s) > 4) lfList in
  List.map (fun el -> (string_to_exp (String.sub el 0 3),
                       String.sub el 4 ((String.length el)-4))) validLfList

(* [init_profile j] takes in Json file j and parses it, in order to store that
   information in an object of type profile
 * requires: a Json file*)
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
  let e = j|>member "email"|>to_string in
  {user_id = id; name = name; photo = photo; school = school; group_id_list = groups;
   description = desc; interest_list = interests; experience = exp; role = role;
   looking_for = look_for; github_url = github; email = e}

(* [user_id p] takes in a profile p and returns the unique user id associated
   with that profile
 * requires: a valid profile*)
let user_id p = p.user_id

(* [name p] takes in a profile p and returns the name of the user associated
   with that profile
 * requires: a valid profile*)
let name p = p.name

(* [photo p] takes in a profile p and returns the unique will return the photo
   of the user associated with that profile, encoded as a Base64 string
 * requires: a valid profile*)
let photo p = !(p.photo)

(* [school p] takes in a profile p and returns the school of the user associated
   with that profile
 * requires: a valid profile*)
let school p = p.school

(* [groups p] takes in a profile p and returns the groups of the user associated
   with that profile
 * requires: a valid profile*)
let groups p = p.group_id_list

(* [description p] takes in a profile p and returns the description of the user
   associated with that profile
 * requires: a valid profile*)
let description p = p.description

(* [interests p] takes in a profile p and returns a list of keyword interests of
   the user associated with that profile
 * requires: a valid profile*)
let interests p = p.interest_list

(* [experience p] takes in a profile p and returns the experience of the user
   associated with that profile, as a variant
 * requires: a valid profile*)
let experience p = p.experience

(* [role p] takes in a profile p and returns the role of the user associated
   with that profile
 * requires: a valid profile*)
let role p = p.role

(* [looking_for p] takes in a profile p and returns a tuple of experience as
   variant and role as string that the user associated with that profile is
   looking for
 * requires: a valid profile*)
let looking_for p = p.looking_for

(* [github p] takes in a profile p and returns the string to a github.com
   profile URL of the user associated with that profile
 * requires: a valid profile*)
let github p = p.github_url

let email p = p.email

(* [exp_to_string e] takes in an experience e and matches it to its
   corresponding string representation
 * requires: a valid experience of type `BEG, `INT, or `ADV *)
let exp_to_string e =
  match e with
  |`BEG -> "BEG"
  |`INT -> "INT"
  |`ADV -> "ADV"

(* [exp_to_string e] takes in an experience e and matches it to its
   corresponding full string representation
 * requires: a valid experience of type `BEG, `INT, or `ADV *)
let exp_print_string e =
  match e with
  |`BEG -> "Beginner"
  |`INT -> "Intermediate"
  |`ADV -> "Advanced"

(* [looking_for_to_string lf] takes in a list lf and returns the string
   representation of that list
 * requires: a looking_for entry represented as a list *)
let rec looking_for_to_string lf =
  match lf with
  | [] -> ""
  | h::t -> (exp_to_string (fst h)) ^ " " ^ (snd h) ^ ";" ^ (looking_for_to_string t)

(* [edit p field new_val] takes in a profile p, a string field and a string
   new_val. The entry corresponding to field will be updated to contain new_val,
   and an updated profile will be returned
 * requires: a valid profile, a valid field that corresponds to a field in
   profile, and a new_value that is of the same type as the previous one
 * raises failwith exception if field does not correpond to any of the record
   fields of profile  *)
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
  |"email" -> {p with email = new_val}
  | _ -> failwith "Must enter a valid field of profile to edit"

(* [add_group p group_id] takes in a profile p and a group id group_id. It then
   adds the new group's id to the user's group_id entry in profile and returns
   an updated profile
 * requires: a valid profile and group_id *)
let add_group p group_id =
  edit p "group_id_list" (int_list_to_string (group_id::p.group_id_list))

(* [remove_group p group_id] takes in a profile p and a group id group_id. It
   then searches through the group_id entry to remove group_id from the user's
   group list and returns an updated profile
 * requires: a valid profile and group_id *)
let remove_group p group_id =
  edit p "group_id_list"
    (int_list_to_string (List.filter (fun i -> i <> group_id) p.group_id_list))

(* [update_server p] takes in a profile p and uploads it to the server. Returns
   true if it has been uploaded successfully.
 * requires: a valid profile
 * side effects: changes information related to that profile in the server *)
let update_server p =
  let params = [("user_id", string_of_int (p.user_id));("name", (p.name));
  ("photo", (!(p.photo)));("school", (p.school));
  ("group_list", (int_list_to_string (p.group_id_list)));
  ("description", (p.description));
  ("interest_list", list_to_string (p.interest_list));
  ("experience", exp_to_string (p.experience));("role", (p.role));
  ("looking_for", looking_for_to_string (p.looking_for));
  ("github_url", encode_url (p.github_url));("email", (p.email))] in
  let update = (http_post insert_prof_url params) in
  if update = "1" then true else false

(* [cp_looking_for] looks for when the user is done entering looking for
   entries, and connects them together in list form with the role and experience
   the user wants *)
let rec cp_looking_for ()=
  let lf_role = print_read ("Please enter a role you are looking for on your team"^
                           " or type \"done\": ") in
  if String.lowercase_ascii (String.trim lf_role) = "done"
    then []
  else let lf_exp = print_read ("Are you looking for a beginner (BEG), intermediate"^
                                  " (INT), or advanced (ADV) " ^ lf_role ^ " ? ") in
  (string_to_exp lf_exp, lf_role)::(cp_looking_for ())

(* [cp_interests] looks for when the user is done entering their interests, and
   connects them together in list form. Tells the user to enter another interest
   if their entry does not comply with the conditions for a valid interests
   entry*)
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

let image_to_string img =
  let arr = Graphics.dump_image img in
  let mapped = Array.map (fun x -> Array.to_list x) arr in
  let l = Array.to_list (Array.map int_list_to_string mapped) in
  match l with
  | [] -> ""
  | _ -> let s = List.fold_left (fun s1 s2 -> s1^"|"^s2) "" l in
    if s = "" then ""
    else String.sub s 1 ((String.length s)-1)


(* [create_profile id] takes in an id. It prompts a user to enter all of the
   profile fields, and if all entries are valid, it creates a profile for that
   user which has the user_id id.
 * requires: a valid user_id id *)
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
  let e = print_read "What's your email? " in
  (*let pic = ref (image_to_string (Graphics.create_image 100 100)) in*)
  let prof = {user_id = id; name = n; photo = ref ""; school = s; group_id_list = [];
              description = d; interest_list = interests;
              experience = (string_to_exp exp); role = r; looking_for = lf;
              github_url = github; email = e} in
  if (update_server prof) && n <> ""
    then ()
  else
    (print_string "Invalid information, please enter your details again.\n";
    create_profile id)

(* [lookup_profile id] takes in a user_id id and finds the profile associated
   with that id. It queries the server and pulls that profile json from the
   server, and converts it to a profile type
 * requires: a valid user_id id corresponding to anexisting profile*)

let rec lookup_profile id =
  let jProfileString = http_get (get_prof_url ^ (string_of_int id)) in
  let () = print_endline jProfileString in
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
  if p.github_url <> "" then print_endline ("  - Github: " ^ p.github_url);
  if p.email <> "" then print_endline ("  - Email: " ^ p.email)
