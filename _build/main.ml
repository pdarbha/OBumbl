open Helper
open Profile
open Group
open Nethttp_client.Convenience
open Graphics
open Gui

type lr_variant = Login | Register


(* User either invokes login or register,
 * login: accepts username, password and looks up user id
 * register: input username, password, email, profile details (edit profile) -> push registration to server, return user id
 * max length of username is 16, max length of password is 256
 * returns (lr_variant, user id)
 *)
let login_or_register () =
  let input = ref "" in
  let output = ref (Login, "") in
  let () = while !input <> "login" && !input <> "register" do
    print_string "\nEnter \"login\" or \"register\": ";
    input := String.lowercase_ascii (String.trim (read_line ()));
    if !input = "login" then
      (print_string "Enter username: ";
      let username = String.trim (read_line ()) in
      print_string "Enter password: ";
      let password = String.trim (read_line ()) in
      let params = [("username",username);("password",password)] in
      output := (Login, (http_post login_url params)))
    else (if !input = "register" then
      (print_string "Enter username: ";
      let username = String.trim (read_line ()) in
      print_string "Enter password: ";
      let password = String.trim (read_line ()) in
      let params = [("username",username);("password",password)] in
      output := (Register, (http_post register_url params ))))
  done in
  !output

(* Pull all relevant data to user from server as json and store as record *)
let pull_group_data p =
  let group_int_list = groups p in
  List.map (fun id -> lookup_group id) group_int_list

let perform_action_on_group g p action=
  match action with
  | "about" -> about_group g
  | "invites" -> invites g
  | "swipe" -> swipe g
  | "leave" -> leave p g
  | _ -> print_string "Invalid command. Try again.\n"

let rec repl p group_list =
  show_groups group_list;
  let resp =
    if group_list = []
      then print_read ("Enter \"new group\" to create a new group,"^
                       " \"edit profile\", or \"quit\": ")
    else print_read ("Enter \"about\", \"invites\", or \"swipe\", followed by a"^
                     " group's project code, to see the respective information, "^
                     "\"new group\" to create a new group, \"leave\" followed by"^
                     " the group's project code to leave the group, \"edit profile\","^
                     " or \"quit\": ") in
  if resp = "quit"
    then print_endline "\nThanks for using OBumbl!\n"
  else
    ((match (String.split_on_char ' ' resp) with
    | "edit"::"profile"::[] ->
      let field = print_read ("Type the field you would like to edit (\"name\","^
                              " \"school\", \"description\", \"interest_list\","^
                              " \"experience\", \"role\", \"looking_for\", "^
                              "\"github_url\", or ): \"email\"): ") in
      let field_value =
        (match field with
        | "name" | "school" | "description" | "experience" | "role" | "github_url" | "email" ->
          print_read ("What would you like as your new " ^ field ^ "? ")
        | "interest_list" -> list_to_string (cp_interests ())
        | "looking_for" -> looking_for_to_string (cp_looking_for ())
        | _ -> print_string "Invalid field.\n"; "") in
      if (((field = "name" && field_value <> "") || field <> "name") &&
          update_server (edit p field field_value) = true)
        then ()
      else print_string "Profile edit unsuccessful.\n"
    | "new"::"group"::[] ->
      let purpose_list = List.map (fun g -> purpose g) group_list in
      create_group p purpose_list
    | c::x::[] ->
      if group_list = [] then print_string "Invalid command. Try again.\n" else
      let g = find_group_by_code x group_list in
      (match g with
      | None -> print_string "Invalid group code provided.\n"
      | Some g' -> perform_action_on_group g' p c)
    | _ -> print_string "Invalid response. Try again.\n");
    let p' = lookup_profile (user_id p) in
    repl p' (pull_group_data p'))

(* *)
let rec login_loop () =
  let lr = login_or_register () in
  let user_id = int_of_string (snd lr) in
  if user_id = -1 then
    (print_string ((match fst lr with Login -> "Login" | Register -> "Register") ^
                   " unsuccessful.\n");
    login_loop ())
  else
    ((if (fst lr) = Register then create_profile user_id else ());
    let user_profile = lookup_profile user_id in
    let group_data = pull_group_data user_profile in
    repl user_profile group_data)

let rec login_loop_gui () =
  let lr = login_or_register () in
  let user_id = int_of_string (snd lr) in
  if user_id = -1 then
    (print_string ((match fst lr with Login -> "Login" | Register -> "Register") ^
                   " unsuccessful.\n");
     login_loop_gui ())
  else
    ((if (fst lr) = Register then create_profile user_id else ());
     let user_profile = lookup_profile user_id in
     let group_data = pull_group_data user_profile in
     Graphics.open_graph " 1000x750";
     Graphics.set_window_title "OBumbl";
     Gui.draw_start_canvas user_profile group_data;
     let rec loop () = loop () in
     loop ()
    )

(*let gui_loop () =
    Graphics.open_graph " 1000x750";
    Graphics.set_window_title "OBumbl";
    Gui.draw_start_canvas (Profile.testprof ()) Group.empty_group ;
    let rec loop () = loop () in
    loop ()*)

let () =
  let actArg = Sys.argv.(1) in
  print_endline actArg;
  if actArg = "GUI"
  then login_loop_gui ()
  else login_loop ()
