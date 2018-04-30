(*open Profile
open Group*)

type lr_variant = Login | Register


(* User either invokes login or register,
 * login: accepts username, password and looks up user id
 * register: input username, password, email, profile details (edit profile) -> push registration to server, return user id
 * max length of username is 16, max length of password is 256
 * returns (lr_variant, user id)
 *)
let login_or_register =
  let input = ref "" in
  let output = ref (Login, "") in
  let () = while !input <> "login" && !input <> "register" do
    print_string "Enter \"login\" or \"register\": ";
    input := String.lowercase_ascii (String.trim (read_line ()));
    if !input = "login" then
      (print_string "Enter username: ";
      let username = String.trim (read_line ()) in
      print_string "Enter password: ";
      let password = String.trim (read_line ()) in
      output := (Login, (Nethttp_client.Convenience.http_post "http://18.204.146.26/obumbl/login.php" [("username",username);("password",password)])))
    else (if !input = "register" then
      (print_string "Enter username: ";
      let username = String.trim (read_line ()) in
      print_string "Enter password: ";
      let password = String.trim (read_line ()) in
      output := (Register, (Nethttp_client.Convenience.http_post "http://18.204.146.26/obumbl/register.php" [("username",username);("password",password)]))))
  done in
  !output

(* Pull all relevant data to user from server as json and store as record *)
let pull_group_data p =
  let group_int_list = groups p in
  List.map (fun id -> lookup_group id) group_int_list

let rec repl p group_list =
  show_groups group_list;
  let resp = print_read "Enter \"about\", \"invites\", or \"swipe\", followed by a group's project code, to see the respective information, \"new group\" to create a new group, or \"exit\" to quit: " in
  if resp = "exit" then
    print_string "Thanks for using OBumbl!"
  else
    ((match (String.split_on_char ' ' resp) with
    | "new"::"group"::[] -> ignore (create_group p)
    | c::x::[] ->
      let g = find_group_by_code x group_list in
      match g with
      | None -> print_string "Invalid group code provided.\n"
      | Some g' -> match c with
        | "about" -> about_group g'
        | "invites" -> invites g'
        | "swipe" -> swipe g'
        | "leave" -> leave p g'
    | _ -> print_string "Invalid response. Try again.\n");
    let p' = lookup_profile (user_id p) in
    repl p' (pull_group_data p'))

(* *)
let () =
  let lr = login_or_register in
  let user_id = int_of_string (snd lr) in
  if user_id = -1 then
    failwith ((match fst lr with Login -> "Login" | Register -> "Register") ^ " unsuccessful.")
  else
  (if (fst lr) = Register then create_profile user_id else ());
    let user_profile = lookup_profile user_id in
    let group_data = pull_group_data user_profile in
    repl user_profile group_data