open Profile

type lr_variant = Login | Register

let login_or_register =
  let input = ref "" in
  while !input <> "login" && !input <> "register" do
    print_string "Enter \"login\" or \"register\": ";
    input := String.lowercase_ascii (String.trim (read_line ()));
    if !input == "login" then
      (print_string "Enter username: ";
      let username = String.trim (read_line ()) in
      print_string "Enter password: ";
      let password = String.trim (read_line ()) in
      (Login, (Nethttp_client.Convenience.http_post "http://18.204.146.26/obumbl/login.php" [("username",username);("password",password)])))
    else (if !input == "register" then
      print_string "Enter username: ";
      let username = String.trim (read_line ()) in
      print_string "Enter password: ";
      let password = String.trim (read_line ()) in
      (Register, (Nethttp_client.Convenience.http_post "http://18.204.146.26/obumbl/login.php" [("username",username);("password",password)])))
  done
(* User either invokes login or register,
 * login: accepts username, password and looks up user id
 * register: input username, password, email, profile details (edit profile) -> push registration to server, return user id
 * max length of username is 16, max length of password is 256
 * returns (lr_variant, user id)
 *)

(* Pull all relevant data to user from server as json and store as record *)
let pull_data id =
  failwith "undefined"

(* *)
let () =
  let lr = login_or_register in
  let user_id = fst lr in
  let user_profile : profile = Profile.lookup_profile user_id in
  let data = pull_data user_id in
  match snd lr with
  | Login -> failwith "open invites"
  | Register -> failwith "open tags"
