open Profile

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
let pull_data id =
  failwith "undefined"

(* *)
let () =
  let lr = login_or_register in
  let user_id = int_of_string (snd lr) in
  if user_id = -1 then
    failwith ((match fst lr with Login -> "Login" | Register -> "Register") ^ " unsuccessful.")
  else  let user_profile : profile = Profile.lookup_profile user_id in
    let data = pull_data user_id in
    match fst lr with
    | Login -> failwith "open tags"
    | Register -> failwith "open edit profile"
