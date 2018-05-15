open Netencoding.Url
open Nethttp_client.Convenience

(*[encode_url u] takes in a url and encodes it. requires: a valid url*)
let encode_url u = Netencoding.Url.encode u

(*[decode_url u] takes in a url and decodes it. requires: a valid url*)
let decode_url u = Netencoding.Url.decode u

(*[split_string_to_list str] takes in a string and splits it on ;.
requires: a valid string*)
let split_string_to_list str = String.split_on_char ';' str

(*[list_to_string l] takes in a list and turns it into a string.
  requires: a valid list l*)
let list_to_string l=
  match l with
  | [] -> ""
  | _ -> let s = List.fold_left (fun s1 s2 -> s1^";"^s2) "" l in
        if s = "" then ""
        else String.sub s 1 ((String.length s)-1)

(*[int_list_to_string l] takes in a list of ints l and returns a string of those
  list elements. requires: an int list*)
let int_list_to_string l =
  let l' = List.map (string_of_int) l in
  list_to_string l'

(*[print_read s]takes in a string s, reads it and trims it. requires: a string*)
let print_read s =
  let () = print_string s in
  read_line () |> String.trim

(*[insert_prof_url] creates a profile in the server*)
let insert_prof_url = "http://18.204.146.26/obumbl/create_profile.php"

(*[get_prof_url] gets a profile from the server based on a user id*)
let get_prof_url = "http://18.204.146.26/obumbl/get_profile.php?user_id="

(*[get_group_url] gets a group from the server based in a group id*)
let get_group_url = "http://18.204.146.26/obumbl/get_group.php?group_id="

(*[insert_group_url] inserts a group into the server database*)
let insert_group_url = "http://18.204.146.26/obumbl/insert_group.php"

(*[del_group_url] deletes a group from the server database*)
let del_group_url = "http://18.204.146.26/obumbl/delete_group.php"

(*[update_group_lists_url] updates a group list in the database*)
let update_group_lists_url = "http://18.204.146.26/obumbl/update_group_lists.php"

(*[download_groups_url] downloads groups from the database*)
let download_groups_url = "http://18.204.146.26/obumbl/download_groups.php?purpose="

(*[login_url] allows a user to login, and queries the server accordingly*)
let login_url = "http://18.204.146.26/obumbl/login.php"

(*[register_url] allows a user to register and queries the server accordingly*)
let register_url = "http://18.204.146.26/obumbl/register.php"
