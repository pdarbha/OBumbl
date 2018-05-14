open Netencoding.Url
open Nethttp_client.Convenience

let encode_url u = Netencoding.Url.encode u

let decode_url u = Netencoding.Url.decode u

let split_string_to_list str = String.split_on_char ';' str

let list_to_string l=
  match l with
  | [] -> ""
  | _ -> let s = List.fold_left (fun s1 s2 -> s1^";"^s2) "" l in
        if s = "" then ""
        else String.sub s 1 ((String.length s)-1)

let int_list_to_string l =
  let l' = List.map (string_of_int) l in
  list_to_string l'

let print_read s =
  let () = print_string s in
  read_line () |> String.trim

let insert_prof_url = "http://18.204.146.26/obumbl/create_profile.php"

let get_prof_url = "http://18.204.146.26/obumbl/get_profile.php?user_id="

let get_group_url = "http://18.204.146.26/obumbl/get_group.php?group_id="

let insert_group_url = "http://18.204.146.26/obumbl/insert_group.php"

let del_group_url = "http://18.204.146.26/obumbl/delete_group.php"

let update_group_lists_url = "http://18.204.146.26/obumbl/update_group_lists.php"

let download_groups_url = "http://18.204.146.26/obumbl/download_groups.php?purpose="

let login_url = "http://18.204.146.26/obumbl/login.php"

let register_url = "http://18.204.146.26/obumbl/register.php"
