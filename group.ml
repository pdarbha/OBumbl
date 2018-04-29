open Profile
open Yojson.Basic
open Yojson.Basic.Util

type group = {group_id : int; user_id_list: int list; purpose : string; size : int;
              range : (int * int); group_blacklist : int list;
              invited_groups_list : int list; received_invites_list : int list}

let print_read s =
  let () = print_string s in
  read_line()

let init_group j = failwith "implement"

let lookup_group id =
  let jsonGroupString = Nethttp_client.Convenience.http_get ("http://18.204.146.26/obumbl/get_group.php?group_id=" ^ (string_of_int id)) in
  init_group (from_string jsonGroupString)

let rec create_group p =
  let purpose = print_read "Enter project code: " in
  let range_min = print_read "Enter minimum desired group size: " in
  let range_max = print_read "Enter maximum desired group size: " in
  let params = [("user_id_list", string_of_int (user_id p));("purpose", purpose);("size","1");("range_min", range_min);("range_max", range_max);("group_blacklist","");("invited_groups_list","");("received_invites_list","")] in
  let update = (Nethttp_client.Convenience.http_post "http://18.204.146.26/obumbl/insert_group.php" params) in
    if update = "-1" then
      (print_string "Group could not be created, try again.\n";
      init_group p)
    else (lookup_group (int_of_string update))