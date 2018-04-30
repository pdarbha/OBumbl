open Profile
open Yojson.Basic
open Yojson.Basic.Util

type group = {group_id : int; user_id_list: int list; purpose : string; size : int;
              range : (int * int); group_blacklist : int list;
              invited_groups_list : int list; received_invites_list : int list}

let print_read s =
  let () = print_string s in
  read_line()

let init_group j =
  let id = j|>member "group_id"|>to_string|>int_of_string in
  let users = let s = j|>member "user_id_list"|>to_string in
      if s = "" then [] else s|>String.split_on_char ';'|>List.map int_of_string in
  let purpose = j|>member "purpose"|>to_string in
  let size = j|>member "size"|>to_string|>int_of_string in
  let range_min = j|>member "range_min"|>to_string|>int_of_string in
  let range_max = j|>member "range_max"|>to_string|>int_of_string in
  let blacklist = let s = j|>member "group_blacklist"|>to_string in
      if s = "" then [] else s|>String.split_on_char ';'|>List.map int_of_string in
  let invited = let s = j|>member "invited_groups_list"|>to_string in
      if s = "" then [] else s|>String.split_on_char ';'|>List.map int_of_string in
  let received = let s = j|>member "received_invites_list"|>to_string in
      if s = "" then [] else s|>String.split_on_char ';'|>List.map int_of_string in
  {group_id = id; user_id_list = users; purpose = purpose; size = size;
   range = (range_min,range_max); group_blacklist = blacklist;
   invited_groups_list = invited; received_invites_list = received}

let lookup_group id =
  let jsonGroupString = Nethttp_client.Convenience.http_get ("http://18.204.146.26/obumbl/get_group.php?group_id=" ^ (string_of_int id)) in
  init_group (from_string jsonGroupString)

let rec create_group p =
  let purpose = print_read "Enter project code: " in
  if String.contains purpose ' ' then
    (print_string "Invalid project code.\n"; create_group p)
  else
    let range_min = print_read "Enter minimum desired group size: " in
    let range_max = print_read "Enter maximum desired group size: " in
    let params = [("user_id_list", string_of_int (user_id p));("purpose", purpose);("size","1");("range_min", range_min);("range_max", range_max);("group_blacklist","");("invited_groups_list","");("received_invites_list","")] in
    let update = (Nethttp_client.Convenience.http_post "http://18.204.146.26/obumbl/insert_group.php" params) in
      if update = "-1" then
        (print_string "Group could not be created, try again.\n";
        create_group p)
      else (lookup_group (int_of_string update))

let group_to_string g =
  "Project code : "^g.purpose^"\nMinimum size: "^fst(g.range)^"\nMaximum size: "^snd(g.range)^"\n"

let find_group_by_code purpose group_list =
  List.find_opt (fun group -> group.purpose = purpose) group_list

let show_groups group_list =
  print_endline "Your groups are:";
  List.iter (fun g -> print_endline (group_to_string g)) group_list

let about_group g =
  let users = List.map (fun id -> lookup_profile id) g.user_id_list in
  print_string "Group for " ^ g.purpose ^ ":\nMembers (" ^ g.size ^ "[min " ^ g.range_min ^ ", max " ^ g.range_max ^ "] ):\n";
  List.iter (fun p -> about_profile p) users