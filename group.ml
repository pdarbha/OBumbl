(*open Profile
open Yojson.Basic
open Yojson.Basic.Util*)

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
    if ((int_of_string range_min) > (int_of_string range_max) || (int_of_string range_max) < 0) then
      (print_string "Make sure your max size is greater than 0 and greater than or equal to min size.\n"; create_group p)
    else
      let params = [("user_id_list", string_of_int (user_id p));("purpose", purpose);("size","1");("range_min", range_min);("range_max", range_max);("group_blacklist","");("invited_groups_list","");("received_invites_list","")] in
      let update = (Nethttp_client.Convenience.http_post "http://18.204.146.26/obumbl/insert_group.php" params) in
      if update = "-1" then
        (print_string "Group could not be created, try again.\n";
        create_group p)
      else
        let group_id = (int_of_string update) in
        update_server (add_group p group_id); (lookup_group group_id)

let group_to_string g =
  "Project code : "^g.purpose^"\nMinimum size: " ^ (string_of_int (fst (g.range))) ^ "\nMaximum size: "^ (string_of_int (snd (g.range))) ^"\n"

let find_group_by_code purpose group_list =
  List.find_opt (fun group -> group.purpose = purpose) group_list

let find_group_by_id id group_list =
  List.find_opt (fun group -> group.group_id = id) group_list

let show_groups group_list =
  print_endline "Your groups are:";
  List.iter (fun g -> print_endline (group_to_string g)) group_list

let size g = g.size

let range g = g.range

let union g1 g2 =
  let users = int_list_to_string ((g1.user_id_list)@(g2.user_id_list)) in
  let size = string_of_int ((g1.size) + (g2.size)) in
  let range_min = string_of_int (max (fst (g1.range)) (fst (g2.range))) in
  let range_max = string_of_int (min (snd (g1.range)) (snd (g2.range))) in
  let params = [("user_id_list", users);("purpose", g1.purpose);("size",size);("range_min", range_min);("range_max", range_max);("group_blacklist","");("invited_groups_list","");("received_invites_list","")] in
  let update = (Nethttp_client.Convenience.http_post "http://18.204.146.26/obumbl/insert_group.php" params) in
  if update = "-1" then print_string "Errors occured during group acceptance.\n" else ();
  int_of_string update

let delete_group g =
  let profile_list = List.map (fun id -> lookup_profile id) g.user_id_list in
  List.iter (fun p -> ignore (update_server (remove_group p (g.group_id)))) profile_list;
  let update = (Nethttp_client.Convenience.http_post "http://18.204.146.26/obumbl/delete_group.php" [("group_id",string_of_int g.group_id)]) in
  if update = "-1" then
    (print_string "Errors occured during group acceptance.\n";)
  else ()

let about_group g =
  let users = List.map (fun id -> lookup_profile id) g.user_id_list in
  print_string ("Group for " ^ g.purpose ^ " (ID " ^ (string_of_int g.group_id) ^ "):\nMembers (" ^ (string_of_int g.size) ^ "[min " ^ (string_of_int (fst g.range)) ^ ", max " ^ (string_of_int (snd g.range)) ^ "] ):\n");
  List.iter (fun p -> about_profile p) users

let rec invites g =
  if g.received_invites_list = [] then
    print_endline "You have no invitations."
  else
    let inviting_groups = List.map (fun id -> lookup_group id) g.received_invites_list in
    List.iter about_group inviting_groups;
    let resp = print_read "Enter \"accept\" or \"reject\" followed a group's id, or \"back\" to return to the previous page: " in
    match (String.split_on_char ' ' resp) with
    | "accept"::x::[] ->
      delete_group g;
      let acceptedGroupOpt = find_group_by_id (int_of_string x) inviting_groups in
      (match acceptedGroupOpt with
        | None -> print_endline "Invalid group id."; invites g
        | Some acceptedGroup ->
          (delete_group acceptedGroup;
          let profile_union = List.map (fun id -> lookup_profile id) ((g.user_id_list)@(acceptedGroup.user_id_list)) in
          let update = union g acceptedGroup in
          if update = -1 then ()
          else List.iter (fun p -> ignore (update_server (add_group p update))) profile_union))
    | "reject"::x::[] ->
      let other = int_of_string x in
      let blacklist = if List.mem other g.group_blacklist then g.group_blacklist else other::(g.group_blacklist) in
      let g' = {g with received_invites_list = (List.filter (fun x -> x<>other) (g.received_invites_list)); group_blacklist = blacklist} in
      invites g'
    | "back"::[] ->
      let blacklist = int_list_to_string g.group_blacklist in
      let received = int_list_to_string g.received_invites_list in
      let invited = int_list_to_string g.invited_groups_list in
      let params = [("group_id", (string_of_int g.group_id));("group_blacklist",blacklist);("received_invites_list",received);("invited_groups_list",invited)] in
      let update = (Nethttp_client.Convenience.http_post "http://18.204.146.26/obumbl/update_group_lists.php" params) in
      if update = "-1" then
        (print_string "Updating server unsuccessful, try again.\n";
        invites g)
      else ()
    | _ -> print_endline "Invalid response. Try again."; invites g

let swipe g = failwith "undefined"

let rec leave p g =
  let updated_user_ids = (List.filter (fun x -> x<>(user_id p)) g.user_id_list) in
  let updated_users = int_list_to_string updated_user_ids in
  let range_min = string_of_int (fst (g.range)) in
  let range_max = string_of_int (snd (g.range)) in
  let blacklist = int_list_to_string g.group_blacklist in
  let received = int_list_to_string g.received_invites_list in
  let invited = int_list_to_string g.invited_groups_list in
  let params = [("user_id_list", updated_users);("purpose", g.purpose);("size",string_of_int (g.size - 1));("range_min", range_min);
                ("range_max", range_max);("group_blacklist",blacklist);("invited_groups_list",invited);("received_invites_list",received)] in
  let update = (Nethttp_client.Convenience.http_post "http://18.204.146.26/obumbl/insert_group.php" params) in
  if update = "-1" then
    (print_string "Updated group could not be created, try again.\n";
    leave p g)
  else
    let updated_profiles = List.map (fun x -> lookup_profile x) updated_user_ids in
    List.iter (fun x -> ignore (update_server (add_group x (int_of_string update)))) updated_profiles;
    delete_group g

let rec uniqify_list lst nlst =
  match lst with
  |[]->nlst
  |h::t -> if List.mem h nlst then uniqify_list t nlst else uniqify_list t (h::nlst)

let rec exp_role_tuple grp =
  List.map (fun x -> ((profile.lookup_profile x).experience, (profile.lookup_profile x).role)) grp.user_id_list

let looking_for_getter grp =
  let members = grp.user_id_list in
  List.map (fun x -> (profile.lookup_profile x).looking_for) members

let rec score_det lf_grp o_roles =
  match lf_grp with
  |[] -> 0.
  |h::t -> if List.mem h o_roles then (1. +. score_det t o_roles) else score_det t o_roles

let score_det_helper grp othergrp :float =
  score_det (uniqify_list (looking_for_getter grp) []) (exp_role_tuple (othergrp))


let score_determination my_group other_group : float =
  let h = score_det_helper my_group other_group in
  h/. float_of_int other_group.size

let rec matches_list my_group lst_grp =
  match lst_grp with
  |[]->[]
  |h::t -> ((score_determination my_group h), h)::(matches_list my_group t)

let sort_function kv1 kv2 =
  if fst (kv1) > fst (kv2) then 1 else if fst(kv1) < fst(kv2) then -1 else 0

let sort_tup_list lst nlst =
  List.rev (List.sort sort_function lst)
