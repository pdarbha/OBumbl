open Profile
open Yojson.Basic
open Yojson.Basic.Util

type group = {group_id : int; user_id_list: int list; purpose : string; size : int;
              range : (int * int); group_blacklist : int list;
              invited_groups_list : int list; received_invites_list : int list}

let empty_group = {group_id = -1; user_id_list = []; purpose = ""; size = 0;
              range = (0,0); group_blacklist = []; invited_groups_list = [];
              received_invites_list = []}

(*[print_read s] prints string s and then reads a user response*)
let print_read s =
  let () = print_string s in
  read_line()

(*[init_group j] is a group created by parsing a json [j].*)
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

(*[lookup_group id] returns a group. Takes a group id
  and if associated string found on server will create json of string to be used in
  call to init_group. Otherwise, returns empty group. *)
let lookup_group id =
  let jsonGroupString = Nethttp_client.Convenience.http_get ("http://18.204.146.26/obumbl/get_group.php?group_id=" ^ (string_of_int id)) in
  if jsonGroupString = "-1" then empty_group else
  init_group (from_string jsonGroupString)

(*[create_group p] creates a group from a profile p with additional input of
  purpose, min size, and max size of group from user.*)
let rec create_group p purpose_list =
  let purpose = print_read "Enter project code: " in
  if String.contains purpose ' ' then
    (print_string "Invalid project code.\n"; create_group p purpose_list)
  else if List.mem purpose purpose_list then
    (print_string "Already have a group for that project code.\n"; create_group p purpose_list)
  else
    let range_min = print_read "Enter minimum desired group size: " in
    let range_max = print_read "Enter maximum desired group size: " in
    if ((int_of_string range_min) > (int_of_string range_max) || (int_of_string range_max) < 0) then
      (print_string "Make sure your max size is greater than 0 and greater than or equal to min size.\n"; create_group p purpose_list)
    else
      let params = [("user_id_list", string_of_int (user_id p));("purpose", purpose);("size","1");("range_min", range_min);("range_max", range_max);("group_blacklist","");("invited_groups_list","");("received_invites_list","")] in
      let update = (Nethttp_client.Convenience.http_post "http://18.204.146.26/obumbl/insert_group.php" params) in
      if update = "-1" then
        (print_string "Group could not be created, try again.\n";
        create_group p purpose_list)
      else
        let group_id = (int_of_string update) in
        ignore (update_server (add_group p group_id))

(*[group_to_string g] is a readable string containing the purpose, minimum size
  and maximum size of group [g]*)
let group_to_string g =
  "Project code : " ^ g.purpose ^ "\nSize: " ^ (string_of_int g.size) ^ " (min " ^ (string_of_int (fst (g.range))) ^ ", max " ^ (string_of_int (snd (g.range))) ^ ")\nInvites: " ^ (string_of_int (List.length g.received_invites_list)) ^ "\n"

(*[find_group_by_code purpose group_list] is None if no group in group_list has
  purpose [purpose] and Some group if a group has a matching purpose.
  Meant to be called on a user's list of groups(for different projects). Because
  each person can have at most one group per purpose, each group in group_list
  should have unique purpose*)
let find_group_by_code purpose group_list =
  List.find_opt (fun group -> group.purpose = purpose) group_list

(*[find_group_by_id id group_list] is None if no group in group_list has
  an id matching [id] and Some group if a group has a matching id.*)
let find_group_by_id id group_list =
  List.find_opt (fun group -> group.group_id = id) group_list

(*[show_groups group_list] prints a string version of each group in group_list*)
let show_groups group_list =
  if group_list = [] then
    print_endline "\nYou have no groups.\n"
  else
    print_endline "\nYour groups are:";
    List.iter (fun g -> print_endline (group_to_string g)) group_list

(*[size g] is the size of group [g]*)
let size g = g.size

(*[range g] is the range of group [g]*)
let range g = g.range

(*[purpose g] is the purpose of group [g]*)
let purpose g = g.purpose

(*[union g1 g2] is a new group with updated fields created by merging two groups*)
let union g1 g2 =
  let users = int_list_to_string ((g1.user_id_list)@(g2.user_id_list)) in
  let size = string_of_int ((g1.size) + (g2.size)) in
  let range_min = string_of_int (max (fst (g1.range)) (fst (g2.range))) in
  let range_max = string_of_int (min (snd (g1.range)) (snd (g2.range))) in
  let params = [("user_id_list", users);("purpose", g1.purpose);("size",size);("range_min", range_min);("range_max", range_max);("group_blacklist","");("invited_groups_list","");("received_invites_list","")] in
  let update = (Nethttp_client.Convenience.http_post "http://18.204.146.26/obumbl/insert_group.php" params) in
  if update = "-1" then print_string "Errors occured during group acceptance.\n" else ();
  int_of_string update

(*[delete_group g] deletes group [g] from the server and deletes group from all
  the profiles that used to have it in their group_id list.
  returns unit

  potentially update to delete from user's list of groups that have invited them*)
let delete_group g =
  let profile_list = List.map (fun id -> lookup_profile id) g.user_id_list in
  List.iter (fun p -> ignore (update_server (remove_group p (g.group_id)))) profile_list;
  let update = (Nethttp_client.Convenience.http_post "http://18.204.146.26/obumbl/delete_group.php" [("group_id",string_of_int g.group_id)]) in
  if update = "-1" then
    (print_string "Errors occured during group acceptance.\n";)
  else ()

(*[about_group g] prints a string displaying information about a group and the
  users in the group*)
let about_group g =
  let users = List.map (fun id -> lookup_profile id) g.user_id_list in
  print_string ("\nGroup for " ^ g.purpose ^ " (ID " ^ (string_of_int g.group_id) ^ "):\nMembers: " ^ (string_of_int g.size) ^ " (min " ^ (string_of_int (fst g.range)) ^ ", max " ^ (string_of_int (snd g.range)) ^ ")\n");
  List.iter (fun p -> about_profile p) users

(*[invites g] helps manage the invites that a group g has received. User can choose to
  accept or reject an invite or to leave the invite manager screen. Accepting an
  invite forms a new group, and deletes the user's group and the group they joined.
  Rejecting an invite blacklists the rejected group and deletes that invite.
  Going back updates the server to reflect the local user's recent decisions.
*)
let rec invites g =
  if g.received_invites_list = [] then
    print_endline "You have no invitations."
  else
    let inviting_groups = List.map (fun id -> lookup_group id) g.received_invites_list in
    let invites_no_empty = List.filter (fun g -> g.group_id <> -1) inviting_groups in
    List.iter about_group invites_no_empty;
    let resp = print_read "Enter \"accept\" or \"reject\" followed a group's id, or \"back\" to return to the previous page: " in
    match (String.split_on_char ' ' resp) with
    | "accept"::x::[] ->
      let acceptedGroupOpt = find_group_by_id (int_of_string x) invites_no_empty in
      (match acceptedGroupOpt with
        | None -> print_endline "Invalid group id."; invites g
        | Some acceptedGroup ->
          (delete_group g;
          delete_group acceptedGroup;
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

(*[show_group_in_swipes g other] returns true if group [other] meets the
  requirements to be shown in the groups that g can swipe through and false otherwise
*)
let show_group_in_swipes g other =
  let max_can_take_in = snd(g.range) - (g.size) in
  let max_other_can_take_in = snd(other.range) - (other.size) in
  if other.group_id = g.group_id then false
  else if other.size > max_can_take_in then false
  else if g.size > max_other_can_take_in then false
  else if List.mem (other.group_id) (g.group_blacklist) then false
  else if List.mem (other.group_id) (g.invited_groups_list) then false
  else true

(*[get_groups_with_purpose g] returns a list of groups with the same purpose as
  group [g]*)
let get_groups_with_purpose g =
  let purpose = g.purpose in
  let json_string = Nethttp_client.Convenience.http_get ("http://18.204.146.26/obumbl/download_groups.php?purpose=" ^ purpose) in
  let json_list = from_string json_string|>to_list in
  let group_list = List.map (init_group) json_list in
  List.filter (show_group_in_swipes g) group_list

(* takes in an accumulator (usually [] in the beginning) and list and returns a
   key value pair list with keys being elements of list and frequency an integer *)
let rec create_key_freq_list acc= function
  |[] -> acc
  |h::t -> if List.mem_assoc h acc then
           let old_freq = List.assoc h acc in
           let new_acc = (h,old_freq+.1.0)::(List.remove_assoc h acc) in
           create_key_freq_list new_acc t
           else create_key_freq_list ((h,1.0)::acc) t

(*[interests_sum acc other lst] is the sum of the products of the values in lst (which are each
  frequencies for interests of one group) and the frequency of the interest in the other group*)
let rec interests_sum acc other = function
  |[] -> acc
  |(i,f)::t -> if List.mem_assoc i other then
               let other_freq = List.assoc i other in
               interests_sum (acc +. (f *. other_freq)) other t
               else interests_sum acc other t

(*[interests_score g other] is the summed interest score of groups [g] and [other] divided
  by the total number of interests of group [g]
*)
let interests_score g other =
  let group_profiles = List.map (lookup_profile) (g.user_id_list) in
  let other_profiles = List.map (lookup_profile) (other.user_id_list) in
  let group_interests = List.flatten (List.map (fun p -> interests p) group_profiles) in
  let group_interests_freq = create_key_freq_list [] group_interests in
  let group_interests_freq_norm = List.map (fun (k,v) -> (k,v/.(float (g.size)))) group_interests_freq in
  let other_interests = List.flatten (List.map (fun p -> interests p) other_profiles) in
  let other_interests_freq = create_key_freq_list [] other_interests in
  let other_interests_freq_norm = List.map (fun (k,v) -> (k,v/.(float (other.size)))) other_interests_freq in
  (interests_sum 0.0 other_interests_freq_norm group_interests_freq_norm)/.(float (List.length group_interests))

(*[uniqify_list lst nlst] is [lst] with all duplicates removed*)
let rec uniqify_list lst nlst =
  match lst with
  |[]->nlst
  |h::t -> if List.mem h nlst then uniqify_list t nlst else uniqify_list t (h::nlst)

(*[exp_role_tuple grp] is an (experience,role) list generated by finding the desired experience
  and role for each user in a group*)
let rec exp_role_tuple grp =
  List.map (fun x -> (experience (lookup_profile x), role (lookup_profile x))) grp.user_id_list

(*[looking_for_getter grpids] creates a list of all the desired roles and corresponding Experience
  level for each member of a group*)
let rec looking_for_getter grpids =
  match grpids with
  |[]->[]
  |h::t-> (looking_for (lookup_profile h)) @ (looking_for_getter t)


(*[score_det lf_grp o_roles] generates a score based on what a group is looking for
  and what experience another group has and what roles it can offer*)
let rec score_det lf_grp o_roles =
  match lf_grp with
  |[] -> 0.
  |h::t -> if List.mem h o_roles then (1. +. score_det t o_roles) else score_det t o_roles

(*[score_det_helper grp othergrp] calls score_det with a unique list of the desired roles/Experience
  of group [grp] and an (exp,role) list with each users experience and role in othergrp*)
let score_det_helper grp othergrp :float =
  score_det (uniqify_list (looking_for_getter grp.user_id_list) []) (exp_role_tuple (othergrp))

(*[score_determination my_group other_group] is the match score of the desired
  roles/experiences of group [my_group] and the roles/experiences of the members
  of group [other_group] divided by the size of [other_group]*)
let score_determination my_group other_group : float =
  let h = score_det_helper my_group other_group in
  h/. float_of_int other_group.size

(*[total_score g other] is the weighted total match score between a group [g] and
  another group [other] with higher scores indicating a better match*)
let total_score g other =
  let interest_weight = 1.0 in
  let looking_for_weight = 1.0 in
  (interest_weight *. (interests_score g other)) +. (looking_for_weight *. (score_determination g other))

(*[swipe g] produces a list of groups that can be swiped through by group g sorted
  from highest to lowest match score

  [swipe_repl g others] is a repl that allows the user to swipe left(reject) or right(accept)
  on a group, ask for more information about a group or to finish swiping
*)
let swipe g =
  let groups_to_be_matched = get_groups_with_purpose g in
  let sorted = List.sort (fun g1 g2 -> if (total_score g g1)<(total_score g g2) then 1
                          else if (total_score g g1)>(total_score g g2) then -1 else 0) groups_to_be_matched in
  let rec swipe_repl g others =
    match others with
    |[] -> print_endline "There are no more groups to swipe on; returning to previous page."
    |h::t ->
      about_group h;
      let s = print_read "\nEnter \"about\", \"left\", \"right\", or \"done\": " in
      match (String.split_on_char ' ' s) with
      |"about"::_ -> about_group h; swipe_repl g others
      |"left"::_ ->
        let blacklist = (h.group_id)::(g.group_blacklist) in
        let blacklist_str = int_list_to_string blacklist in
        let received = int_list_to_string (g.received_invites_list) in
        let invited = int_list_to_string (g.invited_groups_list) in
        let params = [("group_id", (string_of_int g.group_id));("group_blacklist",blacklist_str);("received_invites_list",received);("invited_groups_list",invited)] in
        ignore (Nethttp_client.Convenience.http_post "http://18.204.146.26/obumbl/update_group_lists.php" params);
        swipe_repl {g with group_blacklist = blacklist} t
      |"right"::_ ->
        let g' = {g with invited_groups_list = (h.group_id)::g.invited_groups_list} in
        let params_h = [("group_id", string_of_int (h.group_id));("group_blacklist", int_list_to_string h.group_blacklist);("invited_groups_list", int_list_to_string (h.invited_groups_list));("received_invites_list", int_list_to_string ((g.group_id)::(h.received_invites_list)))] in
        ignore (Nethttp_client.Convenience.http_post "http://18.204.146.26/obumbl/update_group_lists.php" params_h);
        let params_g = [("group_id", string_of_int (g.group_id));("group_blacklist", int_list_to_string g.group_blacklist);("invited_groups_list", int_list_to_string (g'.invited_groups_list));("received_invites_list", int_list_to_string (g.received_invites_list))] in
        ignore (Nethttp_client.Convenience.http_post "http://18.204.146.26/obumbl/update_group_lists.php" params_g);
        swipe_repl g' t
      |"done"::_ -> print_endline "Returning to previous page."
      | _ -> print_endline "Not a valid command."; swipe_repl g others in
        swipe_repl g sorted

(*[leave p g] is a new group formed by re-creating group g except with profile [p]
  removed. Groups on the server are updated accordingly.*)
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
