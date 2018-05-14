open Helper
open Yojson.Basic
open Yojson.Basic.Util
open Profile
open Nethttp_client.Convenience

(*this represents the day of the week the group is available to meet *)
type day = Sun|Mon|Tues|Wed|Thu|Fri|Sat

(*
  AF: In the tuple, the day represents the day of the week and the 2 integers
    represent the beginning and end of the time they are available to meet. The
    times are in 24 hour form where 8:35 AM is 835 and 8:35 PM is 2035. If the group
    would like to work past midnight, they must split the time into 2 times, one ending
    at midnight of the first day and the next starting at midnight of the next day.
  RI: The second integer must necessarily be greater than the first one.
*)
type schedule = (day*((int*int)list)) list

type group = {group_id : int; user_id_list: int list; purpose : string; size : int;
              range : (int * int); group_blacklist : int list;
              invited_groups_list : int list; received_invites_list : int list;
              schedule: schedule}

let empty_group = {group_id = -1; user_id_list = []; purpose = ""; size = 0;
              range = (0,0); group_blacklist = []; invited_groups_list = [];
              received_invites_list = []; schedule = []}
(*
 * [day_to_string d] is a string representation of [d]
 * This is different from [day_to_string_for_repl] which is used for printing
 *    in the repl but this is used for storing the schedule in the database.
 * requires: [d] to be of type day
 *)
let day_to_string d=
  match d with
  |Sun -> "Sun "
  |Mon -> "Mon "
  |Tues -> "Tue "
  |Wed -> "Wed "
  |Thu -> "Thu "
  |Fri -> "Fri "
  |Sat -> "Sat "

(*
 * [day_from_string d] converts [d] from string to type day
 * This is called when parsing the string from the json so the string will
 *    only be one of these 7 strings.
 * requires: [d] to be a string
 *)
let day_from_string d=
  match d with
  |"Sun" -> Sun
  |"Mon" -> Mon
  |"Tue" -> Tues
  |"Wed" -> Wed
  |"Thu" -> Thu
  |"Fri" -> Fri
  |"Sat" -> Sat

(*
 * [times_from_string s] converts the string representing the list of times
 *    to an (int*int) list representing the beginning and end of the time
 * requires: s to be a string
 *)
let times_from_string s =
  if s = "" then [] else
  let times = String.split_on_char ',' s in
  List.map (fun s -> let sep = (String.index s '-') + 1 in
                     let len = String.length s in
                     (int_of_string (String.sub s 0 (sep-1)),
                      int_of_string (String.sub s (sep) (len-sep)))) times

(*
 * [schedule_from_string s] converts the string representing the schedule
 *    to a schedule which has type schedule
 * requires: s to be a string
 *)
let schedule_from_string s =
  if s = "" then [] else
  let scheds = String.split_on_char ';' s in
  List.map (fun s -> (day_from_string (String.sub s 0 3),
                      times_from_string (String.sub s 4 ((String.length s)-4)))) scheds

(*
 * [times_to_string l] converts [l] to a string that has all the information
 *    to store in the database
 * requires: [l] to be a (int*int) list
 *)
let times_to_string l =
  match l with
  |[] -> ""
  | _ ->
    let s = List.fold_left
            (fun acc (b,e) -> acc^(string_of_int b)^"-"^(string_of_int e)^",") "" l in
    String.sub s 0 ((String.length s)-1)

(*
 * [schedule_to_string l] converts [sched] to a string that has all the information
 *    to store in the database
 * requires: [sched] to be of type schedule
 *)
let schedule_to_string sched =
  match sched with
  |[] -> ""
  |_ ->
    let s = List.fold_left
            (fun acc (d,l) -> acc^(day_to_string d)^(times_to_string l)^";") "" sched in
    String.sub s 0 ((String.length s)-1)

(*
 * [init_group j] parses json [j] and returns the group that contains all the
 *    information stored in [j]
 * requires: [j] to be of type Yojson.Basic.json
 *)
let init_group j =
  let id = j|>member "group_id"|>to_string|>int_of_string in
  let users = let s = j|>member "user_id_list"|>to_string in
      if s = "" then [] else s|>split_string_to_list|>List.map int_of_string in
  let purpose = j|>member "purpose"|>to_string in
  let size = j|>member "size"|>to_string|>int_of_string in
  let range_min = j|>member "range_min"|>to_string|>int_of_string in
  let range_max = j|>member "range_max"|>to_string|>int_of_string in
  let schedule = j|> member "schedule" |> to_string |> schedule_from_string in
  let blacklist = let s = j|>member "group_blacklist"|>to_string in
      if s = "" then [] else s|>split_string_to_list|>List.map int_of_string in
  let invited = let s = j|>member "invited_groups_list"|>to_string in
      if s = "" then [] else s|>split_string_to_list|>List.map int_of_string in
  let received = let s = j|>member "received_invites_list"|>to_string in
      if s = "" then [] else s|>split_string_to_list|>List.map int_of_string in
  {group_id = id; user_id_list = users; purpose = purpose; size = size;
   range = (range_min,range_max); schedule = schedule; group_blacklist = blacklist;
   invited_groups_list = invited; received_invites_list = received}

let invited_groups g = g.invited_groups_list

let groupid g = g.group_id

let insert_group params =
  http_post insert_group_url params

let update_group_lists params =
  http_post update_group_lists_url params

(*[lookup_group id] returns a group. Takes a group id
  and if associated string found on server will create json of string to be used in
  call to init_group. Otherwise, returns empty group. *)
let lookup_group id =
  let jsonGroupString = http_get (get_group_url ^ (string_of_int id)) in
  if jsonGroupString = "-1" then empty_group else
  init_group (from_string jsonGroupString)

(*
 * [get_first_time_for_schedule] prompts the user for the beginning of the
 *    time they are free that day
 * Terminates when user inputs
 *)
let rec get_first_time_for_schedule ()=
  let time1_input = print_read ("\nEnter the start of the time you are free that day"^
                  " in 24 hour time. For example, to say 8:35pm type \"2035\": ") in
  let time1_trimmed =
    String.trim
    (if (String.contains time1_input ':')
      then String.sub time1_input 0 (String.index time1_input ':') ^
           String.sub time1_input ((String.index time1_input ':')+1)
                      ((String.length time1_input)-(String.index time1_input ':')-1)
     else time1_input) in
  let time1 = try (int_of_string time1_trimmed)
              with _ -> (print_endline "\nEnter the time as a number";
                         get_first_time_for_schedule()) in
  if (((time1 mod 100) >59) || ((time1/100)>23) || time1<0)
    then (print_endline "\nEnter a valid time between 0000 to 2359";
          get_first_time_for_schedule())
  else time1

let rec get_second_time_for_schedule () =
  let time2_input = print_read ("\nEnter the end of the time you are free that day"^
                  " in 24 hour time. For example, to say 8:35pm type \"2035\": ") in
  let time2_trimmed =
    String.trim
    (if (String.contains time2_input ':')
      then String.sub time2_input 0 (String.index time2_input ':') ^
           String.sub time2_input ((String.index time2_input ':')+1)
                      ((String.length time2_input)-(String.index time2_input ':')-1)
     else time2_input) in
  let time2 = try (int_of_string time2_trimmed)
              with _ -> (print_endline "\nEnter the time as a number";
                         get_second_time_for_schedule()) in
  if (((time2 mod 100) >59) || ((time2/100)>23)|| time2<0)
    then (print_endline "\nEnter a valid time between 0000 to 2359";
          get_second_time_for_schedule())
  else time2

let day_to_string_for_repl d =
  match d with
  |Sun -> " SUNDAY "
  |Mon  -> " MONDAY "
  |Tues -> " TUESDAY "
  |Wed -> " WEDNESDAY "
  |Thu -> " THURSDAY "
  |Fri -> " FRIDAY "
  |Sat -> " SATURDAY "

let rec get_times day acc=
  let st = day_to_string_for_repl day in
  let done_or_not = print_read ("\nEnter \"done\" if you are done entering the times"^
                                " you are available on"^st^"or press \"ENTER\" to "^
                                "continue entering times: ") in
  if String.lowercase_ascii (String.trim (done_or_not)) = "done" then acc
  else
    let time1 = get_first_time_for_schedule () in
    let time2 = get_second_time_for_schedule () in
    if time1>=time2
      then (print_endline "\nEnd time must be after start time";get_times day acc)
    else get_times day ((time1,time2)::acc)

let rec create_schedule acc=
  let day_input = print_read ("\nPlease enter a day of the week you are free to work on"^
                           " this project or type \"done\": ") in
  if String.lowercase_ascii (String.trim day_input) = "done"
    then acc
  else
    let day_opt =
      (match (String.lowercase_ascii (String.trim day_input)) with
      |"sunday" -> Some Sun
      |"monday" -> Some Mon
      |"tuesday" -> Some Tues
      |"wednesday" -> Some Wed
      |"thursday" -> Some Thu
      |"friday" -> Some Fri
      |"saturday" -> Some Sat
      | _ -> None) in
    match day_opt with
    |None -> (print_endline ("\nEnter the full name of the day of the week.");
             create_schedule acc)
    |Some day ->
      let curr_times = if List.mem_assoc day acc then List.assoc day acc else [] in
      let times = get_times day curr_times in
      if times = [] then create_schedule acc else
      create_schedule ((day,times)::(List.remove_assoc day acc))

let time_diff t1 t2 =
  let hour_diff = (t1/100) - (t2/100) in
  let min1 = t1 mod 100 in
  let min2 = t2 mod 100 in
  if min1>=min2 then hour_diff*60 + min1 - min2
  else
    let to_next_hour = 60-min2 in
    hour_diff*60 -60 + to_next_hour + min1

let time_overlap g_time o_time =
  let (b,e) = g_time in
  let (ob,oe) = o_time in
  if (oe<b||e<ob) then 0 else
  if oe<e then if b>ob then time_diff oe b else time_diff oe ob
  else if b>ob then time_diff e b else time_diff e ob

let rec remove_overlaps_for_a_day s =
  match s with
  |(b1,e1)::(b2,e2)::t ->
    if (time_overlap (b1,e1) (b2,e2)) > 0
      then remove_overlaps_for_a_day (((min b1 b2),(max e1 e2))::t)
    else (b1,e1)::(remove_overlaps_for_a_day ((b2,e2)::t))
  | _ -> s


let rec remove_overlaps s =
  match s with
  |[] -> []
  |(day,l)::t ->
    let sorted_list_of_day = List.sort (fun (b1,_) (b2,_)-> b1-b2) l in
    (day,(remove_overlaps_for_a_day sorted_list_of_day)) :: (remove_overlaps t)

(*[create_group p] creates a group from a profile p with additional input of
  purpose, min size, and max size of group from user.*)
let rec create_group p purpose_list =
  let purpose = print_read "Enter project code: " in
  if String.contains purpose ' '
    then (print_string "Invalid project code.\n"; create_group p purpose_list)
  else if List.mem purpose purpose_list
    then (print_string "Already have a group for that project code.\n";
          create_group p purpose_list)
  else
    let range_min =
      try (int_of_string (print_read "Enter minimum desired group size: "))
      with _ -> -1 in
    let range_max =
      try (int_of_string (print_read "Enter maximum desired group size: "))
      with _ -> -1 in
    if (range_min = -1 || range_max = -1)
      then (print_endline "Your group size must be an integer";
            create_group p purpose_list)
    else if (range_min > range_max || range_max < 0)
      then (print_string ("Make sure your max size is greater than 0 and greater"^
                         " than or equal to min size.\n");
            create_group p purpose_list)
    else
      let schedule = create_schedule [] in
      let sched_string = schedule_to_string schedule in
      let params = [("user_id_list", string_of_int (user_id p));("purpose", purpose);
                    ("size","1");("range_min", string_of_int range_min);
                    ("range_max", string_of_int range_max);("group_blacklist","");
                    ("invited_groups_list","");("received_invites_list","");
                    ("schedule",sched_string)] in
      let update = (insert_group params) in
      if update = "-1" then
        (print_string "Group could not be created, try again.\n";
        create_group p purpose_list)
      else
        let group_id = (int_of_string update) in
        ignore (update_server (add_group p group_id))

(*[group_to_string g] is a readable string containing the purpose, minimum size
  and maximum size of group [g]*)
let group_to_string g =
  let inviting_groups = List.map (fun id -> lookup_group id) g.received_invites_list in
  let invites_no_empty = List.filter (fun g -> g.group_id <> -1) inviting_groups in
  "Project code : " ^ g.purpose ^ "\nSize: " ^ (string_of_int g.size) ^
  " (min " ^ (string_of_int (fst (g.range))) ^ ", max " ^
  (string_of_int (snd (g.range))) ^ ")\nInvites: " ^
  (string_of_int (List.length invites_no_empty)) ^ "\n"

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

(*[users g] gets the list of users of group [g]*)
let users g = g.user_id_list

(*[schedule g] gets the schedule of group [g]*)
let schedule g = g.schedule

let remove_overlaps_union s1 s2 =
  let rec remove_overlaps_helper s1 s2 =
    match s1 with
    |[] -> s2
    |(day,l)::t ->
      if List.mem_assoc day s2 then
        (day,l@(List.assoc day s2))::
          (remove_overlaps_helper t (List.remove_assoc day s2))
      else (day,l)::(remove_overlaps_helper t s2) in
  remove_overlaps (remove_overlaps_helper s1 s2)

(*[union g1 g2] is a new group with updated fields created by merging two groups*)
let union g1 g2 =
  let users = int_list_to_string ((g1.user_id_list)@(g2.user_id_list)) in
  let size = string_of_int ((g1.size) + (g2.size)) in
  let range_min = string_of_int (max (fst (g1.range)) (fst (g2.range))) in
  let range_max = string_of_int (min (snd (g1.range)) (snd (g2.range))) in
  let schedule = schedule_to_string
                    (remove_overlaps_union (g1.schedule) (g2.schedule)) in
  let params = [("user_id_list", users);("purpose", g1.purpose);("size",size);
                ("range_min", range_min);("range_max", range_max);
                ("group_blacklist","");("invited_groups_list","");
                ("received_invites_list",""); ("schedule",schedule)] in
  let update = (insert_group params) in
  if update = "-1" then print_string "Errors occured during group acceptance.\n";
  int_of_string update

(*[delete_group g] deletes group [g] from the server and deletes group from all
  the profiles that used to have it in their group_id list.
  returns unit

  potentially update to delete from user's list of groups that have invited them*)
let delete_group g =
  let prof_list = List.map (fun id -> lookup_profile id) g.user_id_list in
  List.iter (fun p -> ignore (update_server (remove_group p (g.group_id)))) prof_list;
  let update = (http_post del_group_url [("group_id",string_of_int g.group_id)]) in
  if update = "-1"
    then (print_string "Errors occured during group acceptance.\n";)
  else ()

(*[about_group g] prints a string displaying information about a group and the
  users in the group*)
let about_group g =
  let users = List.map (fun id -> lookup_profile id) g.user_id_list in
  print_string ("\nGroup for " ^ g.purpose ^ " (ID " ^ (string_of_int g.group_id) ^
                "):\nMembers: " ^ (string_of_int g.size) ^ " (min " ^
                (string_of_int (fst g.range)) ^ ", max " ^
                (string_of_int (snd g.range)) ^ ")\n" ^ "Schedule:");
  List.iter (fun (d,l) -> print_string ("\n"^(day_to_string_for_repl d)^
                          ": "^(times_to_string l))) g.schedule;
  print_string "\n";
  List.iter (fun p -> about_profile p) users

let update_and_return g =
  let blacklist = int_list_to_string g.group_blacklist in
  let received = int_list_to_string g.received_invites_list in
  let invited = int_list_to_string g.invited_groups_list in
  let params = [("group_id", (string_of_int g.group_id));
                ("group_blacklist",blacklist);("received_invites_list",received);
                ("invited_groups_list",invited)] in
  (update_group_lists params)

let rec invites_received g =
let inviting_groups = List.map (fun id -> lookup_group id) g.received_invites_list in
List.filter (fun g -> g.group_id <> -1) inviting_groups


(*[invites g] helps manage the invites that a group g has received. User can choose to
  accept or reject an invite or to leave the invite manager screen. Accepting an
  invite forms a new group, and deletes the user's group and the group they joined.
  Rejecting an invite blacklists the rejected group and deletes that invite.
  Going back updates the server to reflect the local user's recent decisions.
*)
let rec invites g =
  let inviting_groups = List.map (fun id -> lookup_group id) g.received_invites_list in
  let invites_no_empty = List.filter (fun g -> g.group_id <> -1) inviting_groups in
  if invites_no_empty = []
    then (print_endline "\n\nYou have no invitations.\n";
          ignore (update_and_return g))
  else
    (List.iter about_group invites_no_empty;
    let resp = print_read ("Enter \"accept\" or \"reject\" followed a group's id,"^
                           " or \"back\" to return to the previous page: ") in
    match (String.split_on_char ' ' resp) with
    | "accept"::x::[] ->
      let other = try (int_of_string x) with _ -> -1 in
      if other = -1
        then (print_string "\nYou must enter the group's id # to accept the invite\n";
              invites g)
      else
        let acceptedGroupOpt = find_group_by_id (other) invites_no_empty in
        (match acceptedGroupOpt with
          | None -> print_endline "Invalid group id."; invites g
          | Some acceptedGroup ->
            (delete_group g;
            delete_group acceptedGroup;
            let profile_union = List.map (fun id -> lookup_profile id)
                                  ((g.user_id_list)@(acceptedGroup.user_id_list)) in
            let update = union g acceptedGroup in
            if update <> -1
            then List.iter (fun p -> ignore (update_server (add_group p update)))
                  profile_union))
    | "reject"::x::[] ->
      let other = try (int_of_string x) with _ -> -1 in
      if other = -1
        then (print_string "\nYou must enter the group's id # to reject the invite\n";
              invites g)
      else
        let blacklist = if List.mem other g.group_blacklist then g.group_blacklist
                        else other::(g.group_blacklist) in
        let g' = {g with
                  received_invites_list =
                      (List.filter (fun x -> x<>other) (g.received_invites_list));
                  group_blacklist = blacklist} in
        invites g'
    | "back"::[] ->
      let update = update_and_return g in
      if update = "-1" then
        (print_string "Updating server unsuccessful, try again.\n";
        invites g)
    | _ -> print_endline "Invalid response. Try again."; invites g)

let rec gui_invites_accept g og =
    (delete_group g;
     delete_group og;
     let profile_union = List.map (fun id -> lookup_profile id)
         ((g.user_id_list)@(og.user_id_list)) in
     let update = union g og in
     if update <> -1
     then List.iter (fun p -> ignore (update_server (add_group p update)))
         profile_union)

let rec gui_invites_reject g ogid =
    let new_blacklist = if List.mem ogid g.group_blacklist then g.group_blacklist
      else (ogid)::(g.group_blacklist) in
    let g' = {g with
            received_invites_list =
                (List.filter (fun x -> x<>ogid) (g.received_invites_list));
            group_blacklist = new_blacklist} in
      g'

let rec gui_back g =
  let update = update_and_return g in
  if update = "-1" then
  print_string "Updating server unsuccessful, try again.\n"


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
  let json_string = http_get (download_groups_url ^ purpose) in
  let json_list = from_string json_string|>to_list in
  let group_list = List.map (init_group) json_list in
  List.filter (show_group_in_swipes g) group_list


let rec total_minutes acc sched =
  match sched with
  |[] -> acc
  |(_,l)::t ->
    let rec add_minutes_for_day acc times =
    match times with
    |[] -> acc
    |(b,e)::t -> add_minutes_for_day ((time_diff e b)+acc) t in
    total_minutes (acc + (add_minutes_for_day 0 l)) t

let rec overlap_for_one_day acc g_times o_times =
  match g_times with
  |[] -> acc
  |g_time::t ->
    let rec overlap_for_one_time acc g_time o_times =
    match o_times with
    |[] -> acc
    |o_time::t ->
      overlap_for_one_time ((time_overlap g_time o_time)+acc) g_time t in
    overlap_for_one_day (acc + (overlap_for_one_time 0 g_time o_times)) t o_times

let rec schedule_sum acc o_sched g_sched =
  match g_sched with
  |[] -> acc
  |(day,times)::t ->
    if List.mem_assoc day o_sched
      then schedule_sum
              (acc + (overlap_for_one_day 0 times (List.assoc day o_sched))) o_sched t
    else schedule_sum acc o_sched t

let schedule_score g other =
  let g_sched = g.schedule in
  let o_sched = other.schedule in
  let score = schedule_sum 0 o_sched g_sched in
  let total = total_minutes 0 g_sched in
  (float score)/.(float total)


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
  let group_interests =
      List.flatten (List.map (fun p -> interests p) group_profiles) in
  let group_interests_freq = create_key_freq_list [] group_interests in
  let group_interests_freq_norm =
      List.map (fun (k,v) -> (k,v/.(float (g.size)))) group_interests_freq in
  let other_interests =
      List.flatten (List.map (fun p -> interests p) other_profiles) in
  let other_interests_freq = create_key_freq_list [] other_interests in
  let other_interests_freq_norm =
      List.map (fun (k,v) -> (k,v/.(float (other.size)))) other_interests_freq in
  let total_interest =
      interests_sum 0.0 other_interests_freq_norm group_interests_freq_norm in
  total_interest/.(float (List.length group_interests))

(*[uniqify_list lst nlst] is [lst] with all duplicates removed*)
let rec uniqify_list lst nlst =
  match lst with
  |[]->nlst
  |h::t -> if List.mem h nlst then uniqify_list t nlst else uniqify_list t (h::nlst)

(*[exp_role_tuple grp] is an (experience,role) list generated by finding the desired experience
  and role for each user in a group*)
let rec exp_role_tuple grp =
  List.map (fun x -> (experience (lookup_profile x), role (lookup_profile x)))
            grp.user_id_list

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
  |h::t -> if List.mem h o_roles
            then (1. +. score_det t o_roles)
           else score_det t o_roles

(*[score_det_helper grp othergrp] calls score_det with a unique list of the desired roles/Experience
  of group [grp] and an (exp,role) list with each users experience and role in othergrp*)
let score_det_helper grp othergrp :float =
  score_det (uniqify_list (looking_for_getter grp.user_id_list) [])
            (exp_role_tuple (othergrp))

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
  let schedule_weight = 1.0 in
  (schedule_weight *. (schedule_score g other)) +.
    (interest_weight *. (interests_score g other)) +.
  (looking_for_weight *. (score_determination g other))

  let get_sorted_groups g =
    let groups_to_be_matched = get_groups_with_purpose g in
    List.sort (fun g1 g2 -> if (total_score g g1)<(total_score g g2) then 1
                        else if (total_score g g1)>(total_score g g2) then -1 else 0)
      groups_to_be_matched

(*[swipe g] produces a list of groups that can be swiped through by group g sorted
  from highest to lowest match score

  [swipe_repl g others] is a repl that allows the user to swipe left(reject) or right(accept)
  on a group, ask for more information about a group or to finish swiping
*)
let swipe g =
  let groups_to_be_matched = get_groups_with_purpose g in
  let sorted = List.sort (fun g1 g2 -> if (total_score g g1)<(total_score g g2) then 1
                          else if (total_score g g1)>(total_score g g2) then -1 else 0)
                          groups_to_be_matched in
  let rec swipe_repl g others =
    match others with
    |[] ->
      print_endline ("\n\nThere are no more groups to swipe on;"^
                    " returning to previous page.\n")
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
        let params = [("group_id", (string_of_int g.group_id));
                      ("group_blacklist",blacklist_str);
                      ("received_invites_list",received);
                      ("invited_groups_list",invited)] in
        ignore (update_group_lists params);
        swipe_repl {g with group_blacklist = blacklist} t
      |"right"::_ ->
        let g' = {g with invited_groups_list = (h.group_id)::g.invited_groups_list} in
        let params_h = [("group_id", string_of_int (h.group_id));
                        ("group_blacklist", int_list_to_string h.group_blacklist);
                        ("invited_groups_list",
                            int_list_to_string (h.invited_groups_list));
                        ("received_invites_list",
                            int_list_to_string
                                ((g.group_id)::(h.received_invites_list)))] in
        ignore (update_group_lists params_h);
        let params_g = [("group_id", string_of_int (g.group_id));
                        ("group_blacklist", int_list_to_string g.group_blacklist);
                        ("invited_groups_list",
                            int_list_to_string (g'.invited_groups_list));
                        ("received_invites_list",
                            int_list_to_string (g.received_invites_list))] in
        ignore (update_group_lists params_g);
        swipe_repl g' t
      |"done"::_ -> print_endline "Returning to previous page."
      | _ -> print_endline "Not a valid command."; swipe_repl g others in
  swipe_repl g sorted

let blacklist g ogid =
  let new_blacklist = (ogid)::(g.group_blacklist) in
  let blacklist_str = int_list_to_string new_blacklist in
  let received = int_list_to_string (g.received_invites_list) in
  let invited = int_list_to_string (g.invited_groups_list) in
  let params = [("group_id", (string_of_int g.group_id));
              ("group_blacklist",blacklist_str);
              ("received_invites_list",received);
                ("invited_groups_list",invited)] in
  ignore (update_group_lists params);
 let g' = {g with group_blacklist = new_blacklist} in g'

let invite_helper g og =
  let g' = {g with invited_groups_list = (og.group_id)::g.invited_groups_list} in
  let params_h = [("group_id", string_of_int (og.group_id));
                ("group_blacklist", int_list_to_string og.group_blacklist);
                ("invited_groups_list",
                    int_list_to_string (og.invited_groups_list));
                ("received_invites_list",
                    int_list_to_string
                        ((g.group_id)::(og.received_invites_list)))] in
  ignore (update_group_lists params_h);
  let params_g = [("group_id", string_of_int (g.group_id));
                ("group_blacklist", int_list_to_string g.group_blacklist);
                ("invited_groups_list",
                    int_list_to_string (g'.invited_groups_list));
                ("received_invites_list",
                    int_list_to_string (g.received_invites_list))] in
  ignore (update_group_lists params_g);
  g'

(*[leave p g] is a new group formed by re-creating group g except with profile [p]
  removed. Groups on the server are updated accordingly.*)
let rec leave p g =
  let updated_user_ids = (List.filter (fun x -> x<>(user_id p)) g.user_id_list) in
  (if updated_user_ids <> [] then
  let updated_users = int_list_to_string updated_user_ids in
  let range_min = string_of_int (fst (g.range)) in
  let range_max = string_of_int (snd (g.range)) in
  let blacklist = int_list_to_string g.group_blacklist in
  let received = int_list_to_string g.received_invites_list in
  let invited = int_list_to_string g.invited_groups_list in
  let params = [("user_id_list", updated_users);("purpose", g.purpose);
                ("size",string_of_int (g.size - 1));("range_min", range_min);
                ("range_max", range_max);("group_blacklist",blacklist);
                ("invited_groups_list",invited);("received_invites_list",received)] in
  let update = (insert_group params) in
  if update = "-1" then
    (print_string "Updated group could not be created, try again.\n";
    leave p g)
  else
    let updated_profiles = List.map (fun x -> lookup_profile x) updated_user_ids in
    List.iter (fun x -> ignore (update_server (add_group x (int_of_string update))))
      updated_profiles);
    delete_group g
