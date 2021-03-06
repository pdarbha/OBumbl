open Graphics
open Profile
open Group
open Helper

(*[pull_group_data p] takes in a group p and looks up the group;s data.
  requires: a valid group*)
let pull_group_data p =
  let group_int_list = groups p in
  List.map (fun id -> lookup_group id) group_int_list

(*[quit_button ()] draws a quit button at the top right of the screen*)
let quit_button () =
  draw_rect (size_x () - 30) (size_y () -30) 20 20;
  moveto (size_x () -22) (size_y () -25);
  draw_string "X"

(*[back_button ()] draws a quit button at the bottom right of the screen*)
let back_button () =
  draw_rect (size_x () - 70) (size_y () - 600) 50 20;
  moveto (size_x () -62) (size_y () -598);
  draw_string "Back"

let draw_login_register () =
  clear_graph ();
  set_color Graphics.blue;
  let x_size = size_x () in
  let y_size = size_y () in
  draw_rect ((x_size/2) - 150) (y_size/2- 100)
    300 100;
  moveto ((x_size/2)- 15) (y_size/2- 60);
  draw_string "LOGIN";
  draw_rect ((x_size/2) - 150) (y_size/2- 200)
    300 100;
  moveto ((x_size/2)-20) (y_size/2- 160);
  draw_string "REGISTER";
  moveto ((x_size/2)-15) (y_size/2 + 200);
  set_text_size 300;
  draw_string "OBUMBL";
  quit_button ()

(*[s_exp p] takes in a profile and returns a string of that profile's experience
requires: a valid profile*)
let s_exp (p:profile) : string =
  let e = p |> experience in
  match e with
  |`BEG -> "BEG"
  |`INT -> "INT"
  |`ADV -> "ADV"

(*[top3interests lst] takes in a lst of interests and returns the first 3.
  requires: a list *)
let top3interests lst =
  match lst with
  |h::h2::h3::t -> h^", "^h2^", "^h3
  |h::h2::[] -> h^", "^h2
  |h::[] -> h
  |[] -> "none"

(*draw_profile p takes in a profile and draws the profile information onto the
  canvas. requires: a valid profile p*)
let draw_profile (p:profile) =
  set_color (Graphics.white);
  fill_rect ((size_x () / 2)) ((size_y () / 2) - 50)
    375 310;
  set_color (Graphics.black);
  moveto ((size_x () / 2) + 180) ((size_y ())/2 + 240);
  draw_string ("user id: "^(string_of_int (p |> user_id)));
  moveto ((size_x () / 2) + 20) ((size_y ())/2 + 220);
  draw_string ("school: "^ (p |> school));
  moveto ((size_x () / 2) + 20) ((size_y ())/2 + 200);
  draw_string ("experience: "^ (s_exp p)) ;
  moveto ((size_x () / 2) + 20) ((size_y ())/2 + 180);
  draw_string ("role: "^(p |> role));
  moveto ((size_x () / 2) + 20) ((size_y ())/2 + 160);
  draw_string ("looking for: "^(looking_for_to_string (p |>looking_for)));
  moveto ((size_x () / 2) + 20) ((size_y ())/2 + 140);
  draw_string ("Top 3 Interests: "^(top3interests (interests p)));
  moveto ((size_x () / 2) + 20) ((size_y ())/2 + 120);
  draw_string ("Description: "^(description p));

  (*an exception type*)
exception Oe of string

(*[fileterer il g] takes in a list of elemnts il and an element g and removes g
  from the list. requires: g to have the same type as the elements of il *)
let rec filterer il g =
  match il with
  |[] -> []
  |h::t -> if h = g then filterer t g else h::filterer t g

(*[welcome_screen p gd] draws the welcome screen first seen when entering the
  gui. Displays options to go to the group, new group and view profie windows.*)
let rec welcome_screen p gd =
  clear_graph ();
  quit_button ();
  set_color Graphics.magenta;
  moveto (size_x ()/2 - 5) (size_y () - 30);
  draw_string "OBumbl";
  moveto (size_x ()/2 - 10) (size_y () - 45);
  draw_string ("Hello "^(name p)^"!");
  draw_rect ((size_x () * 2)/5 - 300) ((size_y ())/5 + 200)
    300 100;
  draw_rect ((size_x () * 4)/5 -200) (size_y ()/2 + 100)
    300 100;
  draw_rect ((size_x () * 4)/5 -200) (((size_y ()/2) -100))
    300 100;
  set_color Graphics.blue;
  moveto (((size_x () * 2)/5) - 300) ((size_y ())/5 + 360);
  draw_string "To start swiping or to check your invite list,";
  moveto (((size_x () * 2)/5) - 300) ((size_y ())/5 + 345);
  draw_string "first select a group in the groups screen.";
  moveto (((size_x () * 2)/5) - 190) ((size_y ())/5 + 250);
  draw_string "Groups";
  moveto ((size_x () * 4)/5 -75) (size_y ()/2 + 150);
  draw_string "New Group";
  moveto ((size_x () * 4)/5 -85) ((size_y ()/2) -50);
  draw_string "View Profile";
  bd_welcome_screen p gd

(*[bd_listener_view_profile p gd] will look for button presses within the view
  profile window, and will transfer the user there if they press that box*)
  and bd_listener_view_profile p gd =
    let s = wait_next_event [Button_down] in
    let coords = s.mouse_x, s.mouse_y in
    match coords with
    |(x,y) ->
      if x > (size_x () - 70) && x < (size_x () - 20) &&
        (y > (size_y () - 600)) && (y < (size_y () - 580))
        then welcome_screen p gd
      else if x > (size_x () - 30) && x < (size_x () - 10) && y > (size_y () -30)
          && y < (size_y () - 10)
        then failwith "quitting"
      else if x > (70) && x < 150 && y > (size_y () - 600)
           && y < (size_y () - 580)
        then edit_profile_screen p gd
      else
          bd_listener_view_profile p gd

(*[bd_listener_no_groups p gd swipes] will look for button presses within the
  groups box in the welcome screen. will display different windows based on
  whether the user has groups or not *)
  and bd_listener_no_groups p gd swipes =
    let s = wait_next_event [Button_down] in
    let coords = s.mouse_x, s.mouse_y in
    match coords with
    |(x,y) ->
      if x > (size_x () - 70) && x < (size_x () - 20)
        && (y > (size_y () - 600)) && (y < (size_y () - 580))
        then (if swipes then groups_screen p gd 0 0 else welcome_screen p gd)
      else if x > (size_x () - 30) && x < (size_x () - 10) && y > (size_y () -30)
          && y < (size_y () - 10)
        then failwith "quitting" else bd_listener_no_groups p gd swipes

(*[groups_screen p' gd' c ic] draws the groups screen in the gui, and displays
  relevant information about those ggroups. takes in a profile p', profile gd',
  and two ints c and ic to act as counters when scrolling through lists.
  requires: p' to be a valid profile and gd' to be valid group data*)
  and groups_screen p' gd' c ic =
    let p = lookup_profile (user_id p') in
    let gd = pull_group_data p in
    clear_graph ();
    quit_button ();
    back_button ();
    draw_rect ((size_x () / 2) - 400) ((size_y ())/2 - 100)
      800 400;
    try (let g = lookup_group (List.nth (groups p) c) in
        set_color Graphics.black;
         draw_rect ((size_x () / 2) + 300) ((size_y () / 2) - 175)
           100 50;
         moveto ((size_x () / 2) + 330) ((size_y ())/2 - 155);
         draw_string "Next"; (*fix functionality*)
         moveto ((size_x () / 2) - 50) ((size_y ())/2 + 270);
         (*draw_string ("Groupid: " ^ (groupid (List.nth gd c)));*)
         moveto ((size_x () / 2) - 350) ((size_y ())/2 + 225);
         draw_string ("Purpose: " ^ (purpose g));
         moveto ((size_x () / 2) - 350) ((size_y ())/2 + 175);
         draw_string ("Size: " ^ (string_of_int (size g)));
         moveto ((size_x () / 2) - 350) ((size_y ())/2 + 125);
        (if size g >= fst(range g) then set_color Graphics.green
         else set_color Graphics.red);
         draw_string ("Range: " ^ "("^string_of_int(fst(range g))^
                      ", "^string_of_int(snd(range g))^")");
         moveto ((size_x () / 2) - 350) ((size_y ())/2 + 75);
        set_color Graphics.black;
        draw_string ("Schedule: " ^ schedule_to_string (schedule g));
        set_color Graphics.cyan;
         set_color Graphics.black;
         draw_profile (lookup_profile (List.nth (users g) ic));
         draw_rect ((size_x () / 2) + 325) ((size_y () / 2) - 80)
           50 20;
         moveto ((size_x () / 2) + 340) ((size_y ())/2 - 75);
         draw_string "Next";
         draw_rect ((size_x () / 2) + 425) ((size_y ())/2 + 200)
           100 50;
         moveto ((size_x () / 2) + 430) ((size_y ())/2 +205);
         draw_string "Start Swiping";
         draw_rect ((size_x () / 2) + 425) ((size_y ())/2 + 125)
           100 50;
         moveto ((size_x () / 2) + 430) ((size_y ())/2 +130);
         draw_string ("Invite List");
         draw_rect ((size_x () / 2) - 400) ((size_y () / 2) - 175)
           100 50;
         moveto ((size_x () / 2) - 390) ((size_y ())/2 -155);
          draw_string ("Leave Group");
        bd_listener_groups_screen p gd c ic)
    with Failure("nth") -> moveto ((size_x () / 2) - 350) ((size_y ())/2 + 195);
      draw_string "You have no groups to display. Go back and create a group.";
      bd_listener_no_groups p gd false

(*[bd_welcome_screen p gd] takes in a profile p and gd and looks for button
  presses in other boxes on the screen. will transfer the user to the correct
  windows accordingly. *)
  and bd_welcome_screen p gd =
    let s = wait_next_event [Button_down] in
    let coords = s.mouse_x, s.mouse_y in
    match coords with
    |(x,y) ->
      if x > (((size_x () * 2)/5) - 300) && x < (((size_x () * 2)/5)) &&
                  y > ((size_y ())/5 + 200) && y < ((size_y ())/5 + 300)
        then groups_screen p gd 0 0
      else if x >  ((size_x () * 4)/5 - 200) && x < ((size_x () * 4)/5 +100) &&
              y >  (size_y ()/2 + 100) && y < (size_y ()/2 + 200)
        then new_group_screen p gd
      else if x >  ((size_x () * 4)/5 - 200) && x < ((size_x () * 4)/5 +100) &&
              y > ((size_y ()/2) - 100) && y < ((size_y ()/2))
        then view_profile_screen p gd
      else if
          x > (size_x () - 30) && x < (size_x () - 10) && y > (size_y () -30)
          && y < (size_y () - 10)
        then failwith "quitting"
      else bd_welcome_screen p gd

(*[bd_listener_groups_screen p gd c ic] takes in two profiles p and gd and two
  counters c and ic, and listens for button presses to transfer the user to the
  accompanying screens
  requires: a valid profile p, group data gd, and two valid integers c and ic*)
  and bd_listener_groups_screen p gd c ic =
    let s = wait_next_event [Button_down] in
    let coords = s.mouse_x, s.mouse_y in
    match coords with
    |(x,y) ->
      if x > (size_x () - 70) && x < (size_x () - 20) &&
        (y > (size_y () - 600)) && (y < (size_y () - 580))
        then welcome_screen p gd
      else if
          x > (size_x () - 30) && x < (size_x () - 10) && y > (size_y () -30)
          && y < (size_y () - 10)
        then failwith "quitting"
     else if y > ((size_y () / 2) - 175) && y < ((size_y () / 2) - 125) &&
           x > ((size_x () / 2) + 300) && x < ((size_x () / 2) + 400)
        then groups_screen p gd ((c+1) mod (List.length gd)) 0
     else if y > ((size_y () / 2) - 80) && y < ((size_y () / 2) - 60) &&
                x > ((size_x () / 2)+325) && x < ((size_x () / 2) + 375)
        then groups_screen p gd c
              ((ic+1) mod (List.length (users (lookup_group (List.nth (groups p) c)))))
     else if x > ((size_x () / 2) + 425) && x < ((size_x () / 2) + 525) &&
          y> ((size_y ())/2 + 200) && y < ((size_y ())/2 + 250)
        then swipe_screen_helper (lookup_group (List.nth (groups p) c)) p gd
     else if x > ((size_x () / 2) + 425) && x < ((size_x () / 2) + 525) &&
             y > ((size_y ())/2 + 125) && y < ((size_y ())/2 + 175)
        then invite_screen p (invites_received (lookup_group (List.nth (groups p) c)))
                              (lookup_group (List.nth (groups p) c)) 0 0 gd
     else if x > ((size_x () / 2) - 400) && x < ((size_x () / 2) - 300) &&
             y > ((size_y () / 2) - 175) && y < ((size_y () / 2) - 125)
        then ((leave p (lookup_group (List.nth (groups p) c)));
              groups_screen p gd 0 0)
     else bd_listener_groups_screen p gd c ic

(*[view_profile_screen p gd] draws the view profile screen and displays all
  relevant information about a profile. allows the user to go to the edit
  profile screen as well*)
  and view_profile_screen p gd =
    clear_graph ();
    quit_button ();
    back_button ();
    draw_rect ((size_x () / 2) - 400) ((size_y ())/2 - 100)
      800 400;
    moveto ((size_x () / 2)) ((size_y ())/2 + 275);
    draw_string ("Name: "^ ((p |> name)));
    moveto ((size_x () / 2)-300) ((size_y ())/2 + 255);
    draw_string ("User ID: "^ (string_of_int(p |> user_id)));
    moveto ((size_x () / 2) - 300) ((size_y ())/2 + 235);
    draw_string ("school: "^ (p |> school));
    moveto ((size_x () / 2) -300) ((size_y ())/2 + 215);
    draw_string ("experience: "^ (s_exp p)) ;
    moveto ((size_x () / 2) -300) ((size_y ())/2 + 195);
    draw_string ("role: "^(p |> role));
    moveto ((size_x () / 2) -300) ((size_y ())/2 + 175);
    draw_string ("looking for: "^(looking_for_to_string (p |>looking_for)));
    moveto ((size_x () / 2) -300) ((size_y ())/2 + 155);
    draw_string ("Top 3 Interests: "^(top3interests (interests p)));
    moveto ((size_x () / 2) -300) ((size_y ())/2 + 135);
    draw_string ("Description: "^(description p));
    moveto ((size_x () / 2) -300) ((size_y ())/2 + 75);
    draw_string ("github: "^(p |> github));
    moveto ((size_x () / 2) -300) ((size_y ())/2 + 55);
    draw_string ("email: "^(p |> email));
    draw_rect (70) (size_y () - 600) 80 20;
    moveto (72) (size_y () -598);
    draw_string "Edit Profile";
    bd_listener_view_profile p gd

(*[new_group_screen p gd] transfers a user to the repl to create a new group,
  then reopens the window with the user's groups updated and displayed. takes in
  a profile p and group gd
  requires: a valid profile p and group data gd*)
  and new_group_screen p gd =
    close_graph ();
    let purpose_list = List.map (fun g -> purpose g) gd in
    create_group p purpose_list;
    let p' = lookup_profile (user_id p) in
    Graphics.open_graph " 1000x750";
    draw_start_canvas p' (pull_group_data p')

(*[edit_profile_screen p gd] takes in a profile p and group gd and transfers the
  user to the repl to edit their profile. then reopens the window with the
  profile information updated.
  requires: a valid profile p and group data gd*)
  and edit_profile_screen p gd =
    close_graph ();
    let field = print_read ("Type the field you would like to edit (\"name\","^
                            " \"school\", \"description\", \"interest_list\","^
                            " \"experience\", \"role\", \"looking_for\", "^
                            "\"github_url\", or \"email\"): ") in
    let field_value =
      (match field with
      | "name" | "school" | "description" | "experience" | "role" | "github_url"
      | "email" -> print_read ("What would you like as your new " ^ field ^ "? ")
      | "interest_list" -> list_to_string (cp_interests ())
      | "looking_for" -> looking_for_to_string (cp_looking_for ())
      | _ -> print_string "Invalid field.\n"; "") in
    if (((field = "name" && field_value <> "") || field <> "name") &&
        update_server (edit p field field_value) = true)
      then ()
      else print_string "Profile edit unsuccessful.\n";
      let p' = lookup_profile (user_id p) in
      Graphics.open_graph " 1000x750";
      view_profile_screen p' (pull_group_data p')

(*[swipe_screen_helper g p gd] takes in p and gd, and a list of groups g. gets
  the sorted groups from g and returns them. requires: a valid list of groups *)
  and swipe_screen_helper g p gd =
    let sorted = Group.get_sorted_groups g
    in swipe_screen g p sorted gd 0

(*[swipe_screen g p s gd ic] draws the swiping screen in the gui, allowing a
  user to cycle throguh groups and accept or reject them.
  requires: a valid profile p, group list g, schedule s, group data gd
  and counter ic*)
  and swipe_screen g p s gd ic =
    clear_graph ();
    quit_button ();
    back_button ();
    draw_rect ((size_x () / 2) - 400) ((size_y ())/2 - 100)
      800 400;
    if (List.length s) = 0 then (
      moveto ((size_x () / 2) - 25) ((size_y ())/2);
      draw_string "There are no more groups to swipe through. Return to previous page";
      bd_listener_no_groups p gd true
    )
    else
      draw_rect ((size_x () / 2) - 400) ((size_y () / 2) - 175)
        100 50;
      draw_rect ((size_x () / 2) + 300) ((size_y () / 2) - 175)
        100 50;
      moveto ((size_x () / 2) - 370) ((size_y ())/2 - 155);
      draw_string "Reject";
      moveto ((size_x () / 2) + 330) ((size_y ())/2 - 155);
      draw_string "Invite";
      moveto ((size_x () / 2) - 50) ((size_y ())/2 + 270);
      (*draw_string ("Groupid: " ^ (groupid (List.hd s)));*)
      moveto ((size_x () / 2) - 350) ((size_y ())/2 + 225);
      draw_string ("Purpose: " ^ (purpose (List.hd s)));
      moveto ((size_x () / 2) - 350) ((size_y ())/2 + 175);
      draw_string ("Size: " ^ string_of_int (size (List.hd s)));
      moveto ((size_x () / 2) - 350) ((size_y ())/2 + 125);
      draw_string ("Range: " ^ "("^string_of_int(fst(range (List.hd s)))^", "
                   ^string_of_int(snd(range (List.hd s)))^")");
      moveto ((size_x () / 2) - 350) ((size_y ())/2 + 75);
      draw_string ("Schedule: " ^ schedule_to_string (schedule (List.hd s)));
      set_color Graphics.cyan;
      draw_rect ((size_x () / 2)) ((size_y () / 2) - 50)
        375 310;
      set_color Graphics.black;
      draw_profile (lookup_profile (List.nth (Group.users (List.hd s)) ic));
      draw_rect ((size_x () / 2) + 325) ((size_y () / 2) - 80)
        50 20;
      moveto ((size_x () / 2) + 340) ((size_y ())/2 - 75);
      draw_string "Next";
      bd_swipe_listener g p s gd ic

(*[bd_swipe_listener g p so gd ic] listens for button presses on the swipe
  screen, and updates the ser's blacklist and invite list when theuy reject or
  accept a group.
  requires: a valid profile p, group list p, schedule so, group data gd and
  counter ic*)
  and bd_swipe_listener g p so gd ic =
    let s = wait_next_event [Button_down] in
    let coords = s.mouse_x, s.mouse_y in
    match coords with
    |(x,y) ->
      if x > (size_x () - 70) && x < (size_x () - 20) &&
        (y > (size_y () - 600)) && (y < (size_y () - 580))
        then welcome_screen p gd
      else if x > (size_x () - 30) && x < (size_x () - 10) && y > (size_y () -30)
            && y < (size_y () - 10)
        then failwith "quitting"
      else if x > ((size_x () / 2) + 325) && x < ((size_x () / 2) + 375) &&
            (y > ((size_y () / 2) - 80)) && (y < ((size_y () / 2) - 60))
        then swipe_screen g p so gd ((ic+1) mod (List.length (users (List.hd so))))
      else if x > ((size_x () / 2) - 400) && x < ((size_x () / 2) -300) &&
              (y > ((size_y () / 2) - 175)) && (y < ((size_y () / 2) - 125))
        then swipe_screen (blacklist g (groupid (List.hd so))) p (List.tl so) gd ic
      else if x > ((size_x () / 2) + 300) && x < ((size_x () / 2) +400) &&
              (y > ((size_y () / 2) - 175)) && (y < ((size_y () / 2) - 125))
        then swipe_screen (Group.invite_helper g (List.hd so)) p (List.tl so) gd ic
      else bd_swipe_listener g p so gd ic

(*[invite_screen p il g c ic gd] will draw the invites screen of a user and
  write all of the invtes that they have recieved.
  requires: a valid profile p, list of invies il, group list g, counters c and
  ic, and group data gd*)
  and invite_screen p il g c ic gd =
    clear_graph ();
    quit_button ();
    back_button ();
    draw_rect ((size_x () / 2) - 400) ((size_y ())/2 - 100)
      800 400;
    if List.length il = 0 then
      (moveto ((size_x () / 2) - 100) ((size_y ())/2 +100);
       draw_string "No invites for this group. Please go back";
        bd_listener_no_groups p gd true)
    else
      let g' = (List.nth il c) in
    draw_rect ((size_x () / 2) - 400) ((size_y () / 2) - 175)
      100 50;
    draw_rect ((size_x () / 2) + 300) ((size_y () / 2) - 175)
      100 50;
    draw_rect ((size_x () / 2) - 50) ((size_y () / 2) - 175)
      100 50;
    moveto ((size_x () / 2) - 370) ((size_y ())/2 - 155);
    draw_string "Reject";
    moveto ((size_x () / 2) + 330) ((size_y ())/2 - 155);
    draw_string "Accept";
    moveto ((size_x () / 2) -20) ((size_y ())/2 - 155);
    draw_string "Next";
    moveto ((size_x () / 2) - 50) ((size_y ())/2 + 270);
    (*draw_string ("Groupid: " ^ (groupid (List.nth gd c)));*)
    moveto ((size_x () / 2) - 350) ((size_y ())/2 + 225);
    draw_string ("Purpose: " ^ (purpose g'));
    moveto ((size_x () / 2) - 350) ((size_y ())/2 + 175);
    draw_string ("Size: " ^ (string_of_int (size g')));
    moveto ((size_x () / 2) - 350) ((size_y ())/2 + 125);
    draw_string ("Range: " ^ "("^string_of_int(fst(range g'))^", "
                 ^string_of_int(snd(range g'))^")");
    set_color Graphics.cyan;
    set_color Graphics.black;
    draw_profile (lookup_profile (List.nth (users g') ic));
    draw_rect ((size_x () / 2) + 325) ((size_y () / 2) - 80)
      50 20;
    moveto ((size_x () / 2) + 340) ((size_y ())/2 - 75);
    draw_string "Next";
    bd_invite_listener g p gd il c ic

(*[bd_invite_listener g p il c ic] will listen for button presses on the invites
  screen and transfer the user to the correct screens based on which box they
  pressed in.
  requires: a valid group g, profile p, invites list il, and counters c and ic*)
  and bd_invite_listener g p gd il c ic =
    let s = wait_next_event [Button_down] in
    let coords = s.mouse_x, s.mouse_y in
    match coords with
    |(x,y) ->
      if x > (size_x () - 70) && x < (size_x () - 20) && (y > (size_y () - 600))
          && (y < (size_y () - 580))
        then (gui_back g; groups_screen p gd 0 0)
      else if x > (size_x () - 30) && x < (size_x () - 10) && y > (size_y () -30)
          && y < (size_y () - 10)
        then failwith "quitting"
      else if y > ((size_y () / 2) - 175) && y < ((size_y () / 2) - 125)
              && x > ((size_x () / 2) - 50) && x < ((size_x () / 2) + 50)
        then invite_screen p il g ((c+1) mod (List.length il)) 0 gd
      else if y > ((size_y () / 2) - 80) && y < ((size_y () / 2) - 60) &&
              x > ((size_x () / 2) + 325) && x < ((size_x () / 2) + 375)
        then invite_screen p il g c ((ic+1) mod
              (List.length (users (List.nth il c)))) gd
      else if y > ((size_y () / 2) - 175) && y < ((size_y () / 2) - 125) &&
              x > ((size_x () / 2) -400) && x < ((size_x () / 2) -300)
        then invite_screen p (filterer il (List.nth il c))
              (gui_invites_reject g (groupid (List.nth il c)))
              (c mod ((List.length il) -1)) 0 gd
      else if y > ((size_y () / 2) - 175) && y < ((size_y () / 2) - 125) &&
              x > ((size_x () / 2) +300) && x < ((size_x () / 2) +400)
        then
          (Group.gui_invites_accept g (List.nth il c);
          let group_data = pull_group_data p in
          groups_screen p group_data 0 0)
      else bd_invite_listener g p gd il c ic


(*[draw_start_canvas user_prof group_data] takes in a uer profile and some
  group data, and draws a gui screen.
  requires: a valid user_id and group_data*)
  and draw_start_canvas user_prof group_data =
    set_color Graphics.blue;
    welcome_screen user_prof group_data
