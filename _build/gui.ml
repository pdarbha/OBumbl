open Graphics
open Profile
open Group
(*open Camlimages
open Images
open Elements
  open Jpeg*)

(*how to get the profile in
  how to get the sorted groups list
  transitions into repl
  button listening for all buttons
  need to log the invited list and accepted list
  need to swipe and look at invite list based on a code
*)
let buttonclickinfo () =
  let s = wait_next_event [Button_down] in
  print_string (string_of_int s.mouse_x);
  s.mouse_x, s.mouse_y

let quit_button () =
  draw_rect (size_x () - 30) (size_y () -30) 20 20;
  moveto (size_x () -22) (size_y () -25);
  draw_string "X"

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

let rec listen func_bd =
  let s = wait_next_event [Button_down] in
 if s.Graphics.button then func_bd s.Graphics.mouse_x s.Graphics.mouse_y
  else
  listen func_bd

let login_screen () =
  clear_graph ();
  let x_size = size_x () in
  let y_size = size_y () in
  draw_rect ((x_size/2) - 150) (y_size/2- 100)
  300 100;
  moveto ((x_size/2)- 220) (y_size/2- 60);
  draw_string "Username";
  draw_rect ((x_size/2) - 150) (y_size/2- 200)
  300 100;
  moveto ((x_size/2)-220) (y_size/2- 160);
  draw_string "Password";
  moveto ((x_size/2)-15) (y_size/2 + 200);
  quit_button ();
  moveto ((x_size/2)- 125) (y_size/2- 60)
  (*listen (
    if fst (mouse_pos_x) > ((x_size/2) - 150)
    && fst(mouse_pos_x) < ((x_size/2) - 150) then
      if  snd(mouse_pos_x) > (y_size/2- 100) &&
          snd(mouse_pos_x) < (y_size/2) then
        moveto ((x_size/2)-150) (y_size/2- 60);
    (fun x -> draw_string x)
      else if snd(mouse_pos_x) < (y_size/2- 100) &&
              snd(mouse_pos_x) > (y_size/2 - 200) then
    )*)

let s_exp (p:profile) : string =
  let e = p |> Profile.experience in
  match e with
  |`BEG -> "BEG"
  |`INT -> "INT"
  |`ADV -> "ADV"

let top3interests lst =
  match lst with
  |h::h2::h3::t -> h^", "^h2^", "^h3
  |h::h2::[] -> h^", "^h2
  |h::[] -> h
  |[] -> "none"

let rec description_drawer d xsize x y =
  moveto x y;
  if fst (text_size d) <= xsize then draw_string ("description: "^d)
  else draw_string (String.sub d 0 (String.length d / (fst (text_size d)/ xsize)+1));
  description_drawer
    (String.sub d ((String.length d / (fst (text_size d)/ xsize)+1)+1) (String.length d))
    xsize x (y-15)



let draw_profile (p:profile) =
  fill_rect ((size_x () / 2)) ((size_y () / 2) - 50)
    375 310;
  moveto ((size_x () / 2) + 180) ((size_y ())/2 + 240);
  draw_string ("user id: "^(string_of_int (p |> Profile.user_id)));
  moveto ((size_x () / 2) + 20) ((size_y ())/2 + 220);
  draw_string ("school: "^ (p |> Profile.school));
  moveto ((size_x () / 2) + 20) ((size_y ())/2 + 200);
  draw_string ("experience: "^ (s_exp p)) ;
  moveto ((size_x () / 2) + 20) ((size_y ())/2 + 180);
  draw_string ("role: "^(p |> Profile.role));
  moveto ((size_x () / 2) + 20) ((size_y ())/2 + 160);
  draw_string ("looking for: "^(Profile.looking_for_to_string (p |>
                                                   Profile.looking_for)));
  moveto ((size_x () / 2) + 20) ((size_y ())/2 + 140);
  draw_string ("Top 3 Interests: "^(top3interests (Profile.interests p)));
  description_drawer (Profile.description p) 355 ((size_x () / 2) + 20) ((size_y ())/2 + 120)




  let swipe_screen p gd =
    clear_graph ();
    quit_button ();
    back_button ();
    draw_rect ((size_x () / 2) - 400) ((size_y ())/2 - 100)
      800 400;
    draw_rect ((size_x () / 2) - 400) ((size_y () / 2) - 175)
      100 50;
    draw_rect ((size_x () / 2) + 300) ((size_y () / 2) - 175)
      100 50;
    moveto ((size_x () / 2) - 370) ((size_y ())/2 - 155);
    draw_string "Reject";
    moveto ((size_x () / 2) + 330) ((size_y ())/2 - 155);
    draw_string "Invite";
    moveto ((size_x () / 2) - 50) ((size_y ())/2 + 270);
    draw_string ("Groupid: " ^ "placeholder");
    moveto ((size_x () / 2) - 350) ((size_y ())/2 + 225);
    draw_string ("Purpose: " ^ "placeholder");
    moveto ((size_x () / 2) - 350) ((size_y ())/2 + 175);
    draw_string ("Size: " ^ "placeholder");
    moveto ((size_x () / 2) - 350) ((size_y ())/2 + 125);
    draw_string ("Range: " ^ "placeholder");
    set_color Graphics.cyan;
    fill_rect ((size_x () / 2)) ((size_y () / 2) - 50)
      375 310;
    set_color Graphics.black;
    draw_rect ((size_x () / 2)) ((size_y () / 2) - 80)
      50 20;
    draw_rect ((size_x () / 2) + 325) ((size_y () / 2) - 80)
      50 20;
    moveto ((size_x () / 2) + 10) ((size_y ())/2 - 75);
    draw_string "Prev";
    moveto ((size_x () / 2) + 340) ((size_y ())/2 - 75);
    draw_string "Next"(*implement functionality*)


let invite_screen p gd =
  clear_graph ();
  quit_button ();
  back_button ();
  draw_rect ((size_x () / 2) - 400) ((size_y ())/2 - 100)
    800 400;
  draw_rect ((size_x () / 2) - 400) ((size_y () / 2) - 175)
    100 50;
  draw_rect ((size_x () / 2) + 300) ((size_y () / 2) - 175)
    100 50;
  moveto ((size_x () / 2) - 370) ((size_y ())/2 - 155);
  draw_string "Reject";
  moveto ((size_x () / 2) + 330) ((size_y ())/2 - 155);
  draw_string "Accept"



let new_group_screen p gd = failwith "new"

let view_profile_screen p =
  clear_graph ();
  quit_button ();
  back_button ();
  draw_rect ((size_x () / 2) - 400) ((size_y ())/2 - 100)
    800 400;
  moveto ((size_x () / 2)) ((size_y ())/2 + 275);
  draw_string ("Name: "^ ((p |> Profile.name)));
  moveto ((size_x () / 2)-300) ((size_y ())/2 + 255);
  draw_string ("User ID: "^ (string_of_int(p |> Profile.user_id)));
  moveto ((size_x () / 2) - 300) ((size_y ())/2 + 235);
  draw_string ("school: "^ (p |> Profile.school));
  moveto ((size_x () / 2) -300) ((size_y ())/2 + 215);
  draw_string ("experience: "^ (s_exp p)) ;
  moveto ((size_x () / 2) -300) ((size_y ())/2 + 195);
  draw_string ("role: "^(p |> Profile.role));
  moveto ((size_x () / 2) -300) ((size_y ())/2 + 175);
  draw_string ("looking for: "^(Profile.looking_for_to_string (p |>
                                                               Profile.looking_for)));
  moveto ((size_x () / 2) -300) ((size_y ())/2 + 155);
  draw_string ("Top 3 Interests: "^(top3interests (Profile.interests p)));
  moveto ((size_x () / 2) -300) ((size_y ())/2 + 135);
  draw_string ("Description: "^(Profile.description p));
  moveto ((size_x () / 2) -300) ((size_y ())/2 + 115);
  draw_string ("github: "^(p |> Profile.github));
  moveto ((size_x () / 2) -300) ((size_y ())/2 + 105);
  (*draw_string ("email: "^(p |> Profile.email));*)
  (*description_drawer (Profile.description p) 650 ((size_x () / 2) -300) ((size_y ())/2 + 135)*)
  draw_rect (size_x () + 70) (size_y () - 600) 70 20;
  moveto (size_x () +72) (size_y () -598);
  draw_string "Edit Profile"


(* draw_string "Groups";
   draw_string "Start Swiping";
   draw_string "New Group";
   draw_string "Invite List";
   draw_string "Edit Profile"; (*brings you back to repl*) *)


(*let rec bd_welcome_screen p gd =
let s = wait_next_event [Button_down] in
let coords = s.mouse_x, s.mouse_y in
(*if button_down () then*)
  match coords with
  |(x,y) -> if x > (((size_x () * 2)/5) - 300) && x < (((size_x () * 2)/5)) then
      (*if (y > (size_y ())/5 + 360) && (y < ((size_y ())/5 + 460)) then
        swipe_screen () else *)if y > ((size_y ())/5 + 200) && y <
          ((size_y ())/5 + 300) then groups_screen p gd 0
      (*else if y > (((size_y ())/5)+ 90) && y < (((size_y ())/5)+190) then
        invite_screen ()*)
      else if x >  ((size_x () * 4)/5 - 200) && x > ((size_x () * 4)/5 +100)
      then if
        y >  (size_y ()/2 + 100) && y < (size_y ()/2 + 200) then
          new_group_screen p gd else if
          y > ((size_y ()/2) - 50) && y < ((size_y ()/2) +50) then
          view_profile_screen p else if
          x > (size_x () - 30) && x < (size_x () - 10) && y > (size_y () -30)
          && y < (size_y () - 10)
        then failwith "quitting" else bd_welcome_screen p gd*)

  let rec welcome_screen p gd =
    clear_graph ();
    quit_button ();
    set_color Graphics.magenta;
    moveto (size_x ()/2 - 5) (size_y () - 30);
    draw_string "OBumbl";
    moveto (size_x ()/2 - 10) (size_y () - 45);
    draw_string ("Hello "^(Profile.name p)^"!");
    (*draw_rect (((size_x () * 2)/5) - 300) ((size_y ())/5 + 360)
      300 100;*)
    draw_rect ((size_x () * 2)/5 - 300) ((size_y ())/5 + 200)
      300 100;
    (*draw_rect ((size_x () * 2)/5 - 300) (((size_y ())/5)+40)
      300 100;*)
    draw_rect ((size_x () * 4)/5 -200) (size_y ()/2 + 100)
      300 100;
    draw_rect ((size_x () * 4)/5 -200) (((size_y ()/2) -100))
      300 100;
    set_color Graphics.blue;
    (*moveto (((size_x () * 2)/5) - 200) ((size_y ())/5 + 410);
      draw_string "Start Swiping";*)
    moveto (((size_x () * 2)/5) - 300) ((size_y ())/5 + 360);
    draw_string "To start swiping or to check your invite list,";
    moveto (((size_x () * 2)/5) - 300) ((size_y ())/5 + 345);
    draw_string "first select a group in the groups screen.";
    moveto (((size_x () * 2)/5) - 190) ((size_y ())/5 + 250);
    draw_string "Groups";
    (*moveto (((size_x () * 2)/5) - 180) (((size_y ())/5)+90);
      draw_string "Invite List";*)
    moveto ((size_x () * 4)/5 -75) (size_y ()/2 + 150);
    draw_string "New Group";
    moveto ((size_x () * 4)/5 -85) ((size_y ()/2) -50);
    draw_string "View Profile";
    bd_welcome_screen p gd

 and bd_listener_no_groups p gd =
      let s = wait_next_event [Button_down] in
      let coords = s.mouse_x, s.mouse_y in
      (*if button_down () then*)
      match coords with
      |(x,y) -> if x > (size_x () - 70) && x < (size_x () - 20) then
          if (y > (size_y () - 600)) && (y < (size_y () - 580)) then
            welcome_screen p gd
          else if
              x > (size_x () - 30) && x < (size_x () - 10) && y > (size_y () -30)
              && y < (size_y () - 10)
          then failwith "quitting" else bd_listener_no_groups p gd

and groups_screen p gd c ic =
    clear_graph ();
    quit_button ();
    back_button ();
    draw_rect ((size_x () / 2) - 400) ((size_y ())/2 - 100)
              800 400;
    if List.length (Profile.groups p) = 0 then
      (moveto ((size_x () / 2) - 350) ((size_y ())/2 + 195);
        draw_string "You have no groups to display. Go back and create a group.";
               bd_listener_no_groups p gd
              )
            else
            draw_rect ((size_x () / 2) - 400) ((size_y () / 2) - 175)
              100 50;
            draw_rect ((size_x () / 2) + 300) ((size_y () / 2) - 175)
              100 50;
            moveto ((size_x () / 2) - 370) ((size_y ())/2 - 155);
            draw_string "Prev";
            moveto ((size_x () / 2) + 330) ((size_y ())/2 - 155);
            draw_string "Next"; (*fix functionality*)
            moveto ((size_x () / 2) - 50) ((size_y ())/2 + 270);
            (*draw_string ("Groupid: " ^ (Group.groupid (List.nth gd c)));*)
            moveto ((size_x () / 2) - 350) ((size_y ())/2 + 225);
            draw_string ("Purpose: " ^ (Group.purpose (List.nth gd c)));
            moveto ((size_x () / 2) - 350) ((size_y ())/2 + 175);
            draw_string ("Size: " ^ (string_of_int (Group.size (List.nth gd c))));
            moveto ((size_x () / 2) - 350) ((size_y ())/2 + 125);
            draw_string ("Range: " ^ "("^string_of_int(fst(Group.range (List.nth gd c)))^", "^string_of_int(snd(Group.range (List.nth gd c)))^")");
            set_color Graphics.cyan;
            fill_rect ((size_x () / 2)) ((size_y () / 2) - 50)
              375 310;
            set_color Graphics.black;
            draw_rect ((size_x () / 2)) ((size_y () / 2) - 80)
              50 20;
            draw_rect ((size_x () / 2) + 325) ((size_y () / 2) - 80)
              50 20;
            moveto ((size_x () / 2) + 10) ((size_y ())/2 - 75);
            draw_string "Prev";
            moveto ((size_x () / 2) + 340) ((size_y ())/2 - 75);
            draw_string "Next";
            draw_rect ((size_x () / 2) + 425) ((size_y ())/2 + 200)
              100 50;
            moveto ((size_x () / 2) + 430) ((size_y ())/2 +205);
            draw_string "Start Swiping";
            draw_rect ((size_x () / 2) + 425) ((size_y ())/2 + 125)
              100 50;
            moveto ((size_x () / 2) + 430) ((size_y ())/2 +130);
    draw_string ("Invite List")(*^" ("^(string_of_int (List.length (Group.invited_groups (List.nth gd c))))^")")*);
            bd_listener_groups_screen p gd c ic

  and bd_welcome_screen p gd =
            let s = wait_next_event [Button_down] in
            let coords = s.mouse_x, s.mouse_y in
            (*if button_down () then*)
              match coords with
              |(x,y) -> if x > (((size_x () * 2)/5) - 300) && x < (((size_x () * 2)/5)) then
                  (*if (y > (size_y ())/5 + 360) && (y < ((size_y ())/5 + 460)) then
                    swipe_screen () else *)if y > ((size_y ())/5 + 200) && y <
                      ((size_y ())/5 + 300) then groups_screen p gd 0 0
                  (*else if y > (((size_y ())/5)+ 90) && y < (((size_y ())/5)+190) then
                    invite_screen ()*)
                  else if x >  ((size_x () * 4)/5 - 200) && x > ((size_x () * 4)/5 +100)
                  then if
                    y >  (size_y ()/2 + 100) && y < (size_y ()/2 + 200) then
                      new_group_screen p gd else if
                      y > ((size_y ()/2) - 50) && y < ((size_y ()/2) +50) then
                      view_profile_screen p else if
                      x > (size_x () - 30) && x < (size_x () - 10) && y > (size_y () -30)
                      && y < (size_y () - 10)
                    then failwith "quitting" else bd_welcome_screen p gd

and bd_listener_groups_screen p gd c ic =
  let s = wait_next_event [Button_down] in
  let coords = s.mouse_x, s.mouse_y in
  match coords with
  |(x,y) ->
    if x > (size_x () - 70) && x < (size_x () - 20) then
    if (y > (size_y () - 600)) && (y < (size_y () - 580)) then
      welcome_screen p gd
    else if
        x > (size_x () - 30) && x < (size_x () - 10) && y > (size_y () -30)
        && y < (size_y () - 10)
    then failwith "quitting" else
    if y > ((size_y () / 2) - 175) && y < ((size_y () / 2) - 125) then
      (if x > ((size_x () / 2) - 400) && x < ((size_x () / 2) - 300) then
         groups_screen p gd ((c-1) mod (List.length gd)) 0
       else if  x > ((size_x () / 2) + 300) && x < ((size_x () / 2) + 400) then groups_screen p gd ((c+1) mod (List.length gd)) 0
    else if y > ((size_y () / 2) - 80) && y < ((size_y () / 2) - 60) then
      (if x > ((size_x () / 2)) && x < ((size_x () / 2) + 50) then groups_screen p gd c ((ic-1) mod (List.length gd)))
       else if x > ((size_x () / 2)+325) && x < ((size_x () / 2) + 375) then groups_screen p gd c ((ic-1) mod (List.length gd)))
    else if x > ((size_x () / 2) + 425) && x < ((size_x () / 2) + 525) then
      (if y> ((size_y ())/2 + 200) && y < ((size_y ())/2 + 250) then swipe_screen p gd
       else if y > ((size_y ())/2 + 125) && y < ((size_y ())/2 + 175) then invite_screen p gd)
    else bd_listener_groups_screen p gd c ic

      (*prev and next for groups
        prev and next for profiles
      start swiping, invite list*)


let draw_start_canvas user_prof group_data =
  set_color Graphics.blue;
  welcome_screen user_prof group_data

  (*      ANSITerminal.(print_endline [red] "Welcome to OBumbl");
  *)
