open Graphics
(*open Camlimages
open Images
open Elements
  open Jpeg*)

let buttonclickinfo () =
  let s = wait_next_event [Button_down] in
  print_string (string_of_int s.mouse_x);
  s.mouse_x, s.mouse_y

let quit_button () =
  draw_rect (size_x () - 30) (size_y () -30) 20 20;
  moveto (size_x () -22) (size_y () -25);
  draw_string "X"

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

let swipe_screen () = fill_circle 50 50 50
let invite_screen () = failwith "invite"
let groups_screen () = failwith "groups"
let new_group_screen () = failwith "new"
let edit_profile_screen () = failwith "edit"
let quit_screen () = failwith "quitting"
(* draw_string "Groups";
   draw_string "Start Swiping";
   draw_string "New Group";
   draw_string "Invite List";
   draw_string "Edit Profile"; (*brings you back to repl*) *)


let rec bd_welcome_screen () =
let s = wait_next_event [Button_down] in
let coords = s.mouse_x, s.mouse_y in
(*if button_down () then*)
  match coords with
  |(x,y) -> if x > (((size_x () * 2)/5) - 300) && x < (((size_x () * 2)/5)) then
      if (y > (size_y ())/5 + 360) && (y < ((size_y ())/5 + 460)) then swipe_screen ()
      else if y > ((size_y ())/5 + 200) && y < ((size_y ())/5 + 300) then invite_screen ()
      else if y > (((size_y ())/5)+ 90) && y < (((size_y ())/5)+190) then groups_screen ()
      else if x >  ((size_x () * 4)/5 - 200) && x > ((size_x () * 4)/5 +100) then if
        y >  (size_y ()/2 + 100) && y < (size_y ()/2 + 200) then new_group_screen () else if
          y > ((size_y ()/2) - 50) && y < ((size_y ()/2) +50) then edit_profile_screen () else if
          x > (size_x () - 30) && x < (size_x () - 10) && y > (size_y () -30) && y < (size_y () - 10)
        then quit_screen () else bd_welcome_screen () else bd_welcome_screen ()

  let welcome_screen () =
    clear_graph ();
    quit_button ();
    set_color Graphics.magenta;
    moveto (size_x ()/2 - 5) (size_y () - 30);
    draw_string "OBumbl";
    draw_rect (((size_x () * 2)/5) - 300) ((size_y ())/5 + 360)
      300 100;
    draw_rect ((size_x () * 2)/5 - 300) ((size_y ())/5 + 200)
      300 100;
    draw_rect ((size_x () * 2)/5 - 300) (((size_y ())/5)+40)
      300 100;
    draw_rect ((size_x () * 4)/5 -200) (size_y ()/2 + 100)
      300 100;
    draw_rect ((size_x () * 4)/5 -200) (((size_y ()/2) -100))
      300 100;
    set_color Graphics.blue;
    moveto (((size_x () * 2)/5) - 200) ((size_y ())/5 + 410);
  draw_string "Start Swiping";
    moveto (((size_x () * 2)/5) - 190) ((size_y ())/5 + 250);
draw_string "Invite List";
    moveto (((size_x () * 2)/5) - 180) (((size_y ())/5)+90);
 draw_string "Groups";
    moveto ((size_x () * 4)/5 -75) (size_y ()/2 + 150);
draw_string "New Group";
    moveto ((size_x () * 4)/5 -85) ((size_y ()/2) -50);
    draw_string "Edit Profile";
    bd_welcome_screen ()


let draw_start_canvas () =
  set_color Graphics.blue;
  welcome_screen ()

  (*      ANSITerminal.(print_endline [red] "Welcome to OBumbl");
  *)
