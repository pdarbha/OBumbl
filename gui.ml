open Graphics
(*open Camlimages
open Images
open Elements
  open Jpeg*)

let draw_canvas () =
  Graphics.open_graph "1000x750";
  Graphics.set_window_title "OBumbl";
  set_color Graphics.blue;
  draw_rect 0 0 500 500;



  (*main for gui

    let main () =
      Graphics.open_graph "1000x750";
      Graphics.set_window_title "OBumbl";



  *)
  (*      ANSITerminal.(print_endline [red] "Welcome to OBumbl");
  *)
