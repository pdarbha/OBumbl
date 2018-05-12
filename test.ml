open OUnit2
open Profile
open Groups
open Main
open Yojson.Basic.Util

let usernameA = "nameA"
let firstnameA = "Afirst"
let lastnameA = "Alast"
let schoolA = "schoolA"
let groups = []
let descriptionA = "this is A"
let expforinterests = "BEG"
let roleonteam = "manager"
let looking_for = "BEG manager; ADV programmer"
let githuburl = "www.none.com"
let email = "none@none.com"

let aj = Yojson.Basic.Util.from_file "userA"
let initprofileA = Profile.init_profile aj
let profileA = Profile.create_profile (user_id initprofileA) (*must test*)


  (*let n = print_read "\nEnter your full name: " in
    let s = print_read "Enter your school: " in
    let d = print_read "Enter your description: " in
    let interests = cp_interests () in
    let exp = print_read ("Are you a beginner (BEG), intermediate (INT), or "^
                       "advanced (ADV) computer scientist? ") in
    let r = print_read "What is your typical role on a team? " in
    let lf = cp_looking_for () in
    let github = print_read "What's your github URL? " in
    let e = print_read "What's your email? " in
    let prof = {user_id = id; name = n; photo = ref ""; school = s; group_id_list = [];
              description = d; interest_list = interests;
              experience = (string_to_exp exp); role = r; looking_for = lf;
              github_url = github; email = e} in
    if (update_server prof) && n <> ""
    then ()
    else
    (print_string "Invalid information, please enter your details again.\n";
    create_profile id)*)

let tests =
  [
    (*tests for profile functions*)
    "create profile A" >:: (fun _ -> assert_equal 11111 (j |> init_state |> win_score));
    "max" >:: (fun _ -> assert_equal 11111 (j |> init_state |> win_score));
  ]

let suite =
  "OBumbl test suite"
  >::: tests

let _ = run_test_tt_main suite
