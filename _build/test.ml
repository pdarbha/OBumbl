open OUnit2
open Profile
open Groups
open Main
open Yojson.Basic.Util

let useridA = 1
let usernameA = "nameA"
let nameA = "firstA lastA"
let photoA = ref ("this is a string")
let schoolA = "schoolA"
let groupsA = []
let interestsA = "Tennis, reading, coding"
let descriptionA = "this is A"
let expforinterestsA = "BEG"
let roleonteamA = "manager"
let looking_forA = [(`BEG,"manager"); (`ADV, "programmer")]
let githuburlA = "www.none.com"
let emailA = "none@none.com"

let aj = Yojson.Basic.Util.from_file "userA"
let initprofileA = aj |> Profile.init_profile
let profileA = initprofileA |> Profile.user_id |> Profile.create_profile (*should this be the same???*)
let explicitprofileA = {user_id = 1; name = "firstA lastA";
                        photo= ref ("this is a string"); school="school A";
                        group_id_list= []; description= "this is A";
                        interest_list= "Tennis, reading, coding";
                        experience = `BEG ; role= "manager";
                        looking_for= [(`BEG,"manager"); (`ADV, "programmer")];
                        github_url = "www.none.com"; email= "none@none.com"}
let lf1 = [(`BEG, "manager")]
let slf1 = "BEG manager"
let lf2 = [(`BEG, "manager"); (`INT, "computer scientist")]
let slf2 = "BEG manager; INT computer scientist"
let lf3 = [(`ADV, "partner beg leader")]
let slf3  = "ADV partner; Beg leader" (*should these match???*)

let tests =
  [
    (*tests for profile functions*)
    "initialize profile A" >:: (fun _ -> assert_equal initprofileA (profileA));
    "initialize explicit" >:: (fun _ -> assert_equal initprofileA (explicitprofileA));
    (*tests on getters for init_profile function*)
    "user_id getter A" >:: (fun _ -> assert_equal initprofileA |> Profile.user_id (useridA));
    "name getter A" >:: (fun _ -> assert_equal initprofileA |> Profile.name (nameA));
    "photo getter A" >:: (fun _ -> assert_equal initprofileA |> Profile.photo (photoA));
    "groups getter A" >:: (fun _ -> assert_equal initprofileA |> Profile.groups (groupsA));
    "description getter A" >:: (fun _ -> assert_equal initprofileA |> Profile.description (descriptionA));
    "school getter A" >:: (fun _ -> assert_equal initprofileA |> Profile.school (schoolA));
    "interests getter A" >:: (fun _ -> assert_equal initprofileA |> Profile.interests (interestsA));
    "experience getter A" >:: (fun _ -> assert_equal initprofileA |> Profile.experience (expforinterestsA));
    "role getter A" >:: (fun _ -> assert_equal initprofileA |> Profile.role (roleonteamA));
    "looking for getter A" >:: (fun _ -> assert_equal initprofileA |> Profile.looking_for (looking_forA));
    "github getter A" >:: (fun _ -> assert_equal initprofileA |> Profile.github (githuburlA));
    "email getter A" >:: (fun _ -> assert_equal initprofileA |> Profile.email (emailA));
    (*test string_to_exp*)
    "strexp INT" >:: (fun _ -> assert_equal "INT" |> Profile.string_to_exp (`INT));
    "strexp ADV" >:: (fun _ -> assert_equal "ADV" |> Profile.string_to_exp (`ADV));
    "strexp BEG" >:: (fun _ -> assert_equal "BEG" |> Profile.string_to_exp (`BEG));
    "strexp NO" >:: (fun _ -> assert_equal "NO" |> Profile.string_to_exp (`BEG));
    (*string_to_looking_for tests*)
    "slooking_for empty" >:: (fun _ -> assert_equal [] |> Profile.string_to_looking_for ([]));
    "slooking_for 1" >:: (fun _ -> assert_equal slf1 |> Profile.string_to_looking_for (lf1));
    "slooking_for 2" >:: (fun _ -> assert_equal slf2 |> Profile.string_to_looking_for (lf2));
    "slooking_for 3" >:: (fun _ -> assert_equal slf3 |> Profile.string_to_looking_for (lf3));
    (*looking for to string tests*)
    "looking_forstr empty" >:: (fun _ -> assert_equal [] |> Profile.looking_for_to_string ([]));
    "looking_forstr 1" >:: (fun _ -> assert_equal slf1 |> Profile.looking_for_to_string (lf1));
    "looking_forstr 2" >:: (fun _ -> assert_equal slf2 |> Profile.looking_for_to_string (lf2));
    "looking_forstr 3" >:: (fun _ -> assert_equal slf3 |> Profile.looking_for_to_string (lf3));
    "strexp NO" >:: (fun _ -> assert_equal [] |> Profile.looking_for_to_string ([]));

  ]

let suite =
  "OBumbl test suite"
  >::: tests

let _ = run_test_tt_main suite
