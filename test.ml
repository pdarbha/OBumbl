open OUnit2
open Profile
open Group
open Main
open Yojson.Basic
open Yojson.Basic.Util

let useridA = 1
let usernameA = "nameA"
let nameA = "firstA lastA"
let photoA = ref ("this is a string")
let schoolA = "schoolA"
let groupsA = []
let interestsA = ["Tennis";"reading";"coding"]
let descriptionA = "this is A"
let expforinterestsA = `BEG
let roleonteamA = "manager"
let looking_forA = [(`BEG,"manager"); (`ADV, "programmer")]
let githuburlA = "www.none.com"
let emailA = "none@none.com"

let aj = from_file "userA"
let initprofileA = aj |> init_profile
let lf1 = [(`BEG, "manager")]
let slf1 = "BEG manager"
let lf2 = [(`BEG, "manager"); (`INT, "computer scientist")]
let slf2 = "BEG manager; INT computer scientist"
let lf3 = [(`ADV, "partner beg leader")]
let slf3  = "ADV partner; Beg leader" (*should these match???*)

let tests =
  [
    (*tests on getters for init_profile function*)
    "user_id getter A" >:: (fun _ -> assert_equal (initprofileA |> user_id) useridA);
    "name getter A" >:: (fun _ -> assert_equal (initprofileA |> name) nameA);
    "photo getter A" >:: (fun _ -> assert_equal (initprofileA |> photo) !photoA);
    "groups getter A" >:: (fun _ -> assert_equal (initprofileA |> groups) groupsA);
    "description getter A" >:: (fun _ -> assert_equal (initprofileA |> description) descriptionA);
    "school getter A" >:: (fun _ -> assert_equal (initprofileA |> school) schoolA);
    "interests getter A" >:: (fun _ -> assert_equal (initprofileA |> interests) interestsA);
    "experience getter A" >:: (fun _ -> assert_equal (initprofileA |> experience) expforinterestsA);
    "role getter A" >:: (fun _ -> assert_equal (initprofileA |> role) roleonteamA);
    "looking for getter A" >:: (fun _ -> assert_equal (initprofileA |> looking_for) looking_forA);
    "github getter A" >:: (fun _ -> assert_equal (initprofileA |> github) githuburlA);
    "email getter A" >:: (fun _ -> assert_equal (initprofileA |> email) emailA);
    (*test string_to_exp*)
    "strexp INT" >:: (fun _ -> assert_equal ("INT" |> string_to_exp) (`INT));
    "strexp ADV" >:: (fun _ -> assert_equal ("ADV" |> string_to_exp) (`ADV));
    "strexp BEG" >:: (fun _ -> assert_equal ("BEG" |> string_to_exp) (`BEG));
    "strexp NO" >:: (fun _ -> assert_equal ("NO" |> string_to_exp) (`BEG));
    (*string_to_looking_for tests*)
    "slooking_for empty" >:: (fun _ -> assert_equal ("" |> string_to_looking_for) ([]));
    "slooking_for 1" >:: (fun _ -> assert_equal (slf1 |> string_to_looking_for) (lf1));
    "slooking_for 2" >:: (fun _ -> assert_equal (slf2 |> string_to_looking_for) (lf2));
    "slooking_for 3" >:: (fun _ -> assert_equal (slf3 |> string_to_looking_for) (lf3));
    (*looking for to string tests*)
    "looking_forstr empty" >:: (fun _ -> assert_equal ([] |> looking_for_to_string) (""));
    "looking_forstr 1" >:: (fun _ -> assert_equal (lf1 |> looking_for_to_string) (slf1));
    "looking_forstr 2" >:: (fun _ -> assert_equal (lf2 |> looking_for_to_string) (slf2));
    "looking_forstr 3" >:: (fun _ -> assert_equal (lf3 |> looking_for_to_string) (slf3));
    "strexp NO" >:: (fun _ -> assert_equal ([] |> looking_for_to_string) (""));

  ]

let suite =
  "OBumbl test suite"
  >::: tests

let _ = run_test_tt_main suite
