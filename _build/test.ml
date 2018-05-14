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

let edituseridA = 2
let editusernameA = "nameB"
let editnameA = "firstB lastB"
let editphotoA = ref ("this is not a string")
let editschoolA = "schoolB"
let editgroupsA = ""
let editinterestsA = ["NOT" ;"Tennis"; "reading"; "coding"]
let editdescriptionA = "this is B"
let editexpforinterestsA = `INT
let editroleonteamA = "programmer"
let editlooking_forA = [(`INT,"managerB");(`ADV, "Bprogrammer")]
let editgithuburlA = "www.noneB.com"
let editemailA = "none@noneB.com"

let aj = from_file "userA"
let initprofileA = aj |> init_profile
let lf1 = [(`BEG, "manager")]
let slf1 = "BEG manager"
let lf2 = [(`BEG, "manager"); (`INT, "computer scientist")]
let slf2 = "BEG manager; INT computer scientist"
let lf3 = [(`ADV, "partner beg leader")]
let slf3  = "ADV partner; Beg leader" (*should these match???*)

let gempty = Group.empty_group
let groupidx = 123442
let useridlistx = [1;2;3;4;42]
let purposex = "cs3110"
let sizex = 5
let rangeminx = 1
let rangemaxx = 10
let schedule = []
let blacklist = [45;54]
let invited = [7]
let recieved = [90;87]
let schedulex = []

let xj = from_file "groupx"
let initgroupx = xj |> init_group
let gfull1 = from_file "gfull1" |> init_group
let gfull2 = from_file "gfull2" |> init_group
(* let groupx = initgroupx |> user_id |> create_profile   *)
(*can do that later based on purpose list because this comes from the repl*)

let tests =
  [
    (*tests on getters for init_profile function*)
    "user_id getter A" >:: (fun _ -> assert_equal (initprofileA |> user_id)
                               useridA);
    "name getter A" >:: (fun _ -> assert_equal (initprofileA |> name) nameA);
    "photo getter A" >:: (fun _ -> assert_equal (initprofileA |> photo)!photoA);
    "groups getter A" >:: (fun _ -> assert_equal (initprofileA |> groups)
                              groupsA);
    "description getter A" >:: (fun _ -> assert_equal (initprofileA |>
                                                    description) descriptionA);
    "school getter A" >:: (fun _ -> assert_equal (initprofileA |> school)
                              schoolA);
    "interests getter A" >:: (fun _ -> assert_equal (initprofileA |> interests)
                                 interestsA);
    "experience getter A" >:: (fun _ -> assert_equal (initprofileA |>
                                                experience) expforinterestsA);
    "role getter A" >:: (fun _ -> assert_equal (initprofileA|>role)roleonteamA);
    "looking for getter A" >:: (fun _ -> assert_equal (initprofileA |>
                                                    looking_for) looking_forA);
    "github getter A" >:: (fun _ -> assert_equal (initprofileA |> github)
                              githuburlA);
    "email getter A" >:: (fun _ -> assert_equal (initprofileA |> email) emailA);
    (*test string_to_exp*)
    "strexp INT" >:: (fun _ -> assert_equal ("INT" |> string_to_exp) (`INT));
    "strexp ADV" >:: (fun _ -> assert_equal ("ADV" |> string_to_exp) (`ADV));
    "strexp BEG" >:: (fun _ -> assert_equal ("BEG" |> string_to_exp) (`BEG));
    "strexp NO" >:: (fun _ -> assert_equal ("NO" |> string_to_exp) (`BEG));
    (*string_to_looking_for tests*)
    "slooking_for empty" >:: (fun _ -> assert_equal ("" |>string_to_looking_for)
                                 ([]));
    "slooking_for 1" >:: (fun _ -> assert_equal (slf1 |> string_to_looking_for)
                             (lf1));
    "slooking_for 2" >:: (fun _ -> assert_equal (slf2 |> string_to_looking_for)
                             (lf2));
    "slooking_for 3" >:: (fun _ -> assert_equal (slf3 |> string_to_looking_for)
                             (lf3));
    (*looking for to string tests*)
    "looking_forstr empty" >:: (fun _ -> assert_equal([]|>looking_for_to_string)
                                   (""));
    "looking_forstr 1" >:: (fun _ -> assert_equal (lf1 |> looking_for_to_string)
                               (slf1));
    "looking_forstr 2" >:: (fun _ -> assert_equal (lf2 |> looking_for_to_string)
                               (slf2));
    "looking_forstr 3" >:: (fun _ -> assert_equal (lf3 |> looking_for_to_string)
                               (slf3));
    "strexp NO" >:: (fun _ -> assert_equal ([] |> looking_for_to_string) (""));

    (*tests for edit *)
    (*"edit userid" >:: (fun _ -> assert_equal ((edit initprofileA
                 "user_id" edituseridA) |> user_id) (edituseridA));*)
    "edit name" >:: (fun _ -> assert_equal ((edit initprofileA
                                        "name" editnameA) |> name) (editnameA));
    "edit school" >:: (fun _ -> assert_equal ((edit initprofileA "school"
                                        editschoolA) |> school) (editschoolA));
    "edit group id list" >:: (fun _ -> assert_equal ((edit initprofileA
                          "group_id_list" editgroupsA) |> groups)([]));
    "edit description" >:: (fun _ -> assert_equal ((edit initprofileA
                                "description" editdescriptionA) |> description)
                               (editdescriptionA));
    "edit interests" >:: (fun _ -> assert_equal ((edit initprofileA
                  "interest_list" "NOT, Tennis, reading, coding") |> interests)
                             (editinterestsA));
    "edit experience" >:: (fun _ -> assert_equal ((edit initprofileA
                              "experience" "INT") |> experience)
                                      (editexpforinterestsA));
    "edit role" >:: (fun _ -> assert_equal ((edit initprofileA
                            "role" editroleonteamA) |> role) (editroleonteamA));
    "edit looking for" >:: (fun _ -> assert_equal ((edit initprofileA
                                "looking_for" "INT managerB ADV Bprogrammer") |> looking_for)
                               (editlooking_forA));
    "edit github" >:: (fun _ -> assert_equal ((edit initprofileA
                     "github_url" editgithuburlA) |> github) (editgithuburlA));
    "edit email" >:: (fun _ -> assert_equal ((edit initprofileA "email"
                                            editemailA) |> email) (editemailA));
    (*tests for add and remove group_id*)

    (*tests for cp_looking for*)
    (*tests for about profile*)
    (*tests for times from string*)
    (*tests for schedule from string*)
    (*tests for init group *)
    (*tests for insert group and update groups list*)
    (*tests for lookup group*)
    (* tests for get schedule*)
    (*tests for group to string*)
    (*tests for all groups getters and initializing groups*)
    (* "initialize group x" >:: (fun _ -> assert_equal initgroupx (xgroup)); *)
    "get size group x" >:: (fun _ -> assert_equal (initgroupx |> Group.size)
                               (sizex));
    "get range group x" >:: (fun _ -> assert_equal initgroupx |> Group.range
                                        ((rangeminx,rangemaxx)));
    "get purpose group x" >:: (fun _ -> assert_equal initgroupx |> Group.purpose
                                          (purposex));
    "get users group x" >:: (fun _ -> assert_equal initgroupx |> Group.users
                                        (useridlistx));

(* test union*)
    "test union two empty" >:: (fun _ -> assert_equal Group.union gempty gempty
                                   (gempty));
    "test union first empty" >:: (fun _ -> assert_equal Group.union initgroupx
                                     gempty (initgroupx));
    "test union second empty" >:: (fun _ -> assert_equal Group.union gempty
                                      initgroupx (initgroupx));
    "test union both full" >:: (fun _ -> assert_equal Group.union initgroupx
                                   gfull1 (gfull2));
(*test update and return*)
(*test for show group in swipes*)
(*test for get groups with purpose*)
(*tests for create key freq list, for interests sum and interests score*)
(*tests for exp role tuple, looking for getter, score det and score det helper score determination and total score*)
  ]

let suite =
  "OBumbl test suite"
  >::: tests

let _ = run_test_tt_main suite
