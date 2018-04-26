open Group

(* Limitations -- ** sanitize/escape all inputs before pushing to server **
* Name - 64 char (full name) -- alphanumeric + spaces
* set a reasonable image size limit (2mb?) and try to compress - upload as base 64 encoding
* school - 64 char (alphanumeric + spaces)
* groups - cannot join more than 100 groups -- g1;g2;g3;... for integer ids
* description - 800 characters (alphanumeric + spaces + *basic* punctuation)
* interests - 15 char limit per interest, 40 interests max (alphanumeric + spaces) -- i0;i1;i2;...;i39
* experience - "BEG" or "INT" or "ADV"
* role - 32 char (alphanumeric + spaces)
* looking_for - "BEG" or "INT" or "ADV" for exp, 32 for role (alphanumeric + spaces) -- exp;role
* Github URL - 60 char (URL encoded then decoded -- example: https://www.urlencoder.org)
*)

(* will store all information about each person including id, name, photo, description, etc.*)
type profile = {user_id:int; name:string; photo:string; school:string; group_list: group list;
                description: string; interest_list: string list; experience : [ `BEG | `INT | `ADV ];
                role: string; looking_for: ([ `BEG | `INT | `ADV ]*string);
                github_url : string}

(* will take in Json file and parse it and store that information in an object of type profile *)
val init : Yojson.Basic.json -> profile

(* will return the unique user id associated with a profile *)
let user_id p = p.user_id

(* will return the name of a user *)
let name p = p.name

(* will return the photo of a user, encoded as a Base64 string *)
let photo p = p.photo

(* will return the school of a user *)
let school p = p.school

(* will return a list of a user’s tags *)
let groups p = p.group_list

(* will return a user’s description *)
let description p = p.description

(* will return a list of keyword interests for a user *)
let interests p = p.interest_list

(* will return the experience of a user as a variant *)
let experience p = p.experience

(* will return the role of a user *)
let role p = p.role

(* will return a tuple of experience as variant and role as string that user is looking for *)
let looking_for p = p.looking_for

 (* will return a string to a github.com profile URL *)
let github p = p.github_url



(* will accept a profile and two strings (field, value) to be updated and returned in new profile *)
let edit p_old field new_val =
  failwith "todo later"

(* will take in a profile and uploads it to the server and returns true if it is uploaded
 * successfully. Has the side effect of changing information in the server. *)
val update_server : profile -> boolean

(*will take in a profile and the name of a tag and return the tag that has that name in the
 * list of tags stored in the profile *)
val find_tags_by_name : profile -> string -> Tag.tag
