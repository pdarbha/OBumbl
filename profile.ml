open Group

(* will store all information about each person including id, name, photo, description, etc.*)
type profile = {id:int; name:string; photo:string; school;string; groups: group list;
                desc: string; interests: string list; exp: [ `BEG | `INT | `ADV ];
                role: string; look_for: ([ `BEG | `INT | `ADV ]*string);
                github: string}

(* will take in Json file and parse it and store that information in an object of type profile *)
val init : Yojson.Basic.json -> profile

(* will return the unique user id associated with a profile *)
let id p = p.id

(* will return the name of a user *)
let name p = p.name

(* will return the photo of a user, encoded as a Base64 string *)
let photo p = p.photo

(* will return the school of a user *)
let school p = p.school

(* will return a list of a user’s tags *)
let groups p = p.groups

(* will return a user’s description *)
let description p = p.desc

(* will return a list of keyword interests for a user *)
let interests p = p.interests

(* will return the experience of a user as a variant *)
let experience p = p.exp

(* will return the role of a user *)
let role p = p.role

(* will return a tuple of experience as variant and role as string that user is looking for *)
let looking_for p = p.look_for

 (* will return a string to a github.com profile URL *)
let github p = p.github



(* will accept a profile and two strings (field, value) to be updated and returned in new profile *)
let edit p_old field new_val =
  failwith "todo later"

(* will take in a profile and uploads it to the server and returns true if it is uploaded
 * successfully. Has the side effect of changing information in the server. *)
val update_server : profile -> boolean

(*will take in a profile and the name of a tag and return the tag that has that name in the
 * list of tags stored in the profile *)
val find_tags_by_name : profile -> string -> Tag.tag
