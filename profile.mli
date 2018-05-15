(* will store all information about each person including id, name, photo, description, etc.*)
type profile

(* will take in Json file and parse it and store that information in an object of type profile *)
val init_profile : Yojson.Basic.json -> profile

(* will return the unique user id associated with a profile *)
val user_id : profile -> int

(* will return the name of a user *)
val name : profile -> string

(* will return the photo of a user, encoded as a Base64 string *)
val photo : profile -> string

(* will return the school of a user *)
val school : profile -> string

(* will return a list of a user’s tags *)
val groups : profile -> int list

(* will return a user’s description *)
val description : profile -> string

(* will return a list of keyword interests for a user *)
val interests : profile -> string list

(* will return the experience of a user as a variant *)
val experience : profile -> [ `BEG | `INT | `ADV ]

(* will return the role of a user *)
val role : profile -> string

(* will return a tuple of experience as variant and role as string that user is looking for *)
val looking_for : profile -> ([ `BEG | `INT | `ADV ]*string) list

 (* will return a string to a github.com profile URL *)
val github : profile -> string

val email : profile -> string

(* will accept a profile and two strings (field, value) to be updated and returned in new profile *)
val edit : profile -> string -> string -> profile

(* Query server and pull profile json from server, convert to profile type *)
val lookup_profile : int -> profile

(* will take in a profile and uploads it to the server and returns true if it is uploaded
 * successfully. Has the side effect of changing information in the server. *)
val update_server : profile -> bool

(* will create a profile for a user with an inputted user id*)
val create_profile : int -> unit

(*will add a group to a user's groups list, and will update server accordingly*)
val add_group : profile -> int -> profile

(*will remove a group from a user's groups list, and will update server
  accordingly*)
val remove_group : profile -> int -> profile

(*will take in a looking_for list and convert it to string form*)
val looking_for_to_string : ([ `BEG | `INT | `ADV ]*string) list -> string

(*will return all information about a profile to the user*)
val about_profile : profile -> unit

(* will connect inputted interests together in list form if entries are valid*)
val cp_interests : unit -> string list

(* will connect looking for entries together in list form if entries are valid*)
val cp_looking_for : unit -> ([ `BEG | `INT | `ADV ]*string) list

(*will return an experience variant corresponding to a correct string input*)
val string_to_exp : string -> [ `BEG | `INT | `ADV ]

(*will return a looking for list  corresponding to a correct string input*)
val string_to_looking_for : string -> ([ `BEG | `INT | `ADV ]*string) list
