(* will store all information about a group, including name of the group, list
   of user ids, tags, and size of group *)
type group

(* will store all information about a group's schedule, including days and times
   available*)
type schedule

(* will create a group of size one representing the group containing only
   one profile using the profile's id as well as the tag this group corresponds
   to*)
val init_group : Yojson.Basic.json -> group
(* will create a group of size 0 representing a group with no members*)
val empty_group : group

(* will return the size of a group *)
val size : group -> int

(* will return a tuple (min,max) describing the minimum and maximum acceptable
   group size *)
val range : group -> int*int

(* will join two groups to form a larger group *)
val union : group -> group -> int

(*will return the list of users of a group*)
val users: group -> int list

(*will return the list of users of a group*)
val schedule: group -> schedule

(*will return the list of users of a group*)
val groupid: group->int

(*will return the list of users of a group*)
val invites_received: group -> group list

(*will return an updated group with an updated blacklist*)
val blacklist: group ->int -> group

(*will complete an invite for a user when they are in the gui*)
val gui_invites_accept: group -> group -> unit

(*will complete a reject for a user when they are in the gui*)
val gui_invites_reject: group -> int -> group

(*will update the server when a user completes an action that must be logged*)
val gui_back: group -> unit

(*helper function to update a group once they have recieved an invite*)
val invite_helper: group -> group -> group

(*will sort groups based on their total scores to be shown in correct order for
swiping*)
val get_sorted_groups: group -> group list

(* will print purpose, min size, and max size for each group in a group list *)
val show_groups: group list -> unit

(*[delete_group g] deletes group g from a user's list of groups *)
val delete_group: group -> unit

(*[about_group g] takes a group and prints group's purpose, group's id and # of
  members*)
val about_group: group -> unit

(*[invites g] prints out groups that have invited group [g]. User can accept or
  delete groups, or go back. Accepting a group forms a new group and deletes
  other invites*)
val invites: group -> unit

(*[swipe g] displays a list of potential matching groups for group g and
  blacklists groups that the user declines and sends invites to accepted
  groups*)
val swipe: group -> unit

(*[leave p g] creates a new group without user who decided to leave. Leaving a
  group deletes the tag associated with the project for the leaving user. User
  must make a new tag to create group for that purpose*)
val leave: Profile.profile -> group -> unit

(*will take a group id and will query the server to return the string of the
  group associated with that id. will create a json of that string if the group
  exists. if it does not, it will return the empty group. *)
val lookup_group: int -> group

val schedule_to_string: schedule -> string

val purpose : group -> string

val create_group : Profile.profile -> string list -> unit

val find_group_by_code : string -> group list -> group option
