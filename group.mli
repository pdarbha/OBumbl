(* will store all information about a group, including name of the group, list of user ids, tags, and size of group *)
type group

(* will create a group of size one representing the group containing only
   one profile using the profile's id as well as the tag this group corresponds to*)
val init_group : Yojson.Basic.json -> group

(* will return the size of a group *)
val size : group -> int

(* will return a tuple (min,max) describing the minimum and maximum acceptable group size *)
val range : group -> int*int

(* will join two groups to form a larger group *)
val union : group -> group -> int

(* will print purpose, min size, and max size for each group in a group list *)
val show_groups: group list -> unit

(*[delete_group g] deletes group g from a user's list of groups *)
val delete_group: group -> unit

(*[about_group g] takes a group and prints group's purpose, group's id and # of members*)
val about_group: group -> unit

(*[invites g] prints out groups that have invited group [g]. User can accept or delete groups, or go back. Accepting a group forms a new group and deletes other invites*)
val invites: group -> unit

(*[swipe g] displays a list of potential matching groups for group g and blacklists groups that the user declines and sends invites to accepted groups*)
val swipe: group -> unit

(*[leave p g] creates a new group without user who decided to leave. Leaving a group deletes the tag associated with the project for the leaving user. User must make a new tag to create group for that purpose*)
val leave: Profile.profile -> group -> unit

val lookup_group: int -> group

val purpose : group -> string

val create_group : Profile.profile -> string list -> unit

val find_group_by_code : string -> group list -> group option
