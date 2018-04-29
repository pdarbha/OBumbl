
(* will store all information about a group, including name of the group, list of user ids, tags, and size of group *)
type group

(* will create a group of size one representing the group containing only
   one profile using the profile's id as well as the tag this group corresponds to*)
val init_group : int -> Tag.tag -> group

(* will return a sorted list of groups based on a matching algorithm for a single group *)
val match_order : group -> group list -> group list

(* will return the size of a group *)
val size : group -> int

(* will return a tuple (min,max) describing the minimum and maximum acceptable group size *)
val range : group -> int*int

(* will join two groups to form a larger group *)
val union : group -> group -> group
