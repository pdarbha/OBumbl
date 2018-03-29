module type Tag = sig

	(* will store information about the purpose of needing a group that each profile will have. *)
  type tag
  
  (* given a list of tags it will produce a tag that has information common to all of the tags. *)
  val intersect : tag list -> tag
  
  (* given a tag it will return the purpose of the project associated with the tag, which is 
   * stored as a string. *)
	val purpose : tag -> string
  
  (* given a tag it will return a pair of integers referring to the minimum and maximum 
   * number of group members preferred, respectively. *)
	val range : tag -> int*int
  
  (* will create and return a tag given the purpose and range for acceptable number of group members *)
  val create : string -> int -> int -> tag

end