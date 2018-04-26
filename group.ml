open Tag

type group = {group_id : int; purpose : string; user_id_list: int list; size : int; range_min : int; range_max : int}

let init p t= {group_id = 0; user_id_list = [p.id]; tag = t; size = 1; range = t.range} (* todo: take group id from server *)

(* module MakeNewGroup  = functor (A:Group) -> functor (B:Group) ->
struct
  type group *)


let union a b = failwith "unimplemented"

let range a b = (max (fst a.range) (fst b.range), min (snd a.range) (snd b.range))

let new_group a b = {
  group_name = a.group_name (*TODO: change placeholder name*);
  user_id_list = a.user_id_list @ b.user_id_list;
  tag_list = union a.tag_list b.tag_list;
  size = a.size + b.size;

}

let match_order = failwith "unimplemented"

let size g= g.size
