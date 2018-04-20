(*open Tag*)

module Group

type group = {group_name : string ; user_id_list: string list;
              tag_list : tag list; size : int; range : (int*int)}

let init profile= {group_name = profile.id^"'s group"; user_id_list = [id];
                   profile.tag_list = tag; size = 1;
                   range = (profile.min_range, profile.max_range)} (*make sure this exists in profile!!!*)

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

  let size = new_group.size
