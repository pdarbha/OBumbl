open Group

type tag = {purpose : string ; range : (int*int)}

let get_purpose (t:tag) : string = t.purpose

let get_range t = t.range

let create p mi ma = {purpose = p; range = (mi,ma)}

let rec intersect t1 = failwith "unimplemented"

let select_class_list (input : string) (class_list : string list) :string list=
  if List.mem input class_list then class_list else input :: class_list

(*must have some way for the user to select their class from an available list of classes*)
