(*open Group*)

type class = 
  |


let purpose =

type tag = {purpose : string; range : (int*int)}

let get_purpose (t:tag) : string = t.purpose

let get_range t = t.range

let create p mi ma = {purpose = p; range = (mi,ma)}

let intersect = failwith
