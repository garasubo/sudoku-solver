let calc_pos x y = x * 9 + y

let check_num x y num problem = problem.t.((calc_pos x y)*9 + num - 1)

let get_space problem= 
  let rec get_space_inner arr pos = 
    if pos >= 81 then []
    else 
      let ls = get_space_inner arr (pos+1) in
      if arr.(pos)=0 then pos::ls
      else ls
  in
    get_space_inner problem.num 0

let get_possible_num problem x y = 
  let rec get_possible_num_inner arr pos num = 
    if num >= 10 then []
    else 
      let ls = get_possible_num_inner arr pos (num+1) in
        if arr.(pos*9+num) then num::ls
        else ls
  in
    get_possible_num_inner problem.check (calc_pos x y) 1

