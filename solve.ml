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

let get_possible_num problem pos = 
  let rec get_possible_num_inner arr pos num = 
    if num >= 10 then []
    else 
      let ls = get_possible_num_inner arr pos (num+1) in
        if arr.(pos*9+num-1) then num::ls
        else ls
  in
    get_possible_num_inner problem.check pos 1

let set_cell problem pos x = 
  let row = pos / 9 in
  let col = pos % 9 in
  let (bx,by) = ((row/3)*3,(col/3)*3) in
    problem.num.(pos) <- x;
    for i = 0 to 8 do problem.check.((row*9+i)*9+x-1) <- false done;
    for i = 0 to 8 do problem.check.((i*9+col)*9+x-1) <- false done;
    for i = 0 to 8 do problem.check.((calc_pos (bx+i/3) (by+i%3))*9+x-1) <- false done;
    for i = 0 to 8 do problem.check.(pos*9+i) <- false done;

let cell_check problem pos = 
  match get_possible_num pos with
    | [] -> raise No_answer
    | x::[] -> set_cell problem pos x ; true
    | _ -> false

exception No_answer 

let rec solve problem = 
  let rec solve_inner ls = 
    let dfs pos =
      let ls = get_possible_num problem pos in
      let rec dfs_inner num =
        let copy_problem = {num = Array.copy problem.num;check = Array.copy problem.check} in
          set_cell copy_problem pos num;solve copy_problem
      in
        List.fold_left (+) 0 (List.rev_map dfs_inner ls)
    in
      try 
        if List.fold_left (||) false (List.rev_map cell_check ls) then
          solve problem
        else
          dfs (List.hd ls)
      with
          No_answer -> 0
  in
    (* 候補をチェックしていく *)
    match get_space problem with
      |[] -> 1
      |ls -> solve_inner ls


