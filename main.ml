open Str

type problem_t = {
    num : int array;
    check : bool array;
}

exception No_answer
exception Run_error



let calc_pos x y = x * 9 + y

let print_problem problem = 
  for i = 0 to 8 do
    for j = 0 to 8 do 
      print_int problem.num.(calc_pos j i)
    done;
    print_newline ()
  done

let check_num x y num problem = problem.check.((calc_pos x y)*9 + num - 1)

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
  let col = pos mod 9 in
  let (bx,by) = ((row/3)*3,(col/3)*3) in
    problem.num.(pos) <- x;
    for i = 0 to 8 do problem.check.((row*9+i)*9+x-1) <- false done;
    for i = 0 to 8 do problem.check.((i*9+col)*9+x-1) <- false done;
    for i = 0 to 8 do problem.check.((calc_pos (bx+i/3) (by+i mod 3))*9+x-1) <- false done;
    for i = 0 to 8 do problem.check.(pos*9+i) <- false done

let cell_check problem pos = 
  match get_possible_num problem pos with
    | [] -> raise No_answer
    | x::[] -> set_cell problem pos x ; true
    | _ -> false


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
        if List.fold_left (||) false (List.rev_map (cell_check problem) ls) then
          solve problem
        else
          dfs (List.hd ls)
      with
          No_answer -> 0
  in
    (* 候補をチェックしていく *)
    match get_space problem with
      |[] -> print_endline "find ans"; print_problem problem ;1
      |ls -> solve_inner ls



let make_problem () = 
  let num_arr = Array.make 81 0 in
  let check_arr = Array.make 729 true in
  let res = { num = num_arr; check = check_arr} in
    if (Array.length Sys.argv) >=2 then
      let in_chanel = open_in Sys.argv.(1) in
        for i = 0 to 8 do
          let line = input_line in_chanel in
          let nums = Array.of_list (List.map int_of_string (Str.split (Str.regexp" ") line)) in
            for j = 0 to 8 do
              if nums.(j) = 0 then () else set_cell res (calc_pos j i) nums.(j)
            done
        done;
        res
        else raise Run_error


let _ = print_endline "Java + You, Download now.";
        print_int (solve (make_problem ())); print_newline ();
