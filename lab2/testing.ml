let test_counts = ref (0,0) (* total, failed *)

let list_check (name,f,eq,pp,l) = (
  Printf.printf "running \"%s\" tests...\n" name;
  let failed = List.rev (fst (List.fold_left (fun (acc,count) (name, a,b) ->
    let name = (match name with
    | None -> string_of_int count
    | Some(s) -> Printf.sprintf "\"%s\"" s) in
    try
      let b2 = f a in
      ((if (eq b2 b) then (
        Printf.printf "  pass test %s\n" name;
        acc
      ) else (
        Printf.printf "  FAIL test %s\n" name;
        (match pp with
        | Some(str_input,str_output) -> Printf.printf "    input: %s\n    output: %s\n    expected: %s\n" (str_input a) (str_output b2) (str_output b)
        | None -> ());
        (b2::acc))), (count+1))
    with ex ->
      Printf.printf "  FAIL test %s\n    %s\n" name (Printexc.to_string ex);
      ((b(*<--TODO*)::acc), count+1) 
    ) ([],1) l)) in
    let (total,failed) = (List.length l, List.length failed) in
    let (old_total,old_failed) = !test_counts in
    test_counts := (old_total+total, old_failed+failed);
    (total,failed)
  )

let run_tests (name,f,eq,pp,l) =
  list_check (name,f,eq,pp,l)

let get_test_counts (curr_total,curr_failed) =
  let (total,failed) = !test_counts in
  (curr_total+total, curr_failed+failed)

let reset_test_counts () =
  test_counts := (0,0)
