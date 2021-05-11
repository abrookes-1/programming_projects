let test_counts = ref (0,0) (* total, failed *)

let list_check (name,f,l) = (
  Printf.printf "running \"%s\" tests...\n" name;
  let failed = List.rev (fst (List.fold_left (fun (acc,count) (name, a,b) ->
    let name = (match name with
    | None -> string_of_int count
    | Some(s) -> Printf.sprintf "\"%s\"" s) in
    let b2 = f a in
    ((if b2=b then (
      Printf.printf "  pass test %s\n" name;
      acc
    ) else (
      Printf.printf "  FAIL test %s\n" name;
      (b2::acc))), (count+1))
    ) ([],1) l)) in
    let (total,failed) = (List.length l, List.length failed) in
    let (old_total,old_failed) = !test_counts in
    test_counts := (old_total+total, old_failed+failed);
    (total,failed)
  )

let run_tests (name,f,l) =
  list_check (name,f,l)

let get_test_counts () =
  !test_counts

let reset_test_counts () =
  test_counts := (0,0)
