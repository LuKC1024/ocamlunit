open Printf

let string_of_exn = function
  | Match_failure (s, i1, i2) -> sprintf "Match_failure (%s, %d, %d)" s i1 i2
  | Assert_failure (s, i1, i2) -> sprintf "Assert_failure (%s, %d, %d)" s i1 i2
  | Failure s -> sprintf "Failure %s" s
  | Invalid_argument s -> sprintf "Invalid_argument %s" s
  | Not_found -> sprintf "Not_found"
  (* ignore Out_of_memory *)
  (* ignore Stack_overflow *)
  | Sys_error s -> sprintf "Sys_error %s" s
  | End_of_file -> sprintf "End_of_file"
  | Division_by_zero -> sprintf "Division_by_zero"
  | Sys_blocked_io -> sprintf "Sys_blocked_io"
  (* ignore Undefined_recursive_module *)
  | _ -> "?"
;;

type 'a result =
  | Val of 'a
  | Err of exn

type test =
  | Name of (string * test)
  | Base of (unit -> string option)
  | Collection of test list

let raw_assert program check compose_msg =
  Base
    (fun () ->
      let r =
        match program () with
        | v -> Val v
        | exception exn -> Err exn
      in
      match check r with
      | true -> None
      | false -> Some (compose_msg r))
;;

let assert_pred satisfy p2 string_of_value =
  raw_assert
    p2
    (function
      | Val v -> satisfy v
      | Err _ -> false)
    (function
      | Val v -> sprintf "%s doesn't not satisfy the predicate" (string_of_value v)
      | Err e ->
        sprintf
          "Expecting a value, caught an exception %s"
          (string_of_exn e))
;;

let assert_equal v1 promise_v2 string_of_value =
  raw_assert
    promise_v2
    (fun r2 -> r2 = Val v1)
    (function
      | Val v2 ->
        sprintf "Expecting %s, caught %s" (string_of_value v1) (string_of_value v2)
      | Err e ->
        sprintf
          "Expecting %s, caught an exception %s"
          (string_of_value v1)
          (string_of_exn e))
;;

let assert_raise e1 promise_v2 to_string =
  raw_assert
    promise_v2
    (fun r2 -> r2 = Err e1)
    (function
      | Val v2 -> sprintf "Expecting an exception, received %s" (to_string v2)
      | Err e -> sprintf "Caught an unexpected exception %s" (string_of_exn e))
;;

type path_node =
  | Str of string
  | Num of int

let string_of_path_node = function
  | Str s -> s
  | Num n -> string_of_int n
;;

let ( >: ) name test = Name (name, test)
let string_of_path path = String.concat "::" (List.map string_of_path_node path)
let string_of_fail (path, msg) = string_of_path path ^ ": " ^ msg

let run_test t =
  let succ_counter = ref 0 in
  let fails_accumulator = ref [] in
  let rec run_test_helper path = function
    | Name (name, test) -> run_test_helper (Str name :: path) test
    | Collection ts -> run_tests_helper 0 path ts
    | Base thunk ->
      (match thunk () with
      | None -> succ_counter := !succ_counter + 1
      | Some msg -> fails_accumulator := (List.rev path, msg) :: !fails_accumulator)
  and run_tests_helper i path = function
    | [] -> ()
    | t :: ts ->
      run_test_helper (Num i :: path) t;
      run_tests_helper (i + 1) path ts
  in
  run_test_helper [] t;
  let fails = List.rev !fails_accumulator in
  let succ_count = !succ_counter in
  succ_count, fails
;;

let run t =
  let succ_count, fails = run_test t in
  let fail_count = List.length fails in
  let total = succ_count + fail_count in
  List.iter (fun f -> print_endline (string_of_fail f)) fails;
  printf "%d of %d tests failed\n" fail_count total
;;
