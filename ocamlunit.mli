type test =
  | Name of (string * test)
  | Base of (unit -> string option)
  | Collection of test list

val assert_pred : ('a -> bool) -> (unit -> 'a) -> ('a -> string) -> test
val assert_equal : 'a -> (unit -> 'a) -> ('a -> string) -> test
val assert_raise : exn -> (unit -> 'a) -> ('a -> string) -> test
val ( >: ) : string -> test -> test
val run : test -> unit
