(* NOT FINISHED: draw_tree *)
(* -------------------------------------------------------------------------- *)
type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec size tree =
    match tree with
      | Nil -> 0
      | Node (_, left, right) -> 1 + (size left) + (size right)

let rec height tree =
    match tree with
      | Nil -> 0
      | Node (_, left, right) -> 1 + (max (height left) (height right))

let draw_tree tree =
    ()
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(* ---------------------------------- TESTS --------------------------------- *)
let test_fn fn tree =
    print_int (fn tree) ; print_endline ""

let () =
    let a = Node ("test", Nil, Nil) in
    let b = Node ("test", a, Nil) in
    let c = Node ("test", a, b) in
    let d = Node ("test", c, a) in
    let e = Node ("test", d, c) in
    test_fn size Nil;
    test_fn size a;
    test_fn size b;
    test_fn size c;
    test_fn size d;
    test_fn size e;
    test_fn height Nil;
    test_fn height a;
    test_fn height b;
    test_fn height c;
    test_fn height d;
    test_fn height e
(* -------------------------------------------------------------------------- *)
