(* -------------------------------------------------------------------------- *)
type 'a bstree = Nil | Node of 'a * 'a bstree * 'a bstree

type 'a alpha_val = NULL | MIN | MAX | Value of 'a

(* --------- Ex 01 ---------- *)
let rec height tree =
    match tree with
      | Nil -> 0
      | Node (_, left, right) -> 1 + (max (height left) (height right))
(* -------------------------- *)

(* -------------------- *)
(* Used to check if a binary tree is a bstree.
 * I define a new type with MIN, MAX and NULL (for value of empty tree) to be
 * able to compare.
*)
let ( <<= ) a b = match (a, b) with
    | (NULL, _) -> true
    | (_, NULL) -> true
    | (MIN, _) -> true
    | (MAX, MAX) -> true
    | (_, MAX) -> false
    | (c, d) -> c <= d

let ( =>> ) a b = match (a, b) with
    | (NULL, _) -> true
    | (_, NULL) -> true
    | (MIN, MIN) -> true
    | (MIN, _) -> false
    | (_, MAX) -> true
    | (c, d) -> c <= d

let get_value bst =
    match bst with
        | Node (a, _, _) -> a
        | Nil -> NULL

let rec bst_bound bst = match bst with
    | Nil -> Nil
    | Node (value, a, b) -> Node (Value value, bst_bound a, bst_bound b)

let is_bst bst =
    let rec is_bst_loc bst min max = match bst with
        | Nil -> true
        | Node (value, Nil, Nil) -> min <<= value && value =>> max
        | Node (value, left, right) ->
                (min <<= value && value =>> max) &&
                (is_bst_loc left min value) &&
                (is_bst_loc right value max)
    in
    is_bst_loc (bst_bound bst) MIN MAX
(* -------------------- *)

(* -------------------- *)
let rec is_perfect bst = match bst with
    | Nil -> true
    | Node (_, Nil, Nil) -> true
    | Node (_, a, b) -> a != Nil && b != Nil && (is_perfect a) &&
    (is_perfect b)
(* -------------------- *)

(* -------------------- *)
(* value * leftChild * rightChild * balance *)
type 'a balanced_tree = Nil | Node of 'a * 'a balanced_tree * 'a balanced_tree
* int

let balanced_balanced_tree bst = match bst with
    | Nil -> 0
    | Node (_, _, _, b) -> b

let rec balanced_tree_of_bstree (bst: 'a bstree) = match bst with
    | Nil -> Nil
    | Node (a, Nil, Nil) -> Node (a, Nil, Nil, 0)
    | Node (a, b, Nil) -> Node (a, balanced_tree_of_bstree b, Nil, (- (height b)))
    | Node (a, Nil, b) -> Node (a, Nil, balanced_tree_of_bstree b, height b)
    | Node (a, b, c) ->
        let
            left = (balanced_tree_of_bstree b) and
            right = (balanced_tree_of_bstree c)
        in
        Node (a, left, right, (balanced_balanced_tree left) - (balanced_balanced_tree right))

let is_balanced bst =
    let rec is_balanced_tree balanced_bst =
    match balanced_bst with
        | Nil -> true
        | Node (a, Nil, Nil, _) -> true
        | Node (a, b, c, balanced) ->
            (is_balanced_tree b) &&
            (is_balanced_tree c) &&
            (-2) <= balanced &&
            balanced <= 2 in
    is_balanced_tree (balanced_tree_of_bstree bst)

let rec search_bst (bst : 'a bstree) value = match bst with
    | Nil -> false
    | Node (v, left, right) -> if v = value then true else if value < v then
        (search_bst left value) else (search_bst right value)

let add_bst bst value = ()

let delete_bst bst value = ()
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(* Print with colors *)
let print_nfo_endline str =
    print_endline ("\x1b[34m" ^ str ^ "\x1b[0m")

let print_err_endline str =
    print_endline ("\x1b[31m" ^ str ^ "\x1b[0m")

(* Unit tests: assert_equal *)
let assert_equal a b =
    if a = b then print_endline "OK" else print_err_endline "\x1b[31mWrong \x1b[0m"
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(* tests of the day *)
let test_is_bst () =
    print_nfo_endline "-------------------------- is bst \
    -------------------------";
    assert_equal (is_bst Nil) true;
    assert_equal (is_bst (Node (5, Nil, Nil))) true;
    assert_equal (is_bst (Node (5, Node(4, Nil, Nil), Nil))) true;
    assert_equal (is_bst (Node (5, Nil, Node(4, Nil, Nil)))) false;
    assert_equal (is_bst (Node (5, Nil, Node(7, Nil, Nil)))) true;
    assert_equal (is_bst (Node (5, Node(8, Nil, Nil), Nil))) false;
    assert_equal (is_bst (Node (5, Node(8, Nil, Nil), Node(7, Nil, Nil))))
    false;
    assert_equal (is_bst (Node (5, Node(4, Node(3, Nil, Nil), Nil), Node(7, Nil,
    Nil)))) true;
    assert_equal (is_bst (Node (5, Node(4, Node(3, Nil, Nil), Node(9, Nil, Nil)),
    Node(7, Nil, Nil)))) false

let test_is_perfect () =
    print_nfo_endline "-------------------------- is perfect \
    --------------------";
    assert_equal (is_perfect Nil) true;
    assert_equal (is_perfect (Node (5, Nil, Nil))) true;
    assert_equal (is_perfect (Node (5, Node (4, Nil, Nil), Nil))) false;
    assert_equal (is_perfect (Node (5, Node (4, Node (3, Nil, Nil), Nil),
    Nil))) false;
    assert_equal (is_perfect (Node (5, Node (4, Node (3, Node (2, Nil, Nil), Nil), Nil),
    Nil))) false;
    assert_equal (is_perfect (Node (5, Node (4, Node (3, Node (2, Nil, Nil),
    Node (4, Nil, Nil)), Nil), Nil))) false;
    assert_equal (is_perfect (Node (5, Node (4, Node (3, Node (2, Nil, Nil),
    Node (4, Nil, Nil)), Node (5, Nil, Nil)), Nil))) false;
    assert_equal (is_perfect (Node (5, Node (4, Node (3, Node (2, Nil, Nil),
    Node (4, Nil, Nil)), Node (5, Nil, Nil)), Node (18, Nil, Nil)))) true

let test_is_balanced () =
    print_nfo_endline "-------------------------- is balanced \
    --------------------";
    assert_equal (is_balanced Nil) true;
    assert_equal (is_balanced (Node (5, Nil, Nil))) true;
    assert_equal (is_balanced (Node (5, Node (4, Nil, Nil), Nil))) true;
    assert_equal (is_balanced (Node (5, Node (4, Node (3, Nil, Nil), Nil),
    Nil))) true;
    assert_equal (is_balanced (Node (5, Node (4, Node (3, Node (2, Nil, Nil), Nil), Nil),
    Nil))) false;
    assert_equal (is_balanced (Node (5, Node (4, Node (3, Node (2, Nil, Nil),
    Node (4, Nil, Nil)), Nil), Nil))) false;
    assert_equal (is_balanced (Node (5, Node (4, Node (3, Node (2, Nil, Nil),
    Node (4, Nil, Nil)), Node (5, Nil, Nil)), Nil))) false;
    assert_equal (is_balanced (Node (5, Node (4, Node (3, Node (2, Nil, Nil),
    Node (4, Nil, Nil)), Node (5, Nil, Nil)), Node (18, Nil, Nil)))) true

let test_search_bst () =
    print_nfo_endline "-------------------------- search bst \
    --------------------";
    assert_equal (search_bst Nil 3) false;
    assert_equal (search_bst (Node (5, Nil, Nil)) 5) true;
    assert_equal (search_bst (Node (5, Nil, Nil)) 4) false;
    assert_equal (search_bst (Node (5, Node (4, Nil, Nil), Nil)) 3) false;
    assert_equal (search_bst (Node (5, Node (4, Nil, Nil), Nil)) 6) false;
    assert_equal (search_bst (Node (5, Node (4, Nil, Nil), Nil)) 5) true;
    assert_equal (search_bst (Node (5, Node (4, Node (3, Nil, Nil), Nil),
    Nil)) 3) true;
    assert_equal (search_bst (Node (5, Node (4, Node (3, Node (2, Nil, Nil), Nil), Nil),
    Nil)) 2) true;
    assert_equal (search_bst (Node (5, Node (4, Node (3, Node (2, Nil, Nil),
    Node (4, Nil, Nil)), Nil), Nil)) 4) true;
    assert_equal (search_bst (Node (5, Node (4, Node (3, Node (2, Nil, Nil),
    Node (4, Nil, Nil)), Node (5, Nil, Nil)), Nil)) 5) true;
    assert_equal (search_bst (Node (5, Node (4, Node (3, Node (2, Nil, Nil),
    Node (4, Nil, Nil)), Node (5, Nil, Nil)), Node (18, Nil, Nil))) 18) true

let () =
    test_is_bst ();
    test_is_perfect ();
    test_is_balanced ();
    test_search_bst ()
(* -------------------------------------------------------------------------- *)
