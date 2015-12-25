(* NOT FINISHED *)
(* -------------------------------------------------------------------------- *)
type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let square_size = 30

let draw_square x y size =
    Graphics.moveto (x - size) (y - size);
    Graphics.lineto (x + size) (y - size);
    Graphics.lineto (x + size) (y + size);
    Graphics.lineto (x - size) (y + size);
    Graphics.lineto (x - size) (y - size)

let draw_square_text x y size text =
    draw_square x y size;
    Graphics.moveto x y;
    Graphics.draw_string text

let draw_tree_node node =
    match node with
      | Nil -> draw_square_text 250 250 square_size "Nil"
      | Node(a, b, c) ->    draw_square_text 250 250 square_size a;
                            Graphics.moveto (250 + square_size) 250;
                            Graphics.lineto (250 + square_size * 2) 300;
                            draw_square_text (250 + square_size * 2) 300
                            square_size "Nil";
                            Graphics.moveto (250 + square_size) 250;
                            Graphics.lineto (250 + square_size * 2) 200
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(* ------------------------------------- Tests ------------------------------ *)
let () =
    Graphics.open_graph "";
    draw_tree_node (Node ("Test", Nil, Nil));
    print_char (Graphics.read_key ())
(* -------------------------------------------------------------------------- *)
