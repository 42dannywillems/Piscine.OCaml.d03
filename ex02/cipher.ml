(* NOT FINISHED:
 *      - xor: String.get forbidden ?
 *      - need tests
*)
(* -------------------------------------------------------------------------- *)
let rotn_loc n c =
    let c_int = int_of_char c in char_of_int ((c_int + n) mod 127)

let rot42 str =
    String.map (rotn_loc 42) str

let caesar str n =
    String.map (rotn_loc n) str

let xor str key = ()

let rec ft_crypt (str : string) fn_list =
    match fn_list with
        | [] -> str
        | fn_head::fn_tail -> ft_crypt (fn_head str) fn_tail
(* -------------------------------------------------------------------------- *)
