open Structures
open Type

(* TODO *)
let cell_of_char c: cell option = match c with
  | ' ' -> Some(Empty)
  | 'O' -> Some(Boulder)
  | 'X' -> Some(Stone)
  | '-' -> Some(Dirt)
  | 'V' -> Some(Diamo)
  | 'P' -> Some(Walnut)
  | _ -> failwith "unrecognized char"

let parse_file (f: string) =
  let ic = open_in f in
  let niveau=int_of_string (input_line ic)
  in
  let haut = int_of_string (input_line ic) in
  let larg = int_of_string (input_line ic) in
  let joueur = ref (1,0) in
  let map = ref (Matrix.make haut larg Empty) in
  (* i : ligne (inversion à cause de Graphics, j : colonne *)
  for i = haut - 1 downto 0 do
    let line = input_line ic in
    for j = 0 to larg - 1 do
      match cell_of_char line.[j] with
      | None -> joueur := (j,i)
      | Some b -> map := Matrix.set i j b !map
    done;
  done;
  {
    diamonds=(Game.count_diamonds !map);
    player = !joueur;
    map = !map;
    life=3;
    niveau=niveau;
  }
