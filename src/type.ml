(***********************)
(* Projet 2I008, 18/19 *)
(***********************)

open Structures


(** Ce fichier contient les définitions des types principaux du jeu *)


type cell =
    Stone|Boulder|Dirt|Diamo|Empty|Walnut|Bomb of int|Explode

(* TODO *)
let string_of_cell cell =
  match cell with
  |Stone-> "X"
  |Boulder-> "O"
  |Dirt-> "-"
  |Diamo-> "V"
  |Empty-> "."
  |Walnut->"P"
  |_->exit 1

let pp_cell fmt cell = Format.pp_print_string fmt (string_of_cell cell)

type map =
  cell Matrix.t

let pp_map : Format.formatter -> map -> unit = Matrix.pp pp_cell

type game =
  {
    map:map;
    player:(int*int);
    diamonds:int;
    life:int;
    niveau:int;
  }

let print_game (fmt: Format.formatter) (g: game): unit =
  Format.fprintf fmt "@[<v>@[<v 2>{@,map: %a@,player: (%d, %d)@,}@]@,@]"
    pp_map g.map
    (fst g.player) (snd g.player)

type dir =
    Sud|Nord|Est|Ouest

let string_of_dir (dir: dir) =
  match dir with
  |Sud->"Sud"|Nord->"Nord"|Est->"Est"|Ouest->"Ouest"

let pp_dir fmt dir = Format.pp_print_string fmt (string_of_dir dir)
