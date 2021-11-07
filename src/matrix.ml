(***********************)
(* Projet 2I008, 18/19 *)
(***********************)

(** Ce fichier contient une structure de matrice implanté à l'aide de
    maps. *)
open Assoc


(* Définition de type *)
type 'a t =
  {
    larg    : int;
    haut    : int;
    map     : ('a Assoc.t) Assoc.t;
  }

(* à compléter ici: définition de l'exception Out_of_bounds *)
exception Out_of_bounds

(* à ne pas toucher *)
let pp
    (pp1: Format.formatter -> 'a -> unit)
    (fmt: Format.formatter)
    (m  : 'a t): unit =
  Format.fprintf fmt "@[<v 2>{@,larg:%d@,haut:%d@,m:%a}}@]"
    m.larg
    m.haut
    (Assoc.pp (Assoc.pp pp1)) m.map

(* à compléter *)
let make (_haut: int) (_larg: int) (_default: 'a): 'a t=
  {larg=_larg;haut=_haut;map=(constant (constant _default))}

let read (_i: int) (_j: int) (_m: 'a t): 'a =
  if(_i<0 || _i>_m.haut || _j<0 || _j>_m.larg)then
    raise Out_of_bounds
  else
    find _j (find _i _m.map)

let set (_i: int) (_j: int) (_v: 'a) (_m: 'a t): 'a t =
  if(_i<0 || _i>_m.haut || _j<0 || _j>_m.larg)then
    raise Out_of_bounds
  else
    {_m with map=(Assoc.set _i (Assoc.set _j _v (Assoc.find _i _m.map)) _m.map) }

let fold (_f: int -> int -> 'a -> 'b -> 'b) (_m: 'a t) (_acc: 'b): 'b =
  Assoc.fold (fun i assoc acc->Assoc.fold (_f i) 0 (_m.larg-1) assoc acc) 0 (_m.haut-1) _m.map _acc

let iter (_f: int -> int -> 'a -> unit) (_m: 'a t): unit =
  let _= fold (fun i j a _->_f i j a) _m ()
  in
  ()
