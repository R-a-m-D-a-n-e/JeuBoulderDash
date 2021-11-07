
(***********************)
(* Projet 2I008, 18/19 *)
(***********************)

(** Ce fichier contient une structure de map (int -> 'a), les paires
    clé/valeur sont stockés dans une liste ordonnée sur les clés. *)

(* à compléter *)



let remove_assoc (_k: int) (_l: (int * 'a) list): (int * 'a) list =
  let rec loop l accu=
    match l with
    |[]->List.rev accu
    |(x,a)::l->
      if(x>_k)then
        _l
      else
        (if(x==_k)then
           (List.rev accu )@l
         else
           loop l ((x,a)::accu))
  in
  loop _l []

(* à ne pas toucher *)
let pp_assoc
    (pp1: Format.formatter -> 'a -> unit)
    (fmt: Format.formatter)
    (l: (int * 'a) list): unit =
  Format.fprintf fmt "@[[%a@]]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
       (fun fmt (k, v) -> Format.fprintf fmt "(%a, %a)" Format.pp_print_int k pp1 v)
    ) l

(* Définition du type des fonctions des entiers vers 'a *)
type 'a t =
  { assoc : (int * 'a) list;
    default : 'a
  }

(* à compléter *)
let constant (_d: 'a): 'a t =
  {assoc=[];default=_d}

(* à compléter *)
let find (_k: int) (_m: 'a t): 'a =
  let rec loop l=
    match l with
    |[]->_m.default
    |(x,a)::l->
      if(x>_k)then
        _m.default
      else
        (if(x==_k)then
           a
         else
           loop l)
  in
  loop _m.assoc

(* à compléter *)
let set (_k: int) (_v: 'a) (_m: 'a t): 'a t =
  let rec loop l accu=
    match l with
    |[]->
      begin
        if(_v<>_m.default)then
          List.rev ((_k,_v)::accu)
        else
          List.rev accu
      end
    |(x,a)::l->
      if(x>_k)then
        begin
          if(_v<>_m.default)then
            (List.rev ((x,a)::(_k,_v)::accu))@l
          else
            (List.rev ((x,a)::accu))@l
        end
      else
        (if(x==_k)then
           begin
             if(_v<>_m.default)then
               (List.rev ((_k,_v)::accu))@l
             else
               (List.rev accu)@l
           end
         else
           loop l ((x,a)::accu))
  in
  {assoc=(loop _m.assoc []);default=_m.default}


(* à compléter *)
let fold (_f: int -> 'a -> 'b -> 'b) (_a: int) (_b: int) (_m: 'a t) (_init: 'b): 'b =
  let rec loop i =
    if(i==_a)then
      _f _a (find _a _m) _init
    else
      _f i (find i _m) (loop (i-1))
  in
  _f _b (find _b _m) (loop (_b-1) )

(* à ne pas toucher *)
let pp
    (pp1: Format.formatter -> 'a -> unit)
    (fmt: Format.formatter)
    (m: 'a t): unit =
  Format.fprintf fmt "@[{default:%a;@, assoc:[@[%a@]]@,}@]"
    pp1 m.default
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@,")
       (fun fmt (k, v) -> Format.fprintf fmt "(%a, %a)" Format.pp_print_int k pp1 v)
    ) m.assoc
