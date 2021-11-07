(***********************)
(* Projet 2I008, 18/19 *)
(***********************)

(** Ce fichier contient les fonctions d'affichage graphique du jeu *)
open Structures
open Graphics


type scale = (int * int) -> (int * int)

let gray = rgb 105 105 105
let brown = rgb 139 69 19
let diamond = rgb 185 242 255

(* à compléter *)
let init_graphics,stone,boulder,dirt,diamonds,walnut,empty,player,life,explode,bomb =
  let stone_img = (Images.lire_image_ppm "ppm/stone.ppm") in
  let boulder_img=(Images.lire_image_ppm "ppm/boulder.ppm") in
  let dirt_img=(Images.lire_image_ppm "ppm/dirt.ppm") in
  let diamonds_img=(Images.lire_image_ppm "ppm/diamonds.ppm") in
  let walnut_img=(Images.lire_image_ppm "ppm/walnut.ppm") in
  let empty_img=(Images.lire_image_ppm "ppm/empty.ppm") in
  let life_img=(Images.lire_image_ppm "ppm/life.ppm") in
  let player_img=(Images.lire_image_ppm "ppm/player.ppm") in
  let explode_img=(Images.lire_image_ppm "ppm/explode.ppm") in
  let bomb_img=(Images.lire_image_ppm "ppm/bomb.ppm") in
  ( fun ()->
      Graphics.open_graph "";
      Graphics.auto_synchronize false
  )
  ,
  (fun ()->stone_img)
  ,
  (fun ()->boulder_img)
  ,
  (fun ()->dirt_img)
  ,
  (fun ()->diamonds_img)
  ,
  (fun ()->walnut_img)
  ,
  (fun ()->empty_img)
  ,
  (fun ()->player_img)
  ,
  (fun ()->life_img)
  ,
  (fun ()->explode_img)
  ,
  (fun ()->bomb_img)


(* à compléter *)
let reinit_graphics (): unit =
  Graphics.clear_graph ()


let compute_scaler (g: Type.game): scale =
  let sx = (size_x ())/g.map.larg in
  let sy = (size_y ())/ g.map.haut in
  fun (x, y) ->
    (x * sx, y * sy)


(*
let compute_scaler (g: Type.game): scale =
  let sx = size_x () in
  let sy = size_y () in
  let rx = (sx - sx / 5) / g.map.larg in
  let ry = (sy - sy / 5) / g.map.haut in
  let sxi = rx * g.map.larg in
  let syi = ry * g.map.haut in
  let margx = (sx - sxi) / 2 in
  let margy = (sy - syi) / 2 in
  fun (x, y) ->
    (margx + x * rx, margy + y * ry)
*)
(* à compléter *)
let draw_rect_cell (_c: color) ((_i, _j): int * int) (_scaler: scale): unit =
  let (x1,y1)=_scaler (_i,_j)
  in
  let (len,_)=_scaler (1,1)
  in
  begin
    Graphics.set_color _c;
    Graphics.draw_rect x1 y1 len len;
  end
(* à compléter *)
let fill_rect_cell (_c: color) ((_i, _j): int * int) (_scaler: scale): unit =
  let (x1,y1)=_scaler (_i,_j)
  in
  let (len,_)=_scaler (1,1)
  in
  begin
    Graphics.set_color _c;
    Graphics.fill_rect x1 y1 len len;
  end

(* à compléter *)
let fill_diamond_cell (_c: color) ((_i, _j): int * int) (_scaler: scale): unit =
  let (x1,y1)=_scaler (_i,_j)
  in
  let (tmp,_)=_scaler (1,1)
  in
  let a=(x1,y1+tmp/2)
  in
  let b=(x1+tmp/2,y1)
  in
  let c=(x1+tmp,y1+tmp/2)
  in
  let d=(x1+tmp/2,y1+tmp)
  in
  begin
    Graphics.set_color _c;
    Graphics.fill_poly [|a;b;c;d|]
  end

(* à compléter *)
let fill_circle_cell (_c: color) ((_i, _j): int * int) (_scaler: scale): unit =
  let (x1,y1)=_scaler (_i,_j)
  in
  let (r,_)=_scaler (1,1)
  in
  begin
    Graphics.set_color _c;
    Graphics.fill_circle (x1+(r/2)) (y1+(r/2)) (r/2)
  end

let fill_ellipse_cell (_c: color) ((_i, _j): int * int) (_scaler: scale): unit =
  let (x1,y1)=_scaler (_i,_j)
  in
  let (r,_)=_scaler (1,1)
  in
  begin
    Graphics.set_color _c;
    Graphics.fill_ellipse (x1+(r/2)) (y1+(r/2)) ((r/2)-2) (r/2)
  end

(* à compléter *)
let draw_cell (_c: Type.cell) (_j, _i) (_scaler: scale): unit =
  let (x1,y1)=_scaler (_i,_j)
  in

  match _c with
  |Stone->Graphics.draw_image
            (Graphics.make_image (stone ())) x1 y1
  |Boulder->Graphics.draw_image
              ( Graphics.make_image (boulder ()) ) x1 y1
  |Dirt->Graphics.draw_image
           (Graphics.make_image (dirt ()) ) x1 y1
  |Diamo->Graphics.draw_image
            (Graphics.make_image (diamonds ()) ) x1 y1
  |Walnut->Graphics.draw_image
             (Graphics.make_image (walnut ()) ) x1 y1
  |Empty->Graphics.draw_image
            (Graphics.make_image (empty ()) ) x1 y1
  |Bomb(_)->Graphics.draw_image
              (Graphics.make_image (bomb ()) ) x1 y1
  |Explode->Graphics.draw_image
              (Graphics.make_image (explode ()) ) x1 y1


(* à compléter *)
let draw_map (_m: Type.map) (_scaler: scale): unit =
  Matrix.iter (fun i j c->
      draw_cell c (i,j) _scaler
    ) _m

(* à compléter *)
let draw_player ((_j, _i): (int * int)) (_scaler: scale): unit =
  (*fill_rect_cell (rgb 255 0 0) (_i,_j) _scaler*)
  let (x1,y1)=_scaler (_i,_j)
  in
  Graphics.draw_image
    (Graphics.make_image (player ())) x1 y1

(* à compléter *)
let draw_game (_g: Type.game) (_scaler: scale) =
  Graphics.set_color (rgb 0 0 0);
  let life_img_make=(Graphics.make_image (life ()))
  in
  let (unite,_)=_scaler (1,1)
  in
  begin
    draw_map _g.map _scaler;
    draw_player _g.player _scaler;
    Graphics.moveto unite ((_g.map.haut-1)*unite);
    Graphics.set_font
      "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
    Graphics.draw_string ("Diamonds <> : "^(string_of_int _g.diamonds));
    Graphics.moveto ((((_g.map.larg-1)/2)*unite)-60) 0;
    Graphics.draw_string ("Niveau : "^(string_of_int _g.niveau));
    for i=0 to _g.life do
      Graphics.draw_image life_img_make
        ((_g.map.larg-1-i)*unite) ((_g.map.haut-1)*unite)
    done;
    Graphics.auto_synchronize true
  end
