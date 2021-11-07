
open Graphics

(*
  lire_image_ppm : string -> color array array
  parametre
    nom : string = nom du fichier ppm
    valeur renvoyee : color array array = tableau des pixels

*)
let lire_image_ppm nom =
  let entree = open_in_bin nom
  in
  let format = input_line entree
  and largeur,hauteur =
    let ligne = ref (input_line entree)
    in
    while !ligne.[0] = '#' do
      ligne := input_line entree
    done ;
    Scanf.sscanf
      !ligne
      "%d %d"
      (fun x y -> x,y)
  and _ = input_line entree (* lecture de la ligne contenant 255 *)
  in
  let img = Array.make_matrix hauteur largeur (rgb 0 0 0)
  and en_couleur = (format = "P6")
  in
  for i = 0 to hauteur - 1 do
    for j = 0 to largeur - 1 do
      img.(i).(j) <-
	if en_couleur then
	  let x = input_byte entree
	  and y = input_byte entree
	  and z = input_byte entree
	  in
	  rgb x y z
	else
	  let x = input_byte entree
	  in
	  rgb x x x
    done
  done ;
  close_in entree ;
  img
