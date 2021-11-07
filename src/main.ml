open Boulder_dash
open Type
open Graphics
open Structures

let  game,setHistory,getHistory  =
  let g = ref None
  in
  (fun (niveau:int)->
     let tmp=(Parse.parse_file
                ("data/level"^(string_of_int niveau)^".lv"))
     in
     g:=Some(tmp);
     Drawing.init_graphics ();
     Graphics.resize_window (tmp.map.larg*20) (tmp.map.haut*20);
     let scale = Drawing.compute_scaler tmp in
     Drawing.reinit_graphics ();
     let () = Drawing.draw_game tmp scale
     in
     tmp
  )
  ,
  (fun (_g:Type.game)->g:=Some(_g))
  ,
  (fun ()->
     match !g with
     |None->exit 1
     |Some(game)->game
  )

let rec start (niveau:int) =
  turn  (game niveau)

and
  turn g =
  let st = wait_next_event [Key_pressed] in
  let g = handle st g in
  let scale = Drawing.compute_scaler g in
  Drawing.reinit_graphics ();
  let () = Drawing.draw_game g scale
  in
  turn g

and

  handle (_state: Graphics.status) (g: game) =
  try
    let (change,_g)=(Game.move_player _state g)
    in
    let nonDead=Game.notDead _g
    in
    if(change)then
      setHistory g;
    if(_state.key=='Z' || _state.key=='z')then
      getHistory ()
    else
      begin
        try
          Game.world_turn
            (Game.decr_bomb
               (Game.remov_explode_bomb
                  (Game.explode_bomb _g)))
        with
        | Game.Dead->
          begin
            if(nonDead)then
              begin
                _g
              end
            else
              begin
                if(_g.life==0)then
                  begin
                    Graphics.draw_image
                      (Graphics.make_image
                         (Images.lire_image_ppm "ppm/GameOver.ppm"))
                      ((_g.map.larg/2)*20-100) (((_g.map.haut/2)*20)-20);
                    Graphics.auto_synchronize false;
                    match (wait_next_event [Key_pressed]).key with
                    |'x'|'X'-> exit 0
                    |_-> start 0
                  end
                else
                  begin
                    {_g with life=_g.life-1}
                  end
              end
          end
      end
  with
  |Game.Win->
    begin
      Graphics.draw_image
        (Graphics.make_image
           (Images.lire_image_ppm "ppm/Win.ppm"))
        (((g.map.larg/2)*20)-20) (((g.map.haut/2)*20)-20);
      Graphics.auto_synchronize false;
      match (wait_next_event [Key_pressed]).key with
      |'n'|'N'-> start (g.niveau+1)
      |_->exit 0
    end

let () =
  let _=start 0
  in ()
