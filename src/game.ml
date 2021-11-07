(***********************)
(* Projet 2I008, 18/19 *)
(***********************)

open Structures



exception Dead
exception Win
exception Explodes of int*int
exception Boom of int*int
exception Noix of int*int
exception Bould of ((int*int)option)

let notDead (_g:Type.game):bool=
  let (x,y)=_g.player
  in
  try
    (Matrix.read (x+1) y _g.map)==Boulder
  with Matrix.Out_of_bounds->true

let win (_g: Type.game) :bool=
  _g.diamonds==0

let move (_i, _j) (_dir: Type.dir):(int*int) =
  match _dir with
  |Sud->(_i-1,_j)
  |Nord->(_i+1,_j)
  |Est->(_i,_j-1)
  |Ouest->(_i,_j+1)

let player_turn (_g: Type.game) (_d: Type.dir):Type.game =
  let (x,y)=move _g.player _d
  in
  try
    let tmp=Matrix.read x y _g.map in
    match tmp with
    |Stone|Explode|Bomb(_)->_g
    |Boulder|Walnut->
      begin
        match _d with
        |Est|Ouest->
          begin
            let (x2,y2)=move (x,y) _d
            in
            match Matrix.read x2 y2 _g.map with
            |Empty->
              {_g with
               map=(Matrix.set x2 y2 tmp
                      (Matrix.set x y Type.Empty _g.map));
               player=(x,y)
              }
            |_->_g
          end
        |_->_g
      end
    |Dirt->
      {
        _g with
        map=(Matrix.set x y Type.Empty _g.map);
        player=(x,y)
      }
    |Diamo->
      let res={ _g with Type.diamonds=_g.diamonds-1;
                        map=(Matrix.set x y Type.Empty _g.map);
                        player=(x,y);
              }
      in
      if(win res)then
        begin
          Drawing.draw_game res (Drawing.compute_scaler res);
          raise Win
        end
      else
        res
    |Empty->{_g with player=(x,y)}
  with Matrix.Out_of_bounds->_g

let is_empty ((x,y):(int*int)) (_g: Type.game):bool=
  try
    (Matrix.read x y _g.map) ==Empty
  with Matrix.Out_of_bounds->false

let position_after_fall (_g: Type.game) ((x,y):(int*int)):(int*int)=
  let tmp=(Matrix.read (x-1) y _g.map)
  in
  if(tmp==Empty)then
    (x-1,y)
  else(
    if(tmp==Walnut)then
      raise(Noix(x-1,y))
    else
      begin
        if(tmp ==Type.Boulder)then
          begin
            try
              if(is_empty (x-1,y+1) _g)then
                (x-1,y+1)
              else
                raise Matrix.Out_of_bounds
            with Matrix.Out_of_bounds->
              begin
                try
                  if(is_empty (x-1,y-1) _g)then
                    (x-1,y-1)
                  else
                    raise Matrix.Out_of_bounds
                with Matrix.Out_of_bounds->(x,y)
              end
          end
        else
          (x,y)
      end
  )
let move_boulder_step (_g: Type.game) ((x,y):(int*int)):Type.game=
  try
    let (x2,y2)=position_after_fall _g (x,y)
    in
    let tmp=Matrix.read x y _g.map
    in
    if((x2,y2)<>_g.player)then
      begin
        if(x2<>x || y2<>y)then
          {_g with map=(Matrix.set x2 y2 tmp
                          (Matrix.set x y Type.Empty _g.map))}
        else
          _g
      end
    else
      raise Dead
  with Noix(x,y)->{_g with map=(Matrix.set x y Type.Diamo (Matrix.set x y Type.Empty _g.map))}

let find_movable_boulder (_g:Type.game):(int*int)option=
  Matrix.fold (
    fun i j cell acc->
      begin
        if(cell==Type.Boulder || cell==Type.Walnut)then
          begin
            try
              let (x,y)=position_after_fall _g (i,j)
              in
              if((x,y)<>(i,j))then
                raise  (Bould (Some(i,j)))
              else
                acc
            with Noix(_,_)->raise (Bould (Some(i,j)))
          end
        else
          acc
      end
  ) _g.map None

let rec  world_turn (_g: Type.game) =
  Drawing.draw_game _g (Drawing.compute_scaler _g );
  try
    let _=(find_movable_boulder _g)
    in
    _g
  with Bould(tmp)->
    begin
      match tmp with
      |Some(x,y)->world_turn (move_boulder_step _g (x,y))
      |_->_g
    end

let count_diamonds (_m:Type.cell Matrix.t):int=
  Matrix.fold
    (
      fun _ _ cell acc->
        if(cell==Type.Diamo || cell=Type.Walnut)then
          acc+1
        else
          acc
    ) _m 0

let put_bomb (_g:Type.game):Type.game=
  let (x,y)=_g.player
  in
  {_g with map=(Matrix.set x y (Type.Bomb(3)) _g.map)}

let find_bomb f (_g:Type.game):(int*int)option=
  Matrix.fold (
    fun i j cell acc->
      match cell with
      |Type.Bomb(x)->
        if(f x)then
          raise (Boom(i,j))
        else
          acc
      |_->acc
  ) _g.map None

let list_explode (x,y):(int*int)list=
  [
    (x+1,y);
    (x,y+1);
    (x,y-1);
    (x-1,y);
    (x,y)
  ]

let explode (x,y) (_g:Type.game):Type.game=

  List.fold_left
    (
      fun (acc:Type.game) (x,y)->
        try
          match Matrix.read x y acc.map with
          |Stone|Diamo|Walnut->acc
          |_->
            {acc with map=(Matrix.set x y Type.Explode acc.map)}
        with
          Matrix.Out_of_bounds->acc
    ) _g (list_explode (x,y))


let rec explode_bomb (_g:Type.game):Type.game=
  Drawing.draw_game _g (Drawing.compute_scaler _g );
  try
    let _=(find_bomb (fun x -> x==1) _g)
    in
    _g
  with Boom(x,y)->
    begin
      explode_bomb (explode (x,y) _g)
    end

let rec decr_bomb (_g:Type.game):Type.game=
  Drawing.draw_game _g (Drawing.compute_scaler _g );
  try
    let _=(find_bomb (fun x -> x<>1) _g)
    in
    _g
  with Boom(x,y)->
    begin
      decr_bomb ({_g with map=(Matrix.set x y (Type.Bomb(1)) _g.map)})
    end

let find_explode_bomb (_g:Type.game):(int*int)option=
  Matrix.fold (
    fun i j cell acc->
      match cell with
      |Type.Explode->raise (Explodes(i,j))
      |_->acc
  ) _g.map None

let rec remov_explode_bomb (_g:Type.game):Type.game=
  Drawing.draw_game _g (Drawing.compute_scaler _g );
  try
    let _=(find_explode_bomb _g)
    in
    _g
  with Explodes(x,y)->
    begin
      remov_explode_bomb ({_g with map=(Matrix.set x y Type.Empty _g.map)})
    end



let move_player (_state: Graphics.status) (_g: Type.game)=
  begin
    match _state.key with
    |'e'|'E'-> true,(player_turn _g Type.Nord)
    |'d'|'D'->  true,(player_turn _g Type.Sud)
    |'s'|'S'->  true,(player_turn _g Type.Est)
    |'f'|'F'->  true,(player_turn _g Type.Ouest)
    |'b'|'B'->true,(put_bomb _g)
    |'x'|'X'-> exit 0
    |_-> false,_g
  end

