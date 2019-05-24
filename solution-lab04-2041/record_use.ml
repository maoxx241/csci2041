(* record type declaration *)
type force_user = {
    name     : string;          (* field 1 *)
    darkside : bool;            (* field 2 *)
    episodes : int list;        (* field 3 *)
  };;

let luke = {                    (* force_user val *)
    name="Luke Skywalker";
    darkside=false;
    episodes=[3;4;5;6;7;8];
  };;

let sidious = {                 (* force_user val *)
    name="Sheev Palpatine";
    darkside=true;
    episodes=[1;2;3;4;5;6];
  };;

let rey = {                     (* force_user val *)
    name="Rey";
    darkside=false;
    episodes=[7;8];
  };;

(* create some new force_user records below *)
let vader = {
    name="Anakin Skywalker";
    darkside=true;
    episodes=[1;2;3;4;5;6];
  };;


let maul = {                    (* force_user val *)
    name="Maul";
    darkside=true;
    episodes=[1]
  };;

let obi = {
    name="Obi-Wan Kenobi";
    darkside=false;
    episodes=[1;2;3;4;5;6];
  };;

(* field access *)
let last_jedi1 = luke.name;;
let last_jedi2 = rey.name;;
let sith_reigh = List.length sidious.episodes;;

(* functions on records *)
let name_of user =              (* retrieve the name *)
  user.name
;;

let episode_count user =        (* count episodes *)
  List.length user.episodes
;;

let seduced user =              (* new record with field changed *)
  let dark_user = {user with darkside=true} in
  dark_user
;;

(* create a new force_user with given episode number appended to end of episodes field *)
let sequel_appearance user episode_num =
  let new_episodes = user.episodes @ [episode_num] in
  let new_user = {user with episodes=new_episodes} in
  new_user
;;

(* succinct version *)
let sequel_appearance user episode_num =
  {user with episodes=user.episodes @ [episode_num]}
;;
