open MiniKanren
open MiniKanrenStd
open Tester
open GT

open Pddl

let show_llist f x =
  let rec show_list x = Printf.sprintf "[%s]" (String.concat "; " x) in
  let rec show_llist x =
    match x with
    | Var _               -> [], Some (show(logic) (fun _ -> "") x)
    | Value Nil           -> [], None
    | Value (Cons (x,xs)) -> let l, q = show_llist xs in f x :: l, q in
  match show_llist x with
  | [], None   -> "[]"
  | [], Some s -> s
  | x , None   -> show_list x
  | x , Some s -> Printf.sprintf "%s ^ %s" (show_list x) s


let answer_show  show_action show_obj x = show(List.ground) (show(Pair.ground) show_action               (show(List.ground) show_obj))               x
let answer_lshow show_action show_obj x = show_llist        (show(Pair.logic ) (show(logic) show_action) (show_llist        (show(logic) show_obj))) x

let answer_reifier action_reifier obj_reifier x = List.reify (Pair.reify action_reifier (List.reify obj_reifier)) x

let answer_run show_action action_reifier show_obj obj_reifier x =
  runR (answer_reifier action_reifier obj_reifier) (answer_show show_action show_obj) (answer_lshow show_action show_obj) x
