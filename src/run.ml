open MiniKanren
open MiniKanrenStd
open Tester
open GT

open MiniKanren_show
open Pddl_hist
open Pddl
open Blocksworld


let answer_run = answer_run show_action action_reifier show_obj obj_reifier

(******************************************)
let () =

  (*******)
  let pref = "check times: " in

  (* answer_run (1)  q qh (pref ^             "checker 1", (fun q ->               checker (l2ll actions) (l2ll init1) goal1 q)); *)
  (* answer_run (1)  q qh (pref ^      "tabled checker 1", (fun q ->        tabled_checker (l2ll actions) (l2ll init1) goal1 q)); *)
  (* answer_run (1)  q qh (pref ^        "hist checker 1", (fun q ->          hist_checker (l2ll actions) (l2ll init1) goal1 q)); *)
  (* answer_run (1)  q qh (pref ^ "tabled hist checker 1", (fun q ->   tabled_hist_checker (l2ll actions) (l2ll init1) goal1 q)); *)


  (* answer_run (1)  q qh (pref ^             "checker 2", (fun q ->               checker (l2ll @@ List.rev actions) (l2ll @@ List.rev init1) goal1 q)); *)
  (* answer_run (1)  q qh (pref ^      "tabled checker 2", (fun q ->        tabled_checker (l2ll @@ List.rev actions) (l2ll @@ List.rev init1) goal1 q)); *)
  (* answer_run (1)  q qh (pref ^        "hist checker 2", (fun q ->          hist_checker (l2ll @@ List.rev actions) (l2ll @@ List.rev init1) goal1 q)); *)
  (* answer_run (1)  q qh (pref ^ "tabled hist checker 2", (fun q ->   tabled_hist_checker (l2ll @@ List.rev actions) (l2ll @@ List.rev init1) goal1 q)); *)

  (*******)

  let checker = hist_checker in
  let actions = l2ll actions in
  need_debug := false;
  (* answer_run (1)  q qh ("blocks world - p01", (fun q -> checker actions (l2ll init01) goal01 q)); print_debug (); *)
  (* answer_run (1)  q qh ("blocks world - p02", (fun q -> checker actions (l2ll init02) goal02 q)); print_debug (); *)
  (* answer_run (1)  q qh ("blocks world - p03", (fun q -> checker actions (l2ll init03) goal03 q)); print_debug (); *)
  answer_run (1)  q qh ("blocks world - p04", (fun q -> checker actions (l2ll init04) goal04 q)); print_debug ();
  (* answer_run (1)  q qh ("blocks world - p05", (fun q -> checker actions (l2ll init05) goal05 q)); print_debug (); *)
  (* answer_run (1)  q qh ("blocks world - p06", (fun q -> checker actions (l2ll init06) goal06 q)); print_debug (); *)
  (* answer_run (1)  q qh ("blocks world - p07", (fun q -> checker actions (l2ll init07) goal07 q)); print_debug (); *)
  (* answer_run (1)  q qh ("blocks world - p08", (fun q -> checker actions (l2ll init08) goal08 q)); print_debug (); *)
  (* answer_run (1)  q qh ("blocks world - p09", (fun q -> checker actions (l2ll init09) goal09 q)); print_debug (); *)
  (* answer_run (1)  q qh ("blocks world - p10", (fun q -> checker actions (l2ll init10) goal10 q)); print_debug (); *)
  ()
