open MiniKanren
open MiniKanrenStd
open Tester
open GT

open MiniKanren_show
open Pddl
open Blocksworld


let bw_run       x = answer_run bw_env x
let bw_state_run x = state_run bw_env x

(******************************************)
let () =
  let checker = checker bw_env in
  let actions = l2ll @@ actions () in

  (* bw_state_run (1) q qh ("sorting test 1", fun q -> sort bw_env.predCmp bw_env.objCmp (l2ll @@ init01 ()) (nil ()) q); *)
  (* bw_state_run (1) q qh ("sorting test 1", fun q -> sort bw_env.predCmp bw_env.objCmp (l2ll @@ init02 ()) (nil ()) q); *)
  (* bw_state_run (1) q qh ("sorting test 1", fun q -> sort bw_env.predCmp bw_env.objCmp (l2ll @@ init03 ()) (nil ()) q); *)



  (* bw_run (1)  q qh ("blocks world - p01", (fun q -> checker actions (l2ll @@ init01 ()) (goal01 ()) q)); *)
  (* bw_run (1)  q qh ("blocks world - p02", (fun q -> checker actions (l2ll @@ init02 ()) (goal02 ()) q)); *)
  (* bw_run (1)  q qh ("blocks world - p03", (fun q -> checker actions (l2ll @@ init03 ()) (goal03 ()) q)); *)
  (* bw_run (1)  q qh ("blocks world - p04", (fun q -> checker actions (l2ll @@ init04 ()) (goal04 ()) q)); *)
  (* bw_run (1)  q qh ("blocks world - p05", (fun q -> checker actions (l2ll @@ init05 ()) (goal05 ()) q)); *)
  (* bw_run (1)  q qh ("blocks world - p06", (fun q -> checker actions (l2ll @@ init06 ()) (goal06 ()) q)); *)
  (* bw_run (1)  q qh ("blocks world - p07", (fun q -> checker actions (l2ll @@ init07 ()) (goal07 ()) q)); *)
  (* bw_run (1)  q qh ("blocks world - p08", (fun q -> checker actions (l2ll @@ init08 ()) (goal08 ()) q)); *)
  (* bw_run (1)  q qh ("blocks world - p09", (fun q -> checker actions (l2ll @@ init09 ()) (goal09 ()) q)); *)
  (* bw_run (1)  q qh ("blocks world - p10", (fun q -> checker actions (l2ll @@ init10 ()) (goal10 ()) q)); *)

  bw_run (1) q qh ("blocks world - p01 (new)", (fun q -> new_checker bw_env (objs01 ()) actions (l2ll @@ init01 ()) (goal01 ()) q));
  (* bw_run (1) q qh ("blocks world - p02 (new)", (fun q -> new_checker bw_env (objs02 ()) actions (l2ll @@ init02 ()) (goal02 ()) q)); *)
  (* bw_run (1) q qh ("blocks world - p03 (new)", (fun q -> new_checker bw_env (objs03 ()) actions (l2ll @@ init03 ()) (goal03 ()) q)); *)
  (* bw_run (1) q qh ("blocks world - p04 (new)", (fun q -> new_checker bw_env (objs04 ()) actions (l2ll @@ init04 ()) (goal04 ()) q)); *)
  (* bw_run (1) q qh ("blocks world - p05 (new)", (fun q -> new_checker bw_env (objs05 ()) actions (l2ll @@ init05 ()) (goal05 ()) q)); *)
  (* bw_run (1) q qh ("blocks world - p06 (new)", (fun q -> new_checker bw_env (objs06 ()) actions (l2ll @@ init06 ()) (goal06 ()) q)); *)
  ()
