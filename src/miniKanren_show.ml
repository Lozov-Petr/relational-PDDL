open MiniKanren
open MiniKanrenStd
open Tester
open GT

open Environment


let state_show env x =
  show List.ground
    (show Pair.ground
      env.showPred
      (show List.ground
        env.showObj)) x

let state_lshow env x =
  show List.logic
    (show Pair.logic
      (show logic env.showPred)
      (show List.logic
        (show logic env.showObj))) x

let state_reify env x =
  List.reify
    (Pair.reify
      env.predReifier
      (List.reify
        env.objReifier)) x

let state_run env x =
  runR (state_reify env)
       (state_show  env)
       (state_lshow env) x

(******************************************)

let answer_show env x =
  show List.ground
    (show Pair.ground
      env.showAct
      (show List.ground
        env.showObj)) x

let answer_lshow env x =
  show List.logic
    (show Pair.logic
      (show logic env.showAct)
      (show List.logic
        (show logic env.showObj))) x

let answer_reifier env x =
  List.reify
    (Pair.reify
      env.actReifier
      (List.reify
        env.objReifier)) x

let answer_run env x =
  runR (answer_reifier env)
       (answer_show env)
       (answer_lshow env) x
