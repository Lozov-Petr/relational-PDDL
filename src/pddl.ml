open MiniKanren
open MiniKanrenStd
open Environment

open MiniKanren_show

type ('a, 'b) value =
  | Variable of 'a
  | Object   of 'b

module For_value = (Fmap2)(struct
  let rec fmap fa fb = function
    | Variable a -> Variable (fa a)
    | Object   b -> Object   (fb b)
  type ('a, 'b) t = ('a, 'b) value
  end)

let var x = inj @@ For_value.distrib @@ Variable x
let obj y = inj @@ For_value.distrib @@ Object   y

type ('a, 'a1, 'a0) gatom =
  | Atom of 'a1 * 'a * 'a0

module For_gatom = (Fmap3)(struct
  let rec fmap fa fa1 fa0 = function
    | Atom (a1_0, a_1, a0_2) -> Atom ((fa1 a1_0), (fa a_1), (fa0 a0_2))
  type ('a, 'a1, 'a0) t = ('a, 'a1, 'a0) gatom end)

let rec atom x__0 x__1 x__2 = inj (For_gatom.distrib (Atom (x__0, x__1, x__2)))

type ('a, 'a2, 'a1, 'a0) gaction =
  | Action of 'a * 'a2 * 'a1 * 'a0

module For_gaction = (Fmap4)(struct
  let rec fmap fa fa2 fa1 fa0 = function
    | Action (a_0, a2_1, a1_2, a0_3) -> Action ((fa a_0), (fa2 a2_1), (fa1 a1_2), (fa0 a0_3))
  type ('a, 'a2, 'a1, 'a0) t = ('a, 'a2, 'a1, 'a0) gaction
  end)

let rec action x__0 x__1 x__2 x__3 = inj (For_gaction.distrib (Action (x__0, x__1, x__2, x__3)))


let rec has inst state res = conde [
  (state === nil ()) &&& (res === !!false);
  (fresh (x xs)
    (state === x % xs)
    (conde [
      (inst === x) &&& (res === !! true);
      (inst =/= x) &&& has inst xs res])
  )]


let rec instance x ren res =
  fresh (k v rs)
    (ren === pair k v % rs)
    (conde [
      (k === x) &&& (v === res);
      (k =/= x) &&& instance x rs res])


let rec instanceArgs args ren res =
  conde[
    (args === nil ()) &&& (res === nil ());
    fresh (x xs q68)
      (args === obj x % xs)
      (res === x % q68)
      (instanceArgs xs ren q68);
    fresh (x xs q70 q71)
      (args === var x % xs)
      (res === q70 % q71)
      (instance x ren q70)
      (instanceArgs xs ren q71)]


let rec evalAtom a ren state res =
  fresh (isPos name args inst q58 hasInst)
    (a === atom isPos name args)
    (inst === pair name q58)
    (instanceArgs args ren q58)
    (has inst state hasInst)
    (conde [
      (isPos === !!true) &&& (hasInst === res);
      (isPos === !!false) &&& (conde [
        (hasInst === !!true) &&& (res === !!false);
        (hasInst === !!false) &&& (res === !!true)])])


let rec evalConj conj_ ren state res = conde [
    (conj_ === nil ()) &&& (res === !!true);
  fresh (x xs q51)
      (conj_ === x % xs)
      (evalAtom x ren state q51)
      (conde [
        (q51 === !!false) &&& (res === !!false);
        (q51 === !!true) &&& evalConj xs ren state res])]


let rec eval expr ren state res = conde [
  (expr === nil ()) &&& (res === !!false);
    (fresh (x xs q44 q45)
      (expr === x % xs)
      (evalConj x ren state q44)
      (conde [
        (q44 === !!true) &&& (res === !!true);
        (q44 === !!false) &&& eval xs ren state res]))]


let rec lookupAction name acts res =
  fresh (x xs n q34 q35 q36)
    (acts === x % xs)
    (x === action n q34 q35 q36)
    (conde [
      (name === n) &&& (x === res);
      (name =/= n) &&& lookupAction name xs res])


let rec zip l1 l2 res =
  conde [
    (l1 === nil ()) &&& (l2 === nil ()) &&& (res === nil ());
    fresh (x xs y ys q29)
      (l1 === x % xs)
      (l2 === y % ys)
      (res === pair x y % q29)
      (zip xs ys q29)]


let rec cmpObjList obj_cmp l1 l2 res = conde [
  (l1 === nil ()) &&& (l2 === nil ()) &&& (res === eq ());
  fresh (x xs y ys resCmp)
    (l1 === x % xs)
    (l2 === y % ys)
    (obj_cmp x y resCmp)
    (conde [
      (resCmp === gt ()) &&& (res === gt ());
      (resCmp === lt ()) &&& (res === lt ());
      (resCmp === eq ()) &&& cmpObjList obj_cmp xs ys res])]


let cmpInst pred_cmp obj_cmp inst1 inst2 res =
  fresh (p1 args1 p2 args2 resCmp)
    (inst1 === pair p1 args1)
    (inst2 === pair p2 args2)
    (pred_cmp p1 p2 resCmp)
    (conde [
      (resCmp === gt ()) &&& (res === gt ());
      (resCmp === lt ()) &&& (res === lt ());
      (resCmp === eq ()) &&& cmpObjList obj_cmp args1 args2 res])


let rec insert pred_cmp obj_cmp inst state res = conde [
   (state === nil ()) &&& (res === inst % nil ());
   fresh (x xs resCmp rs)
     (state === x % xs)
     (cmpInst pred_cmp obj_cmp inst x resCmp)
     (conde [
       (resCmp === gt ()) &&& (res === inst % state);
       (resCmp === eq ()) &&& (res === state);
       (resCmp === lt ()) &&& (res === x % rs) &&& (insert pred_cmp obj_cmp inst xs rs)])]


let rec remove inst state res = conde [
  (state === nil ()) &&& (res === nil ());
  (fresh (x xs rest)
    (state === x % xs)
    (conde [
      (x === inst) &&& (res === xs);
      (x =/= inst) &&& (res === x % rest) &&& remove inst xs rest]))]




let atomUpdate pred_cmp obj_cmp state eff ren res =
  fresh (isPos n args instArgs)
    (eff === atom isPos n args)
    (instanceArgs args ren instArgs)
    (conde [
      (isPos === !!true)  &&& insert pred_cmp obj_cmp (pair n instArgs) state res;
      (isPos === !!false) &&& remove (pair n instArgs) state res])


let rec update pred_cmp obj_cmp state eff ren res = conde [
  (eff === nil ()) &&& (res === state);
  fresh (x y xs q10)
    (eff === x % xs)
    (atomUpdate pred_cmp obj_cmp state x ren q10)
    (update pred_cmp obj_cmp q10 xs ren res)]


let rec sort pred_cmp obj_cmp state acc res = conde [
  (state === nil ()) &&& (res === acc);
  fresh (x xs newAcc)
    (state === x % xs)
    (insert pred_cmp obj_cmp x acc newAcc)
    (sort pred_cmp obj_cmp xs newAcc res)
]


let checker env acts state goal answ =
  let checker checker state answ = conde [
   (answ === nil ()) &&& eval goal (nil ()) state (!!true);
   (fresh (name args xs q3 names pre eff ren q7)
     (answ === pair name args % xs)
     (lookupAction name acts (action q3 names pre eff))
     (zip names args ren)
     (eval pre ren state (!!true))
     (update env.predCmp env.objCmp state eff ren q7)
     (checker q7 xs))] in
  let checker = Tabling.(tabledrec two) checker in
    fresh (sortedState)
      (sort env.predCmp env.objCmp state (nil ()) sortedState)
      (checker sortedState answ)
