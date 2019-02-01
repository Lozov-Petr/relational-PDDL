open MiniKanren
open MiniKanrenStd

type 'a value =
  | Var_ of 'a
  | Obj of 'a

module For_value = (Fmap)(struct
  let rec fmap fa = function
    | Var_ a -> Var_ (fa a)
    | Obj a -> Obj (fa a)
  type 'a t = 'a value
  end)

let rec var_ x__0 = inj (For_value.distrib (Var_ x__0))
and obj x__0 = inj (For_value.distrib (Obj x__0))

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
      (args === var_ x % xs)
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

let rec remove inst state res = conde [
  (state === nil ()) &&& (res === nil ());
  (fresh (x xs rest)
    (state === x % xs)
    (remove inst xs rest)
    (conde [
      (x === inst) &&& (res === rest);
      (x =/= inst) &&& (res === x % rest)]))]

let atomUpdate state eff ren res =
  fresh (isPos n args instArgs)
    (eff === atom isPos n args)
    (instanceArgs args ren instArgs)
    (conde [
      (isPos === !!true) &&& (res === pair n instArgs % state);
      (isPos === !!false) &&& remove (pair n instArgs) state res])

let rec update state eff ren res = conde [
  (eff === nil ()) &&& (res === state);
  fresh (x y xs q10)
    (eff === x % xs)
    (atomUpdate state x ren q10)
    (update q10 xs ren res)]

let checker checker acts state goal answ = conde [
  (answ === nil ()) &&& eval goal (nil ()) state (!!true);
  (fresh (name args xs q3 names pre eff ren q7)
    (answ === pair name args % xs)
    (lookupAction name acts (action q3 names pre eff))
    (zip names args ren)
    (eval pre ren state (!!true))
    (update state eff ren q7)
    (checker acts q7 goal xs))]


let tabled_checker acts state goal answ =
  Tabling.(tabledrec four) checker acts state goal answ

let checker acts state goal answ =
  let rec ch acts state goal answ = checker ch acts state goal answ in
  ch acts state goal answ
