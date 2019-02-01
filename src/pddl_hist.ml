open MiniKanren
open MiniKanrenStd

open Pddl

let rec notSubset st1 st2 res = conde [
  (st1 === nil ()) &&& (res === !!false);
  fresh (x xs q33)
    (st1 === x % xs)
    (has x st2 q33)
    (conde [
      (q33 === !!false) &&& (res === !!true);
      (q33 === !!true) &&& notSubset xs st2 res])]

let notEquiv st1 st2 res =
  fresh (q19)
    (notSubset st1 st2 q19)
    (conde [
      (q19 === !!true) &&& (res === !!true);
      (q19 === !!false) &&& notSubset st2 st1 res])

let rec isNew state hist res = conde [
  (hist === nil ()) &&& (res === !!true);
  fresh (h hs q13 q14)
    (hist === h % hs)
    (notEquiv state h q13)
    (conde [
      (q13 === !!false) &&& (res === !!false);
      (q13 === !!true) &&& isNew state hs res])]


let need_debug = ref false
let debug_list = ref []

let print_debug () = if !need_debug then begin
  Printf.printf "\n*************\n";
  List.iteri (fun i v -> Printf.printf "%4d: %7d\n" i v) !debug_list;
  Printf.printf "-------------\n";
  Printf.printf "Total: %6d\n" (List.fold_left (+) 0 !debug_list);
  Printf.printf "*************\n%!";
  debug_list := []
  end

let rec incr l i =
  match l with
  | []      -> if i = 0 then [1] else 0 :: incr [] (i-1)
  | x :: xs -> if i = 0 then (x+1) :: xs else x :: incr xs (i-1)

let rec checker n acts hist state goal answ =
  if !need_debug then debug_list := incr !debug_list n;
  conde [
  (answ === nil ()) &&& (eval goal (nil ()) state !!true);
  fresh (name args xs q4 q5 names pre eff ren q9 newHist)
    (answ === pair name args % xs)
    (newHist === state % hist)
    (q4 === action q5 names pre eff)
    (lookupAction name acts q4)
    (zip names args ren)
    (eval pre ren state !!true)
    (update state eff ren q9)
    (isNew q9 newHist !!true)
    (checker (n+1) acts newHist q9 goal xs)]

let hist_checker acts state goal answ = checker 0 acts (nil ()) state goal answ

(* let hist_checker acts state goal answ =
  let rec ch acts hist state goal answ = checker ch acts hist state goal answ in
  ch acts (nil ()) state goal answ *)

(*
let tabled_hist_checker acts state goal answ =
  Tabling.(tabledrec five) checker acts (nil ()) state goal answ *)
