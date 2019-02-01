open MiniKanren

type compare = LT | EQ | GT
let lt () = !!LT
let eq () = !!EQ
let gt () = !!GT

type env  = Env.t
type 'a l = 'a logic
type 'a i = ('a, 'a l) injected

type ('act, 'obj, 'pred) environment = {
  showAct    : 'act -> string;
  showObj    : 'obj -> string;
  actReifier : env -> 'act i -> 'act l;
  objReifier : env -> 'obj i -> 'obj l;
  objCmp     : 'obj i  -> 'obj i  -> compare i -> goal;
  predCmp    : 'pred i -> 'pred i -> compare i -> goal;

  showPred    : 'pred -> string;
  predReifier : env -> 'pred i -> 'pred l
}
