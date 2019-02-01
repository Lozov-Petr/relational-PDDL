open MiniKanren
open MiniKanrenStd
open GT

open Pddl
open Environment

@type blocksWorldAction = PICKUP | PUTDOWN | STACK | UNSTACK with show
@type blocksWorldObject = A | B | C | D | E | F | G with show
@type blocksWorldPredicate = CLEAR | ONTABLE | HANDEMPTY | HOLDING | ON with show
type blocksWorldVar = X | Y

let showAct  x = let s = show(blocksWorldAction   ) x in String.sub s 0 (String.length s - 3)
let showObj  x = let s = show(blocksWorldObject   ) x in String.sub s 0 (String.length s - 3)
let showPred x = let s = show(blocksWorldPredicate) x in String.sub s 0 (String.length s - 3)

let actReifier  = MiniKanren.reify
let objReifier  = MiniKanren.reify
let predReifier = MiniKanren.reify

let objCmp a b r = conde [
  (r === eq ()) &&& (a === b);
  (r === gt ()) &&& conde [
    (a === !!A) &&& conde [b === !!B; b === !!C; b === !!D; b === !!E; b === !!F; b === !!G];
    (a === !!B) &&& conde [b === !!C; b === !!D; b === !!E; b === !!F; b === !!G];
    (a === !!C) &&& conde [b === !!D; b === !!E; b === !!F; b === !!G];
    (a === !!D) &&& conde [b === !!E; b === !!F; b === !!G];
    (a === !!E) &&& conde [b === !!F; b === !!G];
    (a === !!F) &&& (b === !!G)];
  (r === lt ()) &&& conde [
    (a === !!B) &&& (b === !!A);
    (a === !!C) &&& conde [b === !!A; b === !!B];
    (a === !!D) &&& conde [b === !!A; b === !!B; b === !!C];
    (a === !!E) &&& conde [b === !!A; b === !!B; b === !!C; b === !!D];
    (a === !!F) &&& conde [b === !!A; b === !!B; b === !!C; b === !!D; b === !!E];
    (a === !!G) &&& conde [b === !!A; b === !!B; b === !!C; b === !!D; b === !!E; b === !!F]]]

let predCmp a b r = conde [
  (r === eq ()) &&& (a === b);
  (r === gt ()) &&& conde [
    (a === !!CLEAR    ) &&& conde [b === !!ONTABLE; b === !!HANDEMPTY; b === !!HOLDING; b === !!ON];
    (a === !!ONTABLE  ) &&& conde [b === !!HANDEMPTY; b === !!HOLDING; b === !!ON];
    (a === !!HANDEMPTY) &&& conde [b === !!HOLDING; b === !!ON];
    (a === !!HOLDING  ) &&& (b === !!ON)];
  (r === lt ()) &&& conde [
    (a === !!ONTABLE  ) &&& (b === !!CLEAR);
    (a === !!HANDEMPTY) &&& conde [b === !!CLEAR; b === !!ONTABLE];
    (a === !!HOLDING  ) &&& conde [b === !!CLEAR; b === !!ONTABLE; b === !!HANDEMPTY];
    (a === !!ON       ) &&& conde [b === !!CLEAR; b === !!ONTABLE; b === !!HANDEMPTY; b === !!HOLDING]]]



let bw_env = {showAct; showObj; showPred; actReifier; objReifier; predReifier; objCmp; predCmp}

(******************************************)

let pair = MiniKanrenStd.pair

let rec l2ll = function
  | []    -> nil ()
  | x::xs -> x % l2ll xs

(******************************************)

let clear x      = atom !!true !!CLEAR (var x % nil())
let ontable x    = atom !!true !!ONTABLE (var x % nil())
let handempty () = atom !!true !!HANDEMPTY (nil())
let holding x    = atom !!true !!HOLDING (var x % nil())
let on x y       = atom !!true !!ON (var x % (var y % nil()))

let not_clear   x    = atom !!false !!CLEAR (var x % nil())
let not_ontable x    = atom !!false !!ONTABLE (var x % nil())
let not_handempty () = atom !!false !!HANDEMPTY (nil())
let not_holding x    = atom !!false !!HOLDING (var x % nil())
let not_on x y       = atom !!false !!ON (var x % (var y % nil()))

let x = !!X
let y = !!Y

let actions () =
    let a1 = l2ll [clear x; ontable x; handempty ()] in
    let a2 = l2ll [not_ontable x; not_clear x; not_handempty (); holding x] in
    let b1 = l2ll [holding x] in
    let b2 = l2ll [not_holding x; clear x; handempty (); ontable x] in
    let c1 = l2ll [holding x; clear y] in
    let c2 = l2ll [not_holding x; not_clear y; clear x; handempty (); on x y] in
    let d1 = l2ll [on x y; clear x; handempty ()] in
    let d2 = l2ll [holding x; clear y; not_clear x; not_handempty (); not_on x y] in
      [action !!PICKUP  (l2ll[x])   (a1%nil()) a2;
       action !!PUTDOWN (l2ll[x])   (b1%nil()) b2;
       action !!STACK   (l2ll[x;y]) (c1%nil()) c2;
       action !!UNSTACK (l2ll[x;y]) (d1%nil()) d2]

(******************************************)

let clear x      = pair !!CLEAR (!!x % nil())
let ontable x    = pair !!ONTABLE (!!x % nil())
let handempty () = pair !!HANDEMPTY (nil())
let on x y       = pair !!ON (l2ll[!!x;!!y])

let a = A
let b = B
let c = C
let d = D
let e = E
let f = F
let g = G

let init01 () = [clear c; clear a; clear b; clear d; ontable c; ontable a; ontable b; ontable d; handempty ()]
let init02 () = [clear b; ontable d; on b c; on c a; on a d; handempty ()]
let init03 () = [clear a; clear c; clear d; ontable a; ontable b; ontable d; on c b; handempty ()]
let init04 () = [clear d; clear c; ontable d; ontable a; on c e; on e b; on b a; handempty ()]
let init05 () = [clear b; clear e; clear c; ontable d; ontable e; ontable c; on b a; on a d; handempty ()]
let init06 () = [clear d; ontable b; on d e; on e c; on c a; on a b; handempty ()]
let init07 () = [clear d; clear f; ontable c; ontable b; on d a; on a c; on f e; on e b; handempty ()]
let init08 () = [clear a; clear b; clear e; clear c; clear d; ontable f; ontable b; ontable e; ontable c; ontable d; on a f; handempty ()]
let init09 () = [clear a; ontable c; on a d; on d b; on b f; on f e; on e c; handempty ()]
let init10 () = [clear e; ontable d; on e g; on g b; on b a; on a f; on f c; on c d; handempty ()]

(******************************************)

let on x y = atom !!true !!ON (obj !!x%(obj !!y%nil()))
let conj x = l2ll [l2ll x]

let goal01 () = conj [on d c; on c b; on b a]
let goal02 () = conj [on d c; on c a; on a b]
let goal03 () = conj [on a b; on b c; on c d]
let goal04 () = conj [on a e; on e b; on b d; on d c]
let goal05 () = conj [on d c; on c b; on b a; on a e]
let goal06 () = conj [on d c; on c b; on b e; on e a]
let goal07 () = conj [on c b; on b a; on a e; on e f; on f d]
let goal08 () = conj [on e f; on f c; on c b; on b a; on a d]
let goal09 () = conj [on e f; on f a; on a b; on b c; on c d]
let goal10 () = conj [on a g; on g d; on d b; on b c; on c f; on f e]
