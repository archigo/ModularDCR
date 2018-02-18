module Bitvector

type Bitvector = uint64 [] 

let noSegments n = n/64 + if n%64 <> 0 then 1 else 0 
let noIdx (x : Bitvector) = (Array.length x) * 64 // TODO: I really should represent the actual length. 

let inline bit k = (k >>> 6, k &&& 0x3f)

let copy = Array.copy : Bitvector -> Bitvector
let zero (n : int) = Array.zeroCreate (noSegments n) : Bitvector
let ones (n : int) = 
  let allset = (uint64) -1L
  let a = Array.init (noSegments n) (fun i -> allset)
  if n%64 <> 0 then 
    // Mask off unused elements.
    a.[noSegments n-1] <- (1UL <<< n%64) - 1UL
  a

let length = noIdx
let isZero = Array.forall ((=) 0UL) 

let inline isset k (x : Bitvector) =
  let seg, idx = bit k
  x.[seg] &&& (1UL <<< idx) <> 0UL

let inline seti k (x : Bitvector) =
  let seg, idx = bit k
  x.[seg] <- x.[seg] ||| (1UL <<< idx)

let inline clri k (x : Bitvector) = 
  let seg, idx = bit k
  x.[seg] <- x.[seg] &&& ~~~(1UL <<< idx)

let inline set k x = 
  if isset k x then x
  else 
    let y = copy x
    seti k y
    y
    
let inline clr k x = 
  if not (isset k x) then x
  else 
    let y = copy x
    clri k y
    y

let init n f = 
  let x = zero n
  for i in 0 .. n-1 do
    if f i then
      seti i x 
  x

let inline exists N comp = 
  let mutable i = 0
  let mutable found = false
  while i < N && not found do
    found <- comp i 
    i <- i + 1
  found

let inline forall N comp = 
  exists N (comp >> not) |> not

let inline disjoint (x : Bitvector) (y : Bitvector) = 
  forall (Array.length x) <| fun i ->
    x.[i] &&& y.[i] = 0UL

let inline implies (x : Bitvector) (y : Bitvector) = 
  forall (Array.length x) <| fun i -> 
    x.[i] &&& y.[i] = x.[i]

let ofSeq (n : int) idxs = 
  let x = zero n
  for idx in idxs do
    seti idx x
  x

let _clr_lsb w = w &&& (w-1L)
let _clr_except_lsb w = w &&& -w

let _lsb w = 
  let mutable k = 0
  let w = _clr_except_lsb w
  if w &&& 0xffffffff00000000L <> 0L then k <- k + 32
  if w &&& 0xffff0000ffff0000L <> 0L then k <- k + 16
  if w &&& 0xff00ff00ff00ff00L <> 0L then k <- k + 8
  if w &&& 0xf0f0f0f0f0f0f0f0L <> 0L then k <- k + 4
  if w &&& 0xccccccccccccccccL <> 0L then k <- k + 2
  if w &&& 0xaaaaaaaaaaaaaaaaL <> 0L then k <- k + 1
  k

let inline _count w = 
  let mutable v = int64 w
  let mutable c = 0
  while v <> 0L do 
    v <- v &&& (v - 1L)
    c <- c + 1
  c

let count (x : Bitvector) = 
  Array.fold (fun c w -> c + _count w) 0 x

type Iterator = 
  struct
    val x : Bitvector
    val N : int
    val mutable s : int
    val mutable w : int64
    new (x) = { 
      x = x
      N = Array.length x
      s = -1
      w = 0L
    }
  end

  interface System.Collections.Generic.IEnumerator<int> with
    member __.Current =
      if __.s < __.N then 
        64 * __.s + _lsb __.w 
      else
        raise (System.InvalidOperationException 
          ("Bitvector.iterator dereferenced at PTE"))

  interface System.Collections.IEnumerator with
    member __.Current = 
      (__ :> System.Collections.Generic.IEnumerator<int>).Current :> obj

    member __.MoveNext () = 
      if __.w <> 0L then
        __.w <- _clr_lsb __.w
      while __.w = 0L && __.s+1 < __.N do
        __.s <- __.s + 1
        __.w <- int64 __.x.[__.s] 
      __.w <> 0L 

    member __.Reset () = 
      raise (System.NotSupportedException 
        ("Bitvector.iterator does not support .Reset()"))

  interface System.IDisposable with 
    member __.Dispose () = ()

        
type Iterable = 
  struct 
    val x : Bitvector
    new (x) = { x=x }
  end
  interface System.Collections.Generic.IEnumerable<int> with
    member __.GetEnumerator () =
      new Iterator (__.x) :> System.Collections.Generic.IEnumerator<int> 
  interface System.Collections.IEnumerable with
    member __.GetEnumerator () = 
      new Iterator (__.x) :> System.Collections.IEnumerator

let toSetSeq x = 
  new Iterable (x)

let toSetEnum x = 
  toSetSeq x :> System.Collections.Generic.IEnumerable<int>

let lsb (x : Bitvector) = 
  let mutable i = 0
  let mutable found = false
  let N = Array.length x
  while i < N && not found do 
    found <- x.[i] <> 0UL
    i <- i + 1
  if not found then
    failwith "lsb on zero bitvector"
  else
    (i-1) * 64 + _lsb (int64 x.[i-1])
        
module Operators = 

  let inline (&&&=) (x : Bitvector) (y : Bitvector) = 
    Array.iteri (fun i w -> x.[i] <- x.[i] &&& w) y ; x

  let inline (|||=) (x : Bitvector) (y : Bitvector) = 
    Array.iteri (fun i w -> x.[i] <- x.[i] ||| w) y ; x

  let inline pnegi (x : Bitvector) = 
    Array.iteri (fun i w -> x.[i] <- ~~~w) x ; x
  
  let inline nonzero N comp = 
    exists N (fun i -> comp i <> 0UL) 

  let inline (|||) (x : Bitvector) (y : Bitvector) = Array.map2 (|||) x y : Bitvector
  let inline (&&&) (x : Bitvector) (y : Bitvector) = Array.map2 (&&&) x y : Bitvector
  let inline (~~~) x   = Array.map  (~~~) x   : Bitvector



