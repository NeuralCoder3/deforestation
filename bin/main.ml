let time f =
  let start = Sys.time () in
  let result = f () in
  let stop = Sys.time () in
  (result, stop -. start)

let n = 1000000

let rec unfoldr f a =
  match f a with
  | None -> []
  | Some (x, y) -> x :: unfoldr f y

let rec foldr f z = function 
  | [] -> z
  | x :: xs -> f x (foldr f z xs)

let rec map f = function
  | [] -> []
  | x :: xs -> f x :: map f xs

let upto from limit =
    unfoldr (fun x -> if x <= limit then Some (x, x + 1) else None) from

let sum = foldr (+) 0


(* Philip Wadler deforestation 1981 *)
let rec gen stop yield next seed =
  if stop seed then [] else yield seed :: gen stop yield next (next seed)

let upto2 from limit = gen (fun x -> x > limit) Fun.id ((+) 1) from

let baseline () = sum (map (fun x -> x * x) (upto 1 n))
let imperative () = 
  let acc = ref 0 in
  for i = 1 to n do
    acc := !acc + i * i
  done;
  !acc
let functional () = 
  let rec loop acc c = 
    if c > n then acc else loop (acc + c * c) (c + 1)
  in loop 0 1

let wadler1 () = sum (map (fun x -> x * x) (upto2 1 n))
(* 
map f (map g xs ) = map (fun x -> f (g x)) xs
foldr f a (map g xs) = foldr (fun x acc -> f (g x) acc) a xs
map f (gen stop yield next seed) = gen stop (fun x -> f (yield x)) next seed
foldr f a (gen stop yield next seed) = 
  let rec loop acc c = 
    if stop c then acc else loop (f (yield c) acc) (next c)
*)
(* inline *)
let wadler2 () = foldr (+) 0 (map (fun x -> x * x) (gen (fun x -> x > n) Fun.id ((+) 1) 1))
(* map gen *)
let wadler3 () = foldr (+) 0 (gen (fun x -> x > n) (fun x -> x * x) ((+) 1) 1)
(* foldr map *)
let wadler4 () = foldr (fun x acc -> x * x + acc) 0 (gen (fun x -> x > n) Fun.id ((+) 1) 1)
(* foldr gen *)
let wadler5 () = 
  let rec loop acc c = 
    if c > n then acc else loop (acc + c * c) (c + 1)
  in loop 0 1

(* Andy Gill build-foldr 1993 *)
(* foldr k z (build g) = g k z *)
(* rank 2 type: 
https://ocaml.org/manual/5.2/polymorphism.html#s%3Ahigher-rank-poly
https://soap.coffee/~lthms/posts/RankNTypesInOCaml.html
*)
(* let build (g: type b. (a -> b -> b) -> b -> b) : a list = *)
let build g =
  g (fun x acc -> x :: acc) []

(* [@unboxed] *)
type 'a list_consumer = {list_consumer: 'b. ('a -> 'b -> 'b) -> 'b -> 'b}
(* [@@unboxed] *)
let build_safe {list_consumer} = list_consumer (fun x acc -> x :: acc) []
(* [@@unboxed] *)

(* let rec upto' from limit cons nil = if from > limit then nil else cons from (upto' (from + 1) limit cons nil) *)
let rec upto' from limit =
  let rec f x cons nil = if x > limit then nil else cons x (f (x + 1) cons nil) in 
  f from
let upto_gill from limit = build (upto' from limit)
let map_gill f xs = build (fun cons nil -> foldr (fun x acc -> cons (f x) acc) nil xs)
let gill1 () = sum (map_gill (fun x -> x * x) (upto_gill 1 n))
(* placed in *)
let gill2 () = foldr (+) 0 (build (fun cons nil -> foldr (fun x acc -> cons (x*x) acc) nil (build (upto' 1 n))))
let gill2_safe () = foldr (+) 0 (build_safe {list_consumer = (fun cons nil -> foldr (fun x acc -> cons (x*x) acc) nil 
    (build (let rec f x cons nil = if x > n then nil else cons x (f (x + 1) cons nil) in f 1)))})
(* foldr build *)
let gill3 () = (fun cons nil -> foldr (fun x acc -> cons (x*x) acc) nil (build (upto' 1 n))) (+) 0
let gill4 () = (fun cons nil -> (upto' 1 n) (fun x acc -> cons (x*x) acc) nil) (+) 0
let gill5 () = 
      let rec upto' from limit = if from > limit then 0 else (from*from) + (upto' (from + 1) limit)
      in upto' 1 n


(* Stream Fusion 2007 *)
type ('a,'s) step = Done | Yield of 'a * 's | Skip of 's
type 'a stream = Stream : ('s -> ('a,'s) step) * 's -> 'a stream
(* or record/module for encapsulation *)
let stream xs = 
  let next = function 
    | [] -> Done
    | x :: xs -> Yield (x, xs)
  in
  Stream (next, xs)

let unstream (Stream (next, s)) =
  let rec unfold s =
    match next s with
    | Done -> []
    | Skip s -> unfold s
    | Yield (x, s) -> x :: unfold s
  in unfold s

let stream_map f (Stream (next_arg, s)) =
  let next s =
    match next_arg s with
    | Done -> Done
    | Skip s' -> Skip s'
    | Yield (x, s') -> Yield (f x, s')
  in
  Stream (next, s)

let map_stream f xs = xs |> stream |> stream_map f |> unstream

let stream_foldr (f:'a -> 'b -> 'b) (z:'b) (Stream (next, s)) =
  let rec foldr s =
    match next s with
    | Done -> z
    | Skip s -> foldr s
    | Yield (x, s) -> f x (foldr s)
  in foldr s

let foldr_stream (f:'a -> 'b -> 'b) (z:'b) (xs:'a list) = xs |> stream |> stream_foldr f z

let stream_upto from limit =
  let next n =
    if n > limit then Done
    else Yield (n, n + 1)
  in Stream (next, from)
let upto_stream from limit = stream_upto from limit |> unstream
let sum_stream xs = foldr_stream (+) 0 xs

let stream1 () = sum_stream (map_stream (fun x -> x * x) (upto_stream 1 n))
let stream2 () = 
  upto_stream 1 n
  |> map_stream (fun x -> x * x) 
  |> sum_stream 
(* inline *)
let stream3 () = 
  stream_upto 1 n |> unstream
  |>  stream |> stream_map (fun x -> x * x) |> unstream 
  |> stream |> stream_foldr (+) 0 
(* stream unstream *)
let stream4 () = 
  stream_upto 1 n 
  |> stream_map (fun x -> x * x) 
  |> stream_foldr (+) 0 

let stream5 () = 
  let next1 x = if x > n then Done else Yield (x, x + 1) in 
  let start1 = 1 in

  let f x = x * x in
  let next2 s =
    match next1 s with
    | Done -> Done
    | Skip s' -> Skip s'
    | Yield (x, s') -> Yield (f x, s')
  in
  let start2 = start1 in

  let f = (+) in
  let rec foldr s =
    match next2 s with
    | Done -> 0
    | Skip s -> foldr s
    | Yield (x, s) -> f x (foldr s)
  in foldr start2


let stream6 () = 
  let next1 x = if x > n then Done else Yield (x, x + 1) in 
  let start1 = 1 in

  let f1 x = x * x in
  let start2 = start1 in

  let f2 = (+) in
  let rec foldr s =
    match next1 s with
    | Done -> 0
    | Skip s -> foldr s
    | Yield (x, s) -> f2 (f1 x) (foldr s)
  in foldr start2


let stream7 () = 
  let rec foldr s =
    match if s > n then Done else Yield (s, s + 1) with
    | Done -> 0
    | Skip s -> foldr s
    | Yield (x, s) -> x*x+ (foldr s)
  in foldr 1

let stream8 () = 
  let rec foldr s =
    if s > n then 0 else s*s + (foldr (s + 1))
  in foldr 1

let () =
  let functions = [
      ("baseline", baseline); 
      ("imperative", imperative);
      ("functional", functional);
      ("wadler1", wadler1);
      ("wadler2", wadler2);
      ("wadler3", wadler3);
      ("wadler4", wadler4);
      ("wadler5", wadler5);
      ("gill1", gill1);
      ("gill2", gill2);
      ("gill2_safe", gill2_safe);
      ("gill3", gill3);
      ("gill4", gill4);
      ("gill5", gill5);
      ("stream1", stream1);
      ("stream2", stream2);
      ("stream3", stream3);
      ("stream4", stream4);
      ("stream5", stream5);
      ("stream6", stream6);
      ("stream7", stream7);
      ("stream8", stream8)
  ] in
  functions |> List.iter (fun (name, f) ->
    let result, time = time f in
    print_endline (Printf.sprintf "%s: %d, %f" name result time)
  )