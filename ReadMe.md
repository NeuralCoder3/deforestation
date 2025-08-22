# Deforestation

The code is located in `bin/main.ml`.

Deforestation is one of the big functional programming optimizations.
The objective is to cut out intermediate manifestation of datastructures.

One common example is the sum of the square numbers up to n:
```ocaml
sum (map square (upto 1 n))
``` 
which is ten to a few hundret times slower than
```ocaml
let acc = ref 0 in
for i = 1 to n do
    acc := !acc + i * i
done; !acc
```
or even the functional version
```ocaml
let rec loop acc c = 
    if c > n then acc else loop (acc + c * c) (c + 1)
in loop 0 1
```

The general idea is to cut out the generators and consumers.
Modern techniques generalized this idea to arbitrary 
recursive types, their catamorphisms (fold/consumers) and anamorphisms (generators).

The original idea comes from Philip Wadler from 1981.
The presented technique (at least for lists) boils down to the equations:
```
map f (map g xs ) = map (fun x -> f (g x)) xs
foldr f a (map g xs) = foldr (fun x acc -> f (g x) acc) a xs
map f (gen stop yield next seed) = gen stop (fun x -> f (yield x)) next seed
foldr f a (gen stop yield next seed) = 
  let rec loop acc c = 
    if stop c then acc else loop (f (yield c) acc) (next c)
```

Andy Gill improved on this approach in 1993.
However, they used a rank-2 type with custom functions to rewrite the standard library functions.
The list-functions are automatically (and internally) translated to 
the anamorphism `build` (basically abstracting over the constructors of list)
and the catamorphism `fold` (going through a list replacing the constructors with functions).
The idea is that `foldr a f (x1 :: x2 :: x3 :: nil) = f(x1, f(x2, f(x3, a)))`.
```
foldr k z (build g) = g k z
```

In 2007, the technique was refined to Stream Fusion.
Stream Fusion expresses lifts all list functions to a Stream type
abstracting over the recursion. 
Most functions (except catamorphisms) are non-recursive element-wise transformers on a Stream accumulator.
All list functions can be liftet together with conversions `stream` and `unstream`.
Inlining and other standard optimizations suffice together with
```
stream (unstream s) = s
```

Our code (in `bin/main.ml`) contains all three above approaches in different stages of optimizations together with their runtime.

A good overview can be found in "Deforestation" by Vincent St-Amour 2012 (https://www.ccs.neu.edu/home/amal/course/7480-s12/deforestation-notes.pdf).

## Comparison to imperative languages

Imperative languages usually do not have such optimizations.
For one, the algorithms are usually written in a low-level manner
with mutation (as allocation is either non-ideomatic or tedious)
On the other hand, side effects such as mutation makes it hard to apply such optimizations.

On a low-level, loop fusion comes close to the idea of deforestation.
Multiple iterations over the same data is combined and fused into a single iteration.

Modern imperative languages use a style akin to stream fusion by operating
directly on iterators and chaining the transformations.
This corresponds to going into a stream (the iterator) and chaining the computations.