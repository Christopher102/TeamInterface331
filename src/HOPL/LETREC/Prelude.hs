module HOPL.LETREC.Prelude (Def, prelude) where

import HOPL.Types (Id, Source)

type Def = (Id, Source)

{-
  IMPORTANT! We will compile prelude definitions in last-to-first order, which
  means that dependencies must follow dependent definitions. As an example,
  both `concat` and `reverse` depend on `append`, hence the definitions for
  `concat` and `reverse` must precede the definition of `append` in the list.
-}

prelude :: [Def]
prelude = zip preludeNames preludeDefs

preludeNames :: [Id]
preludeNames =
  [ "power",
    "factorial",
    "reverse",
    "concat",
    "append"
  ]

preludeDefs :: [Source]
preludeDefs =
  [ "letrec pow(a) = proc(b)\
    \          if zero?(b) then 1\
    \          else *(a, ((pow a) -(b,1)))\
    \in pow",
    "letrec fact(n) =\
    \          if zero?(n) then 1\
    \          else *(n, (fact -(n,1)))\
    \in fact",
    "letrec rev(xs) =\
    \          if null?(xs) then xs\
    \          else ((append (rev cdr(xs))) list(car(xs)))\
    \in rev",
    "letrec cat(xs) =\
    \          if null?(xs) then emptylist\
    \          else ((append car(xs)) (cat cdr(xs)))\
    \in cat",
    "letrec app(xs) = proc(ys)\
    \          if null?(xs) then ys\
    \          else cons(car(xs), ((app cdr(xs)) ys))\
    \in app"
  ]
