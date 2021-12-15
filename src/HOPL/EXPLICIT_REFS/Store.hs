{-
 -  HOPL/EXPLICIT_REFS/Store.hs
 -
 -  Reference implementation of the toy language EXPLICIT_REFS from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the "world's dumbest model of the store:  the store
 -  is a list and a reference is number which denotes a position in the list."
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.EXPLICIT_REFS.Store
  ( Store,
    emptyStore,
    newref,
    deref,
    setref,
  )
where

import HOPL.EXPLICIT_REFS.DataStructures (StoVal)
import HOPL.Types (Reference)

type Store = [StoVal]

emptyStore :: Store
emptyStore = []

-- Free store represented as a list
newref :: StoVal -> Store -> (Reference, Store)
newref val store = (length store, store ++ [val])

deref :: Reference -> Store -> StoVal
deref ref store = store !! ref

setref :: Reference -> StoVal -> Store -> Store
setref ref val store = take ref store ++ (val : drop (ref + 1) store)
