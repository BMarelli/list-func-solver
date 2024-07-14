# The FList ADT
Here we define the specification of the FList ADT.

```
adt FList (A :: Set) where
    import Nat, String, NonEmpty, Funcs, MonadFL

    length :: FList A -> Nat
    fromList :: List A -> FList A
    toList :: FList A -> List A

    apply :: Funcs -> FList A -> MonadFL (FList A)
    
    zero :: Orientation -> FList A -> MonadFL (FList A)
    succesor :: Orientation -> FList A -> MonadFL (FList A)
    delete :: Orientation -> FList A -> MonadFL (FList A)
    rep :: NonEmpty Funcs -> FList A -> MonadFL (FList A)


lenght <> = 0
lenght <x1, ..., xn> = n

fromList [] = <>
fromList [x1, ..., xn] = <x1, ..., xn>

toList <> = []
toList <x1, ..., xn> = [x1, ..., xn]

apply (Zero o) l = zero o l
apply (Succ o) l = succesor o l
apply (Delete o) l = delete o l
apply (Rep fns) l = rep fns l
apply (Void) l = l

zero L <> = return <0>
zero L <x1, ..., xn> = return <0, x1, ..., xn>
zero R <> = return <0>
zero R <x1, ..., xn> = return <x1, ..., xn, 0>

succesor L <> = failFL "Empty list"
succesor L <x1, ..., xn> = return <x1 + 1, x2, ..., xn>
succesor R <> = failFL "Empty list"
succesor R <x1, ..., xn> = return <x1, ..., xn, xn + 1>

delete L <> = failFL "Empty list"
delete L <x1, ..., xn> = return <x2, ..., xn>
delete R <> = failFL "Empty list"
delete R <x1, ..., xn> = return <x1, ..., xn-1>

rep (f :| fs) <> = failFL "List too short"
rep (f :| fs) <x1> = failFL "List too short"
rep (f :| fs) <x1, ..., xn> = return <x1, ..., xn> if x1 == xn, x > 1
rep (f :| fs) <x1, ..., xn> = rep (f :| fs) ((f :| fs) <x1, ..., xn>)
                              if x1 != xn, x > 1
```

The FList ADT is defined in the [FList.hs](src/FList/FList.hs) file. Also see an example of an implementation of the FList ADT in the [List.hs](src/FList/List.hs) file.
