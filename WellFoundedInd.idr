module WellFoundedInd

data Acc : (a -> a -> Type) -> a -> Type where
  MkAcc : {a : Type} -> (x : a) -> ((y : a) -> r y x -> Acc r y) -> Acc r x

total
accRec : {P : a -> Type}
      -> ((x : a) -> ((y : a) -> r y x -> P y) -> P x)
      -> {x : a} -> Acc r x -> P x
accRec rec (MkAcc x f) = rec x $ \y, ryx => accRec rec (f y ryx)

total
wellFounded : (r : a -> a -> Type) -> Type
wellFounded {a} r = (x : a) -> Acc r x

total
wfRec : {P : a -> Type}
     -> ((x : a) -> ((y : a) -> r y x -> P y) -> P x)
     -> wellFounded r -> (x : a) -> P x
wfRec rec wf x = accRec rec (wf x)

total
LengthLT : List a -> List a -> Type
LengthLT xs ys = LT (length xs) (length ys)

total
ltZeroAbsurd : LTE (S n) 0 -> a
ltZeroAbsurd lt impossible

total
lteSplit : LTE a b -> Either (a = b) (LT a b)
lteSplit {b} LTEZero with (b)
  | Z = Left Refl
  | S b' = Right (LTESucc LTEZero)
lteSplit (LTESucc lte) with (lteSplit lte)
  | Left e = rewrite e in Left Refl
  | Right lt = Right (LTESucc lt)

total
lengthLTSplit : LengthLT xs (y :: ys) -> Either (length xs = length ys) (LengthLT xs ys)
lengthLTSplit lt with (lteSplit lt)
  | Left e = rewrite succInjective _ _ e in Left Refl
  | Right (LTESucc lt') = Right lt'

total
wfLengthLT : {a : Type} -> wellFounded (LengthLT {a})
wfLengthLT Nil = MkAcc _ $ \_, yltx => ltZeroAbsurd yltx
wfLengthLT (x :: xs) with (wfLengthLT xs)
  | MkAcc _ f = MkAcc _ $ \y, yltx => case lengthLTSplit yltx of
    Left e => MkAcc _ $ \y', ylty => f y' (rewrite (sym e) in ylty)
    Right lt => f y lt

total
quickSort : Ord a => List a -> List a
quickSort {a} = wfRec {P = const (List a)} quickSort' wfLengthLT
  where
    quickSort' : (xs : List a) -> ((ys : List a) -> LengthLT ys xs -> List a) -> List a
    quickSort' [] _ = []
    quickSort' (x :: xs) rec
      =  rec (filter (\v => v <= x) xs) (LTESucc (filterSmaller _))
      ++ [x]
      ++ rec (filter (\v => v > x) xs) (LTESucc (filterSmaller _))

total
example : quickSort [5, 2, 3, 1, 4] = [1, 2, 3, 4, 5]
example = Refl
