
Lemma bool_decidable : forall a b : bool,
    {a = b} + {a <> b}.
Proof.
   decide equality.
Defined.

Lemma eq_eq : forall b,
    bool_decidable b b = left (eq_refl b).
Proof.
  now destruct b.
Defined.

Lemma eq_left_eq : forall {A} {a b : A} {p : {a = b} + {a <> b}} {p' : a = b},
    p = left p' -> a = b.
Proof.
  auto.
Qed.

Lemma true_eq : true = true.
Proof.
  exact (eq_left_eq (eq_eq true)).
Qed.
