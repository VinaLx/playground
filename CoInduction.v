Require Import Utf8.

CoInductive Stream (A : Type) : Type :=
| st_cons : A → Stream A → Stream A
.

Arguments st_cons [A].

Definition st_head {A} (s : Stream A) :=
  match s with
  | st_cons a _ => a
  end
.


Definition st_tail {A} (s : Stream A) :=
  match s with
  | st_cons _ s' => s'
  end
.

Fixpoint st_drop {A} (n : nat) (s : Stream A) :=
  match n with
  | O => s
  | S n' => st_drop n' (st_tail s)
  end
.

Definition st_at {A : Type} (n : nat) (s : Stream A) := st_head (st_drop n s).

CoInductive stream_eq {A : Type} : Stream A → Stream A → Prop :=
| st_refl : forall s1 s2
  , st_head s1 = st_head s2
  -> stream_eq (st_tail s1) (st_tail s2)
  → stream_eq s1 s2
.

Derive Inversion st_invert with (forall A s1 s2, @stream_eq A s1 s2) Sort Prop.

Check st_invert.

Theorem st_eq_refl : forall A (s : Stream A),
    stream_eq s s.
Proof.
  cofix eq_refl; intros; constructor; auto.
Qed.


Theorem st_eq_sym : forall A (s1 s2: Stream A),
    stream_eq s1 s2 → stream_eq s2 s1.
Proof.
  cofix eq_sym; intros; constructor; destruct H; auto.
Qed.

Theorem st_eq_trans : forall A (s1 s2 s3: Stream A),
    stream_eq s1 s2 → stream_eq s2 s3 → stream_eq s1 s3.
Proof.
  cofix eq_trans; intros; constructor;
    destruct H, H0; [congruence | eauto].
Qed.

Theorem st_eq_at : forall A n (s1 s2 : Stream A),
    stream_eq s1 s2 → st_at n s1 = st_at n s2.
Proof.
  induction n; simpl; intros;
    unfold st_at in *; inversion H; simpl in *; subst; eauto.
Qed.

Lemma st_head_at_0 : forall A (s : Stream A),
    st_head s = st_at 0 s.
Proof.
  auto.
Qed.

Theorem at_st_eq : forall A (s1 s2 : Stream A),
    (forall n, st_at n s1 = st_at n s2) → stream_eq s1 s2.
Proof.
  cofix at_st_eq; intros; constructor.
  - repeat rewrite st_head_at_0. auto.
  - eapply at_st_eq. intros.
    specialize (H (S n)).
    unfold st_at in *; now simpl in H.
Qed.

Inductive list_eq {A} : list A -> list A -> Prop :=
| eq_nil  : list_eq nil nil
| eq_cons : forall x xs ys, list_eq xs ys -> list_eq (cons x xs) (cons x ys)
.

Definition list_eq_ind' : forall A (P : list A → list A → Prop)
  , P nil nil
  → (forall x xs ys, P xs ys -> P (cons x xs) (cons x ys))
  → forall xs ys : list A, list_eq xs ys -> P xs ys :=

  fun A P base ind =>
  fix rec (xs ys : list A) (p : list_eq xs ys) :=
    match p with
    | eq_nil => base
    | eq_cons x xs ys p' => ind x xs ys (rec xs ys p')
    end
.

Theorem steam_eq_ind : forall A (P : Stream A -> Stream A -> Prop)
  , (forall s1 s2 : Stream A
      , P s1 s2 → P (st_tail s1) (st_tail s2) ∧ st_head s1 = st_head s2)
  → forall s1 s2 : Stream A, P s1 s2 -> stream_eq s1 s2.
Proof.
  intros * IH. cofix ind; intros.
  destruct (IH _ _ H) as (Pt & E).
  constructor; eauto.
Qed.

Theorem at_st_eq' : forall A (s1 s2 : Stream A),
    (forall n, st_at n s1 = st_at n s2) → stream_eq s1 s2.
Proof.
  intros.
  apply steam_eq_ind with (P := fun s1 s2 => forall n, st_at n s1 = st_at n s2); auto.
  - intros. split; intros.
    + specialize (H0 (S n)). unfold st_at in *. auto.
    + specialize (H0 0). now repeat rewrite st_head_at_0.
Qed.
