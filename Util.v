Require Import List.
Require Import Sumbool.
Definition atat {A B:Type} (f: A -> B) x := f x.
Infix "@@" := atat (at level 60).
Definition doll {A B C: Type} (g: B -> C) (f: A -> B) (x: A) := g (f x).
Infix "$" := doll (at level 60).
  
Fixpoint filter_dec {A : Type} {P Q: A -> Prop}
  (f : forall x, {P x} + {Q x}) (l:list A) : list A :=
  match l with 
  | nil => nil
  | x :: l => if (f x) then x::(filter_dec f l) else filter_dec f l
  end.

Lemma filter_dec_In : forall {A: Type} {P Q: A -> Prop} (f : forall x, {P x} + {Q x}) (x:A) (l: list A),
  In x (filter_dec f l) -> In x l /\ P x.
Proof.
intros A P Q f.
 induction l; simpl.
  intro H; elim H.
  
  case (f a); simpl in |- *.
   intros H _H; elim _H; intros.
    split; [ left | rewrite <- H0 in |- * ]; assumption.
    
    elim (IHl H0); intros.
    split; [ right | idtac ]; assumption.
    
   intros _ H; elim (IHl H); intros.
   split; [ right | idtac ]; assumption.
Qed.

Lemma filter_dec_length : forall {A: Type} {P Q: A -> Prop} (f : forall x, {P x} + {Q x}) (xs: list A),
  length (filter_dec f xs) <= length xs.
Proof.
intros A P Q f xs; induction xs; simpl.
 apply Le.le_refl.

 case (f a); intro; simpl.
  apply Le.le_n_S; apply IHxs.

  apply le_S; apply IHxs.
Qed.

Lemma filter_dec_In_not1 : forall {A: Type} {P: A -> Prop} (f : forall x, {P x} + {~P x}) (x:A) (xs: list A),
  In x xs -> P x -> In x (filter_dec f xs).
Proof.
intros A P f x xs; induction xs; simpl.
intro H; elim H.
intros.
case (f a).

elim H; intro HH; [rewrite HH; left; reflexivity | right; apply (IHxs HH H0)].
elim H; intro HH; [rewrite HH; intro HH0; elim HH0; apply H0 | intros _; apply (IHxs HH H0)].
Qed.

Lemma filter_dec_In_not2 : forall {A: Type} {P: A -> Prop} (f : forall x, {~P x} + {P x}) (x:A) (xs: list A),
  In x xs -> ~ P x -> In x (filter_dec f xs).
Proof.
intros A P f x xs; induction xs; simpl.
intro H; elim H.
intros.
case (f a).

elim H; intro HH; [rewrite HH; left; reflexivity | right; apply (IHxs HH H0)].
elim H; intro HH; [rewrite HH; intro HH0; elim H0; apply HH0 | intros _; apply (IHxs HH H0)].
Qed.

Section Equiv.
  Variable A : Type.
  Variable R : A -> A -> Prop.

  Variable R_refl : forall x, R x x.

  Section EquivAux.
    Variable neqdec : forall (x y: A), {~R x y} + {R x y}.
    Require Import Recdef.
    Function div_equiv_aux (xs : list A) {measure length xs} : list A :=
      match xs with
      | nil => nil
      | x :: xs => x :: div_equiv_aux (filter_dec (neqdec x) xs)
    end.
    intros.
    simpl.
    apply Lt.le_lt_n_Sm.
    apply filter_dec_length.
    Defined.

    Lemma div_In_aux : forall (x:A) (xs: list A),
      In x xs -> exists x':A, R x' x /\ In x' (div_equiv_aux xs).
    Proof.
    intros x xs; functional induction (div_equiv_aux xs).

     intro H; elim H.

     intro H0; elim H0; intro H.
     rewrite H; exists x.
     split; [apply R_refl | left; reflexivity].

     case (neqdec x0 x); intro HH.

      elim (IHl (filter_dec_In_not2 (neqdec x0) _ _ H HH)).
      intros x' _H; inversion _H; exists x'.
      split; [apply H1 | right; apply H2].

     exists x0; split; [apply HH | left; reflexivity].
    Qed.

    Lemma div_In_incl_aux : forall (x:A) (xs: list A),
      In x (div_equiv_aux xs) -> In x xs.
    Proof.
    intros x xs; functional induction (div_equiv_aux xs).
     intro H; elim H.

     simpl; intro _H; elim _H; intro HH; [left; apply HH | right].
 
     elim (filter_dec_In _ _ _ (IHl HH)).
     intros H1 _; apply H1.
    Qed.
  End EquivAux.

  Variable eqdec : forall (x y: A), {R x y} + {~R x y}.
  Definition div_equiv xs := div_equiv_aux (fun x y => sumbool_not _ _ (eqdec x y)) xs.

  Lemma div_In : forall (x:A) (xs: list A),
    In x xs -> exists x':A, R x' x /\ In x' (div_equiv xs).
  Proof.
  apply div_In_aux.
  Qed.
  Lemma div_In_incl : forall (x:A) (xs: list A),
    In x (div_equiv xs) -> In x xs.
  Proof.
  apply div_In_incl_aux.
  Qed.
End Equiv.
Implicit Arguments div_equiv [A R].
