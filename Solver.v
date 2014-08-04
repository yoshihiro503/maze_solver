Require Sumbool List Streams.
Import  Sumbool List.
Require Import Util.

Parameters maze node : Set.
Parameter node_dec : forall (x y: node), {x = y} + {x <> y}.
Parameters start goal : node.
Parameters next : node -> list node.
Definition is_next y x := In y (next x).

Inductive path : Set :=
| PUnit (_: node)
| PCons (_:node) (_: path).

Fixpoint plength (p : path) :=
  match p with
  | PUnit _ => O
  | PCons _ p => S (plength p)
  end.
Definition endof (p : path) : node :=
  match p with
  | PUnit x => x
  | PCons y _ => y
  end.

Definition expand (p: path) : list path :=
  map (fun y => PCons y p) (next (endof p)).

Definition path_equiv (p1 p2: path) :=
  endof p1 = endof p2.
Definition path_equiv_dec (p1 p2: path) :
  {path_equiv p1 p2} + {~ path_equiv p1 p2} :=
  node_dec (endof p1) (endof p2).

Fixpoint accessibles (start:node) (len:nat) : list path :=
  match len with
  | O => (PUnit start) :: nil
  | S n' => div_equiv path_equiv_dec (flat_map expand (accessibles start n'))
  end.

Definition goals_len n :=
  filter_dec (fun p => node_dec (endof p) goal) (accessibles start n).

CoFixpoint from n := Streams.Cons n (from (S n)).
Definition goals := Streams.map goals_len (from 0).


Inductive Path (x : node) : path -> Prop :=
| P1 : Path x (PUnit x)
| P2 : forall y p, is_next y (endof p) -> Path x p ->
   Path x (PCons y p).

Lemma expand1 : forall p p',
  In p' (expand p) -> exists y, is_next y (endof p) /\ p' = PCons y p.
Proof.
 unfold expand. intros p p' H.
 destruct (in_map_iff (fun y => PCons y p) (next (endof p)) p') as [H0 _].
 destruct (H0 H) as [y _H].  destruct _H.
 exists y. split; [apply H2 | rewrite H1; reflexivity].
Qed.

Lemma expand2 : forall y p,
  is_next y (endof p) -> In (PCons y p) (expand p).
Proof.
 unfold is_next. intros y p H. apply (in_map (fun y => PCons y p) _ _ H).
Qed.

Lemma expand_length : forall p p',
  In p' (expand p) -> plength p' = S (plength p).
Proof.
 intros p p' H. destruct (expand1 p p' H) as [y H0]. destruct H0. rewrite H1. reflexivity.
Qed.

Lemma path_equiv_refl : forall (p : path), path_equiv p p.
Proof. reflexivity. Qed.

Theorem soundness : forall x n p,
  In p (accessibles x n) -> Path x p /\ plength p = n.
Proof.
induction n; simpl; intros p H.
 destruct H; [ | destruct H ].
 rewrite <- H. split; [ apply P1 | reflexivity ].

 destruct (in_flat_map expand (accessibles x n) p) as [H1 _].
 destruct (H1 (div_In_incl _ _ _ _ _ H)). destruct H0.
 destruct (IHn _ H0).
 destruct (expand1 _ _ H2) as [y _H]. destruct _H.
 rewrite H6. simpl. split; [apply (P2 x _ _ H5 H3)| rewrite H4; reflexivity].
Qed.

Theorem completeness : forall x p,
  Path x p -> exists p0,
    endof p = endof p0 /\ plength p = plength p0 /\ In p0 (accessibles x (plength p)).
Proof.
induction p; simpl; intro H.
 inversion H. exists (PUnit n). simpl.
 split; [ | split; [ | left]]; reflexivity.
 
 cut (exists p1 : _, endof p1 = n /\ plength p1 = plength (PCons n p) /\
   In p1 (flat_map expand (accessibles x (plength p)))).
  intro _H. destruct _H as [p1 _H0]. destruct _H0 as [H01 _H01]. destruct _H01 as [H02 H0].
  destruct (div_In _ _ path_equiv_refl path_equiv_dec p1 _ H0) as [p0 _H0].
  destruct _H0. unfold path_equiv in H1.
  destruct (in_flat_map expand (accessibles x (plength p)) p0) as [HH _].
  destruct (HH (div_In_incl _ _ _ _ _ H2)) as [p2 _H1]. destruct _H1.
  destruct (soundness _ _ _ H3). exists p0.
  split; [ rewrite H1; rewrite H01; reflexivity | ].
  split; [rewrite <- H6; rewrite(expand_length _ _ H4); reflexivity | ].
  solve [apply H2].

  inversion H. destruct (IHp H3) as [p00 _H0]. destruct _H0 as [H4 _H1].
  destruct _H1. exists (PCons n p00). simpl.
  split; [reflexivity|]. split; [ rewrite H5; reflexivity | ].
  apply <- (in_flat_map expand (accessibles x (plength p)) (PCons n p00)).
  exists p00. split; [ apply H6 | ]. rewrite H4 in H2.
  solve [apply (expand2 _ _ H2)].
Qed.

Lemma gloals_len_end : forall p n, In p (goals_len n) -> endof p = goal.
Proof.
 intros. destruct (filter_dec_In _ _ _ H). assumption.
Qed.
