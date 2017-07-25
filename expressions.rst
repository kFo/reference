===========
Expressions
===========

Universes
=========

Every type in Lean is, by definition, an expression of type ``Sort u`` for some universe level ``u``. A universe level is one of the following:

* a natural number, ``n``
* a universe variable, ``u`` (declared with the command ``universe`` or ``universes``)
* an expression ``u + n``, where ``u`` is a universe level and ``n`` is a natural number
* an expression ``max u v``, where ``u`` and ``v`` are universes
* an expression ``imax u v``, where ``u`` and ``v`` are universe levels

The last one denotes the universe level ``0`` if ``v`` is ``0``, and ``max u v`` otherwise.

**Examples**

.. code-block:: lean

   universes u v

   #check Sort u
   #check Sort 5
   #check Sort (u + 1)
   #check Sort (u + 3)
   #check Sort (max u v)
   #check Sort (max (u + 3) v)
   #check Sort (imax (u + 3) v)
   #check Prop
   #check Type 

Expression Syntax
=================

The set of expressions in Lean is defined inductively as follows:

* ``Sort u`` : the universe of types at universe level ``u``
* ``c`` : where ``c`` is an identifier denoting an axiomatically declared constant or a defined object
* ``x`` : where ``x`` is a variable the local context in which the expression is interpreted
* ``Π x : α, β`` : the type of functions taking an element ``x`` of ``α`` to an element of ``β``, where ``β`` is an expression whose type is a ``Sort``
* ``s t`` : the result of applying ``s`` to ``t``, where ``s`` and ``t`` are expressions
* ``λ x : α, t`` : the function mapping any value ``x`` of type ``α`` to ``t``, where ``t`` is an expression
* ``let x := t in s`` : a local definition, denote the value of ``s`` when ``x`` denotes ``t``

Every well formed term in Lean has a *type*, which itself is an expression of type ``Sort u`` for some ``u``. The fact that a term ``t`` has type ``α`` is written ``t : α``. 

For an expression to be well formed, its components have to satisfy certain typing constraints. These, in turn, determine the type of the resulting term, as follows:

* ``Sort u : Sort (u + 1)``
* ``c : α``, where ``α`` is the type that ``c`` has been declared or defined to have
* ``x : α``, where ``α`` is the type that ``x`` has been assigned in the local context where it is interpreted
* ``(Π x : α, β) : Sort (imax u v)`` where ``α : Sort u``, and ``β : Sort v`` assuming ``x : α`` 
* ``s t : β [t/x]`` where ``s`` has type ``Π x : α, β`` and ``t`` has type ``α``
* ``(λ x : α, t) : Π x : α, β`` if ``t`` has type ``β`` whenever ``x`` has type ``α``
* ``(let x := t in s) : β [t/x]`` where ``s`` has type ``Π x : α, β``, and ``t`` has type ``β`` assuming ``x : α`` 

``Prop`` abbreviates ``Sort 0``, ``Type`` abbreviates ``Sort 1``, and ``Type u`` abbreviates ``Sort (u + 1)`` when ``u`` is a universe variable. We say "``α`` is a type" to express ``α : Type u`` for some ``u``, and we say "``p`` is a proposition" to express ``p : Prop``. Using the *propositions as types* correspondence, given ``p : Prop``, we refer to an expression ``t : p`` as a *proof* of ``p``. In contrast, given ``α : Type u`` for some ``u`` and ``t : α``, we sometimes refer to ``t`` as *data*.

When the expression ``β`` in ``Π x : α, β`` does not depend on ``x``, it can be written ``α → β``. As usual, the variable ``x`` is bound in ``Π x : α, β``, ``λ x : α, t``, and ``let x := t in s``. The expression ``∀ x : α, β`` is alternative syntax for ``Π x : α, β``, and is intended to be used when ``β`` is a proposition. An underscore can be used to generate an internal variable in a binder, as in ``λ _ : α, t``.

In addition to the elements above, expressions can also contain *metavariables*, that is, temporary placeholders, that are used in the process of constructing terms. They can also contain *macros*, which are used to annotate or abbreviate terms. Terms that are added to the environment contain neither metavariable nor variables, which is to say, they are fully elaborated and make sense in the empty context.
 
Constants can be declared in various ways, such as by the ``constant(s)`` and ``axiom(s)`` keywords, or as the result of an ``inductive`` or ``structure`` declarations. Similarly, objects can be defined in various ways, such as using ``def``, ``theorem``, or the equation compiler. See :doc:`declarations` for more information.

Writing an expression ``(t : α)`` forces Lean to elaborate ``t`` so that it has type ``α`` or report an error if it fails.

Lean supports anonymous constructor notation, anonymous projections, and various forms of match syntax, including destructuring ``λ`` and ``let``. These, as well as notation for common data types (like pairs, lists, and so on) are discussed in :doc:`declarations` in connection with inductive types. 

**Examples**

.. code-block:: lean

    universes u v w

    variables (p q : Prop)
    variable  (α : Type u)
    variable  (β : Type v)
    variable  (γ : α → Type w)
    variable  (η : α → β → Type w)

    constants δ ε : Type u
    constants cnst : δ
    constant  f : δ → ε

    variables (a : α) (b : β) (c : γ a) (d : δ)

    variable  g  : α → β
    variable  h  : Π x : α, γ x
    variable  h' : Π x, γ x → δ

    #check Sort (u + 3)
    #check Prop
    #check Π x : α, γ x
    #check f cnst
    #check λ x, h x
    #check λ x, h' x (h x)
    #check (λ x, h x) a
    #check λ _ : ℕ, 5
    #check let x := a in h x

    #check Π x y, η x y
    #check Π (x : α) (y : β), η x y
    #check λ x y, η x y 
    #check λ (x : α) (y : β), η x y 
    #check let x := a, y := b in η x y

    #check (5 : ℕ)
    #check (5 : (λ x, x) ℕ)
    #check (5 : ℤ)
 
Implicit Arguments
==================

When declaring arguments to defined objects in Lean (for example, with ``def``, ``theorem``, ``constant``, ``inductive``, or ``structure``; see :doc:`declarations`) or when declaring variables and parameters in sections (see :doc:`infrastructure`), arguments can be annotated as *explicit* or *implicit*. This determines how expressions containing the object are interpreted.

* ``(x : α)`` : an explicit argument of type ``α``
* ``{x : α}`` : an implicit argument, eagerly inserted
* ``⦃x : α⦄`` or ``{{x : α}}`` : an implicit argument, weakly inserted
* ``[x : α]`` : an implicit argument that should be inferred by type class resolution
* ``(x : α := t)`` : an optional argument, with default value ``t``
* ``(x : α . t)`` : an implicit argument, to be synthesized by tactic ``t``

The name of the variable can be ommitted from a class resolution argument, in which case an internal name is generated.

When a function has an explicit argument, you can nonetheless ask Lean's elaborator to infer the argument automatically, by entering it as an underscore (``_``). Conversely, writing ``@foo`` indicates that all of the arguments to be ``foo`` are to be given explicitly, independent of how ``foo`` was declared.

**Examples** 

.. code-block:: lean

    universe u

    def ex1 (x y z : ℕ) : ℕ := x + y + z

    #check ex1 1 2 3

    def id1 (α : Type u) (x : α) : α := x 

    #check id1 nat 3
    #check id1 _ 3

    def id2 {α : Type u} (x : α) : α := x

    #check id2 3
    #check @id2 ℕ 3
    #check (id2 : ℕ → ℕ)

    def id3 {{α : Type u}} (x : α) : α := x

    #check id3 3
    #check @id3 ℕ 3
    #check (id3 : Π α : Type, α → α)

    class cls := (val : ℕ)
    instance cls_five : cls := ⟨5⟩ 

    def ex2 [c : cls] : ℕ := c.val

    example : ex2 = 5 := rfl

    def ex2a [cls] : ℕ := ex2

    example : ex2a = 5 := rfl

    def ex3 (x : ℕ := 5) := x

    #check ex3 2
    #check ex3
    example : ex3 = 5 := rfl

    meta def ex_tac : tactic unit := tactic.refine ``(5)

    def ex4 (x : ℕ . ex_tac) := x

    example : ex4 = 5 := rfl

Basic Data Types and Assertions
===============================

The core library contains a numboer of basic data types, such as the natural numbers (``ℕ``, or ``nat``), the integers (``ℤ``), the booleans (``bool``), and common operations on these, as well as the usual logical quantifiers and connectives. Some example are given below. A list of common notations and their precedences can be found in a `file <https://github.com/leanprover/lean/blob/master/library/init/core.lean>`_ in the core library. The core library also contains a number of basic data type constructors. Definitions can also be found the `data <https://github.com/leanprover/lean/blob/master/library/init/data>`_ directory of the core library. For more information, see also :doc:`libraries`.

.. code-block:: lean

    /- numbers -/
    section
    variables a b c d : ℕ
    variables i j k : ℤ

    #check a^2 + b^2 + c^2
    #check (a + b)^c ≤ d 
    #check i ∣ j * k 
    end

    /- booleans -/
    section
    variables a b c : bool

    #check a && (b || c)
    end

    /- pairs -/
    section
    variables (a b c : ℕ) (p : ℕ × bool)

    #check (1, 2)
    #check p.1 * 2
    #check p.2 && tt
    #check ((1, 2, 3) : ℕ × ℕ × ℕ)
    end 

    /- lists -/
    section
    variables x y z : ℕ 
    variables xs ys zs : list ℕ
    open list

    #check (1 :: xs) ++ (y :: zs) ++ [1,2,3] 
    #check append (cons 1 xs) (cons y zs)
    #check map (λ x, x^2) [1, 2, 3]
    end

    /- sets -/
    section
    variables s t u : set ℕ

    #check ({1, 2, 3} ∩ s) ∪ ({x | x < 7} ∩ t)
    end

    /- strings and characters -/
    #check "hello world"
    #check 'a'

    /- assertions -/
    #check ∀ a b c n : ℕ, a ≠ 0 ∧ b ≠ 0 ∧ c ≠ 0 ∧ n > 2 → a^n + b^n ≠ c^n
    def unbounded (f : ℕ → ℕ) : Prop := ∀ M, ∃ n, f n ≥ M 

.. _constructors_projections_and_matching:

Constructors, Projections, and Matching
=======================================

Lean foundation, the *Calculus of Inductive Constructions*, supports the declaration of *inductive types*. Such types can have any number of *constructors*, and an associated *eliminator* (or *recursor*). Inductive types with one constructor, known as *structures*, have *projections*. The full syntax of inductive types is described in :doc:`declarations`, but here we describe some syntactic elements that facilitate their use in expressions.

When Lean can infer the type of an expression and it is an inductive type with one constructor, then one can write ``⟨a1, a2, ..., an⟩`` to apply the constructor without naming it. For example, ``⟨a, b⟩`` denotes ``prod.mk a b`` in a context where the expression can be inferred to be a pair, and ``⟨h₁, h₂⟩`` denotes ``and.intro h₁ h₂`` in a context when the expression can be inferred to be a conjunction. The notation will nest constructions automatically, so ``⟨a1, a2, a3⟩`` is interpreted as ``prod.mk a1 (prod.mk a2 a3)`` when the expression is expected to have a type of the form ``α1 × α2 × α3``. (The latter is interpreted as ``α1 × (α2 × α3)``, since the product associates to the right.)    

Similarly, one can use "dot notation" for projections: one can write ``p.fst`` and ``p.snd`` for ``prod.fst p`` and ``prod.snd p`` when Lean can infer that ``p`` is an element of a product, and ``h.left`` and ``h.right`` for ``and.left h`` and ``and.right h`` when ``h`` is a conjunction.

The anonymous projector notation can used more generally for any objects defined in a *namespace* (see :doc:`infrastructure`). For example, if ``l`` has type ``list α`` then ``l.map f`` abbreviates ``list.map f l``, in which ``l`` has been placed at the first argument position where ``list.map`` expects a ``list``.
 
Finally, for data types with one constructor, one destruct an element by pattern matching using the ``let`` and ``assume`` constructs, as in the examples below. Internally, these are interpreted using the ``match`` construct, which is in turn compiled down for the eliminator for the inductive type, as described in :doc:`declarations`. 

.. code-block:: lean

    universes u v
    variables {α : Type u} {β : Type v}

    def p : ℕ × ℤ := ⟨1, 2⟩ 
    #check p.fst 
    #check p.snd 

    def p' : ℕ × ℤ × bool := ⟨1, 2, tt⟩ 
    #check p'.fst
    #check p'.snd.fst
    #check p'.snd.snd

    def swap_pair (p : α × β) : β × α :=
    ⟨p.snd, p.fst⟩

    theorem swap_conj {a b : Prop} (h : a ∧ b) : b ∧ a :=
    ⟨h.right, h.left⟩ 
    
    #check [1, 2, 3].append [2, 3, 4]
    #check [1, 2, 3].map (λ x, x^2)

    example (p q : Prop) : p ∧ q → q ∧ p :=
    λ h, ⟨h.right, h.left⟩ 

    def swap_pair' (p : α × β) : β × α :=
    let (x, y) := p in (y, x) 

    theorem swap_conj' {a b : Prop} (h : a ∧ b) : b ∧ a :=
    let ⟨ha, hb⟩ := h in ⟨hb, ha⟩ 

    def swap_pair'' : α × β → β × α :=
    λ ⟨x, y⟩, (y, x) 

    theorem swap_conj'' {a b : Prop} : a ∧ b → b ∧ a :=
    assume ⟨ha, hb⟩, ⟨hb, ha⟩ 

Structured Proofs
=================

Syntactic sugar is provided for writing structured proof terms:

* ``assume h : p, t`` is sugar for ``λ h : p, t``
* ``have h : p, from s, t`` is sugar for ``(λ h : p, t) s``
* ``suffices h : p, from s, t`` is sugar for ``(λ h : p, s) t`` 
* ``show p, t`` is sugar for ``(t : p)``

As with ``λ``, multiple variables can be bound with ``assume``, and types can be ommitted when they can be inferred by Lean. Lean also allows the syntax ``assume : p, t``, which gives the assumption the name ``this`` in the local context.  Similarly, Lean recognizes the variants ``have p, from s, t`` and ``suffices p, from s, t``, which use the name ``this`` for the new hypothesis.

The notation ``‹p›`` is notation for ``(by assumption : p)``, and can therefore be used to apply hypotheses in the local context.

As noted in :ref:`constructors_projections_and_matching`, anonymous constructors and projections and match syntax can be used in proofs just as in expressions that denote data.

**Examples**

.. code-block:: lean

    example (p q r : Prop) : p → (q ∧ r) → p ∧ q :=
    assume h₁ : p,
    assume h₂ : q ∧ r,
    have h₃ : q, from and.left h₂,
    show p ∧ q, from and.intro h₁ h₃ 

    example (p q r : Prop) : p → (q ∧ r) → p ∧ q :=
    assume : p,
    assume : q ∧ r,
    have q, from and.left this,
    show p ∧ q, from and.intro ‹p› this 

    example (p q r : Prop) : p → (q ∧ r) → p ∧ q :=
    assume h₁ : p,
    assume h₂ : q ∧ r,
    suffices h₃ : q, from and.intro h₁ h₃,
    show q, from and.left h₂ 

Computation
===========

Two expressions that differ up to a renaming of their bound variables are said to be *α-equivalent*, and are treated as syntactically equivalent by Lean.

Every expression in Lean has a natural computational interpretation, unless it involves classical elements that block computation, as described in the next section. The system recognizes the following notions of *reduction*:

* *β-reduction* : An expression ``(λ x, t) s`` β-reduces to ``t[s/x]``, that is, the result of replacing ``x`` by ``s`` in ``t``.
* *ζ-reduction* : An expression ``let x := s in t`` ζ-reduces to ``t[s/x]``.
* *δ-reduction* : If ``c`` is a defined constant with definition ``t``, then ``c`` δ-reduces to to ``t``.
* *ι-reduction* : When a function defined by recursion on an inductive type is applied to an element given by an explicit constructor, the result ι-reduces to the specified function value, as described in :ref:`Inductive_Definitions`.

The reduction relation is transitive, which is to say, is ``s`` reduces to ``s'`` and ``t`` reduces to ``t'``, then ``s t`` reduces to ``s' t'``, ``λ x, s`` reduces to ``λ x, s'``, and so on. If ``s`` and ``t`` reduce to a common term, they are said to be *definitionally equal*. Definitional equality is defined to be the smallest equivalence relation that satisfies all these properties and also includes α-equivalenece and the following two relations:

* *η-equivalence* : An expression ``(λx, t x)`` is η-equivalent to ``t``, assuming ``x`` does not occur in ``t``. 
* *proof irrelevance* : If ``p : Prop``, ``s : p``, and ``t : p``, then ``s`` and ``t`` are  considered to be equivalent.

This last fact reflects the intuition that once we have proved a proposition ``p``, we only care that is has been proved; the proof does nothing more than witness the fact that ``p`` is true.

Definitional equality is a strong notion of equalty of values. Lean's logical foundations sanction treating definitionally equal terms as being the same when checking that a term is well-typed and/or that it has a given type.

The reduction relation is believed to be strongly normalizing, which is to say, every sequence of reductions applied to a term will eventually terminate. The property guarantees that Lean's type-checking algorithm terminates, at least in principle. The consistency of Lean and its soundness with respect to a set-theoretic semantics do not depend on either of these properties.

Lean provides two commands to compute with expressions:

* ``#reduce t`` : use the kernel type-checking procedures to carry out reductions on ``t`` until no more reductions are possible, and show the result
* ``#eval t`` : evaluate ``t`` using a fast bytecode evalator, and show the result

Every computable definition in Lean is compiled to bytecode at definition time. Bytecode evaluation is more liberal than kernel evaluation: types and all propositional information are erased, and functions are evaluated using a stack-based virtual machine. As a result, ``#eval`` is more efficient than ``#reduce,`` and can be used to execute complex programs. In contrast, ``#reduce`` is designed to be small and reliable, and to produce type-correct terms at each step. Bytecode is never used in type checking, so as far as soundness and consistency are concerned, only kernel reduction is part of the trusted computing base.

**Examples**

.. code-block:: lean

    #reduce (λ x, x + 3) 5
    #eval   (λ x, x + 3) 5

    #reduce let x := 5 in x + 3
    #eval   let x := 5 in x + 3

    def f x := x + 3

    #reduce f 5
    #eval   f 5

    #reduce @nat.rec (λ n, ℕ) (0 : ℕ) (λ n recval : ℕ, recval + n + 1) (5 : ℕ)
    #eval   @nat.rec (λ n, ℕ) (0 : ℕ) (λ n recval : ℕ, recval + n + 1) (5 : ℕ)

    def g : ℕ → ℕ 
    | 0     := 0
    | (n+1) := g n + n + 1

    #reduce g 5
    #eval   g 5

    #eval   g 50000

    example : (λ x, x + 3) 5 = 8 := rfl
    example : (λ x, f x) = f := rfl
    example (p : Prop) (h₁ h₂ : p) : h₁ = h₂ := rfl

Note: the combination of proof irrelevance and singleton ``Prop`` elimination in ι-reduction renders the ideal version of definitional equality, as described above, undecidable. Lean's procedure for checking definitional equality is only an approximation to the ideal. It is not transitive, as illustrated by the example below. Once again, this does not compromise the consistency or soundness of Lean; it only means that Lean is more conservative in the terms it recognizes as well typed, and this does not cause problems in practice. Singleton elimination will be discussed in greater detail in :ref:`Inductive_Definitions`.

**Example**

.. code-block:: lean

    def R (x y : unit) := false 
    def accrec := @acc.rec unit R (λ_, unit) (λ _ a ih, ()) ()
    example (h) : accrec h = accrec (acc.intro _ (λ y, acc.inv h)) := rfl
    example (h) : accrec (acc.intro _ (λ y, acc.inv h)) = () := rfl
    example (h) : accrec h = () := sorry   -- rfl fails


Axioms
======

Lean's foundational framework consists of:

* the core elements of the calculus of constructions, with type universes and dependent function types, as described above
* inductive types, as described in :doc:`declarations`. 

In addition, the core library defines (and trusts) the following axiomatic extensions:

* propositional extensionality: ...
* quotients: ...
* choice: ...

The last principle, in conjunction with the others, makes the axiomatic foundation classical. Functions that make use of ``choice`` to produce data are incompatible with a computational interpretation, and do not produce bytecode. They have to be declared ``noncomputable``.

For metaprogramming purposes, Lean also allows the definition of objects which stand outside the object language. These are denoted with the ``meta`` keyword, as described in :doc:`programming`.
