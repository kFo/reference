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

Basic Syntax
============

The set of expressions in Lean is defined inductively as follows:

* ``Sort u`` : the universe of types at universe level ``u``
* ``c`` : where ``c`` is an identifier denoting an axiomatically declared constant or a defined object
* ``x`` : where ``x`` is a variable the local context in which the expression is interpreted
* ``Π x : α, β`` : the type of functions taking an element ``x`` of ``α`` to an element of ``β``, where ``β`` is an expression whose type is a ``Sort``
* ``s t`` : the result of applying ``s`` to ``t``, where ``s`` and ``t`` are expressions
* ``λ x : α, t`` : the function mapping any value ``x`` of type ``α`` to ``t``, where ``t`` is an expression
* ``let x := t in s``, denoting a local definition, that is, the value of ``s`` when ``x`` denotes ``t``

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

In addition to the elements above, expressions can also contain *metavariables*, that is, temporary placeholders, that are used in the process of constructing terms. They can also contain *macros*, which are used to annotate or abbreviate terms.
Terms that are added to the environment contain neither metavariable nor variables, which is to say, they are fully elaborated and make sense in the empty context.
 
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

When declaring arguments to defined objects in Lean (for example, with ``def``, ``theorem``, ``constant``, ``inductive``, or ``structure``; see [Declarations](declarations.html)) or when declaring variables and parameters in sections (see [Infrastructure](infrastructure.html)), arguments can be annotated as *explicit* or *implicit*. This determines how expressions containing the object are interpred.

* ``(x : α)`` : an explicit argument of type ``α``
* ``{x : α}`` : an implicit argument, eagerly inserted
* ``⦃x : α⦄`` or ``{{x : α}}`` : an implicit argument, weakly inserted
* ``[x : α]`` : an implicit argument, which should be inferred by type class resolution
* ``(x : α := t)`` : an optional argument, with default value ``t``
* ``(x : α . t)`` : an implicit argument, to be synthesized by tactic ``t``

The name of the variable can be ommitted from a class resolution argument, in which case an internal name is generated.

**Examples** 

.. code-block:: lean

    universe u

    def ex1 (x y z : ℕ) : ℕ := x + y + z

    #check ex1 123

    def id1 {α : Type u} (x : α) : α := x

    #check id1 3
    #check (id1 : ℕ → ℕ)

    def id2 {{α : Type u}} (x : α) : α := x

    #check id2 3
    #check (id2 : Π α : Type, α → α)

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


Structured Proofs
=================

Syntactic sugar is provided for writing structured proof terms:

* ``assume h : p, t`` is sugar for ``λ h : p, t``
* ``have h : p, from s, t`` is sugar for ``(λ h : p, t) s``
* ``suffices h : p, from s, t`` is sugar for ``(λ h : p, s) t`` 
* ``show p, t`` is sugar for ``(t : p)``

As with ``λ``, multiple variables can be bound with ``assume``, and types can be ommitted when they can be inferred by Lean. Lean also allows the syntax ``assume : p, t``, which gives the assumption the name ``this`` in the local context.  Similarly, Lean recognizes the variants ``have p, from s, t`` and ``suffices p, from s, t``, which use the name ``this`` for the new hypothesis.

The notation ``‹p›`` is notation for ``(by assumption : p)``, and can therefore be used to apply hypotheses in the local context.

Anonymous constructor notation, anonymous projections, and match syntax, as described in :doc:`declarations`, is useful for writing proofs as well as defining data.

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

(Explain the notions of reduction and evaluation.)

Axioms
======

Lean's foundational framework consists of:

* the core syntax of the calculus of constructions, as described above
* inductive types, as described in Chapter *Declarations*. 

In addition, the core library defines (and trusts) the following axiomatic extension:

* propositional extensionality: ...
* quotients: ...
* choice: ...

The last principle, in conjunction with the others, makes the axiomatic foundation classical. Functions that make use of ``choice`` to produce data are incompatible with a computational interpretation, and do not produce bytecode. They have to be declared ``noncomputable``.

(Say something about the ``meta`` keyword.)


