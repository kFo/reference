.. _declarations:

============
Declarations
============

.. _declaration_names:

Declaration Names
=================

A declaration name is a :ref:`hierarchical identifier <identifiers>` that is interpreted relative to the current namespace as well as (during lookup) to the set of open namespaces.

.. code-block:: lean

   namespace a
     constant b.c : ℕ
     #print b.c -- constant a.b.c : ℕ
   end a

   #print a.b.c -- constant a.b.c : ℕ
   open a
   #print b.c -- constant a.b.c : ℕ

Declaration names starting with an underscore are reserved for internal use. Names starting with the special atomic name ``_root_`` are interpreted as absolute names.

.. code-block:: lean

   constant a : ℕ
   namespace a
     constant a : ℤ
     #print _root_.a -- constant a : ℕ
     #print a.a  --
   end a

Contexts and Telescopes
=======================

When processing user input, Lean first parses text to a raw expression format. It then uses background information and type constants to disambiguate overloaded symbols and infer implicit arguments, resulting in a fully-formed expression. This process is known as *elaboration*.

As hinted in :ref:`expression_syntax`, expressions are parsed and elaborated with respect to an *environment* and a *local context*. Roughly speaking, an environment represents the state of Lean at the point where an expression is parsed, including previously declared axioms, constants, definitions, and theorems. In a given environment, a *local context* consists of a sequence ``(a₁ : α₁) (a₂ : α₂) ... (aₙ : αₙ)`` where each ``aᵢ`` is a name denoting a local constant and each ``αᵢ`` is an expression of type ``Sort u`` for some ``u`` which can involve elements of the environment and the local constants ``aⱼ`` for ``j < i``. 

Intuitively, a local context is a list of variables that are held constant while an expression is being elaborated. Consider the following example:

.. code-block:: lean

   def (a b : ℕ) : ℕ → ℕ := λ c, a + (b + c)

Here the expression ``λ c, a + (b + c)`` is elaborated in the context ``(a : ℕ) (b : ℕ`` and the expression ``a + (b + c)`` is elaborated in the context ``(a : ℕ) (b : ℕ) (c : ℕ)``. If you replace the expression ``a + (b + c)`` with an underscore, the error message from Lean will include the current *goal*:

.. code-block:: text

   a b c : ℕ
   ⊢ ℕ

Here ``a b c : ℕ`` indicates the local context, and the second ``ℕ`` indicates the expected type of the result.

A *context* is sometimes called a *telescope*, but the latter is used more generally to include a sequence of declarations occuring relative to a given context. For example, relative to the context ``(a₁ : α₁) (a₂ : α₂) ... (aₙ : αₙ)``, the types ``βᵢ`` in a telescope ``(b₁ : β₁) (b₂ : β₂) ... (bₙ : βₙ)`` can refer to ``a₁, ..., aₙ``. Thus a context can be viewed as a telescope relative to the empty context.

Telescopes are often used to describe a list of arguments, or parameters, to a declaration. In such cases, it is often notationally convenient to let ``(a : α)`` stand for a telescope rather than just a single argument. In general, the annotations described in :ref:`implicit_arguments` can be used to mark arguments as implicit.

.. _basic_declarations:

Basic Declarations
==================

Lean provides ways of adding new objects to the environment. The following provide straightforward ways of declaring new objects:

* ``constant c : α`` : declares a constant named ``c`` of type ``α``, where ``c`` is a :ref:`declaration name <declaration_names>`.
* ``axiom c : α`` : alternative syntax for ``constant``
* ``def c : α := t`` : defines ``c`` to denote ``t``, which should have type ``α``.
* ``theorem c : p := t`` : similar to ``def``, but intended to be used when ``p`` is a proposition.
* ``lemma c : p := t`` : alternative syntax for ``theorem``

It is sometimes useful to be able to simulate a definition or theorem without naming it or adding it to the environment.

* ``example : α := t`` : elaborates ``t`` and checks that it has sort ``α`` (often a proposition), without adding it to the environment.

``constant`` and ``axiom`` have plural versions, ``constants`` and ``axioms``.

In ``def``, the type (``α`` or ``p``, respectively) can be omitted when it can be inferred by Lean. Constants declared with ``theorem`` or ``lemma`` are marked as ``irreducible``. 

Any of ``def``, ``theorem``, ``lemma``, or ``example`` can take a list of arguments (that is, a context) before the colon. If ``(a : α)`` is a context, the definition ``def foo (a : α) : β := t`` is interpreted as ``def foo : Π a : α, β := λ a : α, t``. Similarly, a theorem ``theorem foo (a : α) : p := t`` is interpreted as ``theorem foo : ∀ a : α, p := assume a : α, t``. (Remember that ``∀`` is syntactic sugar for ``Π``, and ``assume`` is syntactic sugar for ``λ``.)

.. code-block:: lean

   constant  c : ℕ
   constants (d e : ℕ) (f : ℕ → ℕ)
   axiom     cd_eq : c = d

   def foo : ℕ := 5
   def bar := 6
   def baz (x y : ℕ) (s : list ℕ) := [x, y] ++ s

   theorem foo_eq_five : foo = 5 := rfl
   theorem baz_theorem (x y : ℕ) : baz x y [] = [x, y] := rfl
   lemma baz_lemma (x y : ℕ) : baz x y [] = [x, y] := rfl

   example (x y : ℕ) : baz x y [] = [x, y] := rfl
   
.. _inductive_types:

Inductive Types
===============

Lean's axiomatic foundation allows users to declare arbitrary inductive families, following the pattern described by [Dybjer]_. To make the presentation more manageable, we first describe inductives *types*, and then describe the generalization to inductive *families* in the next section. The declaration of an inductive type has the following form:

.. code-block:: text

   inductive foo (a : α) : Sort u
   | constructor₁ : Π (b : β₁), foo
   | constructor₂ : Π (b : β₂), foo
   ...
   | constructorₙ : Π (b : βₙ), foo

Here ``(a : α)`` is a context and each ``(b : βᵢ)`` is a telescope in the context ``(a : α)`` together with ``(foo : Sort u)``, subject to the following constraints.

Suppose the telescope ``(b : βᵢ)`` is ``(b₁ : βᵢ₁) ... (bᵤ : βᵢᵤ)``. Each argument in the telescope is either *nonrecursive* or *recursive*.

- An argument ``(bⱼ : βᵢⱼ)`` is *nonrecursive* if it does not refer to ``foo,`` the inductive type being defined. In that case, ``βᵢ`` can be any type, so long as it does not refer to any nonrecursive arguments.

- An argument ``(bⱼ : βᵢⱼ)`` is *recursive* if it is of the form ``Π (d : δ), foo`` where ``(d : δ)`` is a telescope which does not refer to ``foo`` or any nonrecursive arguments.

The inductive type ``foo`` represents a type that is freely generated by the constructors. Each constructor can take arbitrary data and facts as arguments (the nonrecursive arguments), as well as indexed sequences of elements of ``foo`` that have been previously constructed (the recursive arguments). In set theoretic models, such sets can be represented by well-founded trees labeled by the constructor data, or they can defined using other transfinite or impredicative means.

The declaration of the type ``foo`` as above results in the addition of the following constants to the environment:

- the *type former* ``foo : Π (a : α), Sort u``
- for each ``i``, the *constructor* ``foo.constructorᵢ : Π (a : α) (b : βᵢ), foo a``
- the *eliminator* ``foo.rec``, which takes arguments

  + ``(a : α)`` (the parameters)
  + ``{C : foo a → Type u}`` (the *motive* of the elimination)
  + for each ``i``, the *minor premise* corresponding to ``constructorᵢ``
  + ``(x : foo)`` (the *major premise*)

  and returns an element of ``C x``. Here, The ith minor premise is a function which takes

  +  ``(b : βᵢ)`` (the arguments to the constructor)
  + an argument of type ``Π (d : δ), C (bⱼ d)`` corresponding to each recursive argument ``(bⱼ : βᵢⱼ)``, where ``βᵢⱼ``  is of the form ``Π (d : δ), foo`` (the recursive values of the function being defined)

  and returns an element of ``C (constructorᵢ a b)``, the intended value of the function at ``constructorᵢ a b``.

The eliminator represents a principle of recursion: to construct an element of ``C x`` where ``x : foo a``, it suffices to consider each of the cases where ``x`` is of the form ``constructorᵢ a b`` and to provide an auxiliary construction in each case. In the case where some of the arguments to ``constructorᵢ`` are recursive, we can assume that we have already constructed values of ``C y`` for each value ``y`` constructed at an earlier stage. 

Under the propositions-as-type correspondence, when ``C x`` is an element of ``Prop``, the eliminator represents a principle of induction. In order to show ``∀ x, C x``, it suffices to show that ``C`` holds for each constructor, under the inductive hypothesis that it holds for all recursive inputs to the constructor.

The eliminator and constructors satisfy the following identities, in which all the arguments are shown explicitly. Suppose we set ``F := foo.rec a C f₁ ... fₙ``. Then for each constructor, we have the definitional reduction:

.. code-block :: text
  
   F (constructorᵢ a b) = fᵢ b ... (λ d : δᵢⱼ, F (bⱼ d)) ...

where the ellipses include one entry for each recursive argument.

The type former, constructors, and eliminator are all part of Lean's axiomatic foundation, which is to say, they are part of the trusted kernel.

Below are some common examples of inductive types, many of which are defined in the core library.

.. code-block:: lean

  namespace hide
  universes u v

  -- BEGIN
  inductive empty : Type

  inductive unit : Type
  | star : unit

  inductive bool : Type
  | ff : bool
  | tt : bool

  inductive prod (α : Type u) (β : Type v) : Type (max u v)
  | mk : α → β → prod

  inductive sum (α : Type u) (β : Type v)
  | inl : α → sum
  | inr : β → sum

  inductive sigma (α : Type u) (β : α → Type v)
  | mk : Π a : α, β a → sigma

  inductive false : Prop

  inductive true : Prop
  | trivial : true

  inductive and (p q : Prop) : Prop 
  | intro : p → q → and

  inductive or (p q : Prop) : Prop
  | inl : p → or
  | inr : q → or

  inductive Exists (α : Type u) (p : α → Prop) : Prop
  | intro : ∀ x : α, p x → Exists

  inductive subtype (α : Type u) (p : α → Prop) : Type u
  | intro : ∀ x : α, p x → subtype

  inductive nat : Type
  | zero : nat
  | succ : nat → nat

  inductive list (α : Type u)
  | nil : list 
  | cons : α → list → list

  -- full binary tree with nodes and leaves labeled from α 
  inductive bintree (α : Type u)
  | leaf : α → bintree
  | node : bintree → α → bintree → bintree

  -- every internal node has subtrees indexed by ℕ
  inductive cbt (α : Type u) 
  | leaf : α → cbt
  | node : (ℕ → cbt) → cbt
  -- END
  end hide

Note that in the syntax of the inductive definition ``foo``, the context ``(a : α)`` is left implicit. In other words, constructors and recursive arguments are written as though they have return type ``foo`` rather than ``foo a``.

Elements of the context ``(a : α)`` can be marked implicit as described in :ref:`implicit_arguments`. These annotations bear only on the type former, ``foo``. Lean uses a heuristic to determine which arguments to the constructors should be marked implicit, namely, an argument is marked implicit if it can be inferred from the type of a subsequent argument. If the annotation ``{}`` appears after the constructor, a argument is marked implicit if it can be inferred from the type of a subsequent argument *or the return type*. For example, it is useful to let ``nil`` denote the empty list of any type, since the type can usually be inferred in the context in which it appears. These heuristics are imperfect, and you may sometimes wish to define your own constructors in terms of the default ones. In that case, use the ``[pattern]`` :ref:`attribute <attributes>` to ensure that these will be used appropriately by the :ref:`equation compiler <equation_compiler>`.

There are restrictions on the universe ``u`` in the return type ``Sort u`` of the type former. There are also resrictions on the universe ``u`` in the return type ``Sort u`` of the motive of the eliminator. These will be discussed in the next section in the more general setting of inductive families.

Lean allows some additional syntactic conveniences. You can omit the return type of the type former, ``Sort u``, in which case Lean will infer the minimal possible nonzero value for ``u``. As with function definitions, you can list arguments to the constructors before the colon. In an enumerated type (that is, one where the constructors have no arguments), you can also leave out the return type of the constructors. 

.. code-block:: lean

  namespace hide
  universe u

  -- BEGIN
  inductive weekday
  | sunday | monday | tuesday | wednesday 
  | thursday | friday | saturday

  inductive nat 
  | zero
  | succ (n : nat) : nat

  inductive list (α : Type u)
  | nil {} : list
  | cons (a : α) (l : list) : list

  @[pattern]
  def list.nil' (α : Type u) : list α := list.nil

  def length {α : Type u} : list α → ℕ
  | (list.nil' .(α)) := 0
  | (list.cons a l) := 1 + length l
  -- END

  end hide

.. _inductive_families:

Inductive Families
==================

In fact, Lean implements a slight generalization of the inductive types described in the previous section, namely, inductive *families*. The declaration of an inductive family in Lean has the following form:

.. code-block:: text

   inductive foo (a : α) : Π (c : γ), Sort u
   | constructor₁ : Π (b : β₁), foo t₁ 
   | constructor₂ : Π (b : β₂), foo t₂ 
   ...
   | constructorₙ : Π (b : βₙ), foo tₙ

Here ``(a : α)`` is a context, ``(c : γ)`` is a telescope in context ``(a : γ)``, each ``(b : βᵢ)`` is a telescope in the context ``(a : α)`` together with ``(foo : Π (c : γ), Sort u)`` subject to the constraints below, and each ``tᵢ`` is a tuple of terms in the context ``(a : α) (b : βᵢ)`` having the types ``γ``. Instead of defining a single inductive type ``foo a``, we are now defining a family of types ``foo a c`` indexed by elements ``c : γ``.  Each constructor, ``constructorᵢ``, places its result in the type ``foo a tᵢ``, the member of the family with index ``tᵢ``. 

The modifications to the scheme in the previous section are straightforward. Suppose the telescope ``(b : βᵢ)`` is ``(b₁ : βᵢ₁) ... (bᵤ : βᵢᵤ)``.

- As before, an argument ``(bⱼ : βᵢⱼ)`` is *nonrecursive* if it does not refer to ``foo,`` the inductive type being defined. In that case, ``βᵢ`` can be any type, so long as it does not refer to any nonrecursive arguments.

- An argument ``(bⱼ : βᵢⱼ)`` is *recursive* if it is of the form ``Π (d : δ), foo s`` where ``(d : δ)`` is a telescope which does not refer to ``foo`` or any nonrecursive arguments and ``s`` is a tuple of terms in context ``(a : α)`` and the previous nonrecursive ``bⱼ``'s with types ``γ``.

The declaration of the type ``foo`` as above results in the addition of the following constants to the environment:

- the *type former* ``foo : Π (a : α) (c : γ), Sort u``
- for each ``i``, the *constructor* ``foo.constructorᵢ : Π (a : α) (b : βᵢ), foo a tᵢ``
- the *eliminator* ``foo.rec``, which takes arguments

  + ``(a : α)`` (the parameters)
  + ``{C : Π (c : γ), foo a c → Type u}`` (the motive of the elimination)
  + for each ``i``, the minor premise corresponding to ``constructorᵢ``
  + ``(x : foo a)`` (the major premsise) 

  and returns an element of ``C x``. Here, The ith minor premise is a function which takes

  +  ``(b : βᵢ)`` (the arguments to the constructor)
  + an argument of type ``Π (d : δ), C s (bⱼ d)`` corresponding to each recursive argument ``(bⱼ : βᵢⱼ)``, where ``βᵢⱼ``  is of the form ``Π (d : δ), foo s``

  and returns an element of ``C tᵢ (constructorᵢ a b)``.

Suppose we set ``F := foo.rec a C f₁ ... fₙ``. Then for each constructor, we have the definitional reduction, as before:

.. code-block :: text
  
   F (constructorᵢ a b) = fᵢ b ... (λ d : δᵢⱼ, F (bⱼ d)) ...

where the ellipses include one entry for each recursive argument.

The following are examples of inductive families.

.. code-block:: lean

  namespace hide
  universe u

  -- BEGIN
  inductive vector (α : Type u) : ℕ → Type u
  | nil  : vector 0
  | succ : Π n, vector n → vector (n + 1)

  -- 'is_prod s n' means n is a product of elements of s
  inductive is_prod (s : set ℕ) : ℕ → Prop
  | base : ∀ n ∈ s, is_prod n
  | step : ∀ m n, is_prod m → is_prod n → is_prod (m * n)

  inductive eq {α : Sort u} (a : α) : α → Prop
  | refl : eq a
  -- END

  end hide

We can now describe the constraints on the return type of the type former, ``Sort u``. We can always take ``u`` to be ``0``, in which case we are defining an inductive family of propositions. If ``u`` is nonzero, however, it must satisfy the following constraint: for each type ``βᵢⱼ : Sort v`` ocurring in the constructors, we must have ``u ≥ v``. In the set-theoretic interpretation, this ensures that the universe in which the resulting type resides is large enough to contain the inductively generated family, given the number of distinctly-labeled constructors. The restriction does not hold for inductively defined propositions, since these contain no data.

Putting an inductive family in ``Prop``, however, does impose a restriction on the eliminator. Generally speaking, for an inductive family in ``Prop``, the motive in the eliminator is required to be in ``Prop``. But there is an exception to this rule: you are allowed to eliminate from an inductively defined ``Prop`` to an arbitrary ``Sort`` when there is only one constructor, and each argument to that constructor is either in ``Prop`` or an index. The intuition is that in this case the elimination does not make use of any information that is not already given by the mere fact that the type of argument is inhabited. This special case is known as *singleton elimination*.

Mutual and Nested Inductive Definitions
=======================================

.. _equation_compiler:

The Equation Compiler
=====================

(Define the syntax, explaining patterns and inaccessible terms. Include well founded recursion.)

Match Expressions
=================

(Give the syntax for this, as well as de-structuring ``let`` and ``assume``.)

Structures and Records
======================


Type Classes
============

(Classes and instances. Anonymous instances. Local instances.) 


.. [Dybjer] Dybjer, Peter, *Inductive Families*. Formal Aspects of Computing 6, 1994, pages 440-465.
