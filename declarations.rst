.. _declarations:

============
Declarations
============

.. _decl_names:

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
   end a

Basic Declarations
==================

All but the last of these provide straightforward ways of adding new objects to the environment:

* ``constant c : α`` : declares a constant named ``c`` of type ``α``, where ``c`` is a :ref:`declaration name <decl_names>`.
* ``axiom c : α`` : alternative syntax for ``constant``
* ``def c : α := t`` : defines ``c`` to denote ``t``, which should have type ``α``.
* ``theorem c : p := t`` : similar to ``def``, but intended to be used when ``p`` is a proposition.
* ``lemma c : p := t`` : alternative syntax for ``theorem``
* ``example : α := t`` : elaborates ``t`` and checks that it has sort ``α`` (often a proposition), without adding it to the environment.

``constant`` and ``axiom`` have plural versions, ``constants`` and ``axioms``.

In ``def``, ``theorem``, and ``lemma``, the type (``α`` or ``p``, respectively) can be omitted when it can be inferred by Lean. These elements can take a list of arguments before the colon, which are interpreted as a lambda abstraction.

**Examples**

.. code-block :: lean

   constant  c : ℕ
   constants (d e : ℕ) (f : ℕ → ℕ)
   axiom     cd_eq : c = d

   def foo : ℕ := 5
   def bar := 6
   def baz (x y : ℕ) (s : list ℕ) := [x, y] ++ s

   theorem foo_eq_five : foo = 5 := rfl
   lemma   baz_lemma   (x y : ℕ) : baz x y [] = [x, y] := rfl
   theorem baz_theorem (x y : ℕ) : baz x y [] = [x, y] := rfl

.. _Inductive_Definitions:

Inductive Definitions
=====================

(Give syntax for inductive definitions, including nested and mutual definitions.) 

Some Basic Types
================

(Give syntax for natural numbers, bool, unit, product, sum, sigmas, subtypes, lists. Also: propositional connectives and quantifiers. Also bounded quantification. Also number systems, ``fin``.)

The Equation Compiler
=====================

(Define the syntax, explaining patterns and inaccessible terms. Include well founded recursion.)

Match Expressions
=================

(Give the syntax for this, as well as de-structuring ``let`` and ``assume``.)

Structures and Records
======================


