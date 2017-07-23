============
Declarations
============

Identifiers
===========

An *atomic name* is an alphanumeric string that does not begin with a numeral. A (hierarchical) *identifier*, or *name*, consists of one or more *atomic names* separated by periods.

**Examples**

.. code-block:: lean

   def foo := 5
   def foo.bar.baz := 6
   def foo.γ1.δ3 := 7

Basic Declarations
==================

All but the last of these provide straightforward ways of adding new objects to the environment:

* ``constant c : α`` : declares a constant named ``c`` of type ``α``, where ``c`` is an identifier
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


