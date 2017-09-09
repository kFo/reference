=======
Tactics
=======

Tactic Mode
===========

Anywhere an expression is expected, Lean will accept a sequence of instructions bracketed by the keywords ``begin`` and ``end``. The input between these keywords represents a *tactic*, typically a compound sequence of basic tactics, each possibly applied to suitable arguments, separated by commas. When processing such a tactic block, Lean's elaborator executes the compound tactic with the expectation that it will produce an expression of the required type.

Individual tactics act on one or more *goals*, each of the form ``a : α ⊢ p``, where ``a : α`` is a context and ``p`` is the target type. Tactics are typically used to prove a theorem, in which case ``p`` is a ``Prop``, but they can be used to construct an element of an arbitrary ``Type`` as well. 

At the outset, the elaborator presents the tactic block with a goal that consists of the local context in which the expression is being elaborated together with its expected type. Individual tactics can change goals and introduce new subgoals. A sequence of tactics is done when no subgoals remain, that is, when the compound tactic has succeeded in constructing an expression of the requisite type. 

Tactics can fail. For example, a tactic may fail to make progress, or may not be appropriate to the goal. Other tactics can catch or handle those failures (see :numref:`tactic_combinators`), but otherwise an error message is presented to the user.

Results produced by tactics are checked by the kernel for correctness. This provides another possible point of failure: a tactic block can, in principle, claim success but produce a term that fails to type check.

Tactics are themselves Lean expressions of a special ``tactic`` type. This makes it possible to implement Lean tactics in Lean itself; see :numref:`Chapter %s <metaprogramming>`. Tactics in a ``begin ... end`` block, however, are parsed in a special *interactive mode* that provides a more convenient manner of expression. In this section, we will focus exclusively on this interactive syntax.

You can use the keyword ``by`` instead of ``begin ... end`` to invoke a single tactic rather than a comma-separated sequence.

.. code-block:: lean

    example (p q : Prop) : p ∧ q → q ∧ p :=
    begin
      intro h,
      cases h,
      split,
      repeat { assumption }
    end

    example (p q : Prop) : p ∧ q → q ∧ p :=
    assume ⟨h₁, h₂⟩,
    and.intro (by assumption) (by assumption)

The documentation below coincides with documentation strings that are stored in the Lean source files and displayed by editors. The argument types are as follows:

- ``id`` : an :ref:`identifier <identifiers>`
- ``expr`` : an :ref:`expression <expression_syntax>`
- ``<binders>`` : a sequence of identifiers and expressions ``(a : α)`` where ``a`` is an identifier and ``α`` is a ``Type`` or a ``Prop``.

An annotation ``t?`` means that the argument ``t`` is optional, and an annotation ``t*`` means any number of instances, possibly none. Many tactics parse arguments with additional tokens like ``with``, ``at``, ``only``, ``*``, or ``⊢``, as indicated below. The token ``*`` is typically used to denote all the hypotheses, and ``⊢`` is typically used to denote the goal.

.. _basic_tactics:

Basic Tactics
=============

``intro id?``

    This tactic applies to a goal that is either a Pi/forall or starts with a let binder.

    If the current goal is a Pi / forall ``∀ x : T, U`` (resp. ``let x := t in U``) then ``intro`` puts ``x : T`` (resp. ``x := t``) in the local context. The new subgoal target is ``U``.

    If the goal is an arrow ``T → U``, then it puts ``h : T`` in the local context and the new goal target is ``U``.

    If the goal is neither a Pi/forall nor begins with a let binder, the tactic ``intro`` applies the tactic ``whnf`` until the tactic ``intro`` can be applied or the goal is not head reducible.

``intros id*``

    Similar to ``intro`` tactic. The tactic ``intros`` will keep introducing new hypotheses until the goal target is not a Pi/forall or let binder.

    The variant ``intros h_1 ... h_n`` introduces ``n`` new hypotheses using the given identifiers to name them.

``introv id*``

    The tactic ``introv`` allows the user to automatically introduce the variables of a theorem and explicitly name the hypotheses involved. The given names are used to name non-dependent hypotheses.

    Examples:
    
    .. code-block:: lean

        example : ∀ a b : nat, a = b → b = a :=
        begin
        introv h,
        exact h.symm
        end
    
    The state after ``introv h`` is
    
    .. code-block:: text

        a b : ℕ,
        h : a = b
        ⊢ b = a
    
    .. code-block:: lean

        example : ∀ a b : nat, a = b → ∀ c, b = c → a = c :=
        begin
        introv h₁ h₂,
        exact h₁.trans h₂
        end
    
    The state after ``introv h₁ h₂`` is
    
    .. code-block:: text

        a b : ℕ,
        h₁ : a = b,
        c : ℕ,
        h₂ : b = c
        ⊢ a = c

``rename id id``

    The tactic ``rename h₁ h₂`` renames hypothesis ``h₁`` to ``h₂`` in the current local context.

``apply expr``

    The ``apply`` tactic applies to any goal. The argument term is a term well-formed in the local context of the main goal. The tactic tries to match the current goal against the conclusion of the type of term. If it succeeds, then the tactic returns as many subgoals as the number of premises that have not been fixed by type inference or type class resolution. Non dependent premises are added before dependent ones.

    The ``apply`` tactic uses higher-order pattern matching, type class resolution, and first-order unification with dependent types.

``fapply expr``

    Similar to the ``apply`` tactic, but also creates subgoals for dependent premises that have not been fixed by type inference or type class resolution.

``eapply expr``

    Similar to the ``apply`` tactic, but only creates subgoals for non-dependent premises that have not been fixed by type inference or type class resolution.

``apply_with expr (tactic.apply_cfg)``

    Similar to the ``apply`` tactic, but allows the user to provide a ``apply_cfg`` configuration object.

``apply_instance``

    This tactic tries to close the main goal ``... ⊢ U`` by generating a term of type ``U`` using type class resolution.

``refine expr``

    This tactic applies to any goal. It behaves like ``exact``, but with a big difference: the user can leave some holes ``_`` in the term, and ``refine`` will generate as many subgoals as there are holes.

    Note that some holes may be implicit. The type of each hole must either be synthesized by the system or declared by an explicit type ascription like (e.g. ``(_ : nat → Prop)``).

``assumption``

    This tactic looks in the local context for a hypothesis whose type is equal to the goal target. If it finds one, it uses it to prove the goal, and otherwise it fails.

``change expr (with expr)? (at (* | (⊢ | id)*))?``

    This tactic applies to any goal. ``change U`` replaces the target ``T`` of the main goal to ``U`` provided that ``U`` is well formed with respect to the local context of the main goal and ``T`` and ``U`` are definitionally equal. 

    ``change U at h`` will change a local hypothesis to ``U``. 

    ``change A with B at h1 h2 ...`` will replace ``A`` with ``B`` in all the supplied hypotheses (or ``*``), or in the goal if no ``at`` clause is specified, provided that ``A`` and ``B`` are definitionally equal.

``exact expr``

    This tactic applies to any goal, providing an exact proof term. If ``T`` is the goal and ``p`` is a term of type ``U`` then ``exact p`` succeeds iff ``T`` and ``U`` are definitionally equal.

``exacts ([expr, ...] | expr)``

    Like ``exact``, but takes a list of terms and checks that all goals are discharged after the tactic.

``revert id*``

    ``revert h₁ ... hₙ`` applies to any goal with hypotheses ``h₁ ... hₙ``. It moves the hypotheses and their dependencies to the target of the goal. This tactic is the inverse of `intro`.

(To do: add ``ex_falso``, ``generalize``, ``trivial``, ``admit``, ``contradiction``.)

.. _structured_tactic_proofs:

Structured Tactic Proofs
========================

Tactic blocks can have nested ``begin ... end`` blocks and, equivalently, blocks ``{ ... }`` enclosed with curly braces. Opening such a block focuses on the current goal, so that no other goals are visible within the nested block. Closing a block while any subgoals remain results in an error.

``assume (: expr | <binders>)``

    Assuming the target of the goal is a Pi or a let, ``assume h : T`` unifies the type of the binder with ``T`` and introduces it with name ``h``, just like ``intro h``. If ``h`` is absent, the tactic uses the name ``this``. If ``T`` is omitted, it will be inferred. 

    ``assume (h₁ : T₁) ... (hₙ : Tₙ)`` introduces multiple hypotheses. Any of the types may be omitted, but the names must be present.

``have id? (: expr)? (:= expr)?``

    ``have h : T := p`` adds the hypothesis ``h : T`` to the current goal if ``p`` a term of type ``T``. If ``T`` is omitted, it will be inferred. 

    ``have h : T`` adds the hypothesis ``h : T`` to the current goal and opens a new subgoal with target ``T``. The new subgoal becomes the main goal. If ``T`` is omitted, it will be replaced by a fresh metavariable.

    If ``h`` is omitted, the name ``this`` is used.

``let id? (: expr)? (:= expr)?``

    ``let h : T := p`` adds the hypothesis ``h : T := p`` to the current goal if ``p`` a term of type ``T``. If `T` is omitted, it will be inferred.

    ``let h : T`` adds the hypothesis ``h : T := ?M`` to the current goal and opens a new subgoal ``?M : T``. The new subgoal becomes the main goal. If ``T`` is omitted, it will be replaced by a fresh metavariable.

    If ``h`` is omitted, the name ``this`` is used.

``suffices id? (: expr)?``

    ``suffices h : T`` is the same as ``have h : T, tactic.swap``. In other words, it adds the hypothesis ``h : T`` to the current goal and opens a new subgoal with target ``T``.

``show expr``

    ``show T`` finds the first goal whose target unifies with ``T``. It makes that the main goal, performs the unification, and replaces the target with the unified version of ``T``. 

``from expr``

    A synonym for ``exact`` that allows writing ``have/suffices/show ..., from ...`` in tactic mode. 

.. code-block :: lean

    variables (p q : Prop)

    example : p ∧ (p → q) → q ∧ p :=
    begin
      assume h : (p ∧ (p → q)),
      have h₁ : p, from and.left h,
      have : p → q := and.right h,
      suffices : q, from and.intro this h₁,
      show q, from ‹p → q› h₁ 
    end

    example (p q : Prop) : p → p → p :=
    begin
      assume h (h' : p),
      from h
    end

    example : ∃ x, x = 5 :=
    begin
      let u := 3 + 2,
      existsi u, reflexivity
    end

.. _tactics_for_inductive_types:

Tactics for Inductive Types
===========================

(To do: ``induction``, ``cases``, ``destruct``, ``left``, ``right``, ``split``, ``constructor``, ``existsi``, etc.)

.. _tactic_combinators:

Tactic Combinators
==================

.. _the_rewriter:

The Rewriter
============

(also unfolding definitions)

.. _the_simplifier:

The Simplifier
==============


The SMT State
=============

