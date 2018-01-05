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

An annotation ``t?`` means that the argument ``t`` is optional, and an annotation ``t*`` means any number of instances, possibly none. Many tactics parse arguments with additional tokens like ``with``, ``at``, ``only``, ``*``, or ``⊢``, as indicated below. The token ``*`` is typically used to denote all the hypotheses, and ``⊢`` is typically used to denote the goal, with ascii equivalent ``|-``.

.. _basic_tactics:

Basic Tactics
=============

``intro id?``

    If the current goal is a Pi / forall ``∀ x : t, u`` (resp. ``let x := t in u``) then ``intro`` puts ``x : t`` (resp. ``x := t``) in the local context. The new subgoal target is ``u``.

    If the goal is an arrow ``t → u``, then it puts ``h : t`` in the local context and the new goal target is ``u``.

    If the goal is neither a Pi/forall nor begins with a let binder, the tactic ``intro`` applies the tactic ``whnf`` until the tactic ``intro`` can be applied or the goal is not head reducible. In the latter case, the tactic fails.

``intros id*``

    Similar to ``intro`` tactic. The tactic ``intros`` will keep introducing new hypotheses until the goal target is not a Pi/forall or let binder.

    The variant ``intros h₁ ... hₙ`` introduces ``n`` new hypotheses using the given identifiers to name them.

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

    The ``apply`` tactic tries to match the current goal against the conclusion of the type of term. The argument term should be a term well-formed in the local context of the main goal. If it succeeds, then the tactic returns as many subgoals as the number of premises that have not been fixed by type inference or type class resolution. Non-dependent premises are added before dependent ones.

    The ``apply`` tactic uses higher-order pattern matching, type class resolution, and first-order unification with dependent types.

``fapply expr``

    Similar to the ``apply`` tactic, but does not reorder goals.

``eapply expr``

    Similar to the ``apply`` tactic, but only creates subgoals for non-dependent premises that have not been fixed by type inference or type class resolution.

``apply_with expr (tactic.apply_cfg)``

    Similar to the ``apply`` tactic, but allows the user to provide a ``apply_cfg`` configuration object.

``apply_instance``

    This tactic tries to close the main goal ``... ⊢ t`` by generating a term of type ``t`` using type class resolution.

``refine expr``

    This tactic behaves like ``exact``, but with a big difference: the user can put underscores ``_`` in the expression as placeholders for holes that need to be filled, and ``refine`` will generate as many subgoals as there are holes.

    Note that some holes may be implicit. The type of each hole must either be synthesized by the system or declared by an explicit type ascription like ``(_ : nat → Prop)``.

``assumption``

    This tactic looks in the local context for a hypothesis whose type is equal to the goal target. If it finds one, it uses it to prove the goal, and otherwise it fails.

``change expr (with expr)? (at (* | (⊢ | id)*))?``

    ``change u`` replaces the target ``t`` of the main goal to ``u`` provided that ``t`` is well formed with respect to the local context of the main goal and ``t`` and ``u`` are definitionally equal. 

    ``change u at h`` will change a local hypothesis to ``u``. 

    ``change t with u at h1 h2 ...`` will replace ``t`` with ``u`` in all the supplied hypotheses (or ``*``), or in the goal if no ``at`` clause is specified, provided that ``t`` and ``u`` are definitionally equal.

``exact expr``

    This tactic provides an exact proof term to solve the main goal. If ``t`` is the goal and ``p`` is a term of type ``u`` then ``exact p`` succeeds if and only if ``t`` and ``u`` can be unified.

``exacts ([expr, ...] | expr)``

    Like ``exact``, but takes a list of terms and checks that all goals are discharged after the tactic.

``revert id*``

    ``revert h₁ ... hₙ`` applies to any goal with hypotheses ``h₁ ... hₙ``. It moves the hypotheses and their dependencies to the target of the goal. This tactic is the inverse of `intro`.

``generalize id? : expr = id``

    ``generalize : e = x`` replaces all occurrences of ``e`` in the target with a new hypothesis ``x`` of the same type.

    ``generalize h : e = x`` in addition registers the hypothesis ``h : e = x``.

``admit``

    Closes the main goal using ``sorry``.

``contradiction``

    The contradiction tactic attempts to find in the current local context an hypothesis that is equivalent to an empty inductive type (e.g. ``false``), a hypothesis of the form ``c_1 ... = c_2 ...`` where ``c_1`` and ``c_2`` are distinct constructors, or two contradictory hypotheses.

``trivial``

    Tries to solve the current goal using a canonical proof of ``true``, or the ``reflexivity`` tactic, or the `contradiction` tactic.

``exfalso``

    Replaces the target of the main goal by ``false``.

``clear id*``

    ``clear h₁ ... hₙ`` tries to clear each hypothesis ``hᵢ`` from the local context.

``specialize expr``

    The tactic ``specialize h a₁ ... aₙ`` works on local hypothesis ``h``. The premises of this hypothesis, either universal quantifications or non-dependent implications, are instantiated by concrete terms coming either from arguments ``a₁`` ... ``aₙ``. The tactic adds a new hypothesis with the same name ``h := h a₁ ... aₙ`` and tries to clear the previous one.

``by_cases expr (with id)?``

    ``by_cases p with h`` splits the main goal into two cases, assuming ``h : p`` in the first branch, and ``h : ¬ p`` in the second branch.

    This tactic requires that ``p`` is decidable. To ensure that all propositions are decidable via classical reasoning, use  ``local attribute classical.prop_decidable [instance]``.

``by_contradiction id?``

    If the target of the main goal is a proposition ``p``, ``by_contradiction h`` reduces to goal to proving ``false`` using the additional hypothesis ``h : ¬ p``. If ``h`` is omitted, a name is generated automatically.

    This tactic requires that ``p`` is decidable. To ensure that all propositions are decidable via classical reasoning, use  ``local attribute classical.prop_decidable [instance]``.

``by_contra id?``

    An abbreviation for ``by_contradiction``.


Equality and Other Relations
============================

``reflexivity``

    This tactic applies to a goal whose goal has the form ``t ~ u`` where ``~`` is a reflexive relation, that is, a relation which has a reflexivity lemma tagged with the attribute ``[refl]``. The tactic checks whether ``t`` and ``u`` are definitionally equal and then solves the goal.

``refl``

    Shorter name for the tactic ``reflexivity``.

``symmetry``

    This tactic applies to a goal whose target has the form ``t ~ u`` where ``~`` is a symmetric relation, that is, a relation which has a symmetry lemma tagged with the attribute ``[symm]``. It replaces the goal with ``u ~ t``.

``transitivity ?expr``

    This tactic applies to a goal whose target has the form ``t ~ u`` where ``~`` is a transitive relation, that is, a relation which has a transitivity lemma tagged with the attribute ``[trans]``.

    ``transitivity s`` replaces the goal with the two subgoals ``t ~ s`` and ``s ~ u``. If ``s`` is omitted, then a metavariable is used instead.


.. _structured_tactic_proofs:

Structured Tactic Proofs
========================

Tactic blocks can have nested ``begin ... end`` blocks and, equivalently, blocks ``{ ... }`` enclosed with curly braces. Opening such a block focuses on the current goal, so that no other goals are visible within the nested block. Closing a block while any subgoals remain results in an error.

``assume (: expr | <binders>)``

    Assuming the target of the goal is a Pi or a let, ``assume h : t`` unifies the type of the binder with ``t`` and introduces it with name ``h``, just like ``intro h``. If ``h`` is absent, the tactic uses the name ``this``. If ``T`` is omitted, it will be inferred. 

    ``assume (h₁ : t₁) ... (hₙ : tₙ)`` introduces multiple hypotheses. Any of the types may be omitted, but the names must be present.

``have id? (: expr)? (:= expr)?``

    ``have h : t := p`` adds the hypothesis ``h : t`` to the current goal if ``p`` a term of type ``t``. If ``t`` is omitted, it will be inferred. 

    ``have h : t`` adds the hypothesis ``h : t`` to the current goal and opens a new subgoal with target ``t``. The new subgoal becomes the main goal. If ``t`` is omitted, it will be replaced by a fresh metavariable.

    If ``h`` is omitted, the name ``this`` is used.

``let id? (: expr)? (:= expr)?``

    ``let h : T := p`` adds the hypothesis ``h : t := p`` to the current goal if ``p`` a term of type ``t``. If `t` is omitted, it will be inferred.

    ``let h : t`` adds the hypothesis ``h : t := ?M`` to the current goal and opens a new subgoal ``?M : t``. The new subgoal becomes the main goal. If ``t`` is omitted, it will be replaced by a fresh metavariable.

    If ``h`` is omitted, the name ``this`` is used.

``suffices id? (: expr)?``

    ``suffices h : t`` is the same as ``have h : t, tactic.swap``. In other words, it adds the hypothesis ``h : t`` to the current goal and opens a new subgoal with target ``t``.

``show expr``

    ``show t`` finds the first goal whose target unifies with ``t``. It makes that the main goal, performs the unification, and replaces the target with the unified version of ``t``. 

``from expr``

    A synonym for ``exact`` that allows writing ``have/suffices/show ..., from ...`` in tactic mode. 

.. code-block :: lean

    variables (p q : Prop)

    example : p ∧ (p → q) → q ∧ p :=
    begin
      assume h : p ∧ (p → q),
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

Inductive Types
===============

The following tactics are designed specifically to work with elements on an inductive type.

``induction expr (using id)? (with id*)? (generalizing id*)?``

    Assuming ``x`` is a variable in the local context with an inductive type, ``induction x`` applies induction on ``x`` to the main goal, producing one goal for each constructor of the inductive type, in which the target is replaced by a general instance of that constructor and an inductive hypothesis is added for each recursive argument to the constructor. If the type of an element in the local context depends on ``x``, that element is reverted and reintroduced afterward, so that the inductive hypothesis incorporates that hypothesis as well.

    For example, given ``n : nat`` and a goal with a hypothesis ``h : P n`` and target ``Q n``, ``induction n`` produces one goal with hypothesis ``h : P 0`` and target ``Q 0``, and one goal with hypotheses ``h : P (nat.succ a)`` and ``ih₁ : P a → Q a`` and target ``Q (nat.succ a)``. Here the names ``a`` and ``ih₁`` ire chosen automatically.

    ``induction e``, where ``e`` is an expression instead of a variable, generalizes ``e`` in the goal, and then performs induction on the resulting variable.

    ``induction e with y₁ ... yₙ``, where ``e`` is a variable or an expression, specifies that the sequence of names ``y₁ ... yₙ`` should be used for the arguments to the constructors and inductive hypotheses, including implicit arguments. If the list does not include enough names for all of the arguments, additional names are generated automatically. If too many names are given, the extra ones are ignored. Underscores can be used in the list, in which case the corresponding names are generated automatically.

    ``induction e using r`` allows the user to specify the principle of induction that should be used. Here ``r`` should be a theorem whose result type must be of the form ``C t``, where ``C`` is a bound variable and ``t`` is a (possibly empty) sequence of bound variables

    ``induction e generalizing z₁ ... zₙ``, where ``z₁ ... zₙ`` are variables in the local context, generalizes over ``z₁ ... zₙ`` before applying the induction but then introduces them in each goal. In other words, the net effect is that each inductive hypothesis is generalized.

``cases (id :)? expr (with id*)?``

    Assuming ``x`` is a variable in the local context with an inductive type, ``cases x`` splits the main goal, producing one goal for each constructor of the inductive type, in which the target is replaced by a general instance of that constructor. If the type of an element in the local context depends on ``x``, that element is reverted and reintroduced afterward, so that the case split affects that hypothesis as well.

    For example, given ``n : nat`` and a goal with a hypothesis ``h : P n`` and target ``Q n``, ``cases n`` produces one goal with hypothesis ``h : P 0`` and target ``Q 0``, and one goal with hypothesis ``h : P (nat.succ a)`` and target ``Q (nat.succ a)``. Here the name ``a`` is chosen automatically.

    ``cases e``, where ``e`` is an expression instead of a variable, generalizes ``e`` in the goal, and then cases on the resulting variable.

    ``cases e with y₁ ... yₙ``, where ``e`` is a variable or an expression, specifies that the sequence of names ``y₁ ... yₙ`` should be used for the arguments to the constructors, including implicit arguments. If the list does not include enough names for all of the arguments, additional names are generated automatically. If too many names are given, the extra ones are ignored. Underscores can be used in the list, in which case the corresponding names are generated automatically.

    ``cases h : e``, where ``e`` is a variable or an expression, performs cases on ``e`` as above, but also adds a hypothesis ``h : e = ...`` to each hypothesis, where ``...`` is the constructor instance for that particular case.

``case id id* { tactic }``

    Focuses on the ``induction``/``cases`` subgoal corresponding to the given introduction rule, optionally renaming introduced locals.

    .. code-block:: text

        example (n : ℕ) : n = n :=
        begin
          induction n,
          case nat.zero { reflexivity },
          case nat.succ a ih { reflexivity }
        end

``destruct expr``

    Assuming ``x`` is a variable in the local context with an inductive type, ``destruct x`` splits the main goal, producing one goal for each constructor of the inductive type, in which ``x`` is assumed to be a general instance of that constructor. In contrast to ``cases``, the local context is unchanged, i.e. no elements are reverted or introduced.

    For example, given ``n : nat`` and a goal with a hypothesis ``h : P n`` and target ``Q n``, ``destruct n`` produces one goal with target ``n = 0 → Q n``, and one goal with target ``∀ (a : ℕ), (λ (w : ℕ), n = w → Q n) (nat.succ a)``. Here the name ``a`` is chosen automatically.

``existsi``

    ``existsi e`` will instantiate an existential quantifier in the target with ``e`` and leave the instantiated body as the new target. More generally, it applies to any inductive type with one constructor and at least two arguments, applying the constructor with ``e`` as the first argument and leaving the remaining arguments as goals.

    ``existsi [e₁, ..., eₙ]`` iteratively does the same for each expression in the list.

``constructor``

    This tactic applies to a goal such that its conclusion is an inductive type (say ``I``). It tries to apply each constructor of ``I`` until it succeeds.

``econstructor``

    Similar to ``constructor``, but only non-dependent premises are added as new goals.  

``left``

    Applies the first constructor when the type of the target is an inductive data type with two constructors.

``right``

    Applies the second constructor when the type of the target is an inductive data type with two constructors.

``split``

    Applies the constructor when the type of the target is an inductive data type with one constructor.

``injection expr (with id*)?``

    The ``injection`` tactic is based on the fact that constructors of inductive data types are injections. That means that if ``c`` is a constructor of an inductive datatype, and if ``(c t₁)`` and ``(c t₂)`` are two terms that are equal then  ``t₁`` and ``t₂`` are equal too.

    If ``q`` is a proof of a statement of conclusion ``t₁ = t₂``, then injection applies injectivity to derive the equality of all arguments of ``t₁`` and ``t₂`` placed in the same positions. For example, from ``(a::b) = (c::d)`` we derive ``a=c`` and ``b=d``. To use this tactic ``t₁`` and ``t₂`` should be constructor applications of the same constructor.

    Given ``h : a::b = c::d``, the tactic ``injection h`` adds two new hypothesis with types ``a = c`` and ``b = d`` to the main goal. The tactic ``injection h with h₁ h₂`` uses the names ``h₁`` an ``h₂`` to name the new hypotheses.

``injections (with id*)?``

    ``injections with h₁ ... hₙ`` iteratively applies ``injection`` to hypotheses using the names ``h₁ ... hₙ``.

.. _tactic_combinators:

Tactic Combinators
==================

*Tactic combinators* build compound tactics from simpler ones.

``repeat { tactic }``

    ``repeat { t }`` repeatedly applies ``t`` until ``t`` fails. The compound tactic always succeeds.

``try { tactic }``

    ``try { t }`` tries to apply tactic ``t``, but succeeds whether or not ``t`` succeeds.

``skip``

    A do-nothing tactic that always succeeds.

``solve1 { tactic }``

    ``solve1 { t }`` applies the tactic ``t`` to the main goal and fails if it is not solved. 

``abstract id? { tactic }``

    ``abstract id { t }`` tries to use tactic ``t`` to solve the main goal. If it succeeds, it abstracts the goal as an independent definition or theorem with name ``id``. If ``id`` is omitted, a name is generated automatically.

``all_goals { tactic }``

    ``all_goals { t }`` applies the tactic ``t`` to every goal, and succeeds if each application succeeds.

``any_goals { tactic }``

    ``any_goals { t }`` applies the tactic ``t`` to every goal, and succeeds if at least one application succeeds.

``done``

    Fail if there are unsolved goals.

.. TODO(Jeremy): is there any difference between the next two?

``fail_if_success { tactic }``

    Fails if the given tactic succeeds.

``success_if_fail { tactic }``

    Succeeds if the given tactic succeeds.

``guard_target expr``

    ``guard_target t`` fails if the target of the main goal is not ``t``.

``guard_hyp id := expr``

    ``guard_hyp h := t`` fails if the hypothesis ``h`` does not have type ``t``.

.. _the_rewriter:

The Rewriter
============

.. TODO(Jeremy): explain rewrite_cfg. What do symm and occs do?

``rewrite ([ (←? expr), ... ] | ←? expr) (at (* | (⊢ | id)*))? tactic.rewrite_cfg?``

    ``rewrite e`` applies identity ``e`` as a rewrite rule to the target of the main goal. If ``e`` is preceded by left arrow (``←`` or ``<-``), the rewrite is applied in the reverse direction. If ``e`` is a defined constant, then the equational lemmas associated with ``e`` are used. This provides a convenient way to unfold ``e``.

    ``rewrite [e₁, ..., eₙ]`` applies the given rules sequentially.

    ``rewrite e at l`` rewrites ``e`` at location(s) ``l``, where ``l`` is either ``*`` or a list of hypotheses in the local context. In the latter case, a turnstile ``⊢`` or ``|-`` can also be used, to signify the target of the goal. 

``rw``

    An abbreviation for ``rewrite``.

``rwa``

    ``rewrite`` followed by ``assumption``.

``erewrite``

    A variant of ``rewrite`` that uses the unifier more aggressively, unfolding semireducible definitions.

``erw``

    An abbreviation for ``erewrite``.

``subst expr``

   Given hypothesis ``h : x = t`` or ``h : t = x``, where ``x`` is a local constant, ``subst h`` substitutes ``x`` by ``t`` everywhere in the main goal and then clears ``h``. 

.. _the_simplifier:

The Simplifier and Congruence Closure
=====================================

.. TODO(Jeremy): explain various config options in all tactics

``simp only? (* | [(* | (- id | expr)), ...]?) (with id*)? (at (* | (⊢ | id)*))? tactic.simp_config_ext?``

    The ``simp`` tactic uses lemmas and hypotheses to simplify the main goal target or non-dependent hypotheses. It has many variants.

    ``simp`` simplifies the main goal target using lemmas tagged with the attribute ``[simp]``.

    ``simp [h₁ h₂ ... hₙ]`` simplifies the main goal target using the lemmas tagged with the attribute ``[simp]`` and the given ``hᵢ``'s, where the ``hᵢ``'s are expressions. These expressions may contain underscores, in which case they are replaced by metavariables that ``simp`` tries to instantiate. If a ``hᵢ`` is a defined constant ``f``, then the equational lemmas associated with ``f`` are used. This provides a convenient way to unfold ``f``.

    ``simp [*]`` simplifies the main goal target using the lemmas tagged with the attribute ``[simp]`` and all hypotheses.
  
    ``simp *`` is a shorthand for ``simp [*]``.

    ``simp only [h₁ h₂ ... hₙ]`` is like ``simp [h₁ h₂ ... hₙ]`` but does not use ``[simp]`` lemmas

    ``simp [-id₁, ... -idₙ]`` simplifies the main goal target using the lemmas tagged with the attribute ``[simp]``, but removes the ones named ``idᵢ``.

    ``simp at h₁ h₂ ... hₙ`` simplifies the non-dependent hypotheses ``h₁ : T₁`` ... ``hₙ : Tₙ``. The tactic fails if the target or another hypothesis depends on one of them. The token ``⊢`` or ``|-`` can be added to the list to include the target.

    ``simp at *`` simplifies all the hypotheses and the target.

    ``simp * at *`` simplifies target and all (non-dependent propositional) hypotheses using the other hypotheses.

    ``simp with attr₁ ... attrₙ`` simplifies the main goal target using the lemmas tagged with any of the attributes ``[attr₁]``, ..., ``[attrₙ]`` or ``[simp]``.

``dsimp only? (* | [(* | (- id | expr)), ...]?) (with id*)? (at (* | (⊢ | id)*))? tactic.dsimp_config?``

    ``dsimp`` is similar to ``simp``, except that it only uses definitional equalities.

``simp_intros id* only? (* | [(* | (- id | expr)), ...]?) (with id*)? tactic.simp_intros_config?``

    ``simp_intros h₁ h₂ ... hₙ`` is similar to ``intros h₁ h₂ ... hₙ`` except that each hypothesis is simplified as it is introduced, and each introduced hypothesis is used to simplify later ones and the final target. 

    As with ``simp``, a list of simplification lemmas can be provided. The modifiers ``only`` and ``with`` behave as with ``simp``.

``unfold id* (at (* | (⊢ | id)*))? tactic.unfold_config?``

    Given defined constants ``e₁ ... eₙ``, ``unfold e₁ ... eₙ`` iteratively unfolds all occurrences in the target of the main goal, using equational lemmas associated with the definitions.

    As with ``simp``, the ``at`` modifier can be used to specify locations for the unfolding.

``unfold1 id* (at (* | (⊢ | id)*))? tactic.unfold_config?``

    Similar to ``unfold``, but does not iterate the unfolding.

``dunfold id* (at (* | (⊢ | id)*))? tactic.dunfold_config?``

    Similar to ``unfold``, but only uses definitional equalities.

``delta id* (at (* | (⊢ | id)*))?``

    Similar to ``dunfold``, but performs a raw delta reduction, rather than using an equation associated with the defined constants.

``unfold_projs``

    This tactic unfolds all structure projections.

``trace_simp_set``

    Just construct the simp set and trace it. Used for debugging.

``ac_reflexivity``

    Proves a goal with target ``s = t`` when ``s`` and ``t`` are equal up to the associativity and commutativity of their binary operations.

``ac_refl``

    An abbreviation for ``ac_reflexivity``.

``cc``

    Tries to prove the main goal using congruence closure.

Other Tactics
=============

``trace_state``

    This tactic displays the current state in the tracing buffer.

``trace a``

    ``trace a`` displays ``a`` in the tracing buffer.

``type_check expr``

    Type check the given expression, and trace its type.

``apply_opt_param``

    If the target of the main goal is an `opt_param`, assigns the default value.

``apply_auto_param``

    If the target of the main goal is an `auto_param`, executes the associated tactic.

``dedup``

    Renames hypotheses with the same name.

Conversions
===========

The SMT State
=============


