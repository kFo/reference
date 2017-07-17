# Expressions

## Universes

Every type in Lean is, by definition, an expression of type `Sort u` for some universe level `u`. A universe level is one of the following:

* a natural number, `n`
* a universe variable, `u` (declared with the command `universe` or `universes`)
* an expression `u + n`, where `u` is a universe level and `n` is a natural number
* an expression `max u v`, where `u` and `v` are universes
* an expression `imax u v`, where `u` and `v` are universe levels

The last one denotes the universe level `0` if `v` is `0`, and `max u v` otherwise.

### Examples 

```lean
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
```

## Expressions

The set of expressions in Lean is defined inductively as follows:

* `Sort u` : the universe of types at universe level `u`
* `c` : where `c` is an identifier denoting an axiomatically declared constant or a defined object
* `Π x : α, β` : the type of functions taking an element `x` of `α` to an element of `β`, where `β` is an expression whose type is a `Sort`.
* `s t` : the result of applying `s` to `t`, where `s` and `t` are expressions
* `λ x : α, t` : the function mapping any value `x` of type `α` to `t`, where `t` is an expression
* `let x := t in s`, denoting a local definition, that is, the value of `s` when `x` denotes `t`

Every well formed term in Lean has a _type_, which itself is an expression of type `Sort u` for some `u`. The fact that a term `t` has type `α` is written `t : α`. 

For an expression to be well formed, its components have to satisfy certain typing constraints. These, in turn, determine the type of the resulting term, as follows:

* `Sort u : Sort (u + 1)`
* `c : α`, where `α` is the type that `c` has been declared or defined to have
* `Π x : α, β : Sort (imax u v)` where `α : Sort u`, and `β : Sort v` assuming `x : α` 
* `s t : β [t/x]` where `s` has type `Π x : α, β` and `t` has type `α`
* `(λ x : α, t) : Π x : α, β` if `t` has type `β` whenever `x` has type `α`
* `(let x := t in s) : β [t/x]` where `s` has type `Π x : α, β`, and `t` has type `β` assuming `x : α` 

`Prop` abbreviates `Sort 0`, `Type` abbreviates `Sort 1`, and `Type u` abbreviates `Sort (u + 1)` when `u` is a universe variable. We say "`α` is a type" to express `α : Type u` for some `u`, and we say "`p` is a proposition" to express `p : Prop`. Using the "propositions as types" correspondence, given `p : Prop`, we refer to an expression `t : p` as a _proof_ of `p` is an expression. In contrast, given `α : Type u` for some `u` and `t : α`, we sometimes refer to `t` as _data_.

When the expression `β` in `Π x : α, β` does not depend on `x`, the latter can be abbreviated `α → β`. As usual, the variable `x` is bound in `Π x : α, β`, `λ x : α, t`, and `let x := t in s`. The expression `∀ x : α, β` is alternative syntax for `Π x : α, β`, and is intended to be used when `β` is a proposition.

In addition to the elements above, expressions can also contain:

* _metavariables_, that is, temporary placeholders, that are used in the process of constructing terms
* _local constants_, temporary variables that have values in a "local context" 
* _de Bruijn indices_, which are used to represent bound variables.
* _macros_, which are used to annotate or abbreviate terms.

Local constants are used, for example, to construct terms in the interactive tactic mode. Terms that are added to the environment do contain neither metavariables no local constants, which is to say, they are fully elaborated and make sense in the empty context.
 
Constants can be declared in various ways, such as by the `constant(s)` and `axiom(s)` keywords, or as the result of an `inductive` or `structure` declarations. Similarly, objects can be defined in various ways, such as using `def`, `theorem`, or the equation compiler. See Chapter _Declarations_ for more information.

### Examples

```lean
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
#check let x := a in h x

#check Π x y, η x y
#check Π (x : α) (y : β), η x y
#check λ x y, η x y 
#check λ (x : α) (y : β), η x y 
#check let x := a, y := b in η x y 
```

## Implicit Arguments

(Curly braces, double-curly braces, and implicit arguments. Also opt params and auto params.)

## Structured Proofs

(Special syntax for `assume`, `have`, and `show`.)

## Computation

(Explain the notions of reduction and evaluation.)

## Axioms

Lean's foundational framework consists of:

* the core syntax of the calculus of constructions, as described above
* inductive types, as described in Chapter _Declarations_. 

In addition, the core library defines (and trusts) the following axiomatic extension:

* propositional extensionality: ...
* quotients: ...
* choice: ...

The last principle, in conjunction with the others, makes the axiomatic foundation classical. Functions that make use of `choice` to produce data are incompatible with a computational interpretation, and do not produce bytecode. They have to be declared `noncomputable`.

(Say something about the `meta` keyword.)


