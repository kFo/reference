.. _other_commands:

==============
Other Commands
==============

.. _universes_and_variables:

Universes and Variables
=======================

The ``universe`` command introduces a special variable ranging over a type universe level. After the command ``universe u``, a definition or theorem that is declared with a variable ranging over ``Sort u`` is polymorphic over that universe variable. More generally, universe level variables can appear in any :ref:`universe level expression <universes>`. The ``universes`` command can be used to introduce a list of universe level variables. 

The ``variable`` command introduces a single variable declaration, and the ``variables`` command introduces one more more variable declarations. These have no effect until a subsequent definition or theorem declaration, though variables can also be used in a ``#check`` command. When Lean detects a variable name occuring in a definition or theorem, either in the type or the body, it inserts that variable and all the variables it depends on into the local context, as though they have all been declared before the colon. In other words, the declaration is abstracted over those variables. Only the variables that appear and their dependences are added, and are inserted in the order that they were declared. 

Variables may be annotated as implicit as described in :numref:`implicit_arguments`. You can change the annotation of a variable that has previously been declared using another ``variable`` or ``variables`` command, listing the variables with the desired annotation, but omitting their types.

Variables that are only used within a tactic block are not automatically included, since the meaning of a name in the context of a tactic block cannot be predicted at parse time. You can force the inclusion of a variable or list of variables in every declaration using the ``include`` command. To undo the effect of an ``include`` command, use ``omit``. 

.. code-block:: lean

    universe u
    variables {α β : Type u}
    variable  y : α
    variable  z : α

    def ident (x : α) := x

    theorem ident_eq : ∀ x : α, ident x = x := λ x, rfl

    theorem ident_eq' : ident y = y := rfl

    variables {y z}

    variable h : y = z

    example : ident z = y := eq.symm h

    include h
    example : ident z = y :=
    begin 
    symmetry,
    exact h
    end

    omit h

    variable (y)

    def ident2 := y

Sections
========

The scope of a ``universe`` or ``variable`` declaration can be scoped in a *section*. A section begins with a command ``section foo`` and ends with a command ``end foo``, where ``foo`` is an arbitrary name. Alternatively, you can begin a section with the command ``section`` along, and close it with ``end``. The name only serves to help match ``section``/``end`` pairs, and otherwise does not play any role.

Sections also support the commands ``parameter`` and ``parameters``. These are similar to ``variable`` and ``variables`` respectively, except that within the section, later invocations of definitions and theorems that depend on the parameters introduced by these commands do not mention those parameters explicitly. In other words, the parameters are thought of as being fixed throughout the section, whereas definitions and theorems defined in terms of them maintain that fixed dependence. Outside the section, the definitions and theorems are generalized over those variables, just as with the ``variables`` command.

.. code-block:: lean

    section 
    variables (x y : ℕ)

    def foo := x + y

    #check (foo : ℕ → ℕ → ℕ)
    end

    section 
    parameters (x y : ℕ)

    def bar := x + y

    #check (bar : ℕ) 
    #check (bar + 7 : ℕ)
    end

As with the ``variable`` and ``variables`` commands, variables introduced with ``parameter`` and ``parameters`` can be annotated as implicit, and the annotations can be changed after the fact with subsequent declarations that omit the type. The ``include`` and ``omit`` commands can be used with these variables as well.

Sections also delimit the scope of local :ref:`attributes <attributes>` and :ref:`notation declarations <notation_declarations>`.

.. _namespaces:

Namespaces
==========

The commands ``namespace foo ... end foo``, where ``foo`` is a :ref:`declaration name <declaration_names>`, open and close a namespace named ``foo``. Within the namespace, ``foo`` is added as a prefix to all declarations. So, for exampe, ``def bar`` adds an object named ``foo.bar`` to the environment, and declares ``bar`` to be an alias for ``foo.bar`` while the namespace is opened. If there is already an object or alias ``bar`` in the environment, the name is overloaded. Within the namespace, ``foo.bar`` is preferred when an ambiguity needs to be resolved. The prefix ``_root_`` can always be used to specify a full name starting at the top level, so that ``_root_.bar`` refers to the object whose full name is ``bar``.

Namespaces can be nested. In terms of scoping, namespaces behave like sections. For example, variables declared in a namespace stay in scope until the ``end`` command.

The command ``open foo`` opens the namespace, so that ``foo.bar`` is aliased to ``bar``. Once again, if there is already an object or alias ``bar`` in the environment, the name is overloaded (with none of them preferred). The ``open`` command admits these variations:

- ``open foo (bar baz)`` : create aliases *only* for ``bar`` and ``baz``
- ``open foo (renaming bar -> baz)`` : renames ``bar`` to ``baz`` when opening ``foo``
- ``open foo (hiding bar)`` : omits creating an alias for ``bar`` when opening ``baz``

Multiple instances of ``hiding`` and ``renaming`` can be combined in a single ```open`` command.

The ``export`` command is similar to ``open``, except that it serves to copy aliases from one namespace to another, or to the top level. For example, if a file exports ``bar`` from namespace ``foo`` to the top level, then any file that imports it will have the alias for ``foo``.

Declarations within a namespace can bear the ``protected`` modifier. This means that a shortened alias is not generated when the namespace is open. For example, ``nat.rec`` is protected, meaning that opening ``nat`` does *not* generate an alias ``rec``.

Declarations in a namespace or at the top level can also bear the ``private`` modifier, which means that they are added to the environment with an internally generated name and hidden from view outside the file. An alias is generated at the point where the declaration is made and it survives until the namespace is closed, or to the end of the file if the declaration is at the top level. Thus if we declare ``private def bar := ...`` in namespace ``foo``, we can only refer to the object ``bar`` until the namespace is closed. 

.. code-block:: lean

    def baz := 7

    namespace foo
    namespace bar
        def baz := 5
        def fzz := 9
        protected def bloo := 11
        private def floo := 13

        example : foo.bar.baz = 5 := rfl
        example : bar.baz = 5 := rfl
        example : baz = 5 := rfl
        example : _root_.baz = 7 := rfl
    end bar

    example : bar.baz = 5 := rfl
    end foo

    section
    open foo.bar

    example : fzz = 9 := rfl
    -- baz is overloaded and hence ambiguous
    example : foo.bar.baz = 5 := rfl  
    end

    section
    open foo.bar (renaming fzz -> bzz)

    example : bzz = 9 := rfl
    example : foo.bar.bloo = 11 := rfl
    end

    export foo (bar.baz)

    example : bar.baz = 5 := rfl

    export foo.bar

    example : fzz = 9 := rfl

If ``t`` is an element of an inductive type or family ``foo``, then any function ``bar`` defined in the namespace ``foo`` can be treated as a "projection" using the anonymous projector notation described in :numref:`structures_and_records`. Specifically, if the first argument to ``foo.bar`` is of type ``foo``, then ``t.bar x y z`` abbreviates ``foo.bar t x y z``. More generally, as long as ``foo.bar`` has any argument of type ``foo``, then ``t.bar x y z`` is interpreted as the result of applying ``foo.bar`` to ``x``, ``y``, and ``z``, inserting ``t`` at the position of the first argument of type ``foo``.

.. code-block:: lean

    variables (xs ys : list ℕ) (f : ℕ → ℕ)

    #check xs.length
    #check xs.append ys
    #check (xs.append ys).length
    #check xs.map f
    #check xs.reverse.reverse

    example : [1, 2, 3].reverse.map (λ x, x + 2) = [5, 4, 3] := rfl

.. _attributes:

Attributes
==========

Objects in Lean can bear *attributes*, which are tags that are associated to them, sometimes with additional data. You can assign an attribute ``foo`` to a object by preceding its declaration with the annotation ``attribute [foo]`` or, more concisely, ``@[foo]``. 

You can also assign the attribute ``foo`` to a object ``bar`` after it is declared by writing ``attribute [foo] bar``. You can list more than one attribute and more than one name, in which case all the attributes are assigned to all the objects at once.

Finally, you can assign attributes locally by using ``local attribute`` instead of ``attribute``. In that case, the attribute remains associated with the object until the end of the current section or namespace, or until the end of the current file if the command occurs outside any section or namespace.

The set of attributes is open-ended since users can declare additional attributes in Lean (see :numref:`Chapter %s <programming>`. You can ask Lean to give you a list of all the attributes present in the current environment with the command ``#print attributes``. Below are some that are commonly used:

- ``[class]`` : a type class
- ``[instance]`` : an instance of a type class
- ``[priority n]`` : sets the class resolution priority to the natural number ``n``
- ``[refl] : a reflexivity rule for the ``reflexivity`` tactic, for the ``calc`` environment, and for the simpilfier
- ``[symm] : a symmetry rule for the ``symmetry`` tactic
- ``[trans] : a transitivity rule for the ``transitivity`` tactic, for the ``calc`` environment, and for the simplieir
- ``[congr]`` : a congruence rule for the simplifier
- ``[simp]``: a simplifier rule
- ``[recursor]`` : a user-defined elimination principle, used, for example, by the induction tactic

Note that the ``class`` command, as discussed in :numref:`type_classes`, does more than simply assign the attribute. 

There are attributes that control how eagerly definitions are unfolded during elaboration:

- ``[reducible]`` : unfold freely
- ``[semireducible]`` : unfold when inexpensive (the default)
- ``[irreducible]`` : do not unfold

There are also attributes used to specify strategies for elaboration:

- ``[elab_with_expected_type]`` : elaborate the arguments using their expected type (the default)
- ``[elab_simple]`` : elaborate arguments from left to right without propagating information about their types. 
- ``[elab_as_eliminator]`` : uses a separate heuristic to infer higher-order parameters; commonly used for eliminators like recursors and induction principles

.. code-block:: lean

    def foo (x : ℕ) := x + 5

    attribute [simp]
    theorem bar₁ (x : ℕ) : foo x = x + 5 := rfl

    @[simp] theorem bar₂ (x : ℕ) : foo x = x + 5 := rfl

    theorem bar₃ (x : ℕ) : foo x = x + 5 := rfl

    theorem bar₄ (x : ℕ) : foo x = x + 5 := rfl

    attribute [simp] bar₃ bar₄ 

    #print attributes

Options
=======

Lean maintains a number of internal variables that can be set by users to control its behavior. You can set such an option by writing `set_option <name> <value>`.

One very useful family of options controls the way Lean's pretty-printer displays terms. The following options take a value of ``true`` or ``false``:

- ``pp.implicit`` : display implicit arguments
- ``pp.universes`` : display hidden universe parameters
- ``pp.coercions`` : show coercions
- ``pp.notation`` : display output using defined notations
- ``pp.beta`` : beta reduce terms before displaying them

As an example, the following settings yield much longer output:

.. code-block:: lean

    set_option pp.implicit true
    set_option pp.universes true
    set_option pp.notation false
    set_option pp.numerals false

    #check 2 + 2 = 4
    #reduce (λ x, x + 2) = (λ x, x + 3)
    #check (λ x, x + 1) 1

Instructions
============

Commands that query Lean for information are generally intended to be transient, rather than remain permanently in a theory file. Such commands are typically preceded by a hash symbol.

- ``#check t`` : check that ``t`` is well-formed and show its type
- ``#print t`` : print information about ``t``
- ``#reduce t`` : use the kernel reduction to reduce ``t`` to normal form
- ``#eval t`` : use the bytecode evaluator to evaluate ``t``

The form of the output of the ``#print`` command varies depending on its argument. Here are some more specific variations:

- ``#print definition`` : display definition
- ``#print inductive`` : display an inductive type and its constructors
- ``#print notation`` : display all notation
- ``#print notation <tokens>`` : display notation using any of the tokens
- ``#print axioms`` : display assumed axioms
- ``#print options`` : display options set by user
- ``#print prefix <namespace>`` : display all declarations in the namespace
- ``#print classes`` : display all classes
- ``#print instances <class name>`` : display all instances of the given class
- ``#print fields <structure>`` : display all fields of a structure

Here are examples of how these commands are used:

.. code-block:: lean

    def foo (x : ℕ) := x + 2
    
    #check foo
    #print foo
    #reduce foo
    #reduce foo 2
    #eval foo 2

    #print notation
    #print notation + * -
    #print axioms
    #print options
    #print prefix nat
    #print prefix nat.le
    #print classes
    #print instances ring
    #print fields ring

In addition, Lean provides the command ``run_cmd`` to execute an expression of type ``tactic unit`` on an empty goal. (See :numref:`Chapter %s <programming>`.)

.. _notation_declarations:

Notation Declarations
=====================
