.. _other_commands:

==============
Other Commands
==============

.. _universes_and_variables:

Universes and Variables
=======================

The ``universe`` command introduces a special variable ranging over type universes. After the command ``universe u``, a definition or theorem that is declared with a variable ranging over ``Sort u`` is polymorphic over that universe variable. The ``universes`` command can be used to introduce a list of universe variables.

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

(parameters) 

Namespaces
==========

(Include import and export.)

.. _attributes:

Attributes
==========

(Include local attributes.)


Options
=======


Instructions
============

(``#check``, ``#print``, ``#reduce``, ``#eval``, ``run_cmd``.)

Notation Declarations
=====================



Elaborator Hints
================