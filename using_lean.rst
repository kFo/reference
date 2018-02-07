.. _using_lean:

==========
Using Lean
==========

Using Lean Online
=================

You can run a Javascript version of Lean `online <https://leanprover.github.io/live/master>`_ in your browser. It is much slower than running a version of Lean installed on your computer, but it provides a convenient way to experiment with the system.

The online version of Lean checks your input continuously. Error messages, warnings, and output appear in the window on the right-hand side. The editor shares a number of features with Visual Studio Code; for example, you can type unicode characters with a backslash, so ``\and`` yields the unicode symbol for conjunction, and ``\a``, ``\b``, and ``\g`` yield the unicode ``α``, ``β``, and ``γ`` respectively. 


Using Lean with VSCode
======================

Assuming you have installed Lean and `Visual Studio Code <https://code.visualstudio.com>`_, you can add the Lean extension to VSCode by clicking the extension icon in the view bar at left and searching for ``lean``. Once you have installed the extension, clicking on ``lean`` in the extensions panel provides additional information.

With the extension installed, if you create a file with the extension ``.lean`` and edit it, Lean will check the file continuously as you type. For example, if you type the words ``#check id``, the word ``#check`` is underlined in green to indicate a response from the Lean server. Hovering over the word ``#check`` displays the response, in this case, the type of the identity function.

Features
--------

VSCode Intellisense suggests completions as you type. These are context sensitive: if you are typing an identifier, it suggests suitable completions for identifiers, after ``import``, it suggests modules to import, after ``set_option``, it suggests suitable options, and so on.

You can enter Unicode characters with a backslash. For example, ``\a`` inserts an ``α``. You can see the commands provided by the Lean extension by typing ``ctrl-shift-P`` on Windows/Linux or ``cmd-shift-P`` on a Mac, and then typing ``lean`` into the search bar to filter the list. Typing ``ctrl-shift-enter`` opens up a message window which shows you error messages, warnings, output, and goal information when in tactic mode.

Typing an underscore in an expression asks Lean to infer a suitable value for the expression and fill it in automatically. In cases where Lean is unable to determine a value for the argument, the underscore is highlighted, and the error message indicates the type of the "hole" that needs to be filled. This can be extremely useful when constructing proofs incrementally. You can start typing a proof sketch, using either ``sorry`` or an underscore for details you intend to fill in later. Assuming the proof is correct modulo these missing pieces of information, the error message at an unfilled underscore tells you the type of the term you need to construct, typically an assertion you need to justify.

Multi-file Projects
-------------------

If you want to work on a project with multiple files, use the `package_manager`_ to set up a project folder, and then use ``Open Folder`` in the VSCode ``File`` menu to open the root directory for the project.


Using Lean with Emacs
=====================

Assuming you have installed Lean, Emacs, and the Lean Emacs mode according to the instructions on the Lean `Download <https://leanprover.github.io/download/>`_ page, you simply need to create a file with the extension ``.lean`` and edit it in Emacs. The file will be checked continuously as you type. For example, if you type the words ``#check id``, the word ``#check`` is underlined in green to indicate a response from the Lean server. Hovering over the word ``#check`` displays the response, in this case, the type of the identity function.

Features
--------

Lean mode uses an Emacs package named *Flycheck*, as evidenced by the letters ``FlyC`` that appear in the information line.  Flycheck offers a number of commands that begin with ``C-c !``. For example, ``C-c ! n`` moves the cursor to the next error, and ``C-c ! p`` moves the cursor to the previous error. You can get to a help menu that lists these key bindings by clicking on the ``FlyC`` tag. 

It is often inconvenient to have to put the cursor on a highlighted identifier to see an error message or the outcome of a ``#print`` or ``#check`` command. The keystrokes ``C-c C-n`` toggle ``Lean-Next-Error`` mode, in which the next message (or all the messages that occur on the line that the cursor is on, if there are any) appears in a buffer named ``lean-info``. You can position this window anywhere you want using Emacs commands for splitting windows and loading buffers. Pressing ``C-c C-n`` again toggles the mode off.

As with VSCode, the Emacs mode provides context-sensitive tab completion. Typing an underscore in an expression asks Lean to infer a suitable value for the expression and fill it in automatically. As described in the previous section, this provides a convenient way to construct terms and proof interactively.

If you put your cursor on an identifier and hit ``M-.``, Emacs will take you to the identifier's definition, whether it is in the same file, in another file in your project , or in the library. This works even in an autocompletion popup window: if you start typing an identifier, press the tab key, choose a completion from the list of options, and press ``M-.``, you are taken to the symbol's definition.  If you have Emacs 25 or later, you can then press ``M-,`` to go back to the original location.

In tactic mode, if you put your cursor on a tactic (or the keyword ``begin`` or ``end``) and type ``C-c C-g``, Emacs will show you the goal in the ``lean-info`` buffer. Here is another useful trick: if you see some notation in a Lean file and you want to know how to enter it from the keyboard, put the cursor on the symbol and type ``C-c C-k``.

If for some reason the Lean background process does not seem to be responding (for example, the information line no longer shows you type information), type ``C-c C-r`` or ``M-x lean-server-restart-process``, or choose "restart lean process" from the Lean menu, and with luck that will set things right again.

In Lean, the ``#exit`` command halts processing of a file abruptly. Inserting an ``#exit`` therefore prevents Lean from checking the file beyond that point.

Some of the main key bindings are summarized in the table below.

+-------------+-----------------------------------------------------------------------------------+
| Key         | Function                                                                          |
+-------------+-----------------------------------------------------------------------------------+
| ``M-.``     | jump to definition in source file (``lean-find-definition``)                      |
+-------------+-----------------------------------------------------------------------------------+
| ``M-,``     | return to original position (requires Emacs 25)                                   |
+-------------+-----------------------------------------------------------------------------------+
| ``TAB``     | tab complete identifier, option, filename, etc. (``lean-tab-indent-or-complete``) |
+-------------+-----------------------------------------------------------------------------------+
| ``C-c C-k`` | shows the keystroke needed to input the symbol under the cursor                   |
+-------------+-----------------------------------------------------------------------------------+
| ``C-c C-g`` | show goal in tactic proof (``lean-show-goal-at-pos``)                             |
+-------------+-----------------------------------------------------------------------------------+
| ``C-c C-x`` | execute lean in stand-alone mode (``lean-std-exe``)                               |
+-------------+-----------------------------------------------------------------------------------+
| ``C-c C-n`` | toggle next-error-mode: shows next error in dedicated lean-info buffer            |
+-------------+-----------------------------------------------------------------------------------+
| ``C-c C-r`` | restart the lean server                                                           |
+-------------+-----------------------------------------------------------------------------------+
| ``C-c ! n`` | flycheck: go to next error                                                        |
+-------------+-----------------------------------------------------------------------------------+
| ``C-c ! p`` | flycheck: go to previous error                                                    |
+-------------+-----------------------------------------------------------------------------------+
| ``C-c ! l`` | flycheck: show list of errors                                                     |
+-------------+-----------------------------------------------------------------------------------+

Multi-file Projects
-------------------

If you want to work on a project with multiple files, use the `package_manager`_ to set up a project folder, and use ``Open Folder`` in the VSCode ``File`` menu to open the root directory for the project.

.. _package_manager:

Using the Package Manager
=========================

``leanpkg`` is the package manager for the Lean theorem prover. It downloads dependencies and manages what modules you can import in your Lean files.

This section explains the general concepts of ``leanpkg``. For more information on a specific ``leanpkg`` command, execute ``leanpkg help <command>`` as a command line.

Directory Layout
----------------

A Lean package is a directory containing the following items:

* ``src``: a directory in which the package's Lean files are stored.  Imports from other packages are resolved relative to this directory.
* ``leanpkg.toml``: a manifest describing the package name, version, and dependencies.  Dependencies can be either local paths or git URLs.  Git dependencies are pinned to a specific commit and can be upgraded with `leanpkg upgrade`.
* ``leanpkg.path`` and ``_target/deps``: these items are created by ``leanpkg configure`` and should not be added to git.  They contain the paths to the dependencies on the current machine, and their git checkouts, respectively.

Using Lean in a Lean package
----------------------------

* Running the ``lean`` command-line tool from a directory inside your package will automatically use the ``leanpkg.path`` file for import resolution.
* In Emacs, ``lean-mode`` will automatically start a new Lean server process for each visited package.
* In VSCode, open the package as a folder.

Creating new packages
---------------------

The ``leanpkg new`` command creates a new package.  You can use ``leanpkg add`` to add dependencies (or add them manually if you prefer):

.. code-block:: text

   leanpkg new my_awesome_pkg
   cd my_awesome_pkg
   leanpkg add leanprover/mathlib
   # shorthand for `leanpkg add https://github.com/leanprover/mathlib`

You can now add new ``.lean`` files inside the ``src`` directory.

Scratch files
-------------

It is reasonably common to have thousands of "scratch" files lying around that are not part of a package.  Files that are not inside a package themselves can still use dependencies fetched via ``leanpkg``.  These dependencies are stored in ``~/.lean/leanpkg.toml`` and can be modified with ``leanpkg install``:

.. code-block:: text

   leanpkg install https://github.com/leanprover/smt2_interface

After this, you can use the ``smt2_interface`` package in all files that do not belong to a package themselves.

For experimenting inside a Lean package, you can use a directory separate from ``src``, say ``scratch``.  Files in this folder will still be able to import the package's Lean modules, but will not interfere with ``leanpkg build`` etc.

Import resolution
-----------------

Lean supports two kinds of imports:

.. code-block:: text

   import theory.set_theory   -- absolute
   import .basic              -- relative

Relative imports are always relative to the current file name.

Absolute imports are resolved according to the entries in the ``leanpkg.path`` file.  That is, when executing ``import theory.set_theory``, Lean looks for a file called ``theory/set_theory.lean`` in the `src` directories of all (transitive) dependencies as well as the current package.

Format of leanpkg.toml
----------------------

.. code-block:: text

   [package]
   name = "my_awesome_pkg"
   version = "0.1"         # no semantic significance currently
   lean_version = "3.3.0"  # optional, prints a warning on mismatch with Lean executable
   path = "src"            # hard-coded, will be removed in the future
   timeout = 100           # optional, passed to `lean` as `-T` parameter

   [dependencies]
   # local dependency
   demopkg = { path = "relative/path/to/demopkg" }
   # git dependency
   mathlib =
     { git = "https://github.com/leanprover/mathlib",
       rev = "62f7883d937861b618ae8bd645ee16ec137dd0bd" }
