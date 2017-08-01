The Lean Reference Manual
-------------------------

The manual uses Sphinx and restructured text, and the Read the Docs theme.

# How to build

```
pip install sphinx recommonmark
make html
```

The first line is only necessary if we eventually incorporate markdown files as well as restructured text.

On OSX, after the El Capitan update, we need to install our own version of python, or use `--user` flag when installing `sphinx`.

```
pip install --user sphinx recommonmark
pip install --user sphinx_rtd_theme
make html
```

# How to deploy

```
./deploy.sh leanprover reference
```
