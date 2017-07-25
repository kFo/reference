The Lean Reference Manual
-------------------------

The manual uses Sphinx and restructured text, and the Read the Docs theme.

# How to build

```
pip install sphinx recommonmark
make html
```

The first line is only necessary if we eventually incorporate markdown files as restructured text.

# How to deploy

```
./deploy.sh leanprover reference
```

