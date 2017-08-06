The Lean Reference Manual
-------------------------

The manual uses Sphinx and restructured text.

# How to build

The build requires python 3 (install `python3-venv` on ubuntu).

```
make install-deps
make html
```

The call to `make install-deps` is only required the first time, and only if you want to use the bundled version of Sphinx and Pygments with improved syntax highlighting for Lean.

# How to deploy

```
./deploy.sh leanprover reference
```
