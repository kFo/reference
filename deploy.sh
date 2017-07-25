#!/usr/bin/env bash
set -e
if [ "$#" -ne 2 ]; then
    echo "Usage example: $0 leanprover reference"
    exit 1
fi

# Build
make html

# 3. Deploy
mkdir deploy
cd deploy
rm -rf *
git init
cp -r ../_build/html/* .
rm README.html
touch .nojekyll
git add .
git commit -m "Update `date`"
git push git@github.com:$1/$2 +HEAD:gh-pages
cd ../
rm -rf deploy
