#!/usr/bin/env bash

set -e

function die() {
  echo "$1"
  exit -1
}

if [[ -z "$1" ]]; then
  die "Empty commit message, you must specify a commit message"
else
  COMMIT="$1"
fi

if ! [[ "$PWD" =~ docs$ ]]; then
  die "You are not in the docs directory, please navigate to the correct directory"
fi

OUT=$(git status)
if [[ $OUT != *"Your branch is up-to-date with"* ]] || [[ $OUT != *"working tree clean"* ]]; then
  die "You have not pushed these changes yet. I won't automatically publish these changes until you have pushed them."
fi


cd ../build
rm -r ./*
cp -r ../docs/_site/* ./
git add .
git commit -m "$COMMIT"
git push origin master
cd ..
git add build
git commit -m "$COMMIT"
