#!/usr/bin/env bash

set -e

# Clean

rm -f public/app.js

# Build

./scripts/build-prod.sh src/Main.elm

# Deploy

path="deploy"

git worktree add $path netlify

rm -rf $path/*
cp -r public/* $path
git -C $path add .
if git -C $path commit -m "Site updated to $(git log -n 1 --format='%h' master)"; then
  # git -C $path push -u origin HEAD
  echo "push to remote"
fi

git worktree remove --force $path
echo "Success!"
