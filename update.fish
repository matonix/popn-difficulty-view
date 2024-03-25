#!/usr/bin/fish
cabal run; cd site; git add .; git commit -m "update"; git push; cd ..
