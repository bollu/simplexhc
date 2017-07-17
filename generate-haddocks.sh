# Script configured for my computer. 
set -o xtrace
set -e

stack haddock --no-haddock-deps
mv $(stack path --local-doc-root) ./docs

git add docs/
git commit -m "docs update on: $(date)"
git push origin master:docs
