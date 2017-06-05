# Simplexhc

This is supposed to be a simple compiler for a lazy functional programming language
like haskell Core (hence "hc" = "haskell compiler"). `simplex` so it is vaguely related to polyhedral compilation.

I am trying to verify if ideas in polyhedral compilation can be used on lazy
programming languages, by trying to go the `Core` -> `STG` -> `LLVM` route.

Currently, I'm trying to implement a full STG interpreter with eval/apply semantics (not `push/enter`), since that's what GHC uses.

The `master` head is `push/enter`, since I wanted to understand this first, and then move on to `eval/apply`.

I wish to model the sum & product types in `STG` as spaces, perhaps modeled with integer polyhedra, so I can reuse the machinery of [`isl`](http://isl.gforge.inria.fr/). This doesn't really work, since isl needs affine maps, and I don't think there's a reasonable interpretation of "affine" that works to analyse parallelism for lazy languages. Some of the ideas are [written down in my repo: `bollu/dependence-analysis-hask`](https://github.com/bollu/dependence-analysis-hask)
