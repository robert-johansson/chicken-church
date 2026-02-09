# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a native Chicken Scheme reimplementation of the Church probabilistic programming language. Church is a Scheme-based language for generative models and Bayesian inference. Since Church *is* Scheme, parsing comes for free — the implementation focuses on the probabilistic runtime: elementary random primitives (ERPs), conditioning, and inference.


## Commands

```bash
# Install dependencies (Chicken Scheme 5+ required)
chicken-install srfi-1 srfi-27 srfi-69

# Run the test suite (164 tests)
csi -q -s test/run-tests.scm

# Run a Church program
csi -q -s church/church.scm myfile.scm

# Run a ProbMods chapter (extracts and evals ~~~~ code blocks)
csi -q -s run-chapter.scm probmods-chapters/md/conditioning.md

# Run all chapters
for f in probmods-chapters/md/*.md; do echo "=== $f ==="; csi -q -s run-chapter.scm "$f"; done
```

## Architecture

All modules live in `church/` and are loaded via `(include ...)` — there is no Chicken egg/module system. Load order matters (declared in `church/church.scm`).

**Module dependency chain:**

```
erps.scm          ← Foundation: ERP sampling, log-density, proposals (srfi-27)
builtins.scm      ← Church-compatible list/math/string/set utilities (srfi-1, srfi-69)
inference.scm     ← condition, factor, rejection-query (uses chicken condition for abort)
trace.scm         ← Address stack + trace hash-table for MCMC (srfi-69)
enumerate.scm     ← Exact enumeration via call/cc; forks on discrete ERPs
mh.scm            ← Single-site trace-based Metropolis-Hastings
dpmem.scm         ← Dirichlet Process memoization (Chinese Restaurant Process)
conditional.scm   ← Generic query interface dispatching to enumerate/rejection/mh
church.scm        ← Entry point: loads all modules, installs aliases, provides church-load
```

**Key design patterns:**

- **ERP handler dispatch**: The `current-erp-handler` parameter (in `erps.scm`) is the central extension point. Outside inference it just samples; inference engines swap in their own handler to intercept ERP calls for trace management (MH), continuation capture (enumeration), or rejection.
- **Macros for queries**: `rejection-query`, `enumeration-query`, `mh-query`, and `conditional` are all `syntax-rules` macros. The last forms are always the query expression and condition expression; preceding forms are model definitions.
- **Aliasing**: `church.scm` uses `set!` to override Scheme builtins (`iota`, `fold`, `sort`, etc.) with Church-compatible versions after all modules are loaded. Some builtins (like `gensym`, `display`) can't be overridden because Chicken's macro expander uses them.
- **Church-prefixed names**: Many builtins use a `church-` prefix internally (`church-and`, `church-or`, `church->`, `church-fold`, etc.) to avoid conflicts with Scheme builtins during module loading. Tests use these prefixed names directly.

## Testing

The single test file `test/run-tests.scm` uses a custom `check`/`check-approx` harness (no external test framework). Tests cover: arithmetic, list operations, higher-order functions, set operations, statistics, string operations, `mem`, rejection/enumeration/MH queries, support change in MH, and the `conditional` interface. Tests exit with code 1 on any failure.

Statistical tests (MH sampling) use `check-approx` with tolerances, so occasional flaky failures on probabilistic tests are possible.
