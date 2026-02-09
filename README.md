# chicken-church

**Church probabilistic programming language implemented in Chicken Scheme**

## Overview

[Church](https://probmods.org/) is a probabilistic programming language based on Scheme, designed for generative models and Bayesian inference. The original implementation ([webchurch](https://github.com/probmods/webchurch)) compiles Church to JavaScript.

This project reimplements Church natively in Chicken Scheme. Since Church *is* Scheme, parsing comes for free -- the implementation focuses entirely on the probabilistic runtime: elementary random primitives, conditioning, and inference. The result is roughly 2,000 lines of Scheme across nine modules.

Three inference engines are included:

- **Rejection sampling** -- simple but correct
- **Exact enumeration** via `call/cc` -- explores all execution paths
- **Trace-based Metropolis-Hastings MCMC** -- single-site MH with structural change support

Plus **DPmem** (Dirichlet Process memoization) for nonparametric models.

## Quick Start

Install [Chicken Scheme](https://call-cc.org/) 5 or later, then install the required eggs:

```
chicken-install srfi-1 srfi-27 srfi-69
```

Use Church from a Scheme file:

```scheme
(include "church/church.scm")

;; Estimate P(A = 1 | A + B + C >= 2) using Metropolis-Hastings
(define samples
  (mh-query
    5000 10
    (define A (if (flip) 1 0))
    (define B (if (flip) 1 0))
    (define C (if (flip) 1 0))
    (define D (+ A B C))
    A
    (>= D 2)))

(display (exact->inexact (mean samples)))
(newline)
;; => approximately 0.75
```

Run it:

```
csi -q -s myfile.scm
```

## Architecture

| File | Lines | Description |
|------|------:|-------------|
| `church/erps.scm` | 368 | Elementary random primitives: `flip`, `gaussian`, `uniform`, `beta`, `gamma`, `exponential`, `dirichlet`, `multinomial`, etc. Each ERP has a sampler and log-density scorer. |
| `church/builtins.scm` | 562 | Gap-filling utilities: statistics, set operations, list extras, string operations, type conversions, fold variants, `gensym`, `mem`. |
| `church/inference.scm` | 181 | Core conditioning primitives: `condition`, `factor`, `rejection-query`. |
| `church/enumerate.scm` | 215 | Exact enumeration of discrete models using `call/cc`. |
| `church/trace.scm` | 70 | Address tracking and trace data structure for MCMC. |
| `church/mh.scm` | 379 | Single-site Metropolis-Hastings MCMC with structural change support. |
| `church/dpmem.scm` | 52 | Dirichlet Process memoization via the Chinese Restaurant Process. |
| `church/conditional.scm` | 76 | Generic `conditional` query interface that dispatches to the appropriate inference engine. |
| `church/church.scm` | 83 | Entry point: loads all modules, installs aliases. |

## Features

- Three inference strategies: rejection sampling, exact enumeration, and Metropolis-Hastings MCMC
- All standard Church ERPs: `flip`, `gaussian`, `uniform`, `beta`, `gamma`, `exponential`, `dirichlet`, `multinomial`, `uniform-draw`
- Dirichlet Process memoization (`DPmem`) for nonparametric mixture models
- Generic `conditional` interface for model-agnostic querying
- Full suite of Scheme list/functional primitives with Church-compatible semantics
- 164-test suite ported from webchurch

## Examples

### Coin flip with conditioning

```scheme
(include "church/church.scm")

;; What is the bias of a coin that landed heads 8 out of 10 times?
(define samples
  (mh-query
    10000 5
    (define bias (uniform 0 1))
    bias
    (= 8 (length (filter (lambda (x) x)
                         (repeat 10 (lambda () (flip bias))))))))

(display (exact->inexact (mean samples)))
(newline)
```

### Exact enumeration

```scheme
(include "church/church.scm")

;; Compute exact posterior P(A | A + B + C >= 2)
(define dist
  (enumeration-query
    (define A (if (flip) 1 0))
    (define B (if (flip) 1 0))
    (define C (if (flip) 1 0))
    (define D (+ A B C))
    A
    (>= D 2)))

(display dist)
(newline)
;; => ((0 1) (0.25 0.75))
```

### Bayesian reasoning with MH

```scheme
(include "church/church.scm")

;; A bag contains some number of blue and red balls.
;; We draw 5 balls (with replacement) and see 4 blue.
;; What is the probability the bag is mostly blue?

(define samples
  (mh-query
    10000 5
    (define num-blue (+ 1 (sample-integer 10)))
    (define num-red (+ 1 (sample-integer 10)))
    (define total (+ num-blue num-red))
    (define proportion (/ num-blue total))
    (> proportion 0.5)
    (= 4 (length (filter (lambda (x) x)
                         (repeat 5 (lambda () (flip (exact->inexact proportion)))))))))

(display (exact->inexact (mean (map (lambda (x) (if x 1 0)) samples))))
(newline)
```

## Testing

Run the full test suite (164 tests):

```
csi -q -s test/run-tests.scm
```

Expected output:

```
========================================
Test Results: 164 passed, 0 failed
========================================
```

## Background

This project is a native Scheme reimplementation of the Church probabilistic programming language. The original implementation, [webchurch](https://github.com/probmods/webchurch), compiles Church to JavaScript and runs in the browser or Node.js. A copy of webchurch is included in the `webchurch/` directory for reference.

For an introduction to probabilistic programming with Church, see [Probabilistic Models of Cognition](https://probmods.org/) by Noah Goodman and Joshua Tenenbaum.

## License

MIT
