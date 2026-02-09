# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This repository contains the function reference documentation for the **Church** probabilistic programming language. Church is a Scheme-like language designed for probabilistic modeling and Bayesian inference, built on top of JavaScript.

The entire reference is a single HTML file (`ref.html`) that documents all built-in functions. It references an external stylesheet at `css/ref.css` (not included in the repo).

## Structure of ref.html

Each function is documented in a `<div class="function">` block containing:
- `<code class="function-name">` — function signature with parameters
- `<code class="aliases">` — alternate names (e.g., `car` for `first`, `+` for `plus`)
- `<div class="description">` — prose description
- `<table>` — parameter list with name, type, and optional description

Parameters in `[brackets]` are optional. Types include `real`, `nat`, `boolean`, `string`, `function`, `list`, `pair`, and parameterized types like `list<real>`.

## Key Language Concepts

Church functions fall into these categories:
- **Probability distributions**: `flip`, `gaussian`, `uniform`, `beta`, `gamma`, `dirichlet`, `exponential`, `multinomial` — all accept an optional `conditionedValue` parameter for conditioning
- **Inference**: `enumeration-query`, `rejection-query`, `mh-query`, `mh-query-scored`, `conditional` — strategies include enumeration, rejection sampling, and Metropolis-Hastings
- **Stochastic memoization**: `DPmem` (Dirichlet Process memoization)
- **List/functional primitives**: standard Scheme operations (`map`, `fold`, `filter`, `cons`/`pair`, `car`/`first`, `cdr`/`rest`, etc.)
- **Math**: standard numeric and trigonometric functions
- **I/O**: `read-csv`, `read-file`, `write-csv`, `display`
