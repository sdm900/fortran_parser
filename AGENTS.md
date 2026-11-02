# AGENTS.md — fortran_parser

Legacy Fortran 90 runtime string-expression parser. No package manager, CI, linter, or test framework. `gfortran` is the only toolchain (not installed in every environment — verify with `gfortran --version`).

## Layout

- `params.f90` → `extrafunc.f90` → `interpreter.f90` = library, built as `libparse.a`. Compile in that order (module dependencies); see `Makefile` deps.
- `check.f90` = regression suite (~255 numbered cases) + API usage example. `example.f90` = minimal create/evaluate/destroy demo. `bench.f90` / `speed.f90` = perf harnesses.
- `simple/` = obsolete standalone cut-down version (16 funcs, `f_evaluatefn` API, own `build.sh`). Do not mix with the root full version (60+ funcs, `s_*` API). Leave `simple/` alone unless asked.

## Build / test (root `Makefile`)

- `make lib` → `libparse.a` (via `obj/`). `make bin` → `./check` binary (misnomer: target is `bin`, binary is `check`). `make speed` → `./speed` benchmark. `make clean` removes `obj/*`, `libparse.a`, `check`, `speed`.
- Run regression: `make bin && ./check` — every line should say `PASSED` (276 checks: 257 original + 19 for input validation and domain guards); any `FAIL` block prints expected vs actual.
- `s_evaluate` terminates on the first `end_` sentinel; `s_funcpush` grows the stack when full (`>=`) precisely so a trailing sentinel always exists. Keep it that way.
- Input validation is fail-report, never fail-abort: `s_readnumber` uses `IOSTAT` (`'Not a number: ...'`), recursion depth is capped at 1000 (`'Maximum nesting depth exceeded'`), and `varvals` arity is checked (`'Too few variable values'`). Do not add paths that can abort on bad input.
- Domain guards live in `s_evaluate`, not `extrafunc`: `besy0/besy1/besk0/besk1` need `x > 0`, `besn*` additionally need integer `|n| <= 1000000` (loop-bound overflow), `gamma` rejects poles and `|x| > 200`. `floor/ceiling` use `kind=8` + a `2**62` fast path to avoid 32-bit `int` overflow; `//`/`%`/`**` guard `huge(1)` magnitude.
- `if(cond, t, f)` is eager: both branches always evaluate, errors included. Documented in `TOUSE`; changing it needs jump opcodes, not a tweak.
- The library is single-threaded by design (module state: `error`, `variables`, `cachedfuncs`, save `stack` in `s_evaluate`). Never add threads without removing that state first.
- Keep precise FP semantics: the Makefile's commented `-ffast-math` alternatives must stay off (NaN guards + the `s_evaluateerr` stability comparison depend on it).
- Default `F90FLAGS = -g -cpp -fbounds-check -fstack-check -pedantic-errors -Wuninitialized`. Keep `-cpp`: `extrafunc.f90` relies on `#ifdef SYSBESSEL/SYSGAMMA/SYSLGAMMA/SYSERF` to switch between bundled approximations and system intrinsics (default: bundled).
- `make export` runs `cpp` over sources into `exportsrc/` — release artifact only, not part of normal dev loop.

## Git

Only commit when the user explicitly asks for a commit **after a change**. Never commit, amend, or push without explicit permission. A plan or proposed work must never include a commit step; if a commit is needed, ask for permission separately.

Every commit made by an AI assistant MUST include a trailer noting OpenCode authorship and the active model, e.g.:

```
Committed-by: OpenCode (model: <provider>/<model>)
```

## API (`TOUSE`, `interpreter.f90:7-8`)

Lifecycle, in order: `s_createfn(func, vars, funcnum, error)` → `s_evaluatefn` / `s_evaluateerr` → `s_destroyfn(funcnum)`. Also `s_errprecision`. All `error` args are optional `character(len=*)`; success is exactly `'OK'`.
- All numbers are `real(fdp)` with `fdp=8` (double). `varvals` order must match space-separated `vars` string.
- Parser is case-insensitive and strips all spaces (`f_clean`). Variable names allow letters + `_`; watch prefix collisions (`CL` vs `CLL` — see `check.f90` tests 145–148).
- Error propagation syntax: `x[0.1]` in the vars string attaches uncertainty; evaluated with `s_evaluateerr`. A bare `[`-mismatch is a *parse* error (`perr`), unstable propagation is `eeerr='Error value not stable'`, runtime domain errors are `eerr` (e.g. `log(x), x<=0`, `x**y, x<0 and y not integer`, `x//y, x not int`).

## Parser gotchas (from `check.f90`)

- No consecutive binary/unary operators: `x+-1`, `x--1`, `1e-01+-2e-01` are parse errors (`Operator following operator: ...`). Parenthesize negatives: `1e-01/(-2e-01)`, `(-1)/(-2)`.
- `//` = integer divide, `%` = integer modulo (both operands must be integral or you get `x//y, x not int`), `^` and `**` both exponentiate, `=`/`==` both equality. `if(cond, true, false)` requires exactly 3 args.
- `check.f90:s_checkfn` compares with relative tolerance `epsilon(r)` by default; pass explicit `tol=` for transcendental/Bessel comparisons.
