---
name: add-inline-rule
description: Add or port an Erlang-codegen inliner rule to this purerl fork, targeting a specific downstream consumer project's local (non-stdlib) modules. Use when asked to inline/optimize a call to a project-local function, or to audit/port existing inliner rules.
---

# Adding an inliner rule

This purerl fork's Erlang-codegen optimizer
(`purerl/src/Language/PureScript/Erl/CodeGen/Optimizer/`) exists to serve one
specific downstream project, referred to in this skill as **the target
project** — it is fine for *this skill file* to name it explicitly (it's
tooling documentation, not the fork's own source), but the fork's own source
files (`Inliner.hs`, `Optimizer.hs`, comments, identifiers, etc.) must never
name it. Keep comments generic there ("local Array/List modules", not a
project name). The target project is `../pay-backend` relative to this repo's
usual checkout location — confirm the actual path with the user if it isn't
there.

Follow these steps in order for each new/ported rule.

## 1. Confirm the target function is project-local, not stdlib

The target project vendors its own base-like modules under its own `lib/`
directory rather than resolving packages normally. Before assuming a call is
`Data.Array`/`Data.List`/etc., check `lib/<Module>.purs`'s `foreign import`
and the matching `lib/<Module>.erl` for the real implementation.

## 2. Determine the real Erlang module atom

Check the target project's own compiled output at
`output/<Module>/<atom>.erl` rather than guessing the name-mangling.
Convention: dots → underscores, first letter lowercased, camelCase preserved,
`@ps` suffix (e.g. `Array` → `array@ps`, `Data.Array` → `data_array@ps`,
`NonEmptyArray` → `nonEmptyArray@ps`).

## 3. Check `CoreFn/Optimizer.hs` first

`src/Language/PureScript/CoreFn/Optimizer.hs` runs upstream of Erlang codegen
and applies to *every* backend. Anything already handled there doesn't need a
purerl-specific rule — grep it for the target identifier before writing a new
Erlang-AST-level rule. (Past mistake: a `$`/`#` beta-reduction rule was added
to `Inliner.hs` that `optimizeDataFunctionApply` already handled generically;
it was dead code and got removed.)

## 4. Match both call shapes

purerl's codegen produces either:

- curried: `EApp _ (EApp _ (EAtomLiteral (Atom (Just mod) fn)) []) [x]`
  (matched via `isModFn`/`isFn`)
- uncurried/direct-saturated: `EApp _ (EAtomLiteral (Atom (Just mod) fn)) [x]`
  (matched via `isUncurriedFn'`)

`Inliner.hs`'s existing helpers `inlineNonClassFunction` /
`inlineNonClassUnaryFunction` already handle both shapes — mirror their
pattern for new rules instead of reinventing it.

## 5. Constant-folding a literal argument

To fold when an argument is a literal (e.g. an array), match the literal AST
constructor directly (`EArrayLiteral es`) — PureScript literals compile
straight to that node. Don't be misled by how the pretty-printer renders a
node (e.g. `EArrayLiteral es` prints as `array:from_list([es...])`,
`Pretty.hs:121-125`) into thinking there's an intermediate call to match
against, or into "folding" a case that would render identically either way
(no actual optimization, just a no-op rewrite — check the pretty-printer
before assuming a fold is worthwhile).

## 6. Verify by hand

There is no golden/example test suite for Erlang codegen output in this repo.
Build a throwaway scratch PureScript project (module names/shapes mirroring
the real target-project module) under a scratch/temp dir, rebuild the
compiler (`stack build purescript:exe:purs`), compile the scratch project,
and read the generated `.erl` directly to confirm the rewrite fires.
Grepping the target project's own already-compiled `output/**/*.erl` first is
the fastest way to find real, representative call-site shapes (curried vs.
uncurried, literal-arg cases) to design the rule against.

**Caching gotcha:** `purs`'s incremental cache (`output/cache-db.cbor`) keys
on source-file hashes only, not the compiler binary version. After rebuilding
`purs`, delete `output/` (both in the scratch project and, at final
verification time, in the target project) before recompiling — otherwise
unchanged sources silently produce stale output from the previous binary.

## 7. Run the full test suite

`stack test purescript:tests` after each change, to catch regressions
elsewhere — it won't catch Erlang-codegen-specific issues (that's what step 6
is for).

## 8. Wiring reminder

`Optimizer.hs`'s `optimize` function is the single live entry point. Several
passes exist in the codebase but are *not* wired in (`MagicDo`, `Memoize`,
`Unused`, `tidyUp`, `Guards.hs`, and — until revived for `Maybe`/`Either` —
`inlineCommonFnsM`), deliberately disconnected for compile-time savings
(see `git show 3e8ce78d`). Reviving a currently-dead pass into scope is a
bigger decision than adding a new pure rule — confirm scope with the user
before doing it (e.g. `MagicDo` was explicitly deferred as out of scope in a
past pass).

## 9. Documentation in the target project

If asked to annotate inlined functions in the target project's own source:
add `-- | ! Inlined by the compiler` directly above the existing
`-- | > O(...)` complexity line (add both fresh if no doc comment exists).
For typeclass-method rules, annotate the specific inlined *instance*
implementation, not the class member signature — skip instances the rule
doesn't actually cover.

This step edits the target project's repo directly. Confirm that's in scope
for the current task before doing it — permission to edit the target
project for one task (docs, tests) doesn't carry over to unrelated changes
there.

## 10. Test coverage in the target project

If asked to backfill missing tests for touched functions: check the target
project's own test suite first (its `Test/*.purs` / `*Tests.purs` files) for
existing coverage before adding anything.

**After any batch of automated/delegated edits to the target project, always
recompile it before declaring done** — `purs compile --codegen erl
"lib/**/*.purs"` with `output/` cleared per the caching gotcha above.
Delegated documentation/test-writing passes have introduced real compile
errors before (a bare `pure` where the project's own `Prelude` doesn't
export one; a `foreign import` missing from its module's export list;
ambiguous-type-variable test cases needing explicit annotations) — a diff
review alone will not catch these; only an actual build does.

## 11. Flag scope creep

If a delegated task (e.g. "add tests for existing functions") leads to
adding genuinely new API surface in the target project (e.g. a function that
had a live FFI implementation but no PureScript declaration at all, made
callable for the first time to satisfy the task), flag this to the user
explicitly rather than deciding unilaterally to keep or drop it.
