# `dimSums` / `magpply` memory blow-up on large objects — analysis & fix plan

*Applies to magclass 7.5.0. Investigated after `dimSums(x, dim = "w")` OOM-killed
(> 9 GB) on a 0.5°-grid magpie object (~67,420 cells × 18 years × 38 data items) in
magpie4's `production()`.*

## Summary

`dimSums()` reduces over a (sub-)dimension by delegating to `magpply()`, which uses a
generic **data.frame + `tapply`** pipeline operating at *element* granularity. On large
arrays this allocates several multiples of an already-large object and performs millions
of R-level group calls, so peak memory grows ~linearly with the element count with a very
large constant — reaching ~10 GB (OOM) for a ~46 M-element grid object. Summing a
dimension is an associative, vectorisable reduction and should instead use a compiled
array path (`rowSums` / indicator-matrix multiply), which is `O(N)` with a single
result-sized allocation.

## Call path

`R/dimSums.R`:

```r
dimSums <- function(x, dim = 3, na.rm = FALSE)
  magpply(X = x, FUN = sum, DIM = dim, na.rm = na.rm)
```

`R/magpply.R` — the reduction is two lines (64–65), after the summed dim's item labels are
cleared (lines 57–59) so duplicate labels get grouped together:

```r
xd  <- as.data.frame.table(x)                              # (1) whole array -> long format
out <- new("magpie", tapply(xd[[4]], xd[1:3], FUN, ...))   # (2) group-and-apply
```

## Why it explodes

`magpply` accepts an arbitrary `FUN`, so it reduces generically at element granularity:

1. **`as.data.frame.table(x)`** materialises the entire array as a long data.frame with
   **one row per element** — `N_total` rows × 4 columns (three index factors + value).
   For 67,420 × 18 × 38 ≈ **46 M elements** that is a 46 M-row data.frame.

2. **`tapply(values, index[1:3], sum)`** builds the interaction of the three remaining
   index columns (length `N_total`), then `split`s the values into groups and `lapply`s
   `sum` over them. Summing a subdimension yields `G ≈ N_total / k` groups (here `k = 2` →
   **~23 M groups of size 2**): ~23 M list nodes and ~23 M closure calls. This is the
   `tapply → lapply → initArray` seen in the OOM stack trace.

Both steps are `O(N_total)` with a large constant and several concurrent copies
(data.frame, interaction factor, split list, result array). Peak is a multiple of an
object already hundreds of MB.

### Measured cost (synthetic object, same shape, `dim = "w"`)

| N cells | elements | `as.data.frame.table` | `tapply(sum)` | **`dimSums` total** | direct add (`rf+irr`) |
|--------:|---------:|----------------------:|--------------:|--------------------:|----------------------:|
| 10,000  | 6.8 M    | 0.3 s / 296 MB (137 MB df) | 4.7 s / 1.4 GB  | **5.0 s / 1.55 GB**  | 0.09 s / 0.36 GB |
| 30,000  | 20.5 M   | 1.0 s / 741 MB (412 MB df) | 14.4 s / 4.2 GB | **13.8 s / 4.4 GB**  | 0.21 s / 0.99 GB |

Peak memory scales ~linearly with element count; extrapolating to the full 67,420-cell
object (~2.25× the 30k case) gives ~10 GB → OOM, matching observation. `tapply` dominates
both time and memory. A plain `rainfed + irrigated` add (vectorised C) is **~65× faster
and ~4× less memory**, but only works because the summed dimension is known to have two
levels.

## Fix options (in magclass)

Unifying idea: **`dimSums` is always `sum`, so it never needs the generic `tapply` path.**
Ordered by value/risk.

### 1. Dedicated vectorised `dimSums` (recommended)
Give `dimSums` its own implementation instead of delegating to `magpply`:
- **Whole main dimension (1/2/3):** `aperm` so kept dims lead and summed dims trail,
  flatten to a matrix `[prod(kept) × prod(summed)]`, call `rowSums()` (C-level), reshape
  back.
- **Subdimension (e.g. `3.2`):** the expensive spatial×temporal axis is untouched; only
  the small data axis aggregates. Flatten to `M = [prod(other dims) × n_data]` and multiply
  by a tiny `n_out × n_data` **0/1 indicator matrix** `S` built from the surviving
  sub-labels: `tcrossprod(M, S)`. One BLAS call; memory ≈ input + output.

Cost becomes `O(N)` with a small constant — no data.frame, no interaction factor, no
per-group list. A naïve R prototype of the indicator-matmul already halved peak memory
(2.0 GB vs 4.4 GB at 30k) before pushing the reshape into C.

### 2. Swap `tapply` for `rowsum()` inside `magpply` (smaller, still large win)
`base::rowsum(values, group)` computes group sums in C, far leaner than
`tapply(..., sum) → split → lapply`. Special-case `identical(FUN, sum)` (exactly what
`dimSums` passes). Lower blast radius, removes the 23 M-call bottleneck, but still keeps the
`as.data.frame.table` materialisation, so only a partial fix.

### 3. Drop `as.data.frame.table`; reduce on the array directly
Even for general `FUN`, reshape to a matrix and use `apply(mat, 1, FUN)` — avoids the
4-column data.frame and interaction factor (a large share of memory). Still `O(G)` R calls
for non-vectorisable functions, but strictly cheaper and fully general.

### 4. Fast path for common associative FUNs, generic fallback otherwise
`sum, prod, max, min, mean, any, all` have C-level vectorised equivalents
(`rowSums`/`rowMeans`/`matrixStats`/matrix multiply). Dispatch those to the vectorised path
(option 1/2); keep `as.data.frame.table + tapply` only for genuinely arbitrary functions.
Preserves `magpply`'s generality while making the hot paths (`dimSums`, `dimMeans`) cheap.

## Recommendation

Implement **option 1** for `dimSums` (hot path; `sum` is trivially vectorisable), optionally
**option 2/4** to speed up `magpply` generally. Ship with a regression test comparing the
new `dimSums` against the current `magpply`-based output across:
- each main dim (1, 2, 3) and combinations,
- subdimensions (e.g. `3.1`, `3.2`) and mixed main+sub,
- `na.rm = TRUE/FALSE`,
- objects with `NULL` dimnames on some dims,
tolerating floating-point differences at ~1 ULP (the vectorised route sums in a different
order than element-wise `tapply`).

### Downstream note
Once fixed, magpie4's `production()` workaround (summing the water subdimension via
`rainfed + irrigated` instead of `dimSums(dim = "w")`, `R/production.R`) can be reverted to
a plain `dimSums(dim = "w")`.
