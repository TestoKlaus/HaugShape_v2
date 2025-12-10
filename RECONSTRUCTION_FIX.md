# Shape Reconstruction Fix

## Problem Identified

The shape reconstruction feature was producing garbled shapes (random assembly of lines) instead of proper shape outlines. The screenshot shows the issue clearly.

## Root Cause

The problem was a **normalization mismatch** in the Elliptical Fourier Analysis (EFA) reconstruction:

1. **Forward Transform (Analysis)**: When `shape_analysis()` runs with `norm=TRUE` (default), the `efourier()` function:
   - Computes raw Fourier coefficients: `an, bn, cn, dn`
   - Applies `efourier_norm()` to normalize them to: `A, B, C, D`
   - Stores only the normalized coefficients in the PCA model

2. **Inverse Transform (Reconstruction)**: The `efourier_i()` function expects:
   - Raw (non-normalized) coefficients: `an, bn, cn, dn`
   - NOT normalized coefficients: `A, B, C, D`

3. **The Bug**: The reconstruction code was passing normalized `A, B, C, D` coefficients directly to `efourier_i()`, which treated them as if they were raw `an, bn, cn, dn` coefficients. This caused incorrect shape reconstruction.

## Technical Details

### Normalization Formula (from `efourier_norm`)

The normalization applies three transformations to raw coefficients:
1. **Phase alignment** (`theta`): Aligns the first harmonic
2. **Size normalization** (`size = 1/scale`): Normalizes by the first ellipse magnitude
3. **Rotation** (`psi`): Orients to a standard reference frame

```r
normalized = size * rotation_matrix * raw * phase_shift_matrix
```

### Denormalization Formula (Inverse)

To reconstruct properly, we need:
```r
raw = (1/size) * inverse(rotation_matrix) * normalized * inverse(phase_shift_matrix)
```

## Solution Implemented

### 1. Created Denormalization Function

Added `.efourier_denorm()` helper function in `shape_reconstruction_module.R` that:
- Takes normalized coefficients `A, B, C, D` and normalization parameters (`size`, `theta`, `psi`)
- Reverses the normalization transformations
- Returns raw coefficients `an, bn, cn, dn`

### 2. Updated Reconstruction Logic

Modified `.reconstruct_single_shape()` to:
1. Detect if coefficients are normalized (check `model$efa_object$norm`)
2. If normalized:
   - Calculate normalization parameters from the first harmonic
   - Call `.efourier_denorm()` to convert `A, B, C, D` → `an, bn, cn, dn`
3. Pass the (now raw) coefficients to `efourier_i()`

### Code Changes

**File**: `R/shape_reconstruction_module.R`

**Added**:
```r
.efourier_denorm <- function(A, B, C, D, size, theta, psi) {
  # Inverse of normalization transformations
  # Returns list with an, bn, cn, dn
}
```

**Modified**: `.reconstruct_single_shape()` to:
- Check normalization flag
- Calculate normalization parameters from first harmonic coefficients
- Denormalize before calling `efourier_i()`

## Why This Matters

Elliptical Fourier Analysis with normalization (`norm=TRUE`) is the default and recommended approach in Momocs because it:
- Removes size, position, and rotation effects
- Makes shapes comparable across specimens
- Improves PCA interpretation

However, this means the coefficients stored in the model are in "normalized space", and they must be transformed back to "real space" before reconstructing actual shape coordinates.

## Testing

To test the fix:
1. Run shape analysis with `norm=TRUE` (default)
2. Load the reconstruction model
3. Set PC scores to zero (should reconstruct mean shape)
4. Verify the reconstructed shape looks like a proper outline, not random lines

## References

- `efourier()`: Lines 88-186 in `R/momocs_core-out-efourier.R`
- `efourier_norm()`: Lines 197-240 in `R/momocs_core-out-efourier.R`
- `efourier_i()`: Lines 273-300 in `R/momocs_core-out-efourier.R`
- Claude, J. (2008). *Morphometrics with R*. Springer.

## Date
December 10, 2025
