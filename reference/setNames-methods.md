# Get dataset names

Extracts dataset names of a MAgPIE-object

## Usage

``` r
# S4 method for class 'magpie'
setNames(object = nm, nm)
```

## Arguments

- object:

  MAgPIE object

- nm:

  a vector of names current names should be replaced with. If only one
  data element exists you can also set the name to NULL.

## Details

setNames is a shortcut to use a MAgPIE object with manipulated data
names. The setNames method uses the variable names "object" and "nm" in
order to be consistent to the already existing function setNames.

## Methods

- list("signature(object = \\ANY\\)"):

  normal setNames method

- list("signature(object = \\magpie\\)"):

  setNames for MAgPIE objects

## See also

[`getNames`](getNames.md),

Other GetterSetter: [`getCPR()`](getCPR.md),
[`getCells()`](getCells.md), [`getComment()`](getComment.md),
[`getCoords()`](getCoords.md), [`getDim()`](getDim.md),
[`getItems()`](getItems.md), [`getNames()`](getNames.md),
[`getRegionList()`](getRegionList.md), [`getRegions()`](getRegions.md),
[`getSets()`](getSets.md), [`getYears()`](getYears.md),
[`setItems()`](setItems.md)
