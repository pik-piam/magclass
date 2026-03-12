# Package index

## Object Creation

Functions for creating and validating MAgPIE objects.

- [`clean_magpie()`](clean_magpie.md) : MAgPIE-Clean
- [`complete_magpie()`](complete_magpie.md) : complete_magpie
- [`copy.attributes()`](copy.attributes.md)
  [`` `copy.attributes<-`() ``](copy.attributes.md) : Copy Attributes
- [`new.magpie()`](new.magpie.md) : new.magpie

## Getter and Setter Functions

Functions for getting and setting dimension names, items, and metadata
of MAgPIE objects.

- [`getCPR()`](getCPR.md) : Get cells per region
- [`getCells()`](getCells.md) [`` `getCells<-`() ``](getCells.md)
  [`setCells()`](getCells.md) : Get Cells
- [`getComment()`](getComment.md)
  [`` `getComment<-`() ``](getComment.md)
  [`setComment()`](getComment.md) : getComment
- [`getCoords()`](getCoords.md) [`` `getCoords<-`() ``](getCoords.md) :
  Get Coordinates
- [`getDim()`](getDim.md) : getDim
- [`getItems()`](getItems.md) [`` `getItems<-`() ``](getItems.md) : Get
  Items
- [`getNames()`](getNames.md) [`` `getNames<-`() ``](getNames.md) : Get
  dataset names
- [`getRegionList()`](getRegionList.md)
  [`` `getRegionList<-`() ``](getRegionList.md) : Get a list of celluare
  region-belongings
- [`getRegions()`](getRegions.md)
  [`` `getRegions<-`() ``](getRegions.md) : Get regions
- [`getSets()`](getSets.md) [`` `getSets<-`() ``](getSets.md) : Get sets
- [`getYears()`](getYears.md) [`` `getYears<-`() ``](getYears.md)
  [`setYears()`](getYears.md) : Get years
- [`setItems()`](setItems.md) : Set Items
- [`setNames(`*`<magpie>`*`)`](setNames-methods.md) : Get dataset names

## Object Information

Functions for querying object properties like dimensions, size, and
checking object characteristics.

- [`dimExists()`](dimExists.md) : dimExists
- [`fulldim()`](fulldim.md) : Reconstructs full dimensionality of MAgPIE
  objects
- [`hasCoords()`](hasCoords.md) : Has Coordinates
- [`hasSets()`](hasSets.md) : Has Sets
- [`is.temporal()`](is.temporal.md) : is.temporal, is.spatial
- [`isYear()`](isYear.md) : isYear
- [`ncells()`](ncells.md) [`ndata()`](ncells.md)
  [`nregions()`](ncells.md) [`nyears()`](ncells.md) : Count elements
- [`ndim()`](ndim.md) : Count sub-dimensions
- [`sameDims()`](sameDims.md) : sameDims

## Conversion to and from MAgPIE Objects

Functions for converting between MAgPIE objects and other data types
like arrays, data.frames, tibbles, and spatial formats.

- [`as.RasterBrick()`](as.RasterBrick.md) : as.RasterBrick
- [`as.SpatRaster()`](as.SpatRaster.md) : as.SpatRaster
- [`as.SpatRasterDataset()`](as.SpatRasterDataset.md) :
  as.SpatRasterDataset
- [`as.SpatVector()`](as.SpatVector.md) : as.SpatVector
- [`as.array(`*`<magpie>`*`)`](as.array-methods.md) : \~~ Methods for
  Function as.array \~~
- [`as.data.frame(`*`<magpie>`*`)`](as.data.frame-methods.md) : \~~
  Methods for Function as.data.frame \~~
- [`as_tibble(`*`<magpie>`*`)`](as_tibble.magpie.md) : magpie method for
  tibble::as_tibble
- [`unwrap()`](unwrap.md) : Unwrap
- [`wrap()`](wrap.md) : Wrap

## Dimension Manipulation

Functions for manipulating dimensions of MAgPIE objects including
adding, collapsing, and reordering dimensions.

- [`addDim()`](addDim.md) : addDim
- [`add_columns()`](add_columns.md) : add_columns
- [`collapseDim()`](collapseDim.md) : Collapse dataset dimensions
- [`collapseNames()`](collapseNames.md) : Collapse dataset names
- [`dimCode()`](dimCode.md) : dimCode
- [`dimOrder()`](dimOrder.md) : dimOrder
- [`dimReduce()`](dimReduce.md) : dimReduce

## File I/O

Functions for reading and writing MAgPIE objects to various file
formats.

- [`copy.magpie()`](copy.magpie.md) : Copy MAgPIE-files
- [`read.magpie()`](read.magpie.md) : Read MAgPIE-object from file
- [`read.report()`](read.report.md) : Read file in report format
- [`write.magpie()`](write.magpie.md) : Write MAgPIE-object to file
- [`write.report()`](write.report.md) : Write file in report format
- [`write.report2()`](write.report2.md) : Write file in report format

## Aggregation

Functions for summing and aggregating data across dimensions.

- [`colSums(`*`<magpie>`*`)`](colSums-methods.md) : \~~ Methods for
  Function colSums and colMeans \~~
- [`dimSums()`](dimSums.md) : Summation over dimensions
- [`rowSums(`*`<magpie>`*`)`](rowSums-methods.md) : \~~ Methods for
  Function rowSums and rowMeans \~~

## Data Binding

Functions for binding, merging, and expanding MAgPIE objects.

- [`cbind(`*`<magpie>`*`)`](cbind.magpie.md) : cbind method for MAgPIE
  objects
- [`extend()`](extend.md) : extend
- [`magpie_expand()`](magpie_expand.md) : magpie_expand
- [`magpiesort()`](magpiesort.md) : MAgPIE-Sort
- [`matchDim()`](matchDim.md) : Match dimensions of a magpie object to
  those of a reference object
- [`mbind()`](mbind.md) : mbind

## Selection and Calculation

Functions for selecting subsets and performing calculations on MAgPIE
objects.

- [`magpply()`](magpply.md) : magpply
- [`mcalc()`](mcalc.md) : mcalc
- [`mselect()`](mselect.md) [`` `mselect<-`() ``](mselect.md) : MSelect
- [`where()`](where.md) : where

## Display

Functions for displaying, printing, and visualizing MAgPIE objects.

- [`head(`*`<magpie>`*`)`](head.magpie.md) : head/tail

- [`maxample()`](maxample.md) : maxample

- [`mplot()`](mplot.md) : mplot

- [`print(`*`<magpie>`*`)`](print.magpie.md) : print

- [`show(`*`<magpie>`*`)`](show-methods.md) :

  \~~ Method for function `show` \~~

- [`str(`*`<magpie>`*`)`](str.magpie.md) : str

## Temporal Operations

Functions for temporal interpolation, convergence, and time-based
calculations.

- [`commonYears()`](commonYears.md) : commonYears
- [`convergence()`](convergence.md) : convergence
- [`lowpass()`](lowpass.md) : Lowpass Filter
- [`time_interpolate()`](time_interpolate.md) : time_interpolate

## Spatial Operations

Functions for working with spatial data including array reshaping,
coordinate handling, and raster operations.

- [`guessResolution()`](guessResolution.md) : guessResolution
- [`spatRasterToDataset()`](spatRasterToDataset.md) :
  spatRasterToDataset

## Utility Functions

Miscellaneous utility functions.

- [`replace_non_finite()`](replace_non_finite.md) : Replace Non-Finite
  Data
- [`unitjoin()`](unitjoin.md) : joins a data.frame or vector of strings
  with variable and unit separated into a data.frame with variable and
  unit joined as 'variable (unit)'. Use magclass::unitsplit to split
  them again
- [`unitsplit()`](unitsplit.md) : splits a data.frame or vector of
  strings with form 'variable (unit)' into a data.frame with variable
  and unit separated

## Math Operations

Mathematical operations on MAgPIE objects.

- [`log(`*`<magpie>`*`)`](log-methods.md) : log-method for MAgPIE
  objects
- [`round(`*`<magpie>`*`)`](round-methods.md) : Round-method for MAgPIE
  objects
- [`pmax(`*`<magpie>`*`)`](magpie-pmin-pmax.md)
  [`pmin(`*`<magpie>`*`)`](magpie-pmin-pmax.md) : pmin/pmax

## MAgPIE Class and Data

MAgPIE class definition and example data.

- [`magpie-class`](magpie-class.md) [`as.magpie`](magpie-class.md)
  [`as.magpie-methods`](magpie-class.md)
  [`as.magpie,magpie-method`](magpie-class.md)
  [`as.magpie,array-method`](magpie-class.md)
  [`as.magpie,lpj-method`](magpie-class.md)
  [`as.magpie,data.frame-method`](magpie-class.md)
  [`as.magpie,numeric-method`](magpie-class.md)
  [`as.magpie,NULL-method`](magpie-class.md)
  [`as.magpie,quitte-method`](magpie-class.md)
  [`as.magpie,tbl_df-method`](magpie-class.md)
  [`as.magpie,RasterBrick-method`](magpie-class.md)
  [`as.magpie,logical-method`](magpie-class.md)
  [`as.magpie,RasterLayer-method`](magpie-class.md)
  [`as.magpie,RasterStack-method`](magpie-class.md)
  [`as.magpie,SpatRaster-method`](magpie-class.md)
  [`as.magpie,LPJmLData-method`](magpie-class.md)
  [`as.magpie,SpatVector-method`](magpie-class.md)
  [`is.magpie`](magpie-class.md) [`[,magpie-method`](magpie-class.md)
  [`[,magpie,ANY,ANY-method`](magpie-class.md)
  [`[<-,magpie,ANY,ANY-method`](magpie-class.md)
  [`[<-,magpie-method`](magpie-class.md)
  [`Ops,magpie,magpie-method`](magpie-class.md)
  [`Ops,magpie,numeric-method`](magpie-class.md)
  [`Ops,numeric,magpie-method`](magpie-class.md)
  [`ifelse,magpie-method`](magpie-class.md)
  [`is.na,magpie-method`](magpie-class.md)
  [`is.nan,magpie-method`](magpie-class.md)
  [`is.infinite,magpie-method`](magpie-class.md)
  [`is.finite,magpie-method`](magpie-class.md) : Class "magpie" \~\~~
- [`magclass-package`](magclass-package.md)
  [`magclass`](magclass-package.md) : Data Class and Tools for Handling
  Spatial-Temporal Data
- [`magclassdata`](magclassdata.md) : magclassdata
- [`population_magpie`](population_magpie.md) : population_magpie

## Internal Functions

Internal functions and deprecated functions.

- [`add_dimension()`](add_dimension.md) : add_dimension
- [`fulldim()`](fulldim.md) : Reconstructs full dimensionality of MAgPIE
  objects
- [`escapeRegex()`](escapeRegex.md) : escapeRegex
- [`magpie_expand_dim()`](magpie_expand_dim.md) : magpie_expand_dim
- [`sizeCheck()`](sizeCheck.md) : sizeCheck
- [`suppressSpecificWarnings()`](suppressSpecificWarnings.md) :
  suppressSpecificWarnings
