CHANGES:
- removed experimental metadata code (stored in magclass version with tag "metadata" for later reintegration)
- added tests so that the package has now >90% unit test line coverage
- fixed coding style issues
- deprecated function `fulldim` and removed it from all functions in magclass where it had been used. 
- dimSums as well as magpply, `add_columns`, `add_dimension` and `getCPR` work now for all (sub-dimension)
- `getItems` can now be used to replace values or even remove dimensions and it supports named vectors (to define element-wise what should be substituted with what)
- `read.magpie` and `write.magpie` have been streamlined (e.g. ncdf support is no longer coupled to 59199 cells) which might lead to slightly different behavior than before in some instances 
- added `dim` argument for magclass subsetting (e.g. `x[1, dim = 2]` which is identical to `x[,1,]` - useful for generalization of functions that should act on all main dimensions)
- removed `old_dim_convention` function
- deprecated `getRegionList` (use `getItems` instead)
- replaced `write.report` with `write.report2` and deprecated `write.report2` 

NOTE:
- this upload tries to fix the ifelse test problem on the windows r-devel test machine. Since the windows r-devel test machine did not complain (neither for the old nor the new version) the fix is an educated test and only the submission will show whether it properly addresses the problem. 