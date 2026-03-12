# Copy Attributes

This function copies attributes from one object and assigns them to
another.

## Usage

``` r
copy.attributes(
  from,
  to,
  delete = c("names", "row.names", "class", "dim", "dimnames"),
  delete2 = NULL
)

copy.attributes(
  to,
  delete = c("names", "row.names", "class", "dim", "dimnames"),
  delete2 = NULL
) <- value
```

## Arguments

- from:

  object from which the attributes should be taken

- to:

  object to which the attributes should be written

- delete:

  attributes which should not be copied. By default this are class
  specific attributes which might cause problems if copied to another
  object. But you can add or remove attributes from the vector.

- delete2:

  Identical to delete and just added for convenience for the case that
  you want to delete additional attributes but do not want to repeat the
  vector given in delete. In the function both vectors, delete and
  delete2, are just merged to one deletion vector.

- value:

  Same as "from" (object from which the attributes should be taken)

## Functions

- `copy.attributes( to, delete = c("names", "row.names", "class", "dim", "dimnames"), delete2 = NULL ) <- value`:
  assign attributes from object "value"

## See also

Other ObjectCreation: [`clean_magpie()`](clean_magpie.md),
[`complete_magpie()`](complete_magpie.md),
[`new.magpie()`](new.magpie.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
from <- array(12)
attr(from, "blablub") <- "I am an attribute!"
attr(from, "blablub2") <- "I am another attribute!"

print(attributes(from))
#> $dim
#> [1] 1
#> 
#> $blablub
#> [1] "I am an attribute!"
#> 
#> $blablub2
#> [1] "I am another attribute!"
#> 

to <- as.magpie(0)
print(attributes(to))
#> $dim
#> [1] 1 1 1
#> 
#> $class
#> [1] "magpie"
#> attr(,"package")
#> [1] "magclass"
#> 
#> $dimnames
#> $dimnames$fake
#> [1] "GLO"
#> 
#> $dimnames$<NA>
#> NULL
#> 
#> $dimnames$<NA>
#> NULL
#> 
#> 

copy.attributes(to) <- from
print(attributes(to))
#> $dim
#> [1] 1 1 1
#> 
#> $class
#> [1] "magpie"
#> attr(,"package")
#> [1] "magclass"
#> 
#> $dimnames
#> $dimnames$fake
#> [1] "GLO"
#> 
#> $dimnames$<NA>
#> NULL
#> 
#> $dimnames$<NA>
#> NULL
#> 
#> 
#> $blablub
#> [1] "I am an attribute!"
#> 
#> $blablub2
#> [1] "I am another attribute!"
#> 
```
