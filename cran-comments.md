## CRAN note

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Jan Philipp Dietrich <dietrich@pik-potsdam.de>'

Version jumps in minor (submitted: 4.106.2, existing: 4.51.1)

Unknown, possibly mis-spelled, fields in DESCRIPTION:
  'ValidationKey'

Reading CITATION file fails with
  there is no package called 'magclass'
when package is not installed.

## Explanation

The package uses an additional DESCRIPTION entry "ValidationKey" for internal evaluation purposes. It can be safely ignored.
The CITATION files contains calculations which work properly as soon as the package has been installed. So, running citation("magclass") works as intented.


