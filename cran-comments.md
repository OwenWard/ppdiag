## Resubmission

This is a resubmission. In this version I have:

* Completed the changes requested by Gregor Sayer.
* Added missing return value for all plotting functions.
* Removed \dontrun{} around corresponding plotting examples
* Introduced a verbose option where needed so that all
non stop messages in functions are
only printed to the console if verbose is TRUE (default FALSE)
* used on.exit to restore user par options are not changed outside of functions
where it is required.


## Test environments
* local Windows R 4.0.4
* ubuntu 20.04 (github actions) R 4.0.4
* macOS latest (github actions) R 4.0.4
* win-builder (devel and release)

## R CMD check results 

There were no ERRORs or WARNINGs

There was 1 NOTE (win-builder):

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Owen G. Ward <owen.ward@columbia.edu>'

  First CRAN submission.