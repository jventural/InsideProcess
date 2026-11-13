## Submission

This is the first submission of InsideProcess (version 0.1.0).

## Test environments

* Local: Windows 11 x64, R 4.4.1, `R CMD check --as-cran` (with the PDF manual).
* win-builder: R-devel (Windows).

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  New submission.

Words that may be flagged as possibly misspelled in the Description are
proper names of authors cited with their DOI (Benitez, Padilla, Tourangeau,
Rips, Rasinski, Gwet) and standard technical terms (kappa, AC1).

The functions that write files (`rp_form()`, `rp_apps_script()`,
`rp_google_form()`, `rp_coding_template()`, `rp_word()`) have no default
path: the user always gives it, and the examples write to `tempdir()` and
remove the files at the end. No function changes `par()`, `options()` or the
working directory, sets a seed or installs packages.

## Downstream dependencies

There are currently no downstream dependencies for this package.
