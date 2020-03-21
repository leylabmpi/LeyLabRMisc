LeyLab R misc
==============

Misc R functions, rmarkdown templates, etc. for making life a bit easier in the Ley Lab

Feel free to contribute! Just clone the repo, make some changes, commit, and create a pull request.

# Install

## `renv` R package

`renv::install("leylabmpi/LeyLabRMisc")`

## `devtools` R package

`devtools::install_github("leylabmpi/LeyLabRMisc")`

## `source()` function

* Clone the repo: `git clone git@github.com:leylabmpi/LeyLabRMisc.git`
* In your R session, use `source("path/to/repo/init.R")`
  * **Note:** you will need to source each `*.R` file individually

### Submodules

* If this repo will be inside another git repo, then use `git submodule`
  * See [this stack overflow post](https://stackoverflow.com/questions/1811730/how-do-i-work-with-a-git-repository-within-another-repository)

# Functions

* `R/init.R`
  * General functions that could be sourced at the start of a Jupyter Notebook or Rmarkdown file
* `R/itol.R`
  * Functions for creating input files for iTOL
* `R/phyloseq.R`
  * Helper functions for working with phyloseq

# Templates

Rmarkdown templates are in `inst/rmarkdown/templates/`

To use a template:

* Install this package
* Restart R
* Go to `File => New File => R Markdow ... => From Template`


# TODO

## Fix

```
#' Determine counts of setdiff, intersect, & union of 2 vectors (or data.tables)
#'
#' The output is printed text of intersect, each-way setdiff, and union.
#' Data.table compatible! Just make sure to provide sel_col_x and/or sel_col_y
#'
#' @param x vector1 or data.table. If data.table, sel_col_x must not be NULL
#' @param y vector2 or data.table. If data.table, sel_col_y must not be NULL
#' @param sel_col_x If x = data.table, which column to assess?
#' @param sel_col_y If y = data.table, which column to assess?
#' @return NULL
#'
overlap = function(x, y, sel_col_x=NULL, sel_col_y=NULL){
  if(any((class(x)) == 'data.table')){
    if(is.null(sel_col_x)){
      stop('sel_col_x cannot be NULL for data.table objects')
    }
    sel_col_x = ggplot2::enexpr(sel_col_x)
    x = tidytable::dt_distinct(x, !!sel_col_x)
    x = tidytable::dt_pull(x, !!sel_col_x)
  }
  if(any((class(y)) == 'data.table')){
    if(is.null(sel_col_y)){
      stop('sel_col_y cannot be NULL for data.table objects')
    }
    sel_col_y = ggplot2::enexpr(sel_col_y)
    y = tidytable::dt_distinct(y, !!sel_col_y)
    y = tidytable::dt_pull(y, !!sel_col_y)
  }
  cat('intersect(x,y):', length(intersect(x,y)), '\n')
  cat('setdiff(x,y):', length(setdiff(x,y)), '\n')
  cat('setdiff(y,x):', length(setdiff(y,x)), '\n')
  cat('union(x,y):', length(union(x,y)), '\n')
}
```
