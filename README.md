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

# Examples

## Metagenomes

### Reading in bracken tables

```
library(dplyr)
library(ggplot2)
library(data.table)
library(tidytable)
library(LeyLabRMisc)
# listing files
brk_cls_files = list_files(profile_dir, 'all-combined-bracken.tsv') 
brk_cls_files %>% length
# reading tables
brk_cls = brk_cls_files %>% file_list(-2) %>%
    plyr::llply(read_bracken) %>%
    data.table::rbindlist(use.names=TRUE, idcol='dataset')
brk_cls
```

# TODO

