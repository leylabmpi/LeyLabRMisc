#' A simple function that returns a vector of taxonomy levels
#'
#' This just saves some typing, since I find myself constantly typing out:
#' c('Domain', 'Phylum', 'Class', 'Order', 'Family', 'Genus', 'Species')
taxonomy_levels = function(){
  x = c('Domain', 'Phylum', 'Class', 'Order', 'Family', 'Genus', 'Species')
  return(x)
}

#' expand.grid(), but just lower-triange comparisions
#'
#' This is useful when you want pairwise comparisons,
#' but you don't need the reciprical ('a' <=> 'b' & 'b' <=> 'a').
#'
#' @param x a vector
#' @param y a vector
#' @param diag include same-same comparisons ('a' <=> 'b')?
#' @return a data.frame of all non-reciprical comparisons
#' @examples
#' expand.grid.lower(1:3, 1:3)
#' expand.grid.lower(1:3, 1:3, diag=TRUE)
expand.grid.lower = function(x, y, diag=FALSE){
  m = expand.grid(x, y) %>%
    filter(Var1 != Var2) %>%
    mutate(Val = 1) %>%
    spread(Var2, Val) %>%
    as.data.frame
  rownames(m) = m$Var1
  m$Var1 = NULL
  y = which(lower.tri(m, diag = diag), arr.ind=T)
  m = data.frame(Var1 = rownames(m)[y[,'col']],
                 Var2 = rownames(m)[y[,'row']])
  return(m)
}
