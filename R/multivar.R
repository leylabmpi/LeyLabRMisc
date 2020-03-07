#' Wrapper for cmdscale
#'
#' Simple wrapper for cmdscale to provide data.frame formatted table
#'
#' @param dist_mtx distance matrix object
#' @return data.frame
calc_pcoa = function(dist_mtx){
  pcoa = cmdscale(dist_mtx, eig=TRUE)
  df = pcoa$points %>% as.data.frame
  colnames(df) = c('PC1', 'PC2')
  df$PC1_perc_exp = as.vector(pcoa$eig[1])/sum(pcoa$eig) * 100
  df$PC2_perc_exp = as.vector(pcoa$eig[2])/sum(pcoa$eig) * 100
  df$sample = rownames(df)
  return(df)
}

#' vegdist + UniFrac
#'
#' A wrapper around vegan::vegdist and GUniFrac
#' For unifrac: "wunifrac" = weighted unifrac, "unifrac" = unweighted unifrac
#' The function returns a tidy dataframe of PCoA axes (PC1 & PC2),
#' percent variance explained for each PC.
#'
#' @param df sample x taxon dataframe. colnmaes (taxa) must match the tree if the tree is provided
#' @param method distance method (vegdist or UniFrac)
#' @param tree phylogeny with tips matching the df colnames
#' @return data.frame
calc_beta_div = function(df, method, tree){
  vegDists = c('manhattan', 'euclidean', 'canberra', 'clark', 'bray',
               'kulczynski', 'jaccard', 'gower', 'altGower', 'morisita',
               'horn', 'mountford', 'raup', 'binomial', 'chao', 'cao',
               'mahalanobis')
  if(method %in% vegDists){
    d = vegan::vegdist(df, method=method)
    return(d)
  } else
    if (method %in% c('wunifrac', 'unifrac')){
      if(is.null(tree)){
        stop('tree cannot be NULL')
      }
      to_rm = setdiff(tree$tip.label, colnames(df))
      if(length(to_rm) > 0){
        tree = ape::drop.tip(tree, to_rm)
      }
      unifracs = GUniFrac::GUniFrac(df, tree, alpha=c(1))$unifracs
      if(method == 'wunifrac'){
        return(unifracs[, , "d_1"]) # Weighted UniFrac

      }
      if(method == 'unifrac'){
        return(unifracs[, , "d_UW"]) # Unweighted UniFrac
      }
    } else {
      stop(paste0('Method not supported:', method))
    }
}

#' creating a string with distance & percent explained
.dist_fmt = function(dist, PC1_perc_exp, PC2_perc_exp){
  glue::glue('{dist}, PC1: {PC1}%, PC2: {PC2}%', dist=dist,
             PC1=round(PC1_perc_exp, 1),
             PC2=round(PC2_perc_exp, 1))
}

#' PCoA on a 'long' (tidy) tibble, and a long tibble is returned
#'
#' Perform PCoA in a "tidy" way.
#' If multiple methods provided, all PCoA results will be combined into one data.frame.
#'
#' @param df data.frame or tibble
#' @param taxon_col the column specifying taxa or OTUs (no quotes needed)
#' @param sample_col the column specifying sample names (no quotes needed)
#' @param abundance_col the column specifying the taxon abundances in each sample (no quotes needed)
#' @param dists vector of beta-diversity distances ('wunifrac' = weighted UniFrac, 'unifrac' = unweighted Unifrac, see vegan::vegsist for others)
#' @param tree phylogeny for UniFrac calculations. It can have more tips that what is in the data.frame
#' @param threads number of parallel calculations (1 thread per distance)
#' @return data.frame
tidy_pcoa = function(df, taxon_col, sample_col, abundance_col,
                     dists = c('bray', 'jaccard'), tree = NULL, threads=1){
  require(dplyr)
  # convert long to wide
  sample_col_str = rlang::as_string(ensym(sample_col))
  taxon_col = dplyr::enquo(taxon_col)
  sample_col = dplyr::enquo(sample_col)
  abundance_col = dplyr::enquo(abundance_col)
  ## sample x taxon data.frame
  message('Formatting data.frame\n',appendLF=FALSE)
  df_w = df %>%
    dplyr::distinct(!!taxon_col, !!sample_col, !!abundance_col) %>%
    tidyr::spread(!!taxon_col, !!abundance_col, fill=0) %>%
    tibble::column_to_rownames(var=sample_col_str) %>%
    as.data.frame
  # calculating distance s
  message('Calculating distances\n',appendLF=FALSE)
  dists = as.list(dists)
  names(dists) = unlist(dists)
  if(threads > 1){
    doParallel::registerDoParallel(threads)
  }
  df = dists %>%
    plyr::llply(function(x) calc_beta_div(df_w, method=x, tree=tree),
                .parallel=threads > 1) %>%
    lapply(calc_pcoa) %>%
    data.table::rbindlist(use.names=TRUE, fill=TRUE, idcol='distance') %>%
    as_tibble %>%
    dplyr::mutate(distance_percExp = mapply(.dist_fmt, distance, PC1_perc_exp, PC2_perc_exp))

  return(df)
}
