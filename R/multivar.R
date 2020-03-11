#' Wrapper for cmdscale
#'
#' Simple wrapper for cmdscale to provide data.frame formatted table.
#' If the distance matrices contain NAs, the samples containing NAs
#' will be removed (with a warning).
#'
#' @param dist_mtx distance matrix object
#' @return data.frame
calc_pcoa = function(dist_mtx){
  # filtering NAs
  dist_mtx = as.matrix(dist_mtx)
  n_NAs = rowSums(is.na(dist_mtx)) + colSums(is.na(dist_mtx))
  n_samps = nrow(dist_mtx)
  if(n_NAs > 0){
    warning('Number of NAs in dist matrix: ', n_NAs)
    dist_mtx = dist_mtx[rowSums(is.na(dist_mtx)) == 0, colSums(is.na(dist_mtx)) == 0, drop = FALSE]
    warning('Number of samples filtered due to NAs in dist matrix: ', n_samps - nrow(dist_mtx))
  }
  pcoa = cmdscale(as.dist(dist_mtx), k=2, eig=TRUE)
  dist_mtx = NULL
  df = pcoa$points %>% as.data.frame
  colnames(df) = c('PC1', 'PC2')
  df$PC1_perc_exp = as.vector(pcoa$eig[1])/sum(pcoa$eig) * 100
  df$PC2_perc_exp = as.vector(pcoa$eig[2])/sum(pcoa$eig) * 100
  df$sample = rownames(df)
  return(df)
}

#' vegdist + UniFrac calculation
#'
#' A wrapper around vegan::vegdist and rbiom (rbiom used for UniFrac calculations).
#' For unifrac: "wunifrac" = weighted unifrac, "unifrac" = unweighted unifrac.
#' The function returns a tidy dataframe of PCoA axes (PC1 & PC2),
#' percent variance explained for each PC.
#'
#' @param df sample x taxon dataframe. colnmaes (taxa) must match the tree tip labels if the tree is provided
#' @param method distance method (vegdist or UniFrac)
#' @param tree phylogeny with tips matching the df colnames
#' @param threads threads used for UniFrac calculations with rbiom
#' @return data.frame
calc_beta_div = function(df, method, tree, threads=1){
  vegDists = c('manhattan', 'euclidean', 'canberra', 'clark', 'bray',
               'kulczynski', 'jaccard', 'gower', 'altGower', 'morisita',
               'horn', 'mountford', 'raup', 'binomial', 'chao', 'cao',
               'mahalanobis')
  if(method %in% vegDists){
    # standard vegdist diversity
    d = vegan::vegdist(df, method=method)
    return(d)
  } else
    if (method %in% c('wunifrac', 'unifrac')){
      # unifrac with rbiom
      ## threads for rbiom
      options('rbiom.max.threads' = as.integer(threads))
      doParallel::registerDoParallel(threads)
      # tree
      if(is.null(tree)){
        stop('tree cannot be NULL')
      }
      to_rm = setdiff(tree$tip.label, colnames(df))
      if(length(to_rm) > 0){
        tree = ape::drop.tip(tree, to_rm)
      }
      ## UniFrac calc
      if(method == 'wunifrac'){
        return(rbiom::unifrac(t(as.matrix(df)), tree=tree, weighted=TRUE))
      }
      if(method == 'unifrac'){
        return(rbiom::unifrac(t(as.matrix(df)), tree=tree, weighted=FALSE))
      }
    } else {
      stop(paste0('Method not supported:', method))
    }
}

#' creating a string with distance & percent explained
#'
#' @param dist str, distance metric
#' @param PC1_perc_exp float, percent variance explained for PC1
#' @param PC2_perc_exp float, percent variance explained for PC2
#' @return str, formatted as "metric, <PC1_perc_exp>%, <PC2_perc_exp>%"
.dist_fmt = function(dist, PC1_perc_exp, PC2_perc_exp){
  glue::glue('{dist}, PC1: {PC1}%, PC2: {PC2}%',
             dist=dist,
             PC1=round(PC1_perc_exp, 1),
             PC2=round(PC2_perc_exp, 1))
}

#' Simple function for serializing a distance matrix or list of distance matrices
#'
#' Serializing done with the "qs" R package.
#'
#' @param x a distance matrix or list of distance matrices
#' @param file file name to save to
#' @param threads number of threads used for serializing
#' @return the input distance matrix or list of distance matrices
qsave_dist_mtx = function(x, file, threads=1){
  if(!is.null(file)){
    message('Writing distance matrices to: ', file)
    qs::qsave(x, file=file, nthreads=threads)
  }
  return(x)
}

#' PCoA on a 'long' (tidy) tibble, and a long tibble is returned
#'
#' Perform PCoA in a "tidy" way.
#' If multiple diversity metrics are provided (eg., "bray" and "jaccard"),
#' all PCoA results will be combined into one data.frame.
#'
#' Weighted/Unweighted UniFrac is calculated via the rbiom R package. All other
#' beta-diversity metrics are calculated via the vegan R package.
#'
#' @param df data.frame or tibble
#' @param taxon_col the column specifying taxa or OTUs (no quotes needed)
#' @param sample_col the column specifying sample names (no quotes needed)
#' @param abundance_col the column specifying the taxon abundances in each sample (no quotes needed)
#' @param dists vector of beta-diversity distances ('wunifrac' = weighted UniFrac, 'unifrac' = unweighted Unifrac; see vegan::vegsist for others)
#' @param tree phylogeny for UniFrac calculations. It can have more tips that what is in the data.frame
#' @param threads number of parallel calculations of each distance metric (1 thread per distance)
#' @param threads_unifrac number of threads to use for wunifrac & unifrac calculations
#' @param dist_file file name for saving the distance matrices (qs serialization; use ".qs" for the file extension)
#' @return data.frame
tidy_pcoa = function(df, taxon_col, sample_col, abundance_col,
                     dists = c('bray', 'jaccard'), tree = NULL,
                     threads=1, threads_unifrac=1, dist_file = NULL){
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
    plyr::llply(function(x) calc_beta_div(df_w, method=x, tree=tree, threads=threads_unifrac),
                .parallel=threads > 1) %>%
    qsave_dist_mtx(file=dist_file, threads=threads) %>%
    lapply(calc_pcoa) %>%
    data.table::rbindlist(use.names=TRUE, fill=TRUE, idcol='distance') %>%
    as_tibble %>%
    dplyr::mutate(distance_percExp = mapply(.dist_fmt, distance, PC1_perc_exp, PC2_perc_exp))

  return(df)
}
