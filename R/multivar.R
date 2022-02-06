#' calculate Faith's PD
#' @return PhyloMeasures::pd.query return object
.calc_pd = function(mtx, tree){
  mtx %>%
    apply(2, function(x) ifelse(is.na(x) | x <= 0, 0, 1)) %>%
    PhyloMeasures::pd.query(tree, ., standardize = FALSE,
                            null.model="uniform",
                            reps=100, seed=3982)
}

#' Calculate common alpha-diversity metrics
#' You need the "vegan" package installed to your R project and loaded for this code to run
#'
#' Faith's Phylogenetic Diversity ("PD") can be calculated only
#' if a tree is provided. The tree can have extra tips, but there
#' must be tip labels for all taxa in the provided table.
#'
#' @param df sample x taxon abundance table (usual format for vegan)
#' @param tree tree with tips matching taxa in the abundance table (only needed for PD)
#' @param index which of the indices to calculate? (nobs = no. of observations, shannon = Shannon Index, PD = Faith's PD)
#' @return a data.frame of alpha diversity values (and sample names)
#' @export
calc_alpha_div = function(df, tree=NULL, index=c('nobs', 'shannon', 'PD')){
  res = list()
  res[['Sample']] = rownames(df)
  if('nobs' %in% index){
    res[['nobs']] = apply(df, 1, function(x) sum(x > 0)) %>% as.vector
  }
  if('shannon' %in% index){
    res[['shannon']] = vegan::diversity(df, index='shannon') %>% as.vector
  }
  if('PD' %in% index & !is.null(tree)){
    res[['PD']] = .calc_pd(df, tree) %>% as.vector
  }
  return(do.call(cbind, res))
}

#' beta-diversity calculation
#'
#' A wrapper around vegan::vegdist and rbiom (rbiom used for UniFrac calculations).
#' For unifrac: "wunifrac" = weighted unifrac, "unifrac" = unweighted unifrac.
#' The function returns a tidy dataframe of PCoA axes (PC1 & PC2),
#' percent variance explained for each PC.
#'
#' Unifrac is calculated with the https://github.com/cmmr/rbiom package
#' (requires bioconductor packages).
#'
#' If the goal is PCoA, then see the "tidy_PCoA" function.
#'
#' @param df sample x taxon dataframe. Colnames (taxa) must match the tree tip labels if the tree is provided
#' @param tree phylogeny with tips matching the df colnames (only needed for wunifrac & unifrac methods)
#' @param method distance method (vegdist distances; wunifrac=Weighted Unifrac; unifrac=Unweighted Unifrac)
#' @param threads threads used for UniFrac calculations with rbiom
#' @return data.frame
#' @export
calc_beta_div = function(df, tree=NULL,
                         method = c('wunifrac', 'unifrac', 'manhattan', 'euclidean', 'canberra', 'clark', 'bray',
                                    'kulczynski', 'jaccard', 'gower', 'altGower', 'morisita',
                                    'horn', 'mountford', 'raup', 'binomial', 'chao', 'cao', 'mahalanobis'),
                         threads=1){
  vegDists = c('manhattan', 'euclidean', 'canberra', 'clark', 'bray',
               'kulczynski', 'jaccard', 'gower', 'altGower', 'morisita',
               'horn', 'mountford', 'raup', 'binomial', 'chao', 'cao',
               'mahalanobis')
  method = method[1]
  message('Calculating distance: ', method)
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
      if(length(intersect(tree$tip.label, colnames(df))) < ncol(df)){
        stop('tree tip labels and df colnames do not match!')
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
#' @param label1 First PC label
#' @param label2 Seconda PC label
#' @return str, formatted as "metric, <PC1_perc_exp>%, <PC2_perc_exp>%"
#' @export
#' @importFrom glue glue
dist_format = function(dist, PC1_perc_exp, PC2_perc_exp, label1=1, label2=2){
  glue::glue('{dist}, PC{PC_L1}: {PC1}%, PC{PC_L2}: {PC2}%',
             dist=dist,
             PC_L1 = label1,
             PC_L2 = label2,
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
#' @export
qsave_obj = function(x, file, msg = 'Writing file to: ', threads=1){
  if(!is.null(file)){
    message(msg, file)
    qs::qsave(x, file=file, nthreads=threads)
  }
  return(x)
}

#' Convert PCoA object to a tidy dataframe
#'
#' @param pcoa A pcoa object generated by cmdscale
#' @param k The number of PCs to keep
#' @return A data.frame of PCoA points for the top k PCs
#' @importFrom glue glue
#' @export
pcoa2df = function(pcoa, k=3){
  df = pcoa$points %>% as.data.frame
  if(ncol(df) < k){
    k = ncol(df)
  }
  df = df[,1:k]
  colnames(df) = gsub('^', 'PC', 1:k)
  for(i in 1:k){
    x = glue::glue('PC{k}_perc_exp', k=i)
    df[,x] = as.vector(pcoa$eig[i])/sum(pcoa$eig) * 100
  }
  df$sample = rownames(df)
  return(df)
}

#' Wrapper for cmdscale
#'
#' Simple wrapper for cmdscale to provide data.frame formatted table.
#' If the distance matrices contain NAs, the samples containing NAs
#' will be removed (with a warning).
#'
#' @param dist_mtx distance matrix object
#' @return data.frame
#' @export
calc_PCoA = function(dist_mtx, k=2){
  # filtering NAs
  dist_mtx = as.matrix(dist_mtx)
  n_NAs = rowSums(is.na(dist_mtx)) + colSums(is.na(dist_mtx))
  n_samps = nrow(dist_mtx)
  if(n_NAs > 0){
    warning('Number of NAs in dist matrix: ', n_NAs)
    dist_mtx = dist_mtx[rowSums(is.na(dist_mtx)) == 0, colSums(is.na(dist_mtx)) == 0, drop = FALSE]
    warning('Number of samples filtered due to NAs in dist matrix: ', n_samps - nrow(dist_mtx))
  }
  # cmdscale
  pcoa = cmdscale(as.dist(dist_mtx), k=k, eig=TRUE)
  return(pcoa)
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
#' @param k passed to cmdscale
#' @param dist_mtx_file file name for saving the distance matrices (qs serialization; use ".qs" for the file extension)
#' @param pcoa_file file name for saving the raw pcoa results
#' @return a tibble of PCoA info for all selected "dists"
#' @export
#' @importFrom rlang as_string
#' @importFrom dplyr enquo distinct
#' @importFrom tidyr spread
#' @importFrom data.table rbindlist
#' @importFrom plyr llply
tidy_pcoa = function(df, taxon_col, sample_col, abundance_col,
                     dists = c('bray', 'jaccard', 'wunifrac', 'unifrac'),
                     tree = NULL, threads=1, threads_unifrac=1, k=2,
                     dist_mtx_file = NULL, pcoa_file = NULL){
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
    textshape::column_to_rownames(var=sample_col_str) %>%
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
    qsave_obj(file=dist_mtx_file, msg='Writing distance matrices to: ', threads=threads) %>%
    lapply(calc_PCoA, k=k) %>%
    qsave_obj(file=pcoa_file, msg='Writing PCoA objects to: ', threads=threads) %>%
    lapply(pcoa2df, k=k) %>%
    data.table::rbindlist(use.names=TRUE, fill=TRUE, idcol='distance') %>%
    as_tibble %>%
    dplyr::mutate(distance_percExp12 = mapply(dist_format, distance, PC1_perc_exp, PC2_perc_exp,
                                              label1=1, label2=2),
                  distance_percExp13 = mapply(dist_format, distance, PC1_perc_exp, PC3_perc_exp,
                                              label1=1, label2=3))
  if(k > 2){
      df = dplyr::mutate(df, distance_percExp23 = mapply(dist_format, distance,
                                                         PC2_perc_exp, PC3_perc_exp,
                                                         label1=2, label2=3))
  }

  return(df)
}

#' Convert tidy beta diversity table to a wide distance matrix
#'
#' The input should have the columns: Measure, SampleX, SampleY, Value
#'
#' The output can be used for creating a PCoA. dendrogram, etc
#'
#' @param dt data.table, data.frame, or tibble
#' @param measure which beta diversity measure (Measure column) to use?
#' @return a symmetric matrix of distances
#' @export
#' @import tidytable
#' @examples
#' beta2mtx(beta_div_table, measure='braycurtis') %>%
#'     calc_PCoA() %>%
#'     pcoa2df
beta2mtx = function(dt, measure){
    dt = dt %>%
      as.data.table %>%
      filter.(Measure == measure) %>%
      select.(Measure, SampleX, SampleY, Value) %>%
      pivot_wider.(names_from = SampleY, values_from = Value, values_fill = 0) %>%
      select.(-Measure) %>%
      as.data.frame
    rownames(dt) = dt$SampleX
    dt$SampleX = NULL
    return(dt %>% as.matrix)
}
