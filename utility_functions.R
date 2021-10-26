#------ diny -----------------
#' Number of days in the year
#'
#' Computes the number of days in a given year, taking in account leap years
#'
#' @param year Numeric or integer vector of the year (format: 4-digit year, %Y).
#'
#' @return Number of days in that year.
#'
#' @examples
#' diny(1999)
#' diny(2000)
#' diny(2004)
#' diny(2100)
#' diny(1600)
#'
#' @export
diny <- function(year) {
  365 + (year %% 4 == 0) - (year %% 100 == 0) + (year %% 400 == 0)
}

#------ zero_lomf -----------------
#' Last \[non-zero\] Observation Moved Forward (lomf)
#'
#' Finds the index, for each row, of the previous row with a non-zero value
#'
#' @param x Numeric vector.
#' @param first (logical) Whether to consider first value as a non-zero value
#'   whose index is moved forward even if it is zero. This prevents having NAs
#'   in the results and somewhat assumes that, for a time series, the day prior
#'   to the first value is non-zero.
#'
#' @return Numeric vector of the indices of the previous non-zero for each
#'   element of the input vector.
#'
#' @examples
#' test1 <- c(1,1,1,0,0,0,0,1,1)
#' zero_lomf(test1)
#' test2 <- c(0,0,0,0,0,1,1,0,1)
#' zero_lomf(test2, first=FALSE)
#' zero_lomf(test2, first=TRUE)
#'
#' @export
zero_lomf <- function(x, first=TRUE) {
  if (length(x) > 0) {
    non.zero.idx <- which(x != 0)
    if(first==T & x[1]==0)
      non.zero.idx=c(1,non.zero.idx)
    #Repeat index of previous row with non-zero as many times gap until next non-zero values
    rep.int(non.zero.idx, diff(c(non.zero.idx, length(x) + 1)))
  }
}

#------ fread_cols -----------------
#' fread columns
#'
#' Fast data.table-based reading of a subset of columns from a table
#'
#' @param file_name path of the table to be read
#' @param cols_tokeep character vector, names of columns to read
#'
#' @return a data.table with the \code{cols_tokeep} that are found in the table
#'
#' @examples
#' fread_cols(iris, c('Sepal.Length', 'Sepal.Width')
#'
#' @export
fread_cols <- function(file_name, cols_tokeep) {
  #Only read the first row from the file
  header <- fread(file_name, nrows = 1, header = FALSE)
  #Check which columns are in the table
  keptcols <- cols_tokeep[cols_tokeep %chin% unlist(header)]
  missingcols <- cols_tokeep[!(cols_tokeep %chin% unlist(header))]
  paste('Importing', file_name, 'with ', length(keptcols),
        'columns out of ', length(cols_tokeep), 'supplied column names')
  #Reading in table
  paste(missingcols, 'columns are not in the file...')
  
  dt <- fread(input=file_name, header=TRUE,
              select=keptcols, verbose=TRUE)
  return(dt)
}
#------ bin_dt -----------------
#' Bin data.table
#'
#' Bins a data.table over a numeric column.
#'
#' @param in_dt \link[data.table]{data.table} to bin.
#' @param binvar (character) column that will be used to define bins.
#' @param binfunc (character) binning approach. One of 'manual', 'equal_length', 'equal_freq'.
#' @param binarg (numeric) binning argument, depends on binning approach (\code{binfunc}).
#' @param bintrans (character or numeric) transformation of \code{binvar}, default is NULL.
#' @param ndigits (integer) number of decimals to keep for displaying formatted bin limits
#' @param na.rm (logical) whether to include NAs.
#' @param valuevar (character) if na.rm = FALSE, column to use to detect NAs and remove records.
#'
#' @return input \link[data.table]{data.table} with four new columns:
#' \itemize{
#'   \item bin - bin number (1 being the lowest value bin)
#'   \item bin_lmin - bin lower limit
#'   \item bin_lmax - bin higher limit
#'   \item bin_lformat - formatted character of bin limits
#'   (format: \code{round(bin_lmin, ndigits) - round(bin_lmax, ndigits))
#' }
#'
#' @details inspired from [rbin package](https://github.com/rsquaredacademy/rbin).
#' Differences include that it concentrates all binning approaches within a single
#' function and works on a data.table.
#'
#' binfunc: \cr
#' \itemize{
#'   \item 'manual' - bin continuous data manually. \code{binarg} sets the inner bin limits,
#'  such that the final table will have \code{length(binarg) + 1} bins. The lower end of the
#'  first bin is automatically set to be the minimum value in \code{binvar} and the upper end of
#'  the last bin is set to be the maximum value in \code{binvar}
#'
#'   \item 'equal_length' - Bin continuous data such that each bin has the same \code{binvar} interval length.
#'   If \code{bintrans} is not \code{NULL}, then interval length is computed on transformed scale.
#'   \code{binarg} (automatically rounded to the nearest integer) sets the number of bins.
#'
#'   \item 'equal_freq' - Bin continuous data such that each bin has the same number of records.
#'   \code{binarg} (automatically rounded to the nearest integer) sets the number of bins.
#' }
#'
#' bintrans: can either be 'log' (for natural log) or a numeric exponent to transform
#' according to x^bintrans.
#'
#' @seealso for examples, see applications in \code{\link{bin_misclass}},
#' \code{\link{eval_watergap}}, \code{\link{tabulate_globalsummary}},
#' \code{\link{formathistab}}, \code{\link{compare_us}}
#'
#' @export
bin_dt <- function(in_dt, binvar, binfunc, binarg,
                   bintrans=NULL, ndigits=2,
                   na.rm=FALSE, valuevar=NULL) {
  #Inspired from rbin, adapted to dt and simplified
  in_dt <- copy(in_dt)
  
  el_freq <- function(byd, bins) {
    bin_length <- (max(byd, na.rm = TRUE) - min(byd, na.rm = TRUE)) / bins
    append(min(byd, na.rm = TRUE), min(byd, na.rm = TRUE) + (bin_length * seq_len(bins)))[1:bins]
    
  }
  
  eu_freq <- function(byd, bins) {
    bin_length <- (max(byd, na.rm = TRUE) - min(byd, na.rm = TRUE)) / bins
    ufreq      <- min(byd, na.rm = TRUE) + (bin_length * seq_len(bins))
    n          <- length(ufreq)
    ufreq[n]   <- max(byd, na.rm = TRUE) + 1
    return(ufreq)
    
  }
  
  binvar_orig <- copy(binvar)
  
  #Remove NAs
  if (na.rm) {
    in_dt <- in_dt[!is.na(get(eval(valuevar))),]
  }
  
  #Transform data if trans
  if (!is.null(bintrans)) {
    transvar <- paste0(binvar, '_bintrans')
    if (bintrans == 'log') {
      nneg <- in_dt[get(eval(binvar)) <= 0, .N]
      warning(paste0('There are ', nneg, ' records with', binvar, ' <= 0...',
                     'removing them for log transformation'))
      in_dt[, eval(transvar) :=
              log(get(eval(binvar)))]
      
    } else if (is.numeric(bintrans)) {
      in_dt[, eval(transvar) :=
              get(eval(binvar))^eval(bintrans)]
      
    }
    binvar = transvar
  }
  
  byd <- in_dt[, get(eval(binvar))]
  
  if (binfunc == 'manual') {
    l_freq    <- append(min(byd), binarg)
    u_freq    <- c(binarg, (max(byd, na.rm = TRUE) + 1))
    bins      <- length(binarg) + 1
  }
  
  if (binfunc == 'equal_length') {
    bins = round(binarg)
    l_freq    <- el_freq(byd, bins)
    u_freq    <- eu_freq(byd, bins)
  }
  
  if (binfunc == 'equal_freq') {
    bins = round(binarg)
    bin_prop     <- 1 / bins
    bin_length   <- in_dt[, round(.N/bins)]
    first_bins   <- (bins - 1) * bin_length
    residual     <- in_dt[, .N - first_bins]
    bin_rep      <- c(rep(seq_len((bins - 1)), each = bin_length),
                      rep(residual, residual))
    l_freq        <- c(1, (bin_length * seq_len((bins - 1)) + 1))
    u_freq       <- c(bin_length * seq_len((bins - 1)), in_dt[,.N])
    setorderv(in_dt, cols= eval(binvar))
    in_dt[, binid := .I]
    binvar = 'binid'
  }
  
  for (i in seq_len(bins)) {
    in_dt[get(eval(binvar)) >= l_freq[i] & get(eval(binvar)) < u_freq[i],
          bin := i]
    in_dt[bin == i, `:=`(bin_lmin = min(get(eval(binvar_orig)), na.rm=T),
                         bin_lmax = max(get(eval(binvar_orig)), na.rm=T))] %>%
      .[bin == i, bin_lformat := paste(round(bin_lmin, ndigits),
                                       round(bin_lmax, ndigits),
                                       sep='-')]
    
    if (i == bins) {
      in_dt[get(eval(binvar)) == u_freq[i],  bin := i]
    }
  }
  
  if (binfunc == 'equal_freq') {in_dt[, binid := NULL]}
  
  return(in_dt)
}

#------ read_GRDCgauged_paths -----------------
#' Read file paths to streamflow data from GRDC gauging stations
#'
#' Based on selection of gauges, create a list of paths to streamflow data
#' associated with gauges.
#'
#' @param inp_GRDCgaugedir path to directory containing streamflow data GRDC standard files.
#' @param in_gaugep table containing column named \code{GRDC_NO} with the
#' gauge IDs that will be used to generate file path.
#'
#' @return vector of paths to GRDC-formatted streamflow time series tables, assuming
#' that files are called "GRDC_NO.txt", GRDC_NO being replaced with a 7-digit integer.
#'
#' @export
read_GRDCgauged_paths <- function(inp_GRDCgaugedir, in_gaugep, exclude_missing) { #, gaugeid = 'GRDC_NO' down the line
  #Get data paths of daily records for gauge stations
  fileNames <- file.path(inp_GRDCgaugedir,
                         paste(
                           in_gaugep[!is.na(in_gaugep$GRDC_NO),]$GRDC_NO,
                           ".txt", sep=""))
  #Check whether any GRDC record does not exist
  existl <- do.call(rbind, lapply(fileNames, file.exists))
  print(paste(sum(!existl),
              'GRDC records do not exist...'))
  
  if (exclude_missing) {
    fileNames <- fileNames[existl]
  }
  return(fileNames)
}

#------ readformatGRDC -----------------
#' Read and pre-format GRDC data
#'
#' Reads text file of daily discharge data for a single GRDC station.
#' Creates columns for year, month, and date of last non-zero flow day +
#' computes yearly number of days of missing data
#'
#' @param path (character) path to the text file of daily discharge data in
#'   standard GRDC format.
#'
#' @return \link[data.table]{data.table} of daily discharge data with additional columns
#'
#' @export
readformatGRDC<- function(path) {
  #extract GRDC unique ID by formatting path
  gaugeno <- strsplit(basename(path), '[.]')[[1]][1]
  
  #Read GRDC text data
  gaugetab <- cbind(fread(path, header = T, skip = 40, sep=";",
                          colClasses = c('character', 'character', 'numeric',
                                         'numeric', 'integer')),
                    GRDC_NO = gaugeno)%>%
    setnames('YYYY-MM-DD', 'dates') %>%
    setorder(GRDC_NO, dates)
  
  #Format data
  gaugetab[, `:=`(year = as.numeric(substr(dates, 1, 4)), #Create year column
                  month = as.numeric(substr(dates, 6, 7)), #create month column
                  integervalue = fifelse(Original == round(Original), 1, 0) #Flag integer discharge values]
  )]
  
  #For each record, compute date of last non-zero flow day
  gaugetab[, prevflowdate := gaugetab[zero_lomf(Original),'dates', with=F]] %>% #Get previous date with non-zero flow
    .[Original != 0, prevflowdate:=NA] #If non-zero flow, set prevflowdate to NA
  
  #Compute number of missing days per year, excluding NoData values
  gaugetab[!(Original %in% c(-999, -99, -9999, 99, 999, 9999) | is.na(Original)),
           `:=`(missingdays = diny(year)-.N,
                datadays = .N),
           by= 'year']
  
  return(gaugetab)
}

#------ comp_GRDCqstats ----------
#' Compute GRDC discharge statistics.
#'
#' Compute number of years with discharge data,  qmean, mean minimum flow,
#' mean minimum 3-day average flow, q10, q90 and q99 for a GRDC gauging station
#'
#' @param path (character) full path to the formatted daily streamflow record for a GRDC gauging station.
#' @param maxgap (integer) threshold number of missing daily records to consider a calendar year unfit for analysis.
#' @param minyear (integer) start year to include in computing statistics.
#' @param maxyear (integer) last year to include in computing statistics.
#' @param verbose (boolean) whether to print the input path upon executing the function.
#' 
#' @return single-row data.table 
#' 
#' 
#' @export
comp_GRDCqstats <- function(path, maxgap,
                            minyear = 1971 , maxyear = 2000, verbose = FALSE) {
  if (verbose) {
    print(path)
  }
  
  #Read and format discharge records
  gaugeno <- strsplit(basename(path), '[.]')[[1]][1]
  gaugetab <- readformatGRDC(path)
  
  #Remove years with too many gaps, no data records, and
  gaugetabsub <- gaugetab[(missingdays <= maxgap) &
                            (year >= minyear & year <= maxyear) &
                            !(Original %in% c(-999, -99, -9999, 99, 999, 9999) |
                                is.na(Original)),]
  
  gaugestatsyr <- gaugetabsub[, list(
    qminyr = min(Original, na.rm=T),
    q3minyr = min(frollmean(Original, 3, align='center'), na.rm=T)
  ),by=year] %>%
    .[, list(qminyr = mean(qminyr, na.rm=T),
             q3minyr = mean(q3minyr, na.rm=T))]
  
  gaugestats <- gaugetabsub[, list(
    GRDC_NO = unique(GRDC_NO),
    nyears = length(unique(year)),
    qmean = mean(Original, na.rm=T),
    q10 = quantile(Original, 0.9, na.rm=T),
    q90 = quantile(Original, 0.1, na.rm=T),
    q99 = quantile(Original, 0.01, na.rm=T)
  )] %>%
    cbind(gaugestatsyr)
  
  return(gaugestats)
}


#------ getqstats ---------------------------------------------------------------
#Compute a simple set of performance statistics, including rsquare for ols without studentized outliers
getqstats <- function(dt, x, y, rstudthresh= 3, log=FALSE) {
  if (log) {
    in_form = paste0('log10(', y, '+0.1)~log10(', x, '+0.1)')
  } else {
    in_form = paste0(y,'~',x)
  }
  
  mod <- lm(as.formula(in_form), data=dt)
  
  outrows <- setDT(olsrr::ols_prep_rstudlev_data(mod)$`levrstud`)[
    abs(rstudent)>rstudthresh & color == 'outlier & leverage',]
  if (nrow(outrows) >0) {
    dtsub <- dt[-(outrows$obs),]
  } else {
    dtsub <- dt
  }
  
  mod_nooutliers <- lm(as.formula(in_form), data=dtsub)
  
  outstats <- dt[, list(pearsonr = round(cor(get(y), get(x)), 3),
                        mae = round(Metrics::mae(get(y), get(x)), 2),
                        smape = round(Metrics::smape(get(y), get(x)), 2),
                        #pbias = round(Metrics::percent_bias(get(y), get(x))),
                        rsq = round(summary(mod)$r.squared, 3),
                        rsq_nooutliers = round(summary(mod_nooutliers)$r.squared, 3),
                        n_total = .N,
                        noutliers = nrow(outrows)
  )
  ,]
  
  return(outstats)
}
#------ eval_watergap -------
#' Compute modeled discharge statistics for GRDC gauging stations
#'
#' Compute performance statistics (regression R2 and sMAPE) for WaterGAP v2.2 discharge 
#' predictions of long-term mean discharge and Q90 against observed discharge 
#' at GRDC gauging stations with ≥20 years of streamflow data.
#' 
#' @param in_qstats data.table of discharge statistics for GRDC streamflow gauging stations. 
#' Each row corresponds to a single station. Output from \link{comp_GRDCqstats}.
#' @param in_selgauges data.table or data.frame of gauging stations to analyze. 
#' @param binarg discharge bin limits to divide performance statistics asssessment by 
#' (to create size classes based on long-term mean annual flow in m3/s).
#' 
#' 
#' @details gauging stations to analyze \code{in_selgauges}  are further subsetted
#' to keep only those with at least 20 years of daily discharge data.
#' 
#' This function performs a log-log regression across all gauges and non-log regressions
#' for each size class to compute R2.   
#'  
#'  This function was used to produce Table S1 in the Supplementary Information of Messager et al. 2021
#'  
#' @return data.table of performance statistics by streamflow size class for 
#' mean annual flow and Q90.
#' 
#' @export
eval_watergap <- function(in_qstats, in_selgauges, binarg, nyears_min, 
                          maxdor=Inf, discol = 'dis_m3_pyr', binvar='qmean') {
  qsub <- in_qstats[!is.na(GRDC_NO) &
                      nyears >= nyears_min &
                      GRDC_NO %in% in_selgauges$GRDC_NO,] %>%
    .[!(GRDC_NO %in% c('5204010')),]
  #5204010 - issue with units which does not affect zero flow assessment
  #Checked 1160520 - mixed but not clear enough
  
  qsubp <- merge(qsub, in_selgauges, by='GRDC_NO', all.x=T, all.y=F) %>%
    .[dor_pc_pva < maxdor,]
  qsubp_bin <- bin_dt(in_dt = qsubp, binvar='qmean', binarg = binarg, binfunc = 'manual')
  

  qsubp_bin[qmean >= 100,
            getqstats(dt=.SD, x=discol, y='qmean',
                      rstudthresh = 3)]
  
  
  #Get stats for mean Q ~ discol (watergap mean annual)
  qmean_stats <- qsubp_bin[,
                           getqstats(dt=.SD, x=discol, y='qmean',
                                     rstudthresh = 3),
                           by=.(bin, bin_lformat)] %>%
    rbind(
      getqstats(dt=qsubp_bin, x=discol, y='qmean', log=T)[
        , `:=`(bin_lformat='all', bin=length(binarg)+1)]
    ) %>%
    .[, comp := 'qmean_dism3pyr']
  
  #Get stats for Q90 ~ dis_m3_pyr (watergap min monthly)
  # q90_stats <- qsubp_bin[,
  #                        getqstats(dt=.SD, x='dis_m3_pmn', y='q90',
  #                                  rstudthresh = 3),
  #                        by=.(bin, bin_lformat)] %>%
  #   rbind(getqstats(dt=qsubp_bin, x='dis_m3_pmn', y='q90', log=T)[
  #     , `:=`(bin_lformat='all', bin=length(binarg)+1)]) %>%
  #   .[, comp := 'q90_dism3mn']
  
  outstats <- rbind(qmean_stats) %>%
    setorder(-comp, bin)
  return(outstats)
}
