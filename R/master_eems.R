##
#' Takes raw EEMs and absorbance data from Aqualog, returns cleaned, processed data
#' Combines all the 'fewsdom' functions to make one function to process samples.
#'
#' @importFrom stringr str_replace_all
#' @param prjpath path to folder with raw samples
#' @param meta_name the file name of the metadata with the extension
#' @param get_doc logical, if TRUE will use 'get_doc' function to match DOC data to metadata samples, only required if get_doc is TRUE
#' @param doc_file a string of the file path of the DOC file, can be .xlsx or .csv, only required if get_doc is TRUE
#' @param doc_sheet a string of the sheet name of the DOC results, only required if the DOC file is an .xlsx file, only required if get_doc is TRUE
#' @param doc_column a numeric indicating which column the DOC results are stored in in the DOC file, only required if get_doc is TRUE
#' @param name_column a numeric indicating which column the sample/site names are stored in the DOC file, only required if get_doc is TRUE
#' @param nskip a numeric indicating a number of lines to skip when reading in the DOC file, optional, only required if get_doc is TRUE
#' @param doc_delim a string indicating the separation between site name and other identifiers in the DOC data, it will only keep the first piece, only required if get_doc is TRUE
#' @param meta_sheet a string of the metadata sheet name, only required if the metadata file is an .xlsx file
#' @param site_loc a vector indicating the start and end of the site name in the metadata data identifier
#' @param process_file logical, if TRUE it will put a text file in the processed data folder named 'processing_tracking'
#' @param ... additional arguments passed to the 'get_doc', 'clean_files', 'abs_preprocess', 'load_eems', 'eem_process', 'plot_eems', 'get_indices', 'save_eems' functions
#'
#' @return saves processed EEMs as .csv and .rds files, absorbance as .csv and .rds, and metadata as .csv
#' creates plots for each sample and calculates indices.
#'
#' @details The following steps are chosen by default:
#'  -DOC is added to the metadata and the file is overwritten
#'
#'  -The file directory is created, raw files are zipped
#'
#'  -A process file is created, The instrument blank is subtracted from the
#'  samples, raman scattering is removed and interpolated, rayleigh scattering
#'  is removed but not interpolated, widths are determined automatically,
#'  samples are corrected for inner filter effects, raman normalized, and
#'  normalized for DOC, excitation is clipped from 247 to 450 nm, emission
#'  is clipped from 247 to 550 nm
#'
#'  -Sample are plotted individually and a summary plot using the parula
#'  color palette with descriptions for the plot titles, the peaks are not
#'  annotated on the plot. Plots are created for DOC normalized data (_DOC),
#'  and for non normalized data.
#'
#'  -Fluorescence indices are reported for data that has and hasn't been DOC
#'  normalized, sample are reported in rows.
#'
#'  If you want to change these defaults add the appropriate arguments into the
#'  function to change the defaults for the other functions.
#'
#' @export
#'
run_eems <- function(prjpath,
                     meta_name,
                     get_doc=F,
                     doc_file,
                     doc_sheet,
                     doc_column=7,
                     name_column=4,
                     nskip=3,
                     doc_delim="-",
                     meta_sheet="log",
                     site_loc=c(1,7),
                     process_file=T,
                     ...){

  # Get the meta file path
  meta_file <- file.path(prjpath, meta_name)


  if(get_doc == T){
    # Add code to append prjpath to DOC file (in case it's a relative path)
    doc_file <- file.path(prjpath,
                          doc_file)
    meta <- get_doc(doc_file=doc_file,
                    doc_sheet=doc_sheet,
                    doc_column=doc_column,
                    name_column=name_column,
                    nskip=nskip,
                    doc_delim=doc_delim,
                    meta_file=meta_file,
                    meta_sheet=meta_sheet,
                    site_loc=site_loc,
                    ...)
  }

  # create the tracking file path in environment - can I find this one?
  set_tracking_file(file.path(prjpath,
                             "5_Processed",
                             "processing_tracking.txt"))

  #rename files and create and put in folders
  cat("Renaming files and putting in files \n")
  meta <- clean_files(prjpath,
                      meta_file,
                      meta_sheet,
                      ...)

  #convert absorbance files from .dat to .csv
  abs_preprocess(prjpath,
                 "mixed",
                 meta)

  #Load Data in R
  cat("Loading data in R \n")
  data<- load_eems(prjpath)
  X <- data[[1]]
  X_blk <- data[[2]]
  Sabs <- data[[3]]

  #Check data with metadata, remove samples that don't have data
  test <- check_samps(meta, X, X_blk, Sabs)
  meta <- test[[1]]
  X <- test[[2]]
  X_blk <- test[[3]]
  Sabs <- test[[4]]

  ## Check that the instrument blank is ok before continuing with processing steps
  Iblank <- X_blk[[1]]
  Sblank <- X[sapply(X,
                   \(x) grepl("^blk",
                              x$sample,
                              ignore.case = TRUE))]
  class(Sblank) <- "eemlist"
  blanks_eemlist <- eemR::eem_bind(Iblank, Sblank)
  blank_validation <- validate_instrument_blank(blanks_eemlist,
                                                ...)

  ## Process the EEM's
  cat("Processing EEMs and absorbance data \n")
  data_process <- eem_process(prjpath = prjpath,
                               eemlist = X,
                               blanklist = X_blk,
                               abs = Sabs,
                               process_file = process_file,
                               meta = meta,
                               replace_blank = blank_validation,
                               ...)
  X_clean <- data_process[[1]]  #returns non doc normalized data
  abs_clean <- data_process[[2]]

  ## Validation checks on the EEMs and Absorbance data
  # These will raise warnings if some of the absorbances are weird or if the
  cat("Validating check standards \n")
  tea_abs_check <- validate_tea_absorbance(abs_clean,
                                           ...)

  ## Report the Data
  cat("Reporting EEMs and absorbance data \n")

  #create plots
  plot_eems(prjpath = prjpath,
            meta = meta,
            eem=X_clean,
            doc_norm = FALSE,
            ...)

  if(sum(is.na(meta$DOC_mg_L)) < nrow(meta)){
    plot_eems(prjpath = prjpath,
              meta=meta,
              eem=X_clean,
              doc_norm = TRUE,
              save_names="_DOC",
              ...)
  }


  #save indices
  get_indices(X_clean,
              abs_clean,
              meta,
              prjpath = prjpath,
              doc_norm = "both",
              sampsascol = FALSE,
              waves=NULL)

  #save raw files (eems, abs)
  save_eems(X_clean,
            abs_clean,
            meta,
            prjpath = prjpath)

  cat(" \n EEM's have been successfully processed. Look in the 5_Processed folder for plots and indices \n")

}
