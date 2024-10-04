utils::globalVariables(c("%dopar%","i", ".",
                         "...","wavelength", "em", "z", "ex",
                         "x","y", "data_identifier", "replicate_no", "meta","data_process",
                         "meta_clean","raw_eem"))


#' Create file structure
#'
#' Takes a file with EEMs and absorbance data and creates subfolders to store
#' raw and processed data, plots, and output tables
#'
#' @param prjpath a string indicating the project file with the data
#' @export
#'

create_files <- function(prjpath){
  stopifnot(is.character(prjpath)|file.exists(prjpath))
  #put in files
  dir.create(paste(prjpath, "/1_Absorbance", sep=""), showWarnings = F)
  dir.create(paste(prjpath, "/2_Blanks", sep=""), showWarnings = F)
  dir.create(paste(prjpath, "/3_Samples", sep=""), showWarnings = F)
  dir.create(paste(prjpath, "/4_Clean_Absorbance", sep=""), showWarnings = F) #create file for clean files if it doesn't exist yet
  dir.create(paste(prjpath, "/5_Processed", sep=""), showWarnings = F)
  dir.create(paste(prjpath,  "/5_Processed/Figures", sep=""), showWarnings = F)
}

#' Checks if object is an eemlist
#'
#' @param eem an object
#' @noRd
.is_eemlist <- function(eem) {
  ifelse(class(eem) == "eemlist", TRUE, FALSE)
}

#' Checks if object is an eem
#'
#' @param eem an object
#' @noRd
.is_eem <- function(eem) {
  ifelse(class(eem) == "eem", TRUE, FALSE)
}

#' DOC Normalizes eems data
#'
#' @param eem object of class eem or eemlist
#' @param meta dataframe with metadata, needs to have doc data
#' @noRd
#'
.eem_doc_norm <- function(eem, meta){
  #remove EEMs with no DOC data (or value of 0)
  EEM_rm <- meta$unique_ID[meta$DOC_mg_L == 0 | is.na(meta$DOC_mg_L) == T]
  eem <- eem_exclude(eem,
                   exclude=list("ex"=c(), "em"=c(),"sample"= EEM_rm ))

  #check if they've been normalized
  doc_done <- sapply(eem, function(x) attr(x, "is_doc_normalized"))
  to_norm <- which(doc_done == F)

  #normalize those that haven't
  for(x in to_norm){
    eem_name <- eem[[x]]$sample
    eem_index <- which(eem_name == meta$unique_ID)
    eem[[x]]$x <- eem[[x]]$x / as.numeric(meta$DOC_mg_L[eem_index])
    attr(eem[[x]], "is_doc_normalized") <- TRUE
  }

  #double check all are normalized
  doc_done <- sapply(eem, function(x) attr(x, "is_doc_normalized"))
  stopifnot(sum(doc_done==F)==0)
  return(eem)
}

#' Removes DOC normalization on eems data
#'
#' Removes DOC normalization if sample are DOC normalized
#' @param eem object of class eem or eemlist
#' @param meta dataframe with metadata, needs to have doc data
#' @noRd
.eem_doc_rm <- function(eem, meta){
  #check if they've been normalized
  doc_done <- sapply(eem, function(x) attr(x, "is_doc_normalized"))
  to_unnorm <- which(doc_done == T)

  #unnormalize those that haven't
  for(x in to_unnorm){
    eem_name <- eem[[x]]$sample
    eem_index <- which(eem_name == meta$unique_ID)
    eem[[x]]$x <- eem[[x]]$x  * as.numeric(meta$DOC_mg_L[eem_index])
    attr(eem[[x]], "is_doc_normalized") <- FALSE
  }

  #double check all are normalized
  doc_done <- sapply(eem, function(x) attr(x, "is_doc_normalized"))
  stopifnot(sum(doc_done==T)==0)
  return(eem)
}

#' Save files to excel
#'
#' Saves files to excel based on named sheet name
#'
#' @param file name of excel file
#' @param sheet_name sheet name of excel file
#' @param df dataframe to save to excel file
#' @param sampsascol a logical indicating how results should be oriented, TRUE puts samples as columns, FALSE puts samples as rows
#' @noRd
#'
.OSU_excel <- function(file, sheet_name, df, sampsascol){
  wb <- openxlsx::loadWorkbook(file)
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb,sheet_name, df, rowNames = sampsascol)
  openxlsx::saveWorkbook(wb,file,overwrite = TRUE)
}


#' Find difference between EEMs samples
#'
#' @param eemlist1 object of class eemlist
#' @param eemlist2 object of class eemlist
#' @param sampnum numeric, the number of the sample in the eemlist you want to compare.
#'
#' @return a dataframe with the intensity at 10 different points for both EEMs
#' @noRd
samp_dif <- function(eemlist1, eemlist2, sampnum){
  X1 <- eemlist1[[sampnum]]
  X2 <- eemlist2[[sampnum]]

  n_dif <- sum(!(X1$x == X2$x), na.rm=T)
  cat("there are", n_dif, "samples that are not the same \n")

  set.seed(9)
  ex_samp <- sample(1:length(X1$ex), 10)
  em_samp <- sample(1:length(X1$em), 10)
  ints1 <- sapply(1:10, function(x){
    ex <- ex_samp[x]
    em <- em_samp[x]
    val <- X1$x[em, ex]
    val
  })
  ints2 <- sapply(1:10, function(x){
    ex <- ex_samp[x]
    em <- em_samp[x]
    val <- X2$x[em, ex]
    val
  })

  rand_samp <- data.frame(ex = X1$ex[ex_samp], em=X1$em[em_samp],
                          int1 = ints1, int2 = ints2)

  return(rand_samp)
}


#' Check for EEMs that are empty
#'
#' @param eem object of class eem or eemlist
#' @param verbose if you want to print out which samples aren't empty
#' @returns a vector of EEMs with empty EEMs tables (eem$x)
#' @export
#'
empty_eems <- function(eem, verbose=T){
  stopifnot(.is_eem(eem) | .is_eemlist(eem))

  if (.is_eemlist(eem)) {
    res <- sapply(eem, empty_eems, verbose=verbose)
    res <- unlist(res)
    return(res)
  }
  #check if EEM has rows and columns and check's they're not all NA
  blank_check <- F
  if(is.null(dim(eem$x)) == T){blank_check <- T}
  if(nrow(eem$x) == 0 | ncol(eem$x) == 0){blank_check <- T}
  if(sum(is.na(eem$x))== (dim(eem$x)[1]*dim(eem$x)[2])){blank_check <- T}


  if(blank_check == T){
    if(verbose==T){
      cat("Sample", eem$sample, "has no EEMs data \n")
    }
    outcome <- eem$sample
  }else{
    if(verbose == T){
      cat("There were no empty EEMs samples \n")
    }
    outcome <- c()
  }
  return(outcome)
}

#' Returns fewsdom package version loaded
#'
#' @return text string with fewsdom package version
.fewsdom_ver <- function() {
  paste0("fewsdom_", packageVersion("fewsdom"))
  }

#' Generate spectral index documenation for
#'
#' @return data.frame with information documenting column lables of spectral indices spreadsheet
#'
.document_indices <- function() {

  fewsdom_version <- .fewsdom_ver()

  documentation <- data.frame(c(paste0("Peaks extracted using ", fewsdom_version, " package in R."),
                                "For peak definitions see 'eem_coble_peaks2' and 'abs_parm' functions in the fewsdom package.",
                                "The package can be downloaded from https://github.com/FEWSLab/fewsdom",
                                "",
                                "Sheet fluor_indices_DOC contains fluorsecence indices normalized by DOC concentration",
                                "Sheet fluor_indices contains raw fluorsecence indices",
                                "sheet abs_indices contains absorbance indices",
                                "",
                                "Fluorescence Indices",
                                "Coble peaks are based on Coble et al. 2014 and are defined as follows:",
                                "Peak B (pB): ex = 270:280 nm, em = 300:320 nm, Tyrosine-like",
                                "Peak T (pT): ex = 270:280 nm, em = 320:350 nm, Tryptophan-like",
                                "Peak A (pA): ex = 250:260 nm, em = 380:480 nm, Humic-like.",
                                "Peak M (pM): ex = 310:320 nm, em = 380:420 nm, Marine humic-like",
                                "Peak C (pC): ex = 330:350 nm, em = 420:480 nm, Humic-like",
                                "Peak D (pD): ex = 390 nm, em = 509 nm, Soil fulvic acid",
                                "Peak E (pE): ex = 455 nm, em = 521 nm, Soil fulvic acid",
                                "Peak N (pN): ex = 280 nm, em = 370 nm, Plankton derived",
                                "Given that peaks A, B, C, M, and T are not defined at fixed excitation and emission wavelength, the maximum fluorescence value in the region is extracted.",
                                "",
                                "Additional fluorescence indices are based on Hansen et al. 2016.",
                                "Measurements are defined as follows:",
                                "rAT: The ratio of peak A to peak T, indication of the amount of humic like (recalcitrant) to fresh (liable) DOM.",
                                "rCA: The ratio of peak C to peak A, indication of the amount of humic like to fumic like DOM.",
                                "rCM: The ratio of peak C to peak M, indication of the amount of diagenetically altered (blueshifted) DOM.",
                                "rCT: The ratio of peak C to peak T, indication of the amount of humic like (recalcitrant) to fresh (liable) DOM.",
                                "Fluorescence Index (FI): Ratio of fluorescence at ex = 370 nm, em = 470 nm to em = 520 nm.",
                                "Identifies the relative contributions of terrestrial to microbial DOM sources.",
                                "",
                                "Humification Index (HIX): ex = 254 nm, em =sum(435:480 divided by em =sum(435:480, sum(300:345.",
                                "An indication of humic substances or extent of humification. Higher values indicate an higher degree of humification.",
                                "",
                                "Humification Index (HIX_ohno): ex = 254 nm, em =sum(435:480 divided by em =sum(435:480, sum(300:345. HIX proposed by Ohno (2002), both versions of HIX are used throughout the literature. Ohno is better when samples have higher absorbance because it accounts for inner filter effects better.",
                                "Freshness Index (beta/alpha fresh): ex = 310 nm, ratio of em = 380 nm to max in em = 420:435 nm.",
                                "An indication of recently produced DOM, higher values indicate more recently produced DOM.",
                                "",
                                "Relative Fluorescence Efficiency (RFE): Ratio of fluorescence at ex = 370 nm, em = 460 nm to",
                                "absorbance at 370 nm. An indicator of the relative amount of algal to non-algal DOM.",
                                "",
                                "Biological Index (BIX): ex = 310 nm, ratio of em = 380 nm to em = 430 nm.",
                                "An indicator of autotrophic productivity, values above 1 indicate recently produced",
                                "autochthonous DOM.",
                                "",
                                "Absorbance indices",
                                "Absorbance indices based on Hansen et al. 2016. Measurements are defined as follows:",
                                "",
                                "SUVA254, SUVA280, SUVA350, SUVA370: SUVA at 254, 280, 350, and 370 nm.",
                                "Units of L mgC^-1 m^-1.",
                                "Typically higher values are associated with greater aromatic content.",
                                "",
                                "SVA412, SVA440, SVA480, SVA510,",
                                "SVA532, SVA555: SVA at 412, 440, 480, 510, 532, 555 nm.",
                                "Units of L mgC^-1 m^-1.",
                                "Typically higher values are associated with greater aromatic content.",
                                "",
                                "S275_295: Spectral slope between 275 to 295 nm.",
                                "",
                                "S350_400: Spectral slope between 350 to 400 nm.",
                                "",
                                "Spectral slopes are found with a nonlinear fit of an exponential function to",
                                "the absorption spectrum, typically higher  values are associated with",
                                "lower molecular weight materials and/or lower aromaticity.",
                                "",
                                "SR: Spectral slope S275_295 divided by spectral slope S350_400, negatively correlated to DOM molecular weight",
                                "and generally increases on irradiation."))
  return(documentation)
}

.write_processing_tracking <- function(text,
                                       tracking_filename = "processing_tracking.txt",
                                       overwrite = FALSE
                                       ) {

  # Check that we want to do this otherwise exit the function
  stopifnot(is.character(text))

  #create text file to track processing changes
  tracking_path <- file.path(prjpath,
                             "5_Processed",
                             tracking_filename)
  if (overwrite) {
    # If the file should be overwritten (new processing run), create a new file
    file.create(tracking_path,
                overwrite = TRUE)
    write(paste0("PROCESSING STEPS ON ", round(Sys.time(), "secs"), " using ", .fewsdom_ver()),
          file = tracking_path)
  } else {
    # This case we just append a new line to the end of the file with the text
    line_write <- paste(round(Sys.time(),
                              "secs"),
                        text,
                        sep = " - ")
    write(line_write,
          file = tracking_path,
          append = TRUE)
  }

}
