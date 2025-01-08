#' Input DOC data into metadata
#'
#' Takes data from a separate table of DOC results and puts
#' it in the metadata table under the correct sample. The code assumes
#' DOC is reported in mg/L. See 'sample_metadata' in 'fewsdom' package
#' for formatting.
#'
#' The table with DOC data can be either a .csv or excel file.
#'
#' @importFrom stringr str_detect str_split
#' @importFrom openxlsx write.xlsx readWorkbook
#' @importFrom utils write.csv
#'
#' @param doc_file a string of the file path of the DOC file, can be .xlsx or .csv
#' @param doc_sheet a string of the sheet name of the DOC results, only required if the DOC file is an .xlsx file
#' @param doc_column a numeric or string indicating which column the DOC results are stored in in the DOC file. Defaults to "Result.NPOC." since this is what our Shimadzu exports call it
#' @param id_column a numeric indicating which column the unique sample identifiers are stored in the DOC file. Defaults to "Sample.ID" since this is what our Shimadzu exports call it
#' @param nskip a numeric indicating a number of lines to skip when reading in the DOC file, optional
#'
#' @param meta_file a string indicating the file path of the metadata, can be .xlsx or .csv
#' @param meta_sheet a string of the metadata sheet name, only required if the metadata file is an .xlsx file
#' @param site_loc a vector indicating the start and end of the site name in the metadata data identifier
#' @param rewrite a logical, if TRUE original metadata will be saved over with metadata with DOC results, if FALSE it will add "_doc_added" to the end of the table
#' @param ... arguments to pass down to functions with 'get_doc'
#' @export

get_doc <- function(doc_file,
                    doc_sheet=NULL,
                    doc_column="Result.NPOC.",
                    id_column="Sample.ID",
                    nskip = 0,
                    meta_file,
                    meta_sheet=NULL,
                    rewrite=T,
                    ...){
  stopifnot(is.character(c(doc_file, meta_file)) |
              file.exists(doc_file)|
              file.exists(meta_file))

  #read in doc data
    if(stringr::str_detect(doc_file, ".xlsx")){
      doc_result <- tryCatch(
        {openxlsx::readWorkbook(doc_file, sheet=doc_sheet,
                                           startRow = nskip+1, detectDates = T)},
        error = function(e){
          stop(paste("ERROR: Could not open XLSX DOC file",
                     "\n",
                     e))
      })
    } else{
      doc_result <- tryCatch(
        {read.csv(doc_file, skip = nskip)},
          error = function(e){
            stop(paste("ERROR: Could not open CSV DOC file",
                       "\n",
                       e))
          })
    }

  #condense to just DOC data
    doc_result <- doc_result[,c(id_column, doc_column)]
    colnames(doc_result) <- c("sample_id", "DOC")

  #clean site names, we submit samples to the lab with date and time stamps, but we match just site name (for now)
  # doc_result$Site <- sapply(stringr::str_split(doc_result$Site, doc_delim),"[[",1)

  # Match data_identifier (Aqualog default) to Sample.ID (shimadzu default)

  #load metadata
    if(stringr::str_detect(meta_file, ".xlsx")){
      meta <- openxlsx::readWorkbook(meta_file, sheet=meta_sheet, detectDates = T)
    } else{
      meta <- read.csv(doc_file)
    }

  #clean meta names - Commenting this our as its unnecessary and probably makes bugs
  # meta$Site <- substr(meta$data_identifier, site_loc[1], site_loc[2])

  #Put DOC in metadata
    meta$DOC_mg_L <- as.numeric(doc_result$DOC[match(meta$data_identifier, doc_result$sample_id)])

  #remove site column
  # meta <- meta[,-which(colnames(meta) == "Site")]

    #check it "worked"
    if(sum(is.na(meta$DOC_mg_L)) == nrow(meta)){
      warning("No matching DOC results were found, please check your file names and delimeters")
    }

  #write metafile
    if(rewrite==T){
      if(stringr::str_detect(meta_file, ".xlsx")){
        openxlsx::write.xlsx(meta, meta_file, sheetName = meta_sheet,
                   colNames = TRUE, rowNames = F, append = F)} else{
        write.csv(meta, meta_file, col.names = T, row.names = F, quote=F)}
    }else{
      if(stringr::str_detect(meta_file, ".xlsx")){
        new_meta_file <- paste(unlist(str_split(meta_file, ".xlsx"))[1], "_doc_added.xlsx", sep="")
        openxlsx::write.xlsx(meta, new_meta_file, sheetName = meta_sheet,
                   colNames = TRUE, rowNames = F, append = F)} else{
      new_meta_file <- paste(unlist(str_split(meta_file, ".csv"))[1], "_doc_added.csv", sep="")
       write.csv(meta, new_meta_file, col.names = T, row.names = F, quote=F)}
    }
    cat(paste("Linked ", sum(!is.na(meta$DOC_mg_L)), "/", nrow(meta), " samples with DOC data", sep=""))
    cat("\n")

    return(meta)
}
