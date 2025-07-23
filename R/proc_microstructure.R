#' proc_microstructue: Process raw microstructure coding data
#'
#' This function loads the .txt raw data files from ObserverXT. Cleaning the data involves:
#' \itemize{
#'  \item{ 1) separating event log names to get visit, participant, and coder information}
#' \item{ 2) selecting relevant data columns}
#' \item{ 3) removing all events that are not needed/old/duplicate}
#' \item{ 4) re-ordering and re-name data columns}
#' \item{ 5) making wide behavioral data for summary metrics (nbites, bite rate)}
#' \item{ 6) separating event logs by coder and padding for equal entrees - need to fix mismatched number of bites/sips in a different step}
#' \item{ 7) creating variable labels, levels, and dictionaries}
#' }
#'
#'
#' @inheritParams base_wd
#' @inheritParams overwrite
#'
#'
#' @return A list containing 2 data lists:
#' 1) beh_wide contains - data: data.frame summary metrics in wide format by code, dict: data dictionary with variable descriptions
#' 2) event contains - data: event data by coder and time, dict: data dictionary with variable descriptions
#'
#' @examples
#' #if in same working directory as data:
#' ps1_microstructure <- proc_microstructue(base_wd_path)
#'
#' \dontrun{
#'
#' }
#'
#'
#' @export
#'
proc_microstructue <- function(base_wd, overwrite = FALSE) {

  #### 1. Set up/initial checks #####

  # check that base_wd exist and is a string
  data_arg <- methods::hasArg(base_wd)

  if (isTRUE(data_arg)) {
    if (!is.character(base_wd)) {
      stop('base_wd must be entered as a string')
    } else if (!file.exists(base_wd)) {
      stop('base_wd entered, but file does not exist. Check base_wd string.')
    }
  } else if (isFALSE(data_arg)) {
    stop('base_wd must be entered as a string')
  }

  # set paths for other directories
  micro_dir <- file.path(base_wd, 'derivatives', 'meal-videos', 'observerxt_exports')
  phenotype_wd <- file.path(base_wd, 'phenotype')


  # get list of available files
  micro_list <- list.files(micro_dir, pattern = '.txt')

  micro_list <- as.data.frame(micro_list)
  names(micro_list) <- 'filename'

  # need to change to sapply statement when multiple meals coded
  micro_data <- util_microstructure(base_wd, micro_list[['file_str']])

  # add if/else check for file and overwrite option

  filename_csv <- file.path(phenotype_wd, 'microsture.csv')
  write.table(micro_data$beh_wide_data$data, filename_csv, quote = FALSE, sep = ',', col.names = TRUE, row.names = FALSE, na = 'n/a')
}
