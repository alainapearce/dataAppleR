#' util_task_nihtoolbox: Clean and organize NIH toolbox assessment data into rawdata
#'
#' This function formats and organizes NIH toolbox assessment data from bids/sourcedata into rawdata for a given subject. Assessment data includes responses to each trial in the NIH toolbox.
#'
#'
#' @inheritParams util_copy_to_source
#' @inheritParams util_copy_to_source
#' @inheritParams util_task_foodview
#' @inheritParams util_task_foodview
#'
#' @return If return_data is set to TRUE, will return a list with 1 cleaned dataframe per run
#'
#' @examples
#'
#' \dontrun{
#' # process assessment (response) data for the NIH toolbox
#' util_task_nihtoolbox(sub_str = 'sub-001', ses_str = 'ses-1', bids_wd = bids_wd)
#'
#' }
#'
#' @importFrom utils read.csv
#' @export

util_task_nihtoolbox <- function(sub, ses, bids_wd, overwrite = FALSE, return_data = TRUE) {

  print(sub_str)

  #### Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(bids_wd)

  if (isTRUE(data_arg)) {
    if (!is.character(bids_wd)) {
      stop("bids_wd must be entered as a string")
    } else if (!file.exists(bids_wd)) {
      stop("bids_wd entered, but file does not exist. Check bids_wd string.")
    }
  } else if (isFALSE(data_arg)) {
    stop("bids_wd must be entered as a string")
  }

  # get directory paths
  source_beh_wd <- file.path(bids_wd, 'sourcedata', sub_str, ses_str, 'beh')
  raw_beh_wd <- file.path(bids_wd, 'rawdata', sub_str, ses_str, 'beh')

  if (sum(grepl('events', list.files(bids_wd, 'sourcedata', sub_str, ses_str, 'beh'))) > 0){

  } else {
    data_source_file <- file.path(source_beh_wd, paste0(sub_str, '_', ses_str, '_task-nih_toolbox_data.tsv'))
    score_source_file <- file.path(source_beh_wd, paste0(sub_str, '_', ses_str, '_task-nih_toolbox_scores.tsv'))
    registration_source_file <- file.path(source_beh_wd, paste0(sub_str, '_', ses_str, '_task-nih_toolbox_registration_data.tsv'))

    if (file.exists(data_source_file)) {
      data <- read.table(data_source_file, header = TRUE)
    } else {
      print(paste(sub_str, "has no sst assessment data file. Aborting task processing for this sub."))
      return()
    }

    if (file.exists(score_source_file)) {
      scores <- read.table(score_source_file, header = TRUE)
    } else {
      print(paste(sub_str, "has no sst assessment scorese file. Aborting task processing for this sub."))
      return()
    }

    if (file.exists(registration_source_file)) {
      reg_data <- read.table(registration_source_file, header = TRUE)
    } else {
      print(paste(sub_str, "has no sst assessment scorese file. Aborting task processing for this sub."))
      return()
    }
  }




  # Add registration data to assessment data
  data$registration_age <- reg_data$Age
  data$registration_education <- reg_data$Education
  data$registration_mothers_education <- reg_data$MothersEducation
  data$registration_gender <- reg_data$Gender
  data$registration_handedness <- reg_data$Handedness
  data$registration_race <- reg_data$Race
  data$registration_ethnicity <- reg_data$Ethnicity

  # make separate columns for task (e.g., "Flanker Inhibitory Control") and test ages (e.g., "Ages 8-11 v2.1") from Inst (e.g., "NIH Toolbox Flanker Inhibitory Control and Attention Test Ages 8-11 v2.1") ??
  # Separate the 'Inst' column into 'Test' and 'Ages' columns
  data <- tidyr::separate(data, Inst, into = c("Test", "Test_Ages"), sep = "Test", remove = FALSE)

  # Replace values in the 'Test' column
  data <- data %>%
    dplyr::mutate(Test = dplyr::case_when(
      stringr::str_detect(Test, "Flanker Inhibitory Control") ~ "FLANKER",
      stringr::str_detect(Test, "Dimensional Change Card Sort") ~ "CARDSORT",
      stringr::str_detect(Test, "List Sorting Working Memory") ~ "LISTSORT",
      TRUE ~ "other"  # Default case
    ))

  # remove columns where Test = other
  data <- data[!(data$Test %in% "other"),]

  # add subject column
  data$participant_id <- sub_str
  data <- data %>% dplyr::relocate("participant_id") # move sub to first column

  # add session column
  data$session_id <- ses_str
  data <- data %>% dplyr::relocate("session_id", .after = 1) # after col 1

  #### Save in rawdata #####

  # create bids/rawdata directory if it doesn't exist
  if (!dir.exists(raw_beh_wd)) {
    dir.create(raw_beh_wd, recursive = TRUE)
  }

  # define output file with path
  outfile <- file.path(raw_beh_wd, paste0(sub_str, '_ses-', ses, '_task-toolbox_beh.tsv'))

  # export file if doesn't exist or overwrite = TRUE
  if (!file.exists(outfile) | isTRUE(overwrite)) {
    utils::write.table(
      data,
      outfile,
      sep = '\t',
      quote = FALSE,
      row.names = FALSE,
      na = "n/a" # use 'n/a' for missing values for BIDS compliance
    )
  }


  #### Return data #####
  if (isTRUE(return_data)){
    return(dataa = data
    )
  }
}

