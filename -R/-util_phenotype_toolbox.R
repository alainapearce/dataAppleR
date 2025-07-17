#' util_phenotype_toolbox: Clean and organize NIH toolbox score data into phenotype/toolbox.tsv
#'
#' This function formats and adds NIH toolbox scored data from bids/sourcedata into phenotype/toolbox.tsv for a given subject
#'
#'
#' @param sub subject label used in sub-label. Leading zeros not required (integer)
#' @param ses session label used in ses-label (integer)
#' @param bids_wd string with full path to bids directory -- this is the directory that contains sourcedata/ and rawdata/
#' @param overwrite logical indicating if data should be overwritten in /rawdata. Default = FALSE
#' @param return_data logical indicating if data should be returned. Default = FALSE
#'
#' @return If return_data is set to TRUE, will return a dataframe with NIH toolbox score variables for given subject
#'
#' @examples
#'
#' \dontrun{
#' # process score data for the NIH toolbox
#' sub001_toolbox_scores <- util_phenotype_toolbox(sub = 001, ses = 1, bids_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/bids", return = TRUE)
#'
#' }
#'
#' @importFrom utils read.csv
#' @export

util_phenotype_toolbox <- function(sub, ses, bids_wd, overwrite = FALSE, return_data = TRUE) {

  # bids_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/bids"

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

  #### IO setup ####
  if (.Platform$OS.type == "unix") {
    slash <- '/'
  } else {
    slash <- "\\"
    print('util_task_foodview.R has not been thoroughly tested on Windows systems, may have data_path errors. Contact Bari at baf44@psu.edu if there are errors')
  }

  # Get subject number without leading zeros
  sub_num <- as.numeric(sub)

  # Set sub and ses strings
  sub_str <- sprintf("sub-%03d", sub_num)
  ses_str <- paste0("ses-", ses)


  # get directory paths
  source_beh_wd <- paste0(bids_wd, slash, 'sourcedata', slash, sub_str, slash, ses_str, slash, 'beh', slash)
  score_source_file <- list.files(source_beh_wd, pattern = "Assessment Scores", full.names = TRUE)
  phenotype_dir <- paste0(bids_wd, slash, 'phenotype', slash)

  #### Process Scored Data #####

  # load data, abort processing no file or >1 file matches pattern

  if (length(score_source_file) == 1) {
    scores_dat <- read.csv(score_source_file, header = TRUE)
  } else if ( length(score_source_file) == 0) {
    print(paste(sub_str, ses_str, "has no NIH toolbox scores data. Aborting task processing for this sub."))
    return()
  } else if (length(score_source_file) > 1) {
    print(paste(sub_str, ses_str, "has more than 1 NIH toolbox scores data. Should only have 1. Aborting task processing for this sub."))
    return()
  }

  # add subject column
  scores_dat$participant_id <- sub_str
  scores_dat <- scores_dat %>% dplyr::relocate("participant_id") # move to first column

  # Separate the 'Inst' column into 'Test' and 'Ages' columns
  scores_dat <- tidyr::separate(scores_dat, Inst, into = c("Test", "Test_Ages"), sep = "Test", remove = FALSE)

  # Replace values in the 'Test' column
  scores_dat <- scores_dat %>%
    dplyr::mutate(Test = dplyr::case_when(
      stringr::str_detect(Test, "Flanker Inhibitory Control") ~ "FLANKER",
      stringr::str_detect(Test, "Dimensional Change Card Sort") ~ "CARDSORT",
      stringr::str_detect(Test, "List Sorting Working Memory") ~ "LISTSORT",
      TRUE ~ "other"  # Default case
    ))

  # remove columns where Test = other
  scores_dat <- scores_dat[!(scores_dat$Test %in% "other"),]

  #make data wide
  wide_cols <- c("Test_Ages", "RawScore", "Theta", "TScore", "SE", "ItmCnt", "DateFinished", "Computed.Score", "Uncorrected.Standard.Score", "Age.Corrected.Standard.Score", "National.Percentile..age.adjusted.", "Fully.Corrected.T.score")

  # check for multiple rows per task -- dataset should only have 1 row per task
  if (sum(scores_dat$Test == "FLANKER") > 1 | sum(scores_dat$Test == "CARDSORT") > 1 | sum(scores_dat$Test == "LISTSORT") > 1) {
    return(print(paste(sub_str, ses_str, "NIH toolbox score data contains >1 row per task. Aborting processing. Data should only have 1 row per task. Check sourcedata")))
  } else {
    scores_dat_wide <-
      tidyr::pivot_wider(
        scores_dat,
        id_cols = participant_id,
        names_from = Test,
        values_from = wide_cols,
        names_sep = "_" ,
        names_glue = "{Test}_{.value}"
      )
  }

  # add session column
  scores_dat_wide$session_id <- ses_str
  scores_dat_wide <- scores_dat_wide %>% dplyr::relocate("session_id", .after = 1) # move after 1st column

  #### Write to phenotype/toolbox.tsv #####

  toolbox_phenotype_file <- paste0(phenotype_dir, 'toolbox_scores.tsv')

  # if phenotype/toolbox.tsv exists

  if (file.exists(toolbox_phenotype_file)) {

    # load data
    phenotype_dat <- read.delim(toolbox_phenotype_file, na.strings = "n/a")

    # if there is a row with sub and ses in phenotype_dat
    if (any(phenotype_dat$participant_id == sub_str & phenotype_dat$session_id == ses_str)) {

      # if overwrite is TRUE
      if (isTRUE(overwrite)) {

        # remove previous row for subject and session
        phenotype_dat <- subset(phenotype_dat, !(participant_id == sub_str & session_id == ses_str))

        # add new row for subject and session
        phenotype_dat <- dplyr::bind_rows(phenotype_dat, scores_dat_wide)

        # export file
        utils::write.table(
          phenotype_dat,
          toolbox_phenotype_file,
          sep = '\t',
          quote = FALSE,
          row.names = FALSE,
          na = "n/a" # use 'n/a' for missing values for BIDS compliance
        )

      }

      # if sub and session not already in phenotype data
    } else {

      # add row for subject
      phenotype_dat <- dplyr::bind_rows(phenotype_dat, scores_dat_wide)

      # export file
      utils::write.table(
        phenotype_dat,
        toolbox_phenotype_file,
        sep = '\t',
        quote = FALSE,
        row.names = FALSE,
        na = "n/a" # use 'n/a' for missing values for BIDS compliance
      )

    }

    # if phenotype/toolbox.tsv does not exist
  } else {

    # create phenotype_dat from scores_dat_wide
    phenotype_dat <- scores_dat_wide

    # create bids/phenotype directory if it doesn't exist
    phenotype_dir <- paste0(bids_wd, slash, 'phenotype', slash)
    if (!dir.exists(phenotype_dir)) {
      dir.create(phenotype_dir, recursive = TRUE)
    }

    # export file
    utils::write.table(
      phenotype_dat,
      toolbox_phenotype_file,
      sep = '\t',
      quote = FALSE,
      row.names = FALSE,
      na = "n/a" # use 'n/a' for missing values for BIDS compliance
    )

  }


  #### Return data #####
  if (isTRUE(return_data)){
    return(scores_data = scores_dat_wide
    )
  }
}

