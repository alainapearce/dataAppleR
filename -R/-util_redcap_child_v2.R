#' util_redcap_child_v2: Organize child visit 2 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_2_arm_1
#'
#' @param data data from REDCap event child_visit_2_arm_1
#'
#' @return Will return a list including:
#' \itemize{
#'  \item{clean raw child visit 2 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{visit_data_child}
#'    \item{mri_info}
#'    \item{mri_assessment_data}
#'  }
#' @examples
#'
#' # process REDCap data
#' child_visit2_list <- util_redcap_child_v2(data)
#'
#' \dontrun{
#' }
#'
#' @seealso [proc_redcap()]
#'
#' @export


util_redcap_child_v2 <- function(data) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    }
  } else if (isFALSE(data_arg)) {
    stop("child data for REDCap event child_visit_2_arm_1 must be entered as a data.frame")
  }

  # update name of participant ID column
  names(data)[names(data) == "record_id"] <- "participant_id"

  # update date
  names(data)[names(data) == "v2_date"] <- "visit_date"

  # add session column
  data['session_id'] <- 'ses-1'

  # add visit
  data['visit_protocol'] <- 2

  # update date
  names(data)[names(data) == 'v2_date'] <- 'visit_date'
  data['visit_date'] <- lubridate::as_date(data[['visit_date']])

  #reduce columns and update names

  ## visit data ####
  visit_data_child <- data[grepl('_id|notes|^visit', names(data))]

  # remove extra columns, add columns, and re-order
  visit_data_child <- visit_data_child[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(visit_data_child)[grepl('notes', names(visit_data_child))])]

  names(visit_data_child)[names(visit_data_child) == 'v2_post_check_notes'] <- 'v2_notes'
  names(visit_data_child) <- gsub('viewing', 'view', names(visit_data_child))

  names(visit_data_child)[!grepl('_id|v2|^visit', names(visit_data_child))] <- paste0('v2_',  names(visit_data_child)[!grepl('_id|v2|^visit', names(visit_data_child))])

  ## MRI notes ####
  mri_info <- data[grepl('_id|mri|cams|freddy|visit_date', names(data))]

  # remove extra columns, add columns, and re-order
  mri_info <- mri_info[!grepl('resting|freddy_visit_number', names(mri_info))]

  mri_info <- mri_info[c('participant_id', 'session_id', 'visit_date', names(mri_info)[grepl('mri|cams|freddy', names(mri_info))])]

  # fix names
  names(mri_info) <- gsub('run_', 'run', names(mri_info))
  names(mri_info) <- gsub('snack_1', 'snack', names(mri_info))
  names(mri_info) <- gsub('snack_2', 'snack2', names(mri_info))
  names(mri_info) <- gsub('_check', '', names(mri_info))

  names(mri_info)[names(mri_info) == 'post_snack_1_freddy_check'] <- 'post_snack_freddy_check'

  # Replace 'freddy' with 'fullness'
  names(mri_info) <- gsub('freddy', 'fullness', names(mri_info))

  mri_info_json <- json_mri_v2()

  ## MRI behavioral assessment ####
  mri_assessment_data <- data[grepl('_id|_familiarity|_recall|_liking|visit_date', names(data))]

  # remove extra columns, add columns, and re-order
  mri_assessment_data <- mri_assessment_data[c('participant_id', 'session_id', 'visit_date', names(mri_assessment_data)[grepl('_familiarity|_recall|_liking', names(mri_assessment_data))])]

  # score this data?

  mri_assessmnet_json <- json_mri_assessment()

  ## return data ####
  return(list(visit_data_child = visit_data_child,
              mri_info = list(data = mri_info, meta = mri_info_json),
              mri_assessment_data = list(data = mri_assessment_data, meta = mri_assessmnet_json)))
}

