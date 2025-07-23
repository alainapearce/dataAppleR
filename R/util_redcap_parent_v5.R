#' util_redcap_parent_v5: Organize parent visit 5 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_5_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_5_arm_1
#' @param date_data date data for REDCap visit
#'
#' Will return a list including:
#' \itemize{
#'  \item{clean raw and scored parent visit 5 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{nnsa_data}
#'    \item{nns_parent_data}
#'    \item{nns_child_data}
#'    \item{bevq_data}
#'  }
#' @examples
#'
#' # process REDCap data
#' parent_visit5_list <- util_redcap_parent_v5(data, date_data)
#'
#' \dontrun{
#' }
#'
#' @seealso [proc_redcap()]
#'
#' @export

util_redcap_parent_v5 <- function(data, date_data) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop('data must be a data.frame')
    }
  } else if (isFALSE(data_arg)) {
  }

  # update name of participant ID column
  names(data)[names(data) == 'record_id'] <- 'participant_id'

  # merge with date data for v5
  data <- merge(data, date_data[c('participant_id', 'v5_date')], by = 'participant_id', all.x = TRUE)
  names(data)[names(data) == 'v5_date'] <- 'visit_date'
  data['visit_date'] <- lubridate::as_date(data[['visit_date']])

  ## nnsa Data ####
  nnsa_data <- data[, grepl('_id|nnsa|visit_date', names(data))]

  names(nnsa_data) <- gsub('nnsa', 'nnsa', names(nnsa_data))

  # remove extra columns, add columns, and re-order
  nnsa_data <- nnsa_data[c('participant_id', 'visit_date', names(nnsa_data)[grepl('nnsa', names(nnsa_data))])]

  # score
  #nnsa_scored <- dataprepr::score_nnsa(nnsa_data, base_zero = FALSE, id = 'participant_id')
  nnsa_scored <- nnsa_data

  #nnsa_json <- json_nnsa()
  nnsa_json <- NA

  ## nns_parent Data ####
  nns_parent_data <- data[, grepl('_id|nns_parent|visit_date', names(data))]

  # remove extra columns, add columns, and re-order
  nns_parent_data <- nns_parent_data[c('participant_id', 'visit_date', names(nns_parent_data)[grepl('nns_parent', names(nns_parent_data))])]

  # score
  #nns_parent_scored <- dataprepr::score_nns_parent(nns_parent_data, base_zero = FALSE, id = 'participant_id')
  nns_parent_scored <- nns_parent_data

  #nns_parent_json <- json_nns_parent()
  nns_parent_json <- NA

  ## nns_child Data ####
  nns_child_data <- data[, grepl('_id|nns_child|visit_date', names(data))]

  # remove extra columns, add columns, and re-order
  nns_child_data <- nns_child_data[c('participant_id', 'visit_date', names(nns_child_data)[grepl('nns_child', names(nns_child_data))])]

  # score
  #nns_child_scored <- dataprepr::score_nns_child(nns_child_data, base_zero = FALSE, id = 'participant_id')
  nns_child_scored <- nns_child_data

  #nns_child_json <- json_nns_child()
  nns_child_json <- NA


  ## bevq Data ####
  bevq_data <- data[, grepl('_id|bevq|visit_date', names(data))]

  # remove extra columns, add columns, and re-order
  bevq_data <- bevq_data[c('participant_id', 'visit_date', names(bevq_data)[grepl('bevq', names(bevq_data))])]

  # score
  #bevq_scored <- dataprepr::score_bevq(bevq_data, base_zero = TRUE, id = 'participant_id')

  #bevq_json <- json_bevq()
  bevq_json <- NA

  ## return data ####
  return(list(
    nnsa_data = list(data = nnsa_scored, meta = nnsa_json),
    nns_parent_data = list(data = nns_parent_scored, meta = nns_parent_json),
    nns_child_data = list(data = nns_child_scored, meta = nns_child_json),
    bevq_data = list(data = NA, meta = bevq_json)))

    #bevq_data = list(data = bevq_scored, meta = bevq_json)))
}

