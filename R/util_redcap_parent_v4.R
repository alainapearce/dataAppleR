#' util_redcap_parent_v4: Organize parent visit 4 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_4_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_4_arm_1
#' @param date_data date data for REDCap visit
#'
#' Will return a list including:
#' \itemize{
#'  \item{clean raw and scored parent visit 4 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{ecsi_data}
#'    \item{cfsq_data}
#'    \item{asbi_data}
#'  }
#' @examples
#'
#' # process REDCap data
#' parent_visit4_list <- util_redcap_parent_v4(data, date_data)
#'
#' \dontrun{
#' }
#'
#' @seealso [proc_redcap()]
#'
#' @export

util_redcap_parent_v4 <- function(data, date_data) {

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

  # merge with date data for v4
  data <- merge(data, date_data[c('participant_id', 'v4_date')], by = 'participant_id', all.x = TRUE)
  names(data)[names(data) == 'v4_date'] <- 'visit_date'
  data['visit_date'] <- lubridate::as_date(data[['visit_date']])

  ## ecSI-2 Data ####
  ecsi_data <- data[, grepl('_id|ecsi2|visit_date', names(data))]

  names(ecsi_data) <- gsub('ecsi2', 'ecsi', names(ecsi_data))

  # remove extra columns, add columns, and re-order
  ecsi_data <- ecsi_data[c('participant_id', 'visit_date', names(ecsi_data)[grepl('ecsi', names(ecsi_data))])]

  # score
  ecsi2_scored <- dataprepr::score_ecsi2(ecsi_data, base_zero = FALSE, id = 'participant_id')

  #ecsi2_json <- json_ecsi2()
  ecsi2_json <- NA

  ## CFSQ Data ####
  cfsq_data <- data[, grepl('_id|cfsq|visit_date', names(data))]

  # remove extra columns, add columns, and re-order
  cfsq_data <- cfsq_data[c('participant_id', 'visit_date', names(cfsq_data)[grepl('cfsq', names(cfsq_data))])]

  # score
  cfsq_scored <- dataprepr::score_cfsq(cfsq_data, base_zero = FALSE, id = 'participant_id')

  #cfsq_json <- json_cfsq()
  cfsq_json <- NA

  ## asbi Data ####
  asbi_data <- data[, grepl('_id|asbi|visit_date', names(data))]

  # remove extra columns, add columns, and re-order
  asbi_data <- asbi_data[c('participant_id', 'visit_date', names(asbi_data)[grepl('asbi', names(asbi_data))])]

  # score
  asbi_scored <- dataprepr::score_asbi(asbi_data, base_zero = FALSE, id = 'participant_id')

  #asbi_json <- json_asbi()
  asbi_json <- NA

  ## return data ####
  return(list(
    ecsi2_data = list(data = ecsi2_scored, meta = ecsi2_json),
    cfsq_data = list(data = cfsq_scored, meta = cfsq_json),
    asbi_data = list(data = asbi_scored, meta = asbi_json)))
}

