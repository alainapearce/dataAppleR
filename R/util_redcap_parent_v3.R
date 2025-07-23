#' util_redcap_parent_v3: Organize parent visit 3 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_3_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_3_arm_1
#' @param date_data date data for REDCap visit
#'
#' Will return a list including:
#' \itemize{
#'  \item{clean raw and scored parent visit 3 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{tfeq_data}
#'    \item{pwlb_data}
#'    \item{sdor_data}
#'  }
#' @examples
#'
#' # process REDCap data
#' parent_visit3_list <- util_redcap_parent_v3(data, date_data)
#'
#' \dontrun{
#' }
#'
#' @seealso [proc_redcap()]
#'
#' @export

util_redcap_parent_v3 <- function(data, date_data) {

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

  # merge with date data for v3
  data <- merge(data, date_data[c('participant_id', 'v3_date')], by = 'participant_id', all.x = TRUE)
  names(data)[names(data) == 'v3_date'] <- 'visit_date'
  data['visit_date'] <- lubridate::as_date(data[['visit_date']])

  ## TFEQ Data ####
  tfeq_data <- data[, grepl('_id|tfeq|visit_date', names(data))]

  # remove extra columns, add columns, and re-order
  tfeq_data <- tfeq_data[c('participant_id', 'visit_date', names(tfeq_data)[grepl('tfeq', names(tfeq_data))])]

  # fix scoring so base_zero = FALSE for all items (T/F question are base_zero = TRUE)
  tfeq_tf_qs <- c(paste0('tfeq_', seq(1, 36, 1)))

  tfeq_data[names(tfeq_data) %in% tfeq_tf_qs] <- sapply(tfeq_tf_qs, function(x) ifelse(tfeq_data[x] == 0, 1, ifelse(tfeq_data[x] == 1, 2, NA)))

  # score
  tfeq_scored <- dataprepr::score_tfeq(tfeq_data, base_zero = FALSE, id = 'participant_id')

  #tfeq_json <- json_tfeq()
  tfeq_json <- NA

  ## PWLB Data ####
  pwlb_data <- data[, grepl('_id|wlb|visit_date', names(data))]

  names(pwlb_data) <- gsub('wlb', 'pwlb', names(pwlb_data))

  # remove extra columns, add columns, and re-order
  pwlb_data <- pwlb_data[c('participant_id', 'visit_date', names(pwlb_data)[grepl('pwlb', names(pwlb_data))])]

  # score
  pwlb_scored <- dataprepr::score_pwlb(pwlb_data, var_24describe = 'pwlb24_describe', base_zero = FALSE, id = 'participant_id')

  pwlb_json <- json_pwlb()

  ## sdor Data ####
  sdor_data <- data[, grepl('_id|brief2|sdor|visit_date', names(data))]

  # remove extra columns, add columns, and re-order
  sdor_data <- sdor_data[c('participant_id', 'visit_date', names(sdor_data)[grepl('sdor', names(sdor_data))])]

  # score
  sdor_scored <- dataprepr::score_sdor(sdor_data, base_zero = FALSE, id = 'participant_id')

  #sdor_json <- json_sdor()
  sdor_json <- NA

  ## return data ####
  return(list(
    tfeq_data = list(data = tfeq_scored, meta = tfeq_json),
    pwlb_data = list(data = pwlb_scored, meta = pwlb_json),
    sdor_data = list(data = sdor_scored, meta = sdor_json)))
}

