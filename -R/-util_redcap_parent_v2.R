#' util_redcap_parent_v2: Organize parent visit 2 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_2_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_2_arm_1
#' @inheritParams util_redcap_parent_v1
#'
#'
#' @return Will return a list including:
#' \itemize{
#'  \item{clean raw and scored parent visit 2 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{visit_data_parent}
#'    \item{cbq_data}
#'    \item{brief_data}
#'    \item{cshq_data}
#'    \item{bes_data}
#'    \item{ffbs_data}
#'    \item{fsq_data}
#'  }
#' @examples
#'
#' # process REDCap data
#' parent_visit2_list <- util_redcap_parent_v2(data, data_data)
#'
#' \dontrun{
#' }
#'
#' @seealso [proc_redcap()]
#'
#' @export



util_redcap_parent_v2 <- function(data, date_data) {

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

  # add session column
  data['session_id'] <- 'ses-1'

  # merge with date data for V2
  data <- merge(data, date_data[c('participant_id', 'v2_date', 'v2_age', 'sex')], by = 'participant_id', all.x = TRUE)
  names(data)[names(data) == 'v2_date'] <- 'visit_date'
  data['visit_date'] <- lubridate::as_date(data[['visit_date']])

  #reduce columns and update names

  ## Update form Data ####
  visit_data_parent <- data[, grepl('_id|update|visit_date', names(data))]

  # remove extra columns and re-order
  visit_data_parent <- visit_data_parent[, !grepl('contact|moving', names(visit_data_parent))]

  visit_data_parent <- visit_data_parent[c('participant_id', 'session_id', 'visit_date', names(visit_data_parent)[grepl('update', names(visit_data_parent))])]

  # rename columns
  names(visit_data_parent) <- gsub('_form', '', names(visit_data_parent))

  names(visit_data_parent)[names(visit_data_parent) == 'update_med_history_2'] <- 'update_med_history_desc'
  names(visit_data_parent)[names(visit_data_parent) == 'update_prescription_2'] <- 'update_prescription_desc'
  names(visit_data_parent)[names(visit_data_parent) == 'update_dental_2'] <- 'update_dental_desc'
  names(visit_data_parent)[names(visit_data_parent) == 'update_new_illness_2'] <- 'update_new_illness_desc'
  names(visit_data_parent)[names(visit_data_parent) == 'update_diet_change_2'] <- 'update_diet_change_desc'

  visit_data_json <- json_parent_updates()

  ## CBQ Data ####
  cbq_data <- data[, grepl('_id|cbq|visit_date', names(data))]

  # remove extra columns and re-order
  cbq_data <- cbq_data[, !grepl('missingcheck', names(cbq_data))]
  cbq_data <- cbq_data[c('participant_id', 'session_id', 'visit_date', names(cbq_data)[grepl('cbq', names(cbq_data))])]

  # score
  cbq_scored <- dataprepr::score_cbq(cbq_data, base_zero = TRUE, id = 'participant_id', does_not_apply_value = 7)

  cbq_json <- json_cbq()

  ## BRIEF Data ####
  brief_data <- data[, grepl('_id|brief|visit_date|age|sex', names(data))]

  # remove extra columns and re-order
  brief_data <- brief_data[, !grepl('missing_check|timestamp', names(brief_data))] # remove extra columns

  brief_data <- brief_data[c('participant_id', 'session_id', 'visit_date', 'v2_age', 'sex', names(brief_data)[grepl('brief', names(brief_data))])]

  # score
  brief_scored <- dataprepr::score_brief2(brief_data, age_var = 'v2_age', sex_var = 'sex', base_zero = TRUE, male = 'male', female = 'female', id = 'participant_id')

  brief_json <- json_brief2()

  ## CSHQ Data ####
  cshq_data <- data[, grepl('_id|cshq|visit_date', names(data))]

  # remove extra columns and re-order
  cshq_data <- cshq_data[, !grepl('missingcheck|timestamp', names(cshq_data))]

  cshq_data <- cshq_data[c('participant_id', 'session_id', 'visit_date', names(cshq_data)[grepl('cshq', names(cshq_data))])]

  # score
  cshq_data <- util_format_cshq_data(cshq_data)
  cshq_scored <- dataprepr::score_cshq(cshq_data, base_zero = FALSE, reverse_score = FALSE, id = 'participant_id')

  cshq_json <- json_cshq()

  ## BES Data ####
  bes_data <- data[, grepl('_id|bes|visit_date', names(data))]

  # remove extra columns and re-order
  bes_data <- bes_data[, !grepl('missingcheck|timestamp', names(bes_data))]

  bes_data <- bes_data[c('participant_id', 'session_id', 'visit_date', names(bes_data)[grepl('bes', names(bes_data))])]

  # change pna ('Don't want to answer') responses to 99
  bes_data[, grepl('bes', names(bes_data))] <- sapply(bes_data[, grepl('bes', names(bes_data))], function(x) ifelse(x == 4, 99, x))

  bes_data[, grepl('_6|13|14|15|16', names(bes_data))] <- sapply(bes_data[, grepl('_6|13|14|15|16', names(bes_data))], function(x) ifelse(x == 3, 99, x))

  # score
  bes_scored <- dataprepr::score_bes(bes_data, base_zero = TRUE, pna = 99, id = 'participant_id')

  bes_json <- json_bes()

  ## FFBS Data ####
  ffbs_data <- data[, grepl('_id|ffbs|visit_date', names(data))]

  # relocate columns
  ffbs_data <- ffbs_data[c('participant_id', 'session_id', 'visit_date', names(ffbs_data)[grepl('ffbs', names(ffbs_data))])]

  #score
  ffbs_scored <- dataprepr::score_ffbs(ffbs_data, base_zero = TRUE, id = 'participant_id')

  ffbs_json <- json_ffbs()

  ## FSQ Data (feeding strategies questionnaire) ####
  fsq_data <- data[, grepl('_id|fsq|visit_date', names(data))]

  # remove extra columns and re-order
  fsq_data <- fsq_data[, !grepl('missingcheck|timestamp', names(fsq_data))]

  fsq_data <- fsq_data[c('participant_id', 'session_id', 'visit_date', names(fsq_data)[grepl('fsq', names(fsq_data))])]

  #score -- need to develop score script
  fsq_json <- json_fsq()

  ## return data ####
  return(list(
    visit2_updates = list(data = visit_data_parent, meta = visit_data_json),
    cbq_data = list(data = cbq_scored, meta = cbq_json),
    brief_data = list(data = brief_scored, meta = brief_json),
    cshq_data = list(data = cshq_scored, meta = cshq_json),
    bes_data = list(data = bes_scored, meta = bes_json),
    ffbs_data = list(data = ffbs_scored, meta = ffbs_json),
    fsq_data = list(data = fsq_data, meta = fsq_json)
  ))
}

