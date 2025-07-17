#' util_redcap_parent_v5: Organize parent visit 5 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_5_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_5_arm_1
#' @inheritParams util_redcap_parent_v1
#'
#' @return Will return a list including:
#' \itemize{
#'  \item{clean raw and scored parent visit 5 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{visit_data_parent}
#'    \item{household_data}
#'    \item{rank_data}
#'    \item{puberty_data}
#'    \item{cebq_data}
#'    \item{cbq_data}
#'    \item{class_data}
#'    \item{ptsca_data}
#'    \item{pmum_data}
#'    \item{audit_data}
#'    \item{cfpq_data}
#'  }
#'
#' @examples
#'
#' # process REDCap data
#' parent_visit4_list <- util_redcap_parent_v5(data, date_data)
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

  # add session column
  data$session_id <- 'ses-2'

  # merge with date data for V5
  data <- merge(data, date_data[c('participant_id', 'v5_date')], by = 'participant_id', all.x = TRUE)
  names(data)[names(data) == 'v5_date'] <- 'visit_date'
  data['visit_date'] <- lubridate::as_date(data[['visit_date']])

  #reduce columns and update names

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

  ## househole demographics data ####
  household_data <- data[, grepl('_id|demo|visit_date', names(data))]

  # remove extra columns and re-order
  household_data <- household_data[, !grepl('missingcheck', names(household_data))]

  household_data <- household_data[c('participant_id', 'session_id', 'visit_date', names(household_data)[grepl('demo', names(household_data))])]

  # process household data
  household_data <- util_format_household_data(household_data)

  household_json <- json_household()

  ## RANK Data (ranking food item questionnaire) ####
  rank_data <- data[, grepl('_id|rank|visit_date', names(data))]

  # remove extra columns and re-order
  rank_data <- rank_data[, !grepl('missingcheck', names(rank_data))]

  rank_data <- rank_data[c('participant_id', 'session_id', 'visit_date', names(rank_data)[grepl('rank', names(rank_data))])]

  # process rank data
  rank_data <- util_format_rank_data(rank_data)

  # score?

  ## Puberty Data ####
  puberty_data <-data[, grepl('_id|prs|tanner|visit_date', names(data))]

  # remove extra columns and re-order
  puberty_data <- puberty_data[c('participant_id', 'session_id', 'visit_date', names(puberty_data)[grepl('prs|tanner', names(puberty_data))])]

  puberty_data <- util_format_puberty_data(puberty_data, respondent = 'parent')

  puberty_scored <- dataprepr::score_pds(puberty_data, base_zero = FALSE, respondent = 'parent', male = 'male', female = 'female', id = 'participant_id')

  puberty_json <- json_puberty()

  ## CEBQ Data ####
  cebq_data <- data[, grepl('_id|cebq|visit_date', names(data))]

  # remove extra columns and re-order
  cebq_data <- cebq_data[, !grepl('missingcheck', names(cebq_data))]

  cebq_data <- cebq_data[c('participant_id', 'session_id', 'visit_date', names(cebq_data)[grepl('cebq', names(cebq_data))])]

  cebq_scored <- dataprepr::score_cebq(cebq_data, base_zero = TRUE, id = 'participant_id')

  cebq_json <- json_cebq()

  ## CBQ Data ####
  cbq_data <- data[, grepl('_id|cbq|visit_date', names(data))]

  # remove extra columns and re-order
  cbq_data <- cbq_data[, !grepl('missingcheck', names(cbq_data))]

  cbq_data <- cbq_data[c('participant_id', 'session_id', 'visit_date', names(cbq_data)[grepl('cbq', names(cbq_data))])]

  cbq_scored <- dataprepr::score_cbq(cbq_data, base_zero = TRUE, id = 'participant_id', does_not_apply_value = 7)

  cbq_json <- json_cbq()

  ## CSHQ Data ####
  cshq_data <- data[, grepl('_id|cshq|visit_date', names(data))]

  # remove extra columns and re-order
  cshq_data <- cshq_data[, !grepl('missingcheck', names(cshq_data))]

  cshq_data <- cshq_data[c('participant_id', 'session_id', 'visit_date', names(cshq_data)[grepl('cshq', names(cshq_data))])]

  cshq_data <- util_format_cshq_data(cshq_data)

  cshq_scored <- dataprepr::score_cshq(cshq_data, base_zero = FALSE, reverse_score = FALSE, id = 'participant_id')

  cshq_json <- json_cshq()

  ## CLASS Data ####
  class_data <- data[, grepl('_id|class|visit_date', names(data))]

  # remove extra columns and re-order
  class_data <- class_data[, !grepl('missingcheck', names(class_data))]

  class_data <- class_data[c('participant_id', 'session_id', 'visit_date', names(class_data)[grepl('class', names(class_data))])]

  # score? -- need to develop score script

  ## PSTCA Data ####
  pstca_data <- data[, grepl('_id|ptsca|visit_date', names(data))]

  # remove extra columns and re-order
  pstca_data <- pstca_data[, !grepl('missingcheck', names(pstca_data))]

  pstca_data <- pstca_data[c('participant_id', 'session_id', 'visit_date', names(pstca_data)[grepl('ptsca', names(pstca_data))])]

  # fix scale names
  names(pstca_data) <- gsub('ptsca', 'pstca', names(pstca_data))

  # score -- need to develop scorce script

  ## PMUM Data ####
  pmum_data <- data[, grepl('_id|pmum|visit_date', names(data))]

  # remove extra columns and re-order
  pmum_data <- pmum_data[, !grepl('missingcheck', names(pmum_data))]

  pmum_data <- pmum_data[c('participant_id', 'session_id', 'visit_date', names(pmum_data)[grepl('pmum', names(pmum_data))])]

  # score -- need to develop score script

  ## AUDIT Data ####
  audit_data <- data[, grepl('_id|audit|visit_date', names(data))]

  # remove extra columns and re-order
  audit_data <- audit_data[, !grepl('missingcheck', names(audit_data))]

  audit_data <- audit_data[c('participant_id', 'session_id', 'visit_date', names(audit_data)[grepl('audit', names(audit_data))])]

  audit_scored <- dataprepr::score_audit(audit_data, id = 'participant_id', base_zero = TRUE)

  audit_json <- json_audit()

  ## CFPQ Data ####
  cfpq_data <- data[, grepl('_id||cfpq|visit_date', names(data))]

  # remove extra columns and re-order
  cfpq_data <- cfpq_data[, !grepl('missingcheck', names(cfpq_data))]

  cfpq_data <- cfpq_data[c('participant_id', 'session_id', 'visit_date', names(cfpq_data)[grepl('cfpq', names(cfpq_data))])]

  cfpq_scored <- dataprepr::score_cfpq(cfpq_data, base_zero = TRUE, id = 'participant_id')

  cfpq_json <- json_cfpq()

  ## return data ####
  return(list(
    visit5_updates = list(data = visit_data_parent, meta = visit_data_json),
    household_data = list(data = household_data, meta = household_json),
    rank_data = list(data = rank_data, meta = NA),
    puberty_data = list(data = puberty_scored, meta = puberty_json),
    cebq_data = list(data = cebq_scored, meta = cebq_json),
    cbq_data = list(data = cbq_scored, meta = cbq_json),
    cshq_data = list(data = cshq_scored, meta = cshq_json),
    class_data = list(data = class_data, meta = NA),
    pstca_data = list(data = pstca_data, meta = NA),
    pmum_data = list(data = pmum_data, meta = NA),
    audit_data = list(data = audit_scored, meta = audit_json),
    cfpq_data = list(data = cfpq_scored, meta = cfpq_json)))
}

