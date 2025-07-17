#' util_redcap_parent_v3: Organize parent visit 3 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_3_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_3_arm_1
#' @inheritParams util_redcap_parent_v1
#'
#' @return Will return a list including:
#' \itemize{
#'  \item{clean raw and scored parent visit 3 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{visit_data_parent}
#'    \item{spsrq_scored}
#'    \item{pwlb_scored}
#'    \item{tfeq_scored}
#'    \item{class_score}
#'    \item{bisbas_scored}
#'    \item{pstca_scored}
#'    \item{debq_scored}
#'    \item{scpf_scored}
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

  # add session column
  data['session_id'] <- 'ses-1'

  # merge with date data for V3
  data <- merge(data, date_data[c('participant_id', 'v3_date')], by = 'participant_id', all.x = TRUE)
  names(data)[names(data) == 'v3_date'] <- 'visit_date'
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

  ## SPSRQ Data ####
  spsrq_data <- data[, grepl('_id|spsrq|visit_date', names(data))]

  # remove extra columns and re-order
  spsrq_data <- spsrq_data[c('participant_id', 'session_id', 'visit_date', names(spsrq_data)[grepl('spsrq', names(spsrq_data))])]

  spsrq_scored <- dataprepr::score_spsrq(spsrq_data, base_zero = TRUE, id = 'participant_id')

  spsrq_json <- json_spsrq()

  ## PWLB Data ####
  pwlb_data <- data[, grepl('_id|pwlb|visit_date', names(data))]

  # remove extra columns and re-order
  pwlb_data <- pwlb_data[, !grepl('missingcheck|timestamp', names(pwlb_data))]

  pwlb_data <- pwlb_data[c('participant_id', 'session_id', 'visit_date', names(pwlb_data)[grepl('pwlb', names(pwlb_data))])]

  # update names
  names(pwlb_data)[names(pwlb_data) == 'pwlb_24a'] <- 'pwlb_other_desc'

  pwlb_scored <- dataprepr::score_pwlb(pwlb_data, base_zero = TRUE, id = 'participant_id', extra_scale_cols = c('pwlb_other_desc'))

  pwlb_json <- json_pwlb()


  ## TFEQ Data ####
  #note: REACH used tfeq-r18 (revised scale)
  tfeq_data <- data[, grepl('_id|tfeq|visit_date', names(data))]

  # remove extra columns and re-order
  tfeq_data <- tfeq_data[c('participant_id', 'session_id', 'visit_date', names(tfeq_data)[grepl('tfeq', names(tfeq_data))])]

  tfeq_scored <- dataprepr::score_tfeq18(tfeq_data, base_zero = TRUE, id = 'participant_id')

  tfeq_json <- json_tfeq()

  ## CLASS Data ####
  class_data <- data[, grepl('_id|class|visit_date', names(data))]

  # remove extra columns and re-order
  class_data <- class_data[, !grepl('missingcheck|timestamp', names(class_data))] # remove extra columns

  class_data <- class_data[c('participant_id', 'session_id', 'visit_date', names(class_data)[grepl('class', names(class_data))])]

  # score? -- need to develop score script

  ## BISBAS Data ####
  bisbas_data <- data[, grepl('_id|bisbas|visit_date', names(data))]

  # remove extra columns and re-order
  bisbas_data <- bisbas_data[c('participant_id', 'session_id', 'visit_date', names(bisbas_data)[grepl('bisbas', names(bisbas_data))])]

  bisbas_scored <- dataprepr::score_bisbas(bisbas_data, base_zero = TRUE, id = 'participant_id')

  bisbas_json <- json_bisbas()

  ## pstca Data ####
  pstca_data <- data[, grepl('_id|pstca|ptsca|visit_date', names(data))]

  # remove extra columns and re-order
  pstca_data <- pstca_data[, !grepl('missingcheck|timestamp', names(pstca_data))] # remove extra columns

  pstca_data <- pstca_data[c('participant_id', 'session_id', 'visit_date', names(pstca_data)[grepl('ptsca', names(pstca_data))])]

  # fix scale names
  names(pstca_data) <- gsub('ptsca', 'pstca', names(pstca_data))

  # score -- need to develop score script

  ## DEBQ Data ####
  debq_data <- data[, grepl('_id|debq|visit_date', names(data))]

  # remove extra columns and re-order
  debq_data <- debq_data[c('participant_id', 'session_id', 'visit_date', names(debq_data)[grepl('debq', names(debq_data))])]

  # mark item 20 data as missing. debq question 20 was not administered in REACH -- debq question 23 was repeated in its place
  debq_data$debq_20 <- NA

  debq_scored <- dataprepr::score_debq(debq_data, base_zero = TRUE, id = 'participant_id', na_handling = TRUE)

  debq_json <- json_debq()

  ## SCPF Data ####
  scpf_data <- data[, grepl('_id|scpf|visit_date', names(data))]

  scpf_data <- scpf_data[c('participant_id', 'session_id', 'visit_date', names(scpf_data)[grepl('scpf', names(scpf_data))])]

  # mark item 17 data as missing -- was on the questionnaire, but not in the article scoring is based on. It is in the preschool 10 item questionnaire from same group. Removing because not clear how to score.
  scpf_data$scpf_17 <- NA

  scpf_scored <- dataprepr::score_scpf(scpf_data, base_zero = TRUE, id = 'participant_id')

  scpf_json <- json_scpf()


  ## return data ####
  return(list(
    visit3_updates = list(data = visit_data_parent, meta = visit_data_json),
    spsrq_data = list(data = spsrq_scored, meta = spsrq_json),
    pwlb_data = list(data = pwlb_scored, meta = pwlb_json),
    tfeq_data = list(data = tfeq_scored, meta = tfeq_json),
    class_data = list(data = class_data, meta = NA),
    bisbas_data = list(data = bisbas_scored, meta = bisbas_json),
    pstca_data = list(data = pstca_data, meta = NA),
    debq_data = list(data = debq_scored, meta = debq_json),
    scpf_data = list(data = scpf_scored, meta = scpf_json)))

}

