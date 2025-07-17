#' util_redcap_parent_v4: Organize parent visit 4 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_4_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_4_arm_1
#' @inheritParams util_redcap_parent_v1
#'
#' @return Will return a list including:
#' \itemize{
#'  \item{clean raw and scored parent visit 4 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{visit_data_parent}
#'    \item{hfias_scored}
#'    \item{hfssm_scored}
#'    \item{pmum_scored}
#'    \item{cchip_scored}
#'    \item{audit_scored}
#'    \item{hfi_scored}
#'    \item{cfpq_scored}
#'  }
#'
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

  # add session column
  data$session_id <- 'ses-1'

  # merge with date data for V4
  data <- merge(data, date_data[c('participant_id', 'v4_date')], by = 'participant_id', all.x = TRUE)
  names(data)[names(data) == 'v4_date'] <- 'visit_date'
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

  ## HFSSM Data ####
  hfssm_data <- data[, grepl('_id|hfssm|visit_date', names(data))]

  # remove extra columns and re-order
  hfssm_data <- hfssm_data[c('participant_id', 'session_id', 'visit_date', names(hfssm_data)[grepl('hfssm', names(hfssm_data))])]

  # prep hfssm_data for scoring
  hfssm_data[names(hfssm_data)[grepl('ad', names(hfssm_data))]] <- sapply(names(hfssm_data)[grepl('ad', names(hfssm_data))], function(x) ifelse(is.na(hfssm_data[[x]]), NA, ifelse(hfssm_data[[x]] == 0, 1, ifelse(hfssm_data[[x]] == 1, 0, ifelse(hfssm_data[[x]] == 2, 2, NA)))))

  # score
  hfssm_scored <- dataprepr::score_hfssm(hfssm_data, base_zero = TRUE, id = 'participant_id')

  hfssm_json <- json_hfssm()

  ## HFIAS Data ####
  hfias_data <- data[, grepl('_id|^hfi|visit_date', names(data))]

  # remove extra columns and re-order
  hfias_data <- hfias_data[c('participant_id', 'session_id', 'visit_date', names(hfias_data)[grepl('hfi', names(hfias_data))])]

  #fix names
  names(hfias_data) <- gsub('hfi', 'hfias', names(hfias_data))

  hfias_scored <- dataprepr::score_hfias(hfias_data, base_zero = TRUE, id = 'participant_id')

  hfias_json <- json_hfias()

  ## PMUM Data ####
  pmum_data <- data[, grepl('_id|pmum|visit_date', names(data))]

  # remove extra columns and re-order
  pmum_data <- pmum_data[c('participant_id', 'session_id', 'visit_date', names(pmum_data)[grepl('pmum', names(pmum_data))])]

  # score -- need to develop score script

  ## CCHIP Data ####
  cchip_data <- data[, grepl('_id|cchip|visit_date', names(data))]

  # remove extra columns and re-order
  cchip_data <- cchip_data[c('participant_id', 'session_id', 'visit_date', names(cchip_data)[grepl('cchip', names(cchip_data))])]

  cchip_scored <- dataprepr::score_cchip(cchip_data, id = 'participant_id')

  cchip_json <- json_cchip()

  ## AUDIT Data ####
  audit_data <- data[, grepl('_id|audit|visit_date', names(data))]

  # remove extra columns and re-order
  audit_data <- audit_data[, !grepl('missingcheck', names(audit_data))]

  audit_data <- audit_data[c('participant_id', 'session_id', 'visit_date', names(audit_data)[grepl('audit', names(audit_data))])]

  audit_scored <- dataprepr::score_audit(audit_data, id = 'participant_id', base_zero = TRUE)

  audit_json <- json_audit()

  ## Fulkerson HFI Data ####
  hfi_data <- data[, grepl('_id|fhfi|visit_date', names(data))]

  # remove extra columns and re-order
  hfi_data <- hfi_data[c('participant_id', 'session_id', 'visit_date', names(hfi_data)[grepl('fhfi', names(hfi_data))])]

  # fix variable naming
  names(hfi_data) <- gsub('fhfi', 'hfi', names(hfi_data))

  hfi_data <- util_format_hfi_data(hfi_data)

  hfi_scored <- dataprepr::score_hfi(hfi_data, base_zero = TRUE, id = 'participant_id', extra_scale_cols = c('hfi_extra_nondairy', 'hfi_extra_accessible_fridge'))

  hfi_json <- json_hfi()

  ## CFPQ Data ####
  cfpq_data <- data[, grepl('_id|cfpq|visit_date', names(data))]

  # remove extra columns and re-order
  cfpq_data <- cfpq_data[c('participant_id', 'session_id', 'visit_date', names(cfpq_data)[grepl('cfpq', names(cfpq_data))])]

  cfpq_scored <- dataprepr::score_cfpq(cfpq_data, base_zero = TRUE, id = 'participant_id')

  cfpq_json <- json_cfpq()


  ## return data ####

  return(list(
    visit4_updates = list(data = visit_data_parent, meta = visit_data_json),
    hfias_data = list(data = hfias_scored, meta = hfias_json),
    hfssm_data = list(data = hfssm_scored, meta = hfssm_json),
    pmum_data = list(data = pmum_data, meta = NA),
    cchip_data = list(data = cchip_scored, meta = cchip_json),
    audit_data = list(data = audit_scored, meta = audit_json),
    hfi_data = list(data = hfi_scored, meta = hfi_json),
    cfpq_data = list(data = cfpq_scored, meta = cfpq_json)
  ))

}

