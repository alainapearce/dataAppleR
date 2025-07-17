#' util_redcap_parent_v1: Organize parent visit 1 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_1_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_1_arm_1
#' @param date_data date data for REDCap visit
#'
#' Will return a list including:
#' \itemize{
#'  \item{clean raw and scored parent visit 1 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{demo_data}
#'    \item{infancy_data}
#'    \item{household_data}
#'    \item{rank_data}
#'    \item{puberty_data}
#'    \item{cfq_data}
#'    \item{cebq_data}
#'    \item{efcr_data}
#'    \item{chaos_data}
#'    \item{pss_data}
#'    \item{lbc_data}
#'  }
#' @examples
#'
#' # process REDCap data
#' parent_visit1_list <- util_redcap_parent_v1(data, date_data)
#'
#' \dontrun{
#' }
#'
#' @seealso [proc_redcap()]
#'
#' @export

util_redcap_parent_v1 <- function(data, date_data) {

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

  # merge with date data for V1
  data <- merge(data, date_data[c('participant_id', 'v1_date')], by = 'participant_id', all.x = TRUE)
  names(data)[names(data) == 'v1_date'] <- 'visit_date'
  data['visit_date'] <- lubridate::as_date(data[['visit_date']])

  #reduce columns and update names

  ## demographics data ####
  # this data will be split into 3 dataframes:
  # (1) demo_data: data collected as part of the 'Visit 1 Demographics' qualtrics form that will go into participants.tsv (or demographics.tsv) file
  # (2) infancy_data: data collected as part of the 'Visit 1 Demographics' qualtrics form that will go into infancy.tsv file
  # (3) household_data: data collected as part of the 'Parent Household Demographics' qualtrics form

  # select all demo variables
  demo_data_all <- data[, grepl('_id||demo|visit_date', names(data))]

  # remove extra columns, add columns, and re-order
  demo_data_all <- demo_data_all[, !grepl('missingcheck', names(demo_data_all))]

  demo_data_all <- demo_data_all[c('participant_id', 'session_id', 'visit_date', names(demo_data_all)[grepl('demo', names(demo_data_all))])]

  # select columns for participants_data
  demo_data <- demo_data_all[c('participant_id', 'demo_ethnicity', 'demo_race')]

  # select columns for infancy_data
  infancy_data <- demo_data_all[c('participant_id','session_id', 'visit_date', 'demo_birth_length', 'demo_birthweight_pounds', 'demo_birthweight_ounces', 'demo_premature', 'demo_premature_weeks', 'demo_feeding', 'demo_exclusive_feeding', 'demo_tot_breastfeeding', 'demo_solid_food')]

  # derive total birthweight in ounces from lb and oz components
  infancy_data['birthweight_ounces_total'] <- (infancy_data[['demo_birthweight_pounds']])*16 + infancy_data[['demo_birthweight_ounces']]

  infancy_json <- json_v1_infancy()

  # select columns for household_data
  household_data <- demo_data_all[, !(names(demo_data_all) %in% c(names(demo_data[!grepl('_id', names(demo_data))]), names(infancy_data[!grepl('_id|visit_date', names(infancy_data))])))]

  # process household data
  household_data <- util_format_household_data(household_data)

  household_json <- json_household()

  ## RANK Data (ranking food item questionnaire) ####
  rank_data <- data[, grepl('_id|rank|visit_date', names(data))]

  # remove extra columns, add columns, and re-order
  rank_data <- rank_data[c('participant_id', 'session_id', 'visit_date', names(rank_data)[grepl('rank', names(rank_data))])]

  # process rank data
  rank_data <- util_format_rank_data(rank_data)

  rank_json <- json_rank()
  # score?

  ## Puberty Data ####
  puberty_data <- data[, grep('_id|visit_date|prs|tanner|visit_date', names(data))]

  # remove extra columns, add columns, and re-order
  puberty_data['respondent'] <- 'parent'
  names(puberty_data)[names(puberty_data) == 'prs_sex'] <- 'sex'

  puberty_data <- puberty_data[c('participant_id', 'session_id', 'visit_date', 'respondent', 'sex', names(puberty_data)[grepl('prs|tanner', names(puberty_data))])]

  # process puberty data
  puberty_data <- util_format_puberty_data(puberty_data, respondent = 'parent')

  puberty_scored <- dataprepr::score_pds(puberty_data, base_zero = TRUE, respondent = 'parent', male = 'male', female = 'female', id = 'participant_id')

  puberty_json <- json_puberty()

  ## CFQ Data ####
  cfq_data <- data[, grepl('_id|cfq|visit_date', names(data))]

  # remove extra columns, add columns, and re-order
  cfq_data <- cfq_data[, -grep('missingcheck|timestamp', names(cfq_data))]
  cfq_data <- cfq_data[c('participant_id', 'session_id', 'visit_date', names(cfq_data)[grepl('cfq', names(cfq_data))])]

  cfq_scored <- dataprepr::score_cfq(cfq_data, base_zero = TRUE, restriction_split = FALSE, id = 'participant_id', pcw_na_value = 5)

  cfq_json <- json_cfq()

  ## CEBQ Data ####
  cebq_data <- data[, grepl('_id|cebq|visit_date', names(data))]

  # remove extra columns, add columns, and re-order
  cebq_data <- cebq_data[c('participant_id', 'session_id', 'visit_date', names(cebq_data)[grepl('cebq', names(cebq_data))])]

  cebq_scored <- dataprepr::score_cebq(cebq_data, base_zero = TRUE, id = 'participant_id')

  cebq_json <- json_cebq()

  ## EFCR Data ####
  efcr_data <- data[, grepl('_id||efcr|visit_date', names(data))]

  # remove extra columns, add columns, and re-order
  efcr_data <- efcr_data[c('participant_id', 'session_id', 'visit_date', names(efcr_data)[grepl('efcr', names(efcr_data))])]

  efcr_scored <- dataprepr::score_efcr(efcr_data, base_zero = TRUE, id = 'participant_id')

  efcr_json <- json_efcr()

  ## CHAOS Data  ####
  chaos_data <- data[, grepl('_id|chaos|visit_date', names(data))]

  # remove extra columns, add columns, and re-order
  chaos_data <- chaos_data[c('participant_id', 'session_id', 'visit_date', names(chaos_data)[grepl('chaos', names(chaos_data))])]

  chaos_scored <- dataprepr::score_chaos(chaos_data, base_zero = TRUE, id = 'participant_id')

  chaos_json <- json_chaos()


  ## PSS Data  (percieved stress scale) ####
  pss_data <- data[, grepl('_id|pss|visit_date', names(data))]

  # remove extra columns, add columns, and re-order
  pss_data <- pss_data[c('participant_id', 'session_id', 'visit_date', names(pss_data)[grepl('pss', names(pss_data))])]

  pss_scored <- dataprepr::score_pss(pss_data, base_zero = TRUE, id = 'participant_id')

  pss_json <- json_pss()

  ## LBC Data  ####
  lbc_data <- data[, grepl('_id|lbc|visit_date', names(data))]

  # rename columns
  names(lbc_data) <- gsub('lbc_', 'lbc', names(lbc_data))
  names(lbc_data) <- gsub('_a', '_conf', names(lbc_data))

  # remove extra columns, add columns, and re-order
  lbc_data <- lbc_data[, -grep('missingcheck|timestamp', names(lbc_data))]

  lbc_data <- lbc_data[c('participant_id', 'session_id', 'visit_date', names(lbc_data)[grepl('lbc', names(lbc_data))])]

  lbc_scored <- dataprepr::score_lbc(lbc_data, base_zero = TRUE, id = 'participant_id')

  lbc_json <- json_lbc()

  ## return data ####
  return(list(
    demo_data = list(data = demo_data),
    infancy_data = list(data = infancy_data, meta = infancy_json),
    household_data = list(data = household_data, meta = household_json),
    rank_data = list(data = rank_data, meta = rank_json),
    puberty_data = list(data = puberty_scored, meta = puberty_json),
    cfq_data = list(data = cfq_scored, meta = cfq_json),
    cebq_data = list(data = cebq_scored, meta = cebq_json),
    efcr_data = list(data = efcr_scored, meta = efcr_json),
    chaos_data = list(data = chaos_scored, meta = chaos_json),
    pss_data = list(data = pss_scored, meta = pss_json),
    lbc_data = list(data = lbc_scored, meta = lbc_json)))
}

