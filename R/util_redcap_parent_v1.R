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
#'    \item{cebq_data}
#'    \item{cfq_data}
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
  demo_data_all <- demo_data_all[c('participant_id', 'visit_date', names(demo_data_all)[grepl('demo', names(demo_data_all))])]

  names(demo_data_all)[names(demo_data_all) == 'demo_feeding'] <- 'demo_feeding_child'

  # select columns for participants_data
  demo_data <- demo_data_all[c('participant_id', 'demo_child_ethnicity', 'demo_child_race')]
  names(demo_data) <- gsub('child_', '', names(demo_data))

  # select columns for infancy_data
  infancy_data <- demo_data_all[c('participant_id', 'visit_date', 'demo_child_bl', 'demo_child_premature', 'demo_premature_weeks', 'demo_child_feeding', 'demo_bf_months')]

  infancy_data <- util_format_infant_data(infancy_data)

  #re-order
  infancy_data <- infancy_data[c('participant_id', 'visit_date', 'demo_birth_length',  'demo_birthweight_pounds_component', 'demo_birthweight_ounces_component', 'demo_premature', 'demo_premature_weeks', 'demo_feeding', 'demo_exclusive_feeding', 'birthweight_ounces_total')]

  infancy_json <- json_infancy()

  # select columns for household_data
  household_data <- demo_data_all[, !(names(demo_data_all) %in% c(names(demo_data[!grepl('_id', names(demo_data))]), names(infancy_data[!grepl('_id|visit_date', names(infancy_data))])))]

  names(household_data)[names(household_data) == 'demo_child_foster'] <- 'demo_foster'

  household_data <- household_data[!grepl('_child_|bf|demo_foods', names(household_data))]

  # process household data
  household_data <- util_format_household_data(household_data)

  household_json <- json_household()

  ## RANK Data (ranking food item questionnaire) ####
  rank_data <- data[, grepl('_id|demo_foods|visit_date', names(data))]

  names(rank_data) <- gsub('demo_foods', 'rank_', names(rank_data))

  # remove extra columns, add columns, and re-order
  rank_data <- rank_data[c('participant_id', 'visit_date', names(rank_data)[grepl('rank', names(rank_data))])]

  # process rank data
  rank_data <- util_format_rank_data(rank_data)

  rank_json <- json_rank()
  # score?


  ## CFQ Data ####
  cfq_data <- data[, grepl('_id|cfq|visit_date', names(data))]

  # remove extra columns, add columns, and re-order
  cfq_data <- cfq_data[c('participant_id', 'visit_date', names(cfq_data)[grepl('cfq', names(cfq_data))])]

  #fix numbering
  cfq_data <- util_format_cfq_data(cfq_data)

  cfq_scored <- dataprepr::score_cfq(cfq_data, base_zero = FALSE, restriction_split = FALSE, id = 'participant_id', pcw_na_value = 6)

  cfq_json <- json_cfq()

  ## CEBQ Data ####
  cebq_data <- data[, grepl('_id|cebq|cbeq|visit_date', names(data))]

  names(cebq_data) <- gsub('cbeq', 'cebq', names(cebq_data))

  # remove extra columns, add columns, and re-order
  cebq_data <- cebq_data[c('participant_id', 'visit_date', names(cebq_data)[grepl('cebq', names(cebq_data))])]

  cebq_scored <- dataprepr::score_cebq(cebq_data, base_zero = FALSE, id = 'participant_id')

  cebq_json <- json_cebq()

  ## return data ####
  return(list(
    demo_data = list(data = demo_data),
    infancy_data = list(data = infancy_data, meta = infancy_json),
    household_data = list(data = household_data, meta = household_json),
    rank_data = list(data = rank_data, meta = rank_json),
    cfq_data = list(data = cfq_scored, meta = cfq_json),
    cebq_data = list(data = cebq_scored, meta = cebq_json)))
}

