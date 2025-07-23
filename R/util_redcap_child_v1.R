#' util_redcap_child_v1: Organize child visit 1 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_1_arm_1
#'
#' @param data data from REDCap event child_visit_1_arm_1
#' @param date_data date data for REDCap visitd
#'
#' @return Will return a list including:
#' \itemize{
#'  \item{clean raw child visit 1 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{visit_data_child}
#'    \item{food_paradigm_info}
#'    \item{fullness_data}
#'    \item{intake_data}
#'    \item{liking_data}
#'    \item{anthro_data}
#'    \item{rsa_data}
#'    \item{pst_data}
#'  }
#' @examples
#'
#' # process REDCap data
#' child_visit1_list <- util_redcap_child_v1(data)
#'
#' \dontrun{
#' }
#'
#' @seealso [proc_redcap()]
#'
#' @export

util_redcap_child_v1 <- function(data, date_data) {

  #### Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop('data must be a data.frame')
    }
  } else if (isFALSE(data_arg)) {
    stop('child data for REDCap event child_visit_1_arm_1 must be entered as a data.frame')
  }

  # update name of participant ID column
  names(data)[names(data) == 'record_id'] <- 'participant_id'

  data <- merge(data, date_data[c('participant_id', 'v1_age')], by = 'participant_id', all.x = TRUE)

  # add visit number
  data['visit_protocol'] <- 1

  names(data)[names(data) == 'date'] <- 'visit_date'
  data['visit_date'] <- lubridate::as_date(data[['visit_date']])

  ## visit data ####
  visit_data_child <- data[grepl('_id|notes|^visit|preload_condition', names(data))]

  # fix names
  names(visit_data_child)[names(visit_data_child) == 'v1_checklist_notes'] <- 'v1_pre_notes'
  names(visit_data_child)[names(visit_data_child) == 'v1_checklist_notes_post'] <- 'v1_notes'

  names(visit_data_child)[names(visit_data_child) == 'preload_notes'] <- 'preload_prep_notes'
  names(visit_data_child)[names(visit_data_child) == 'preload_finishnotes'] <- 'preload_notes'

  names(visit_data_child)[names(visit_data_child) == 'broccoli_post_notes1'] <- 'postmeal_broccoli_notes'
  names(visit_data_child)[names(visit_data_child) == 'water_post_notes1'] <- 'postmeal_water_notes'


  # reorder
  visit_data_child <- visit_data_child[c('participant_id', 'visit_protocol', 'visit_date', names(visit_data_child)[grepl('^v1', names(visit_data_child))], names(visit_data_child)[grepl('preload', names(visit_data_child))], names(visit_data_child)[grepl('meal', names(visit_data_child))], names(visit_data_child)[grepl('child|rsa_notes$', names(visit_data_child))])]


  ## food paradigm information (does not include intake and freddy values) ####
  food_paradigm_info <- data[grepl('_id|meal|preload_condition|^visit', names(data))]

  # remove extra columns and re-order
  food_paradigm_info <- food_paradigm_info[!grepl('rsa|fullness', names(food_paradigm_info))]

  food_paradigm_info <- food_paradigm_info[c('participant_id', 'visit_protocol', 'visit_date', 'preload_condition', names(food_paradigm_info)[grepl('meal', names(food_paradigm_info))])]

 food_paradigm_json <- json_food_paradigm()

  ## intake_data -- this data can be used for prelim analyses, but eventually will be replaced with double entry data
  intake_data <- data[grepl('_id|preload_condition|container|preweight|bowl|servings|postweight|^visit', names(data))]

  # clean and process intake data
  intake_data <- util_format_intake_data(intake_data)


  intake_data <- intake_data[c('participant_id', 'visit_protocol', 'visit_date', names(intake_data)[grepl('preload', names(intake_data))], names(intake_data)[grepl('mac', names(intake_data))], names(intake_data)[grepl('broccoli', names(intake_data))], names(intake_data)[grepl('grapes', names(intake_data))],names(intake_data)[grepl('carrots', names(intake_data))], names(intake_data)[grepl('graham', names(intake_data))], names(intake_data)[grepl('water', names(intake_data))])]

  intake_json <- json_intake()

  ## freddy data (NO double entry data) ####
  fullness_data <- data[grepl('_id|preload_condition|fullness|^visit', names(data))]

  fullness_data <- fullness_data[c('participant_id', 'visit_protocol', 'visit_date', 'preload_condition', names(fullness_data)[grepl('fullness', names(fullness_data))])]

  fullness_json <- json_fullness()

  ## vas food liking (eah and meal foods) ####
  liking_data <- data[grepl('_id|preload_condition|liking|^visit', names(data))]

  # remove extra columns and re-order
  liking_data <- liking_data[c('participant_id', 'visit_protocol', 'visit_date', 'preload_condition', names(liking_data)[grepl('liking', names(liking_data))])]

  liking_json <- json_liking()

  ## anthro data -- this data can be used for prelim analyses, but eventually will be replaced with double entry data ####
  anthro_data <- data[grepl('_id|height|weight|^visit|parent_1|v1_age|sex', names(data))]

  # remove extra columns and re-order
  anthro_data <- anthro_data[!grepl('preweight|postweight|notes|meters', names(anthro_data))]

  anthro_data <- anthro_data[c('participant_id', 'visit_protocol', 'visit_date', 'v1_age', 'sex', 'parent_1', names(anthro_data)[grepl('height|weight', names(anthro_data))])]

  # rename columns
  names(anthro_data) <- gsub('parent_', 'parent1_', names(anthro_data))
  names(anthro_data)[names(anthro_data) == 'parent1_1'] <- 'parent_relationship'

  names(anthro_data) <- gsub('height_mean', 'height_mean_cm', names(anthro_data))
  names(anthro_data) <- gsub('weight_mean', 'weight_mean_kg', names(anthro_data))

  names(anthro_data) <- gsub('height1', 'height1_cm', names(anthro_data))
  names(anthro_data) <- gsub('height2', 'height2_cm', names(anthro_data))
  names(anthro_data) <- gsub('weight1', 'weight1_kg', names(anthro_data))
  names(anthro_data) <- gsub('weight2', 'weight12_kg', names(anthro_data))

  # compute bmi variables
  anthro_data['child_bmi'] <- round(anthro_data['child_weight_mean_kg'] /((anthro_data['child_height_mean_cm'] / 100) ^ 2), digits = 2)

  anthro_data['child_bmi_z'] <- round(childsds::sds(value = anthro_data[['child_bmi']], age = anthro_data[['v1_age']], sex = anthro_data[['sex']], item = 'bmi', ref = childsds::cdc.ref, type = 'SDS', male = 0, female = 1), digits = 2)

  anthro_data['child_bmi_p'] <- round((childsds::sds(value = anthro_data[['child_bmi']], age = anthro_data[['v1_age']], sex = anthro_data[['sex']], item = 'bmi', ref = childsds::cdc.ref, type = 'perc', male = 0, female =1)) * 100, digits = 2)

  anthro_data['parent1_bmi'] <- round(anthro_data['parent1_weight_mean_kg'] / ((anthro_data['parent1_height_mean_cm'] / 100) ^ 2), digits = 2)

  anthro_measured_json <- json_measured_anthro()

  ## rsa protocol data ####
  rsa_data <- data[grepl('_id|preload_condition|^visit|rsa', names(data))]

  # remove extra columns and re-order
  rsa_data <- rsa_data[!grepl('rsa_removal', names(rsa_data))]

  rsa_data <- rsa_data[c('participant_id', 'visit_protocol', 'visit_date', 'preload_condition', names(rsa_data)[grepl('rsa', names(rsa_data))])]

  # rename columns
  rsa_data['rsa_baseline_start'] <- format(rsa_data[['rsa_baseline_start']], "%H:%M:%S")
  rsa_data['rsa_baseline_end'] <- format(rsa_data[['rsa_baseline_end']], "%H:%M:%S")
  rsa_data['rsa_preload_start'] <- format(rsa_data[['rsa_preload_start']], "%H:%M:%S")
  rsa_data['rsa_preload_end'] <- format(rsa_data[['rsa_preload_end']], "%H:%M:%S")
  rsa_data['rsa_meal_start'] <- format(rsa_data[['rsa_meal_start']], "%H:%M:%S")
  rsa_data['rsa_meal_end'] <- format(rsa_data[['rsa_meal_end']], "%H:%M:%S")
  rsa_data['rsa_postmeal_end'] <- format(rsa_data[['rsa_postmeal_end']], "%H:%M:%S")
  rsa_data['rsa_blanktime_1'] <- format(rsa_data[['rsa_blanktime_1']], "%H:%M:%S")
  rsa_data['rsa_blanktime_2'] <- format(rsa_data[['rsa_blanktime_2']], "%H:%M:%S")

  names(rsa_data) <- gsub('blanktime_', 'blanktime', names(rsa_data))

  rsa_visit_json <- json_rsa_visit()

  ## pst protocol data ####
  pst_data <- data[grepl('_id|^visit|pst', names(data))]

  # remove extra columns and re-order
  pst_data <- pst_data[c('participant_id', 'visit_protocol', 'visit_date', names(pst_data)[grepl('pst', names(pst_data))])]

  # rename
  names(pst_data)[names(pst_data) == 'pre_pst_hunger'] <- 'pst_pre_fullness'
  names(pst_data) <- gsub('match_', 'match', names(pst_data))
  names(pst_data) <- gsub('sorting_', 'sort', names(pst_data))

  # pst_match1_time <- paste0(sprintf('%02d', lubridate::hour(pst_data[['pst_match1_time']])), ':', sprintf('%02d', lubridate::minute(pst_data[['pst_match1_time']])))
  # pst_match1_time <- ifelse(pst_match1_time == 'NA:NA', NA, pst_match1_time)

  # pst_data['pst_match1_time'] <- pst_match1_time

  # pst_match2_time <- paste0(sprintf('%02d', lubridate::hour(pst_data[['pst_match2_time']])), ':', sprintf('%02d', lubridate::minute(pst_data[['pst_match2_time']])))
  # pst_match2_time <- ifelse(pst_match2_time == 'NA:NA', NA, pst_match1_time)

  # pst_data['pst_match2_time'] <- pst_match2_time

  pst_json <- json_pst()

  ## return data ####
  return(list(visit_data_child = visit_data_child,
              food_paradigm_info = list(data = food_paradigm_info, meta = food_paradigm_json),
              intake_data = list(data = intake_data, meta = intake_json),
              fullness_data = list(data = fullness_data, meta = fullness_json),
              liking_data = list(data = liking_data, meta = liking_json),
              anthro_data = list(data = anthro_data, meta = anthro_measured_json),
              rsa_data = list(data = rsa_data, meta = rsa_visit_json),
              pst_data = list(data = pst_data, meta = pst_json)))

}

