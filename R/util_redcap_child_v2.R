#' util_redcap_child_v2: Organize child visit 2 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_2_arm_1
#'
#' @param data data from REDCap event child_visit_2_arm_1
#'
#' @return Will return a list including:
#' \itemize{
#'  \item{clean raw child visit 2 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{visit_data_child}
#'    \item{food_paradigm_info}
#'    \item{fullness_data}
#'    \item{liking_data}
#'    \item{intake_data}
#'    \item{rsa_data}
#'  }
#' @examples
#'
#' # process REDCap data
#' child_visit2_list <- util_redcap_child_v2(data)
#'
#' \dontrun{
#' }
#'
#' @seealso [proc_redcap()]
#'
#' @export


util_redcap_child_v2 <- function(data) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    }
  } else if (isFALSE(data_arg)) {
    stop("child data for REDCap event child_visit_2_arm_1 must be entered as a data.frame")
  }

  # update name of participant ID column
  names(data)[names(data) == 'record_id'] <- 'participant_id'

  # add visit number
  data['visit_protocol'] <- 2

  names(data)[names(data) == 'date'] <- 'visit_date'
  data['visit_date'] <- lubridate::as_date(data[['visit_date']])

  ## visit data ####
  visit_data_child <- data[grepl('_id|notes|^visit|preload_condition', names(data))]

  # fix names
  names(visit_data_child)[names(visit_data_child) == 'v2_checklist_notes'] <- 'v2_pre_notes'
  names(visit_data_child)[names(visit_data_child) == 'v2_checklist_notes_post'] <- 'v2_notes'

  names(visit_data_child)[names(visit_data_child) == 'preload_notes'] <- 'preload_prep_notes'
  names(visit_data_child)[names(visit_data_child) == 'preload_finishnotes'] <- 'preload_notes'


  # reorder
  visit_data_child <- visit_data_child[c('participant_id', 'visit_protocol', 'visit_date', names(visit_data_child)[grepl('^v2', names(visit_data_child))], names(visit_data_child)[grepl('preload', names(visit_data_child))], names(visit_data_child)[grepl('meal', names(visit_data_child))], names(visit_data_child)[grepl('child|rsa_notes$', names(visit_data_child))])]


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
  rsa_data['rsa_blanktime_3'] <- format(rsa_data[['rsa_blanktime_3']], "%H:%M:%S")
  rsa_data['rsa_blanktime_4'] <- format(rsa_data[['rsa_blanktime_4']], "%H:%M:%S")

  names(rsa_data) <- gsub('blanktime_', 'blanktime', names(rsa_data))

  rsa_visit_json <- json_rsa_visit()

  ## return data ####
  return(list(visit_data_child = visit_data_child,
              food_paradigm_info = list(data = food_paradigm_info, meta = food_paradigm_json),
              intake_data = list(data = intake_data, meta = intake_json),
              fullness_data = list(data = fullness_data, meta = fullness_json),
              liking_data = list(data = liking_data, meta = liking_json),
              rsa_data = list(data = rsa_data, meta = rsa_visit_json)))
}

