#' util_redcap_child_v1: Organize child visit 1 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_1_arm_1
#'
#' @param data data from REDCap event child_visit_1_arm_1
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
#'    \item{freddy_data}
#'    \item{intake_data}
#'    \item{liking_data}
#'    \item{kbas_data}
#'    \item{stq_data}
#'    \item{anthro_data}
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

util_redcap_child_v1 <- function(data) {

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
  freddy_data <- data[grepl('_id|preload_condition|fullness|^visit', names(data))]

  freddy_data <- freddy_data[c('participant_id', 'visit_protocol', 'visit_date', 'preload_condition', names(freddy_data)[grepl('fullness', names(freddy_data))])]

  fullness_json <- json_fullness()

  ## vas food liking (eah and meal foods) ####
  liking_data <- data[grepl('_id|preload_condition|liking|^visit', names(data))]

  # remove extra columns and re-order
  liking_data <- liking_data[c('participant_id', 'visit_protocol', 'visit_date', 'preload_condition', names(liking_data)[grepl('liking', names(liking_data))])]

  liking_json <- json_liking()

  ## anthro data -- this data can be used for prelim analyses, but eventually will be replaced with double entry data ####
  anthro_data <- data[grepl('_id|height|weight|^visit', names(data))]

  # remove extra columns and re-order
  anthro_data <- anthro_data[!grepl('preweight|postweight|notes|meters', names(anthro_data))]

  anthro_data <- anthro_data[c('participant_id', 'visit_protocol', 'visit_date', names(anthro_data)[grepl('height|weight', names(anthro_data))])]

  # rename columns
  names(anthro_data) <- gsub('parent_', 'parent1_', names(anthro_data))

  names(anthro_data) <- gsub('child_average_weight', 'child_weight_average', names(anthro_data))

  # re-label parent1 sex
  anthro_data$parent1_sex <- ifelse(anthro_data$parent1_sex == 0, 'female', ifelse(anthro_data$parent1_sex == 1, 'male', NA))

  anthro_json <- json_anthro()

  ## return data ####
  return(list(visit_data_child = visit_data_child,
              food_paradigm_info = list(data = food_paradigm_info, meta = food_paradigm_json),
              intake_data = list(data = intake_data, meta = v1_intake_json),
              freddy_data = list(data = freddy_data, meta = freddy_json),
              liking_data = list(data = liking_data, meta = liking_json),
              kbas_data = list(data = kbas_data, meta = kbas_json),
              stq_data = list(data = stq_data, meta = NA),
              anthro_data = list(data = anthro_data, meta = anthro_json)))

}

