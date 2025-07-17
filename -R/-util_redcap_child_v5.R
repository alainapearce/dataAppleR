#' util_redcap_child_v5: Organize child visit 5 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_5_arm_1
#'
#' @param data data from REDCap event child_visit_5_arm_1
#'
#' @return Will return a list including:
#' \itemize{
#'  \item{clean raw child visit 5 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{visit_data_child}
#'    \item{food_paradigm_info}
#'    \item{eah_wanting}
#'    \item{freddy_data}
#'    \item{intake_data}
#'    \item{liking_data}
#'    \item{hrt_data}
#'    \item{puberty_data}
#'    \item{loc_data}
#'    \item{kbas_data}
#'    \item{stq_data}
#'    \item{tictoc_data}
#'    \item{anthro_data}
#'  }
#'
#' @examples
#'
#' # process REDCap data
#' child_visit5_list <- util_redcap_child_v5(data)
#'
#' \dontrun{
#' }
#'
#' @seealso [proc_redcap()]
#'
#' @export

util_redcap_child_v5 <- function(data) {

  #### Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop('data must be a data.frame')
    }
  } else if (isFALSE(data_arg)) {
    stop('child data for REDCap event child_visit_5_arm_1 must be entered as a data.frame')
  }

  # update name of participant ID column
  names(data)[names(data) == 'record_id'] <- 'participant_id'

  # add session column
  data$session_id <- 'ses-2'

  # add visit
  data['visit_protocol'] <- 5

  # update date
  names(data)[names(data) == 'v5_date'] <- 'visit_date'
  data['visit_date'] <- lubridate::as_date(data[['visit_date']])

  #reduce columns and update names

  ## visit data ####
  visit_data_child <- data[grepl('_id|notes|^visit', names(data))]

  # remove extra columns and re-order
  visit_data_child <- visit_data_child[, !grepl('dxa_id', names(visit_data_child))]

  visit_data_child <- visit_data_child[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(visit_data_child)[grepl('notes', names(visit_data_child))])]

  names(visit_data_child)[names(visit_data_child) == 'v5_post_check_notes'] <- 'v5_notes'
  names(visit_data_child)[names(visit_data_child) == 'v6_pre_check_notes'] <- 'v5_pre_notes'

  names(visit_data_child)[names(visit_data_child) == 'toolbox_age_8_flanker_notes'] <- 'flanker_8yr_notes'
  names(visit_data_child)[names(visit_data_child) == 'toolbox_age_8_dccs_notes'] <- 'dccs_8yr_notes'
  names(visit_data_child)[names(visit_data_child) == 'toolbox_list_sorting_notes'] <- 'listsort_notes'

  names(visit_data_child)[!grepl('_id|v5|^visit', names(visit_data_child))] <- paste0('v5_',  names(visit_data_child)[!grepl('_id|v5|^visit', names(visit_data_child))])

  ## intake-related data ####

  # food paradigm information (does not include fullness, intake)
  food_paradigm_info <- data[, grepl('_id|meal|advertisement_condition|eah_notes|eah_intake_notes|^visit', names(data))]

  # remove extra columns and re-order
  food_paradigm_info <- food_paradigm_info[, !grepl('freddy|consumed|dxa_id', names(food_paradigm_info))]

  food_paradigm_info <- food_paradigm_info[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'advertisement_condition', names(food_paradigm_info)[grepl('meal|eah', names(food_paradigm_info))])]

  # fix variable names
  names(food_paradigm_info) <- gsub('intake_notes', 'prep_notes', names(food_paradigm_info))

  food_paradigm_json <- json_v3v4v5_food_paradigm()


  # eah wanting
  eah_wanting <- data[, grepl('_id|wanting|advertisement_condition|^visit', names(data))]

  # remove extra columns and re-order
  eah_wanting <- eah_wanting[, !grepl('dxa_id', names(eah_wanting))]

  eah_wanting <- eah_wanting[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'advertisement_condition', names(eah_wanting)[grepl('wanting', names(eah_wanting))])]

  eah_wanting_json <- json_eah_wanting()

  ## intake_data -- this data can be used for prelim analyses, but eventually will be replaced with double entry data ####
  intake_data <- data[, grepl('_id|plate|advertisement_condition|^visit', names(data))]

  # remove extra columns and re-order
  intake_data <- intake_data[, !grepl('dxa_id', names(intake_data))]

  intake_data <- intake_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'advertisement_condition', names(intake_data)[grepl('plate', names(intake_data))])]

  intake_json <- json_v3v4v5_intake()

  ## freddy data -- this may or may not be replaced with double entry data ####
  freddy_data <- data[, grepl('_id|freddy|advertisement_condition|^visit', names(data))]

  # Replace 'freddy' with 'fullness'
  names(freddy_data) <- gsub('freddy', 'fullness', names(freddy_data))

  # remove extra columns and re-order
  freddy_data <- freddy_data[, !grepl('dxa_id|check|visit_number', names(freddy_data))]

  freddy_data <- freddy_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'advertisement_condition', names(freddy_data)[grepl('fullness', names(freddy_data))])]

  names(freddy_data) <- gsub('vas', 'liking', names(freddy_data))

  freddy_json <- json_v3v4v5_freddy()

  ## vas food liking (eah and meal foods) ####
  liking_data <- data[, grepl('_id|vas|^visit', names(data))]

  # remove extra columns and re-order
  liking_data <- liking_data[, !grepl('dxa_id|pre_vas_freddy', names(liking_data))]

  liking_data <- liking_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(liking_data)[grepl('vas', names(liking_data))])]

  # fix variable names
  names(liking_data) <- gsub('cookie', 'oreo', names(liking_data))
  names(liking_data) <- gsub('vas', 'liking', names(liking_data))

  liking_json <- json_liking()

  ## hrt ####
  # need to figure out this part
  # hrt_data <- data[, grepl('participant_id|session_id|hrt|food_|^q.*score', names(data))]

  ## Puberty ####
  puberty_data <- data[, grepl('_id|tanner|childrep|visit_date', names(data))]

  # remove extra columns and re-order
  puberty_data <- puberty_data[, !grepl('dxa_id', names(puberty_data))]

  puberty_data <- puberty_data[c('participant_id', 'session_id', 'visit_date', names(puberty_data)[grepl('tanner|childrep', names(puberty_data))])]

  puberty_data <- util_format_puberty_data(puberty_data, respondent = 'child')

  # score
  puberty_scored <- dataprepr::score_pds(puberty_data, respondent = 'child', base_zero = FALSE, male = 'male', female = 'female', id = 'participant_id')

  puberty_json <- json_puberty()

  ## loc ####
  loc_data <-data[, grepl('_id|loc|visit_date', names(data))]

  # remove extra columns and re-order
  loc_data <- loc_data[, !grepl('dxa_id|share_info_parent', names(loc_data))]

  loc_data <- loc_data[c('participant_id', 'session_id', 'visit_date', names(loc_data)[grepl('loc', names(loc_data))])]

  loc_json <- json_loc()

  ## kbas data ####
  kbas_data <- data[, grepl('_id|toy_|food_|^q.*score|visit_date', names(data))]

  # remove extra columns and re-order
  kbas_data <- kbas_data[, !grepl('dxa_id', names(kbas_data))]

  kbas_data <- kbas_data[c('participant_id', 'session_id', 'visit_date', names(kbas_data)[grepl('toy_|food_|^q.*score', names(kbas_data))])]

  # process data
  kbas_data <- util_format_kbas_data(kbas_data)

  kbas_json <- json_kbas()

  ## stq data ####
  stq_data <-data[, grepl('_id|stq|visit_date', names(data))]

  # remove extra columns and re-order
  stq_data <- stq_data[, !grepl('dxa_id', names(stq_data))]

  stq_data <- stq_data[c('participant_id', 'session_id', 'visit_date', names(stq_data)[grepl('stq', names(stq_data))])]

  # score

  ## tictoc data ####
  tictoc_data <-data[, grepl('_id|tictoc|visit_date', names(data))]

  # remove extra columns and re-order
  tictoc_data <- tictoc_data[, !grepl('dxa_id', names(tictoc_data))]

  tictoc_data <- tictoc_data[c('participant_id', 'session_id', 'visit_date', names(tictoc_data)[grepl('stq', names(tictoc_data))])]

  ## anthro data -- this data can be used for prelim analyses, but eventually will be replaced with double entry data ####
  anthro_data <- data[, grepl('_id|height|weight|^visit', names(data))]

  # remove extra columns, add columns, and re-order
  anthro_data <- anthro_data[, !grepl('dxa_id|check|notes|puberty|cooked', names(anthro_data))]

  anthro_data <- anthro_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(anthro_data)[grepl('height|weight', names(anthro_data))])]

  # rename columns
  names(anthro_data) <- gsub('parent_', 'parent1_', names(anthro_data))
  names(anthro_data) <- gsub('parent1_height_sex', 'parent1_sex', names(anthro_data))
  names(anthro_data) <- gsub('child_average_weight', 'child_weight_average', names(anthro_data))

  # re-label parent1 sex
  anthro_data$parent1_sex <- ifelse(anthro_data$parent1_sex == 0, 'female', ifelse(anthro_data$parent1_sex == 1, 'male', NA))

  anthro_json <- json_anthro()

  ## return data ####
  return(list(visit_data_child = visit_data_child,
              food_paradigm_info = list(data = food_paradigm_info, meta = food_paradigm_json),
              eah_wanting = list(data = eah_wanting, meta = eah_wanting_json),
              liking_data = list(data = liking_data, meta = liking_json),
              freddy_data = list(data = freddy_data, meta = freddy_json),
              intake_data = list(data = intake_data, meta = intake_json),
              #hrt_data = list(data = hrt_data, meta = visit_data_json),
              puberty_data = list(data = puberty_scored, meta = puberty_json),
              loc_data = list(data = loc_data, meta = loc_json),
              kbas_data = list(data = kbas_data, meta = kbas_json),
              stq_data = list(data = stq_data, meta = NA),
              tictoc_data = list(data = tictoc_data, meta = NA),
              anthro_data = list(data = anthro_data, meta = anthro_json)))

}

