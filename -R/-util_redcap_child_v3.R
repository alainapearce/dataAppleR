#' util_redcap_child_v3: Organize child visits 3 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_3_arm_1
#'
#' @param data data from REDCap event child_visit_3_arm_1
#'
#' @return Will return a list including:
#' \itemize{
#'  \item{clean raw child visit 3 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{visit_data_child}
#'    \item{food_paradigm_info}
#'    \item{freddy_data}
#'    \item{intake_data}
#'    \item{eah_wanting}
#'    \item{sleeplog_data}
#'  }
#' @examples
#'
#' # process REDCap data
#' child_visit3_list <- util_redcap_child_v3(data)
#'
#' \dontrun{
#' }
#'
#' @seealso [proc_redcap()]
#'
#' @export

util_redcap_child_v3 <- function(data) {

  #### Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop('data must be a data.frame')
    }
  } else if (isFALSE(data_arg)) {
    stop('child data for REDCap event child_visit_3_arm_1 must be entered as a data.frame')
  }

  # update name of participant ID column
  names(data)[names(data) == 'record_id'] <- 'participant_id'

  # add session column
  data['session_id'] <- 'ses-1'

  # add visit
  data['visit_protocol'] <- 3

  # update date
  names(data)[names(data) == 'v3_date'] <- 'visit_date'
  data['visit_date'] <- lubridate::as_date(data[['visit_date']])

  #reduce columns and update names

  ## visit data ####
  visit_data_child <- data[grepl('_id|notes|^visit', names(data))]

  # remove extra columns and re-order
  visit_data_child <- visit_data_child[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(visit_data_child)[grepl('notes', names(visit_data_child))])]

  names(visit_data_child)[names(visit_data_child) == 'v3_post_check_notes'] <- 'v3_notes'
  names(visit_data_child)[names(visit_data_child) == 'v4_pre_check_notes'] <- 'v3_pre_notes'

  names(visit_data_child)[!grepl('_id|v3|^visit', names(visit_data_child))] <- paste0('v3_',  names(visit_data_child)[!grepl('_id|v3|^visit', names(visit_data_child))])

  ## food paradigm data ####

  # food paradigm information (does not include intake and freddy values)
  food_paradigm_info <- data[grepl('_id|meal|advertisement_condition|eah|^visit', names(data))]

  # remove extra columns and re-order
  food_paradigm_info <- food_paradigm_info[!grepl('complete|freddy|wanting|consumed|intake|water', names(food_paradigm_info))]

  food_paradigm_info <- food_paradigm_info[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'advertisement_condition', names(food_paradigm_info)[grepl('meal|eah', names(food_paradigm_info))])]

  names(food_paradigm_info) <- gsub('intake_notes', 'prep_notes', names(food_paradigm_info))

  food_paradigm_json <- json_v3v4v5_food_paradigm()

  # eah wanting ####
  eah_wanting <- data[grepl('_id|advertisement_condition|wanting|^visit', names(data))]

  # remove extra columns and re-order
  eah_wanting <- eah_wanting[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'advertisement_condition', names(eah_wanting)[grepl('wanting', names(eah_wanting))])]

  eah_wanting_json <- json_eah_wanting()

  ## intake_data -- this data can be used for prelim analyses, but eventually will be replaced with double entry data ####
  intake_data <- data[grepl('_id|plate|advertisement_condition|^visit', names(data))]

  # remove extra columns and re-order
  intake_data <- intake_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'advertisement_condition', names(intake_data)[grepl('plate', names(intake_data))])]

  intake_json <- json_v3v4v5_intake()

  ## freddy data ####
  freddy_data <- data[grepl('_id|freddy|advertisement_condition|^visit', names(data))]

  # Replace 'freddy' with 'fullness'
  names(freddy_data) <- gsub('freddy', 'fullness', names(freddy_data))

  # remove extra columns and re-order
  freddy_data <- freddy_data[, !grepl('check|visit_number', names(freddy_data))]

  freddy_data <- freddy_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'advertisement_condition', names(freddy_data)[grepl('fullness', names(freddy_data))])]

  names(freddy_data) <- gsub('vas', 'liking', names(freddy_data))

  freddy_json <- json_v3v4v5_freddy()

  ## sleep log ####
  sleeplog_data <- data[grepl('_id|date|bedtime|asleep|attempt|times|waso|awake|out_on|rating|comment|visit_date', names(data))]

  # remove extra columns and re-order
  sleeplog_data <- sleeplog_data[c('participant_id', 'session_id', 'visit_date', names(sleeplog_data)[grepl('date|bedtime|asleep|attempt|times|waso|awake|out_on|rating|comment', names(sleeplog_data))])]

  sleeplog_data <- util_format_sleep_data(sleeplog_data)


  sleeplog_json <- json_sleeplog()


  ## return data ####
  return(list(visit_data_child = visit_data_child,
              food_paradigm_info = list(data = food_paradigm_info, meta = food_paradigm_json),
              eah_wanting = list(data = eah_wanting, meta = eah_wanting_json),
              freddy_data = list(data = freddy_data, meta = freddy_json),
              intake_data = list(data = intake_data, meta = intake_json),
              sleeplog_data = list(data = sleeplog_data, meta = sleeplog_json)))
}

