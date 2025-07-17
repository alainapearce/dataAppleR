#' util_redcap_child_v4: Organize child visit 4 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_4_arm_1
#'
#' @param data data from REDCap event child_visit_4_arm_1
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
#'    \item{eah_wanting}
#'    \item{freddy_data}
#'    \item{intake_data}
#'    \item{loc_data}
#'    \item{pptq_data}
#'    \item{sic_data}
#'  }
#'
#' @examples
#'
#' # process REDCap data
#' child_visit4_list <- util_redcap_child_v4(data)
#'
#' \dontrun{
#' }
#'
#' @seealso [proc_redcap()]
#'
#' @export

util_redcap_child_v4 <- function(data) {

  #### Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop('data must be a data.frame')
    }
  } else if (isFALSE(data_arg)) {
    stop('child data for REDCap event child_visit_4_arm_1 must be entered as a data.frame')
  }

  # update name of participant ID column
  names(data)[names(data) == 'record_id'] <- 'participant_id'

  # add session column
  data$session_id <- 'ses-1'

  # add visit
  data['visit_protocol'] <- 4

  # update date
  names(data)[names(data) == 'v4_date'] <- 'visit_date'
  data['visit_date'] <- lubridate::as_date(data[['visit_date']])

  #reduce columns and update names

  ## visit data ####
  visit_data_child <- data[grepl('_id|notes|^visit', names(data))]

  # remove extra columns and re-order
  visit_data_child <- visit_data_child[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(visit_data_child)[grepl('notes', names(visit_data_child))])]

  names(visit_data_child)[names(visit_data_child) == 'v4_post_check_notes'] <- 'v4_notes'
  names(visit_data_child)[names(visit_data_child) == 'v5_pre_check_notes'] <- 'v4_pre_notes'

  names(visit_data_child)[!grepl('_id|v4|^visit', names(visit_data_child))] <- paste0('v4_',  names(visit_data_child)[!grepl('_id|v4|^visit', names(visit_data_child))])

  ## intake-related data ####

  # food paradigm information (does not include fullness, intake)
  food_paradigm_info <- data[grepl('_id|meal|advertisement_condition|eah_notes|eah_intake_notes|^visit', names(data))]

  # remove extra columns and re-order
  food_paradigm_info <- food_paradigm_info[!grepl('freddy|consumed', names(food_paradigm_info))]

  food_paradigm_info <- food_paradigm_info[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'advertisement_condition', names(food_paradigm_info)[grepl('meal|eah', names(food_paradigm_info))])]

  # fix names
  names(food_paradigm_info) <- gsub('intake_notes', 'prep_notes', names(food_paradigm_info))

  food_paradigm_json <- json_v3v4v5_food_paradigm()

  # eah wanting
  eah_wanting <- data[grepl('_id|wanting|advertisement_condition|^visit', names(data))]

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

  ## loc ####
  loc_data <-data[grepl('_id|loc|visit_date', names(data))]

  # remove extra columns and re-order
  loc_data <- loc_data[!grepl('share_info_parent', names(loc_data))]

  loc_data <- loc_data[c('participant_id', 'session_id', 'visit_date', names(loc_data)[grepl('loc', names(loc_data))])]

  # add to match with v5 version
  loc_data['loc_16b'] <- NA

  loc_json <- json_loc()

  ## pptq ####
  pptq_data <-data[grepl('_id|pptq|visit_date', names(data))]

  # remove extra columns and re-order
  pptq_data <- pptq_data[c('participant_id', 'session_id', 'visit_date', names(pptq_data)[grepl('pptq', names(pptq_data))])]

  # re-format and get proper question values
  pptq_data <- util_format_pptq_data(pptq_data)

  pptq_data_red <- pptq_data[names(pptq_data)[!grepl('left|mid|right', names(pptq_data))]]

  pptq_scored <- dataprepr::score_pptq(pptq_data_red, pptq_scale = 3, base_zero = FALSE, id = 'participant_id', extra_scale_cols = c('visit_date'))

  pptq_json <- json_pptq()

  ## sic ####
  sic_data <-data[grepl('_id|sic|visit_date', names(data))]

  # remove extra columns and re-order
  sic_data <- sic_data[c('participant_id', 'session_id', 'visit_date', names(sic_data)[grepl('sic', names(sic_data))])]

  # score?

  ## return data ####
  return(list(visit_data_child = visit_data_child,
              food_paradigm_info = list(data = food_paradigm_info, meta = food_paradigm_json),
              eah_wanting = list(data = eah_wanting, meta = eah_wanting_json),
              freddy_data = list(data = freddy_data, meta = freddy_json),
              intake_data = list(data = intake_data, meta = intake_json),
              loc_data = list(data = loc_data, meta = loc_json),
              pptq_data = list(data = pptq_scored, meta = pptq_json),
              sic_data = list(data = sic_data, meta = NA)))
}

