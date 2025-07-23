#' util_redcap_parent_v2: Organize parent visit 2 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_2_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_2_arm_1
#' @param date_data date data for REDCap visit
#'
#' Will return a list including:
#' \itemize{
#'  \item{clean raw and scored parent visit 2 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{cbq_data}
#'    \item{brief_data}
#'  }
#' @examples
#'
#' # process REDCap data
#' parent_visit2_list <- util_redcap_parent_v2(data, date_data)
#'
#' \dontrun{
#' }
#'
#' @seealso [proc_redcap()]
#'
#' @export

util_redcap_parent_v2 <- function(data, date_data) {

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

  # merge with date data for v2
  data <- merge(data, date_data[c('participant_id', 'v2_date', 'sex', 'v2_age')], by = 'participant_id', all.x = TRUE)
  names(data)[names(data) == 'v2_date'] <- 'visit_date'
  data['visit_date'] <- lubridate::as_date(data[['visit_date']])

  ## CBQ Data ####
  cbq_data <- data[, grepl('_id|cbq|visit_date', names(data))]

  # remove extra columns, add columns, and re-order
  cbq_data <- cbq_data[c('participant_id', 'visit_date', names(cbq_data)[grepl('cbq', names(cbq_data))])]

  cbq_scored <- dataprepr::score_cbq(cbq_data, base_zero = FALSE, id = 'participant_id', pna_value = 8)

  cbq_json <- json_cbq()

  ## brief Data ####
  brief_data <- data[, grepl('_id|brief2|cbeq|visit_date|age|sex', names(data))]

  # remove extra columns, add columns, and re-order
  brief_data <- brief_data[c('participant_id', 'visit_date', 'sex', 'v2_age', names(brief_data)[grepl('brief', names(brief_data))])]

  # fix nameing
  names(brief_data) <- gsub('brief2_', 'brief', names(brief_data))

  # limit to those of age that is valid for the BREIF2
  brief_data_age <- brief_data[brief_data[['v2_age']] >= 5 & !is.na(brief_data[['v2_age']]), ]

  # score subset
  brief_scored_age <- dataprepr::score_brief2(brief_data_age, age_var = 'v2_age', sex_var = 'sex', male = 'male', female = 'female', base_zero = TRUE, id = 'participant_id')

  # merge non-scored participants (too young) with scored phenotype data
  bids_phenotype <- rbind(data.table::setDT(brief_scored_age$bids_phenotype), data.table::setDT(brief_data[brief_data[['v2_age']] < 5, ]), fill = TRUE)

  bids_phenotype <- as.data.frame(bids_phenotype)

  # clean merged phenotype data and re-order
  bids_phenotype <- bids_phenotype[!is.na(bids_phenotype['participant_id']), ]
  bids_phenotype <- bids_phenotype[order(bids_phenotype[['participant_id']]), ]

  # make standard list with merged phenotype data
  brief_scored <- list(score_dat = brief_scored_age$score_dat, bids_phenotype = bids_phenotype)

  brief_json <- json_brief2()

  ## return data ####
  return(list(
    cbq_data = list(data = cbq_scored, meta = cbq_json),
    brief_data = list(data = brief_scored, meta = brief_json)))
}

