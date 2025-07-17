#' util_merged_demo: merges and formats demographic data
#'
#' This function merges demographic data across visits and formats/calculates necessary values
#'
#'
#' @param visit1_demo visit 1 demo data.frame
#' @param household_all merged household data.frame
#' @param merged_anthro merged anthro data.fram
#' @param date_data data.frame with all visit date information
#'
#' @examples
#'
#' # process data
#' merge_demo_data <- util_merged_demo(parent_v1_data$demo_data$data, household_all, merged_anthro, date_data)
#'
#' @seealso [proc_redcap()], [util_merge_anthro()], , [util_merge_questionnaires()]
#'
#' @export
#'


util_merged_demo <- function(visit1_demo, household_all, merged_anthro, date_data) {

  # combine demo data from demo_data and household form
  demo_data <- merge(visit1_demo, household_all[c('session_id', 'participant_id', 'demo_education_mom', 'demo_income')], by = 'participant_id', all = TRUE)

  # add dates and ages at start of sessions (V1 and V5) from date_data form
  demo_data <- merge(demo_data, date_data[c('participant_id', 'v1_date', 'v5_date', 'v1_age', 'v5_age', 'sex')], by = 'participant_id', all.x = TRUE)

  demo_data['date_session_start'] <- ifelse(demo_data[['session_id']] == 'ses-1', demo_data[['v1_date']], demo_data[['v5_date']])

  demo_data['age'] <- ifelse(demo_data[['session_id']] == 'ses-1', demo_data[['v1_age']], demo_data[['v5_age']])

  demo_data <- demo_data[!grepl('v1_|v5', names(demo_data))]

  # add key anthro data
  demo_data <- merge(demo_data, merged_anthro[c('participant_id', 'session_id', 'child_bmi', 'child_bmi_p', 'child_bmi_z', 'maternal_bmi', 'maternal_anthro_method')], by=c('participant_id', 'session_id'), all = TRUE)

  # add risk status - compute based on ses-1 maternal_bmi
  ses1_dat <- demo_data[demo_data['session_id'] == 'ses-1', ]

  # calculate risk based on maternal bmi
  ses1_dat$risk_status <- ifelse(ses1_dat$maternal_bmi <= 26, 'low-risk', ifelse(ses1_dat$maternal_bmi >= 29, 'high-risk', NA))

  # calculate child bmi status
  ses1_dat$child_bmi_criteria <- ifelse(is.na(ses1_dat$child_bmi_p), NA, ifelse(ses1_dat$child_bmi_p < 95, 1,0))

  # merge 'risk_status' variable into demo_data
  demo_data <- merge(demo_data, ses1_dat[, c('participant_id', 'risk_status', 'child_bmi_criteria')], by = 'participant_id', all = TRUE)

  # rename columns
  names(demo_data) <- gsub('demo_', '', names(demo_data))
  names(demo_data)[names(demo_data) == 'demo_race'] <- 'race'

  # reorder columns
  demo_data <- demo_data[c('participant_id', 'session_id', 'risk_status', 'child_bmi_criteria', 'date_session_start', 'sex', 'age', 'ethnicity', 'race', 'income', 'education_mom', 'child_bmi', 'child_bmi_p', 'child_bmi_z', 'maternal_bmi', 'maternal_anthro_method')]

  # return data
  return(demo_data)

}
