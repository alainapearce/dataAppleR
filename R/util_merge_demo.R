#' util_merged_demo: merges and formats demographic data
#'
#' This function merges demographic data across visits and formats/calculates necessary values
#'
#'
#' @param visit1_demo visit 1 demo data.frame
#' @param household_all merged household data.frame
#' @param anthro_data merged anthro data.fram
#' @param date_data data.frame with all visit date information
#'
#' @examples
#'
#' # process data
#' merge_demo_data <- util_merged_demo(parent_v1_data$demo_data$data, household_all, anthro_data, date_data)
#'
#' @seealso [proc_redcap()], [util_merge_anthro()], [util_merge_questionnaires()]
#'
#' @export
#'


util_merged_demo <- function(visit1_demo, household_all, anthro_data, date_data) {

  # combine demo data from demo_data and household form
  demo_data <- merge(visit1_demo, household_all[c('participant_id', 'demo_education_mom', 'demo_income')], by = 'participant_id', all = TRUE)

  # add dates and ages at start of sessions (V1 and V5) from date_data form
  demo_data <- merge(demo_data, date_data[c('participant_id', 'v1_date', 'v1_age', 'sex')], by = 'participant_id', all.x = TRUE)

  # add key anthro data
  demo_data <- merge(demo_data, anthro_data[!grepl('sex|v1_age',names(anthro_data))], by='participant_id', all = TRUE)

  # compute bmi variables
  demo_data['child_bmi'] <- round(demo_data['child_weight_mean_kg'] /((demo_data['child_height_mean_cm'] / 100) ^ 2), digits = 2)

  demo_data['child_bmi_z'] <- round(childsds::sds(value = demo_data[['child_bmi']], age = demo_data[['v1_age']], sex = demo_data[['sex']], item = 'bmi', ref = childsds::cdc.ref, type = 'SDS', male = 'male', female = 'female'), digits = 2)

  demo_data['child_bmi_p'] <- round((childsds::sds(value = demo_data[['child_bmi']], age = demo_data[['v1_age']], sex = demo_data[['sex']], item = 'bmi', ref = childsds::cdc.ref, type = 'perc', male = 'male', female = 'female')) * 100, digits = 2)

  demo_data['parent1_bmi'] <- round(demo_data['parent1_weight_mean_kg'] / ((demo_data['parent1_height_mean_cm'] / 100) ^ 2), digits = 2)

  # calculate child bmi status
  demo_data$child_bmi_criteria <- ifelse(is.na(demo_data$child_bmi_p), NA, ifelse(demo_data$child_bmi_p < 95, 1,0))

  # rename columns
  names(demo_data) <- gsub('demo_', '', names(demo_data))
  names(demo_data)[names(demo_data) == 'demo_race'] <- 'race'

  # reorder columns
  demo_data <- demo_data[c('participant_id', 'child_bmi_criteria', 'sex', 'v1_age', 'ethnicity', 'race', 'income', 'education_mom', 'child_bmi', 'child_bmi_p', 'child_bmi_z', 'parent1_bmi')]

  # return data
  return(demo_data)

}
