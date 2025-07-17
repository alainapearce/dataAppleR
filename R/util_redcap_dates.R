#' util_redcap_dates: Organize child date data from REDCap across all visits (called within proc_redcap.R)
#'
#' This function organizes date data from REDCap across visits and events
#'
#' @param child_v1 data from REDCap child visit 1 event
#' @param child_v2 data from REDCap child visit 2 event
#' @param child_v3 data from REDCap child visit 3 event
#' @param child_v4 data from REDCap child visit 4 event
#' @param child_v5 data from REDCap child visit 5 event
#' @param parent_v1 data from REDCap parent visit 1 event
#'
#' @export



util_redcap_dates <- function(child_v1, child_v2, child_v3, child_v4, child_v5, parent_v1) {

  # fix date variables
  names(child_v1)[grepl('date', names(child_v1))] <- 'v1_date'
  names(child_v2)[grepl('date', names(child_v2))] <- 'v2_date'
  names(child_v3)[grepl('date', names(child_v3))] <- 'v3_date'
  names(child_v4)[grepl('date', names(child_v4))] <- 'v4_date'
  names(child_v5)[grepl('date', names(child_v5))] <- 'v5_date'


  # merge necessary data
  date_data <- merge(child_v1[, c('record_id', 'v1_date', 'sex')], child_v2[, c('record_id', 'v2_date')], by = 'record_id', all = TRUE)
  date_data <- merge(date_data, child_v3[, c('record_id', 'v3_date')], by = 'record_id', all = TRUE)
  date_data <- merge(date_data, child_v4[, c('record_id', 'v4_date')], by = 'record_id', all = TRUE)
  date_data <- merge(date_data, child_v5[, c('record_id', 'v5_date')], by = 'record_id', all = TRUE)

  # add child sex and demo_child_dob to date_data
  date_data <- merge(date_data, parent_v1[, c('record_id', 'demo_child_dob')], by = 'record_id', all = TRUE)


  # conver to dates
  date_data[['v1_date']] <- lubridate::as_date(date_data[['v1_date']])
  date_data[['v2_date']] <- lubridate::as_date(date_data[['v2_date']])
  date_data[['v3_date']] <- lubridate::as_date(date_data[['v3_date']])
  date_data[['v4_date']] <- lubridate::as_date(date_data[['v4_date']])
  date_data[['brief_date']] <- date_data[['v2_date']]
  date_data[['demo_child_dob']] <- lubridate::as_date(date_data[['demo_child_dob']])

  # add ages
  date_data[['v1_age']] <- round(lubridate::interval(date_data[['demo_child_dob']], date_data[['v1_date']])/lubridate::years(1), 1)
  date_data[['v2_age']] <- round(lubridate::interval(date_data[['demo_child_dob']], date_data[['v2_date']])/lubridate::years(1), 1)
  date_data[['v3_age']] <- round(lubridate::interval(date_data[['demo_child_dob']], date_data[['v3_date']])/lubridate::years(1), 1)
  date_data[['v4_age']] <- round(lubridate::interval(date_data[['demo_child_dob']], date_data[['v4_date']])/lubridate::years(1), 1)
  date_data[['brief_age']] <- round(lubridate::interval(date_data[['demo_child_dob']], date_data[['brief_date']])/lubridate::years(1),1)

  # re-label sex var and save to sex
  date_data['sex'] <- ifelse(date_data[['sex']] == 0, 'male', ifelse(date_data[['sex']] == 1, 'female', NA))

  # update column names in date_data
  names(date_data)[names(date_data) == 'record_id'] <- 'participant_id'

  # fix order
  date_data <- date_data[c('participant_id', 'sex', 'demo_child_dob', names(date_data)[grepl('date', names(date_data))], names(date_data)[grepl('age', names(date_data))])]

  #return
  return(date_data)
}
