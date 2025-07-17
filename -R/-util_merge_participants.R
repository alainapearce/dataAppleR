#' util_merged_participants: merges and formats participants data for BIDS
#'
#' This function merges demographic data across visits and formats/calculates necessary values to make the participants.tsv BIDS file
#'
#'
#' @param visit1_demo visit 1 demo data.frame
#' @param merged_demo merged demo data.frame
#' @param date_data data.frame with all visit date information
#'
#' @examples
#'
#' # process data
#' merged_participants <- util_merged_participants(parent_v1_data$demo_data$data, merged_demo, date_data)
#'
#' @seealso [proc_redcap()], [util_merge_demo()]
#'
#' @export
#'


util_merged_participants <- function(visit1_demo, merged_demo, date_data) {

  # add merged_demo and date_data (visit dates, visit ages, child sex)
  participants_data <- merge(visit1_demo, date_data, by = 'participant_id', all = TRUE)

  # add risk status -- take from merged_demo
  participants_data <- merge(participants_data, merged_demo[merged_demo[['session_id']] == 'ses-1', c('participant_id', 'risk_status', 'ethnicity', 'race')], by = 'participant_id', all = TRUE)


  # remove birthday and other columns
  participants_data <- participants_data[!grepl('birthdate|brief', names(participants_data))]

  # rename columns
  names(participants_data) <- gsub('demo_', '', names(participants_data))

  names(participants_data)[names(participants_data) == 'v1_date'] <- 'child_protocol_1_date'
  names(participants_data)[names(participants_data) == 'v2_date'] <- 'child_protocol_2_date'
  names(participants_data)[names(participants_data) == 'v3_date'] <- 'child_protocol_3_date'
  names(participants_data)[names(participants_data) == 'v4_date'] <- 'child_protocol_4_date'
  names(participants_data)[names(participants_data) == 'v5_date'] <- 'child_protocol_5_date'
  names(participants_data)[names(participants_data) == 'v1_age'] <- 'child_protocol_1_age'
  names(participants_data)[names(participants_data) == 'v2_age'] <- 'child_protocol_2_age'
  names(participants_data)[names(participants_data) == 'v3_age'] <- 'child_protocol_3_age'
  names(participants_data)[names(participants_data) == 'v4_age'] <- 'child_protocol_4_age'
  names(participants_data)[names(participants_data) == 'v5_age'] <- 'child_protocol_5_age'

  # make column child_protocol_order (e.g., 13425) based on order of child protocol dates - only include visits that have dates (i.e., occurred)

  ## define function to get order of dates for each row
  get_order <- function(row) {

    # specify date columns
    date_cols <- names(participants_data)[grepl('date', names(participants_data))]

    # get number of missing visits (date cols with NA)
    n_na <- sum(is.na(row[date_cols]))

    # get order of dates w/ missing visits at end, collapse integers into single string
    order_all_visits <- paste0(order(row[date_cols], na.last = TRUE), collapse = '')

    # remove last n_na characters from order_all_visits, will yeild a string with length = number of visits attended
    stringr::str_sub(order_all_visits, end=-(n_na + 1))
  }

  ## apply function to get visit order
  participants_data['protocol_visits_order'] <- apply(participants_data, 1, get_order)

  # reorder columns
  participants_data <- participants_data[c('participant_id', 'risk_status', 'sex', 'ethnicity', 'race', names(participants_data)[grepl('age|order', names(participants_data))], names(participants_data)[grepl('date', names(participants_data))])]

  # return data
  return(participants_data)

}
