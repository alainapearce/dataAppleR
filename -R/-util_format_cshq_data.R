#' util_format_cshq_data: formats Child Sleep Habits Questionnaire data to score
#'
#' This function prepares Child Sleep Habits Questionnaire data for scoring with dataprepr::score_cshq by re-leveling scored items to have numeric values 1-3: (3 - Usually, 2 - Sometimes, 1 - Rarely or 1 - not sleepy, 2 - very sleepy, 3 - falls asleep)
#'
#'
#' @param cshq_data cshq extracted from data from REDCap events
#'
#' @examples
#'
#' # process data
#' cshq_data_formatted <- util_format_cshq_data(cshq_data)
#'
#' @seealso [util_redcap_parent_v2()], [util_redcap_parent_v5()]
#'
#' @export
#'


util_format_cshq_data <- function(cshq_data) {

  #### Rename columns ####

  # rename columns for items asking if a behavior is a problem
  names(cshq_data) <- gsub('_a', '_prob', names(cshq_data))

  #### Compute total sleep amounts from hr and min components ####
  cshq_data[, grepl('hour|min', names(cshq_data))] <- sapply(cshq_data[, grepl('hour|min', names(cshq_data))], function(x) as.numeric(x))

  # if min component is NA, calculate with hrs component only, else, calculate with min and hrs components
  cshq_data['cshq_night_sleep_dur_mins'] <- ifelse(is.na(cshq_data[['cshq_sleep_total_mins']]), cshq_data[['cshq_sleep_total_hours']]* 60, (cshq_data[['cshq_sleep_total_hours']]* 60 + cshq_data[['cshq_sleep_total_mins']]))
  cshq_data['cshq_nap_dur_mins'] <- ifelse(is.na(cshq_data[['cshq_nap_mins']]), cshq_data[['cshq_nap_hours']]* 60, (cshq_data[['cshq_nap_hours']]* 60 + cshq_data[['cshq_nap_mins']]))

  #### Update Usually/Sometimes/Rarely ('usr') columns ####

  # specify 'usr' columns to update
  all_nums <- seq(1, 33)
  usr_nums <- all_nums[all_nums != 7 & all_nums != 8]
  usr_cols <- paste0("cshq_", usr_nums)

  # update values to be (2 - Usually, 1 - Sometimes, 0 - Rarely) rather than (0 - Usually, 1 - Sometimes, 2 - Rarely)
  recod_cols <- paste0('cshq_', seq(1,8,1))
  cshq_data[recod_cols] <- sapply(recod_cols, function(x)  ifelse(cshq_data[[x]] == 0, 2, ifelse(cshq_data[[x]] == 2 , 1, (cshq_data[[x]]))))

  # return data
  return(cshq_data)

}
