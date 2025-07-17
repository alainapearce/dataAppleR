#' util_format_sleep_data: format Sleep Log data
#'
#' This function formats Parent-reported sleep log data
#'
#' @param sleeplog_data Sleep Log data from util_redcap_child_v3
#'
#' @return formatted sleeplog_data
#'
#' @examples
#'
#' # process kbas data
#' child_sleeplog <- util_format_sleep_data(data)
#'
#' @seealso [util_redcap_child_v3()]
#'
#' @export

util_format_sleep_data <- function(sleeplog_data) {

  names(sleeplog_data) <- gsub('_mon', '_night1', names(sleeplog_data))
  names(sleeplog_data) <- gsub('_tu|_tues', '_night2', names(sleeplog_data))
  names(sleeplog_data) <- gsub('_wed', '_night3', names(sleeplog_data))
  names(sleeplog_data) <- gsub('_th|_thur', '_night4', names(sleeplog_data))
  names(sleeplog_data) <- gsub('_fri', '_night5', names(sleeplog_data))
  names(sleeplog_data) <- gsub('_sat', '_night6', names(sleeplog_data))
  names(sleeplog_data) <- gsub('_sun', '_night7', names(sleeplog_data))

  sleeplog_data['night1_day'] <- lubridate::wday(lubridate::as_date(sleeplog_data[['date_night1']]), label = TRUE)
  sleeplog_data['night2_day'] <- lubridate::wday(lubridate::as_date(sleeplog_data[['date_night2']]), label = TRUE)
  sleeplog_data['night3_day'] <- lubridate::wday(lubridate::as_date(sleeplog_data[['date_night3']]), label = TRUE)
  sleeplog_data['night4_day'] <- lubridate::wday(lubridate::as_date(sleeplog_data[['date_night4']]), label = TRUE)
  sleeplog_data['night5_day'] <- lubridate::wday(lubridate::as_date(sleeplog_data[['date_night5']]), label = TRUE)
  sleeplog_data['night6_day'] <- lubridate::wday(lubridate::as_date(sleeplog_data[['date_night6']]), label = TRUE)
  sleeplog_data['night7_day'] <- lubridate::wday(lubridate::as_date(sleeplog_data[['date_night7']]), label = TRUE)

  # set up new data set to fix data to days of week
  var_str <- c('date', 'bedtime', 'attempt', 'asleep', 'times', 'waso', 'awake', 'out_on', 'rating', 'comment')
  days_str <- c('_mon', '_tu', '_wed', '_th', '_fri', '_sat', '_sun')
  new_names <- unlist(sapply(var_str, function(x) paste0(x, days_str), simplify = FALSE, USE.NAMES = FALSE))

  new_data <- cbind.data.frame(sapply(new_names, function(x) data.frame(x = rep(NA, nrow(sleeplog_data)))))
  names(new_data) <- new_names

  # get day of week data
  new_data[, grepl('mon', names(new_data))] <- lapply(var_str, function(x) ifelse(sleeplog_data[['night1_day']] == 'Mon', sleeplog_data[[paste0(x, '_night1')]], ifelse(sleeplog_data[['night2_day']] == 'Mon', sleeplog_data[[paste0(x, '_night2')]], ifelse(sleeplog_data[['night3_day']] == 'Mon', sleeplog_data[[paste0(x, '_night3')]], ifelse(sleeplog_data[['night4_day']] == 'Mon', sleeplog_data[[paste0(x, '_night4')]], ifelse(sleeplog_data[['night5_day']] == 'Mon', sleeplog_data[[paste0(x, '_night5')]], ifelse(sleeplog_data[['night6_day']] == 'Mon', sleeplog_data[[paste0(x, '_night6')]], ifelse(sleeplog_data[['night7_day']] == 'Mon', sleeplog_data[[paste0(x, '_night7')]], new_data[[paste0(x, '_mon')]]))))))))

  new_data[, grepl('tu', names(new_data))] <- sapply(var_str, function(x) ifelse(sleeplog_data[['night1_day']] == 'Tue', sleeplog_data[[paste0(x, '_night1')]], ifelse(sleeplog_data[['night2_day']] == 'Tue', sleeplog_data[[paste0(x, '_night2')]], ifelse(sleeplog_data[['night3_day']] == 'Tue', sleeplog_data[[paste0(x, '_night3')]], ifelse(sleeplog_data[['night4_day']] == 'Tue', sleeplog_data[[paste0(x, '_night4')]], ifelse(sleeplog_data[['night5_day']] == 'Tue', sleeplog_data[[paste0(x, '_night5')]], ifelse(sleeplog_data[['night6_day']] == 'Tue', sleeplog_data[[paste0(x, '_night6')]], ifelse(sleeplog_data[['night7_day']] == 'Tue', sleeplog_data[[paste0(x, '_night7')]], new_data[[paste0(x, '_tu')]]))))))))

  new_data[, grepl('wed', names(new_data))] <- sapply(var_str, function(x) ifelse(sleeplog_data[['night1_day']] == 'Wed', sleeplog_data[[paste0(x, '_night1')]], ifelse(sleeplog_data[['night2_day']] == 'Wed', sleeplog_data[[paste0(x, '_night2')]], ifelse(sleeplog_data[['night3_day']] == 'Wed', sleeplog_data[[paste0(x, '_night3')]], ifelse(sleeplog_data[['night4_day']] == 'Wed', sleeplog_data[[paste0(x, '_night4')]], ifelse(sleeplog_data[['night5_day']] == 'Wed', sleeplog_data[[paste0(x, '_night5')]], ifelse(sleeplog_data[['night6_day']] == 'Wed', sleeplog_data[[paste0(x, '_night6')]], ifelse(sleeplog_data[['night7_day']] == 'Wed', sleeplog_data[[paste0(x, '_night7')]], new_data[[paste0(x, '_wed')]]))))))))

  new_data[, grepl('th', names(new_data))] <- sapply(var_str, function(x) ifelse(sleeplog_data[['night1_day']] == 'Thu', sleeplog_data[[paste0(x, '_night1')]], ifelse(sleeplog_data[['night2_day']] == 'Thu', sleeplog_data[[paste0(x, '_night2')]], ifelse(sleeplog_data[['night3_day']] == 'Thu', sleeplog_data[[paste0(x, '_night3')]], ifelse(sleeplog_data[['night4_day']] == 'Thu', sleeplog_data[[paste0(x, '_night4')]], ifelse(sleeplog_data[['night5_day']] == 'Thu', sleeplog_data[[paste0(x, '_night5')]], ifelse(sleeplog_data[['night6_day']] == 'Thu', sleeplog_data[[paste0(x, '_night6')]], ifelse(sleeplog_data[['night7_day']] == 'Thu', sleeplog_data[[paste0(x, '_night7')]], new_data[[paste0(x, '_th')]]))))))))

  new_data[, grepl('fri', names(new_data))] <- sapply(var_str, function(x) ifelse(sleeplog_data[['night1_day']] == 'Fri', sleeplog_data[[paste0(x, '_night1')]], ifelse(sleeplog_data[['night2_day']] == 'Fri', sleeplog_data[[paste0(x, '_night2')]], ifelse(sleeplog_data[['night3_day']] == 'Fri', sleeplog_data[[paste0(x, '_night3')]], ifelse(sleeplog_data[['night4_day']] == 'Fri', sleeplog_data[[paste0(x, '_night4')]], ifelse(sleeplog_data[['night5_day']] == 'Fri', sleeplog_data[[paste0(x, '_night5')]], ifelse(sleeplog_data[['night6_day']] == 'Fri', sleeplog_data[[paste0(x, '_night6')]], ifelse(sleeplog_data[['night7_day']] == 'Fri', sleeplog_data[[paste0(x, '_night7')]], new_data[[paste0(x, '_fri')]]))))))))

  new_data[, grepl('sat', names(new_data))] <- sapply(var_str, function(x) ifelse(sleeplog_data[['night1_day']] == 'Sat', sleeplog_data[[paste0(x, '_night1')]], ifelse(sleeplog_data[['night2_day']] == 'Sat', sleeplog_data[[paste0(x, '_night2')]], ifelse(sleeplog_data[['night3_day']] == 'Sat', sleeplog_data[[paste0(x, '_night3')]], ifelse(sleeplog_data[['night4_day']] == 'Sat', sleeplog_data[[paste0(x, '_night4')]], ifelse(sleeplog_data[['night5_day']] == 'Sat', sleeplog_data[[paste0(x, '_night5')]], ifelse(sleeplog_data[['night6_day']] == 'Sat', sleeplog_data[[paste0(x, '_night6')]], ifelse(sleeplog_data[['night7_day']] == 'Sat', sleeplog_data[[paste0(x, '_night7')]], new_data[[paste0(x, '_sat')]]))))))))

  new_data[, grepl('sun', names(new_data))] <- sapply(var_str, function(x) ifelse(sleeplog_data[['night1_day']] == 'Sun', sleeplog_data[[paste0(x, '_night1')]], ifelse(sleeplog_data[['night2_day']] == 'Sun', sleeplog_data[[paste0(x, '_night2')]], ifelse(sleeplog_data[['night3_day']] == 'Sun', sleeplog_data[[paste0(x, '_night3')]], ifelse(sleeplog_data[['night4_day']] == 'Sun', sleeplog_data[[paste0(x, '_night4')]], ifelse(sleeplog_data[['night5_day']] == 'Sun', sleeplog_data[[paste0(x, '_night5')]], ifelse(sleeplog_data[['night6_day']] == 'Sun', sleeplog_data[[paste0(x, '_night6')]], ifelse(sleeplog_data[['night7_day']] == 'Sun', sleeplog_data[[paste0(x, '_night7')]], new_data[[paste0(x, '_sun')]]))))))))

  # fix date/time formats
  new_data[, grepl('date', names(new_data))] <- lapply(new_data[, grepl('date', names(new_data))], function(x) lubridate::as_date(as.numeric(x)))

  new_data[, grepl('bedtime|attempt|asleep|awake|out_on', names(new_data))] <- lapply(new_data[, grepl('bedtime|attempt|asleep|awake|out_on', names(new_data))], function(x) format(as.POSIXct(as.numeric(x), format = '%H:%M:%S', tz = "UTC"), '%H:%M:%S'))

# add participant id and session id
  new_data['participant_id'] <- sleeplog_data['participant_id']
  new_data['session_id'] <- sleeplog_data['session_id']
  new_data['visit_date'] <- sleeplog_data['visit_date']

# return data
return(new_data)

}
