#' util_format_pptq_data: Prepare PPTQ data for scoring
#'
#' This function prepares PPTQ data for scoring with dataprepr::score_pptq()
#'
#'
#' @param pptq_data PPTQ extracted from data from REDCap event
#'
#' @examples
#'
#' # process data
#' pptq_data_formatted <- util_format_pptq_data(pptq_data)
#'
#' @seealso [util_redcap_child_v1=4()]
#'
#' @export
#'
util_format_pptq_data <- function(pptq_data) {

  # update columns names
  names(pptq_data)[names(pptq_data) == 'pptq_rain_play_computer'] <- 'pptq_example_left'
  names(pptq_data)[names(pptq_data) == 'pptq_rain_watch_tv'] <- 'pptq_example_right'
  names(pptq_data)[names(pptq_data) == 'pptq_rain_depends'] <- 'pptq_example_middle'
  names(pptq_data)[names(pptq_data) == 'pptq_play_on_own'] <- 'pptq_1_left'
  names(pptq_data)[names(pptq_data) == 'pptq_play_with_others'] <- 'pptq_1_right'
  names(pptq_data)[names(pptq_data) == 'pptq_play_depends'] <- 'pptq_1_middle'
  names(pptq_data)[names(pptq_data) == 'pptq_school_worried'] <- 'pptq_2_left'
  names(pptq_data)[names(pptq_data) == 'pptq_school_not_worried'] <- 'pptq_2_right'
  names(pptq_data)[names(pptq_data) == 'pptq_school_depends'] <- 'pptq_2_middle'
  names(pptq_data)[names(pptq_data) == 'pptq_birds_dont_impress'] <- 'pptq_3_left'
  names(pptq_data)[names(pptq_data) == 'pptq_birds_impress'] <- 'pptq_3_right'
  names(pptq_data)[names(pptq_data) == 'pptq_birds_depends'] <- 'pptq_3_middle'
  names(pptq_data)[names(pptq_data) == 'pptq_housework_willing'] <- 'pptq_4_left'
  names(pptq_data)[names(pptq_data) == 'pptq_housework_unwilling'] <- 'pptq_4_right'
  names(pptq_data)[names(pptq_data) == 'pptq_housework_depends'] <- 'pptq_4_middle'
  names(pptq_data)[names(pptq_data) == 'pptq_classmate_not_notice'] <- 'pptq_5_left'
  names(pptq_data)[names(pptq_data) == 'pptq_classmate_notices'] <- 'pptq_5_right'
  names(pptq_data)[names(pptq_data) == 'pptq_classmate_depends'] <- 'pptq_5_middle'
  names(pptq_data)[names(pptq_data) == 'pptq_other_play_join'] <- 'pptq_6_left'
  names(pptq_data)[names(pptq_data) == 'pptq_other_play_dont_join'] <- 'pptq_6_right'
  names(pptq_data)[names(pptq_data) == 'pptq_other_play_depends'] <- 'pptq_6_middle'
  names(pptq_data)[names(pptq_data) == 'pptq_goes_wrong_calm'] <- 'pptq_7_left'
  names(pptq_data)[names(pptq_data) == 'pptq_goes_wrong_nervous'] <- 'pptq_7_right'
  names(pptq_data)[names(pptq_data) == 'pptq_goes_wrong_depends'] <- 'pptq_7_middle'
  names(pptq_data)[names(pptq_data) == 'pptq_trip_exploring'] <- 'pptq_8_left'
  names(pptq_data)[names(pptq_data) == 'pptq_trip_relaxing'] <- 'pptq_8_right'
  names(pptq_data)[names(pptq_data) == 'pptq_trip_depends'] <- 'pptq_8_middle'
  names(pptq_data)[names(pptq_data) == 'pptq_bedroom_messy'] <- 'pptq_9_left'
  names(pptq_data)[names(pptq_data) == 'pptq_bedroom_tidy'] <- 'pptq_9_right'
  names(pptq_data)[names(pptq_data) == 'pptq_bedroom_depends'] <- 'pptq_9_middle'
  names(pptq_data)[names(pptq_data) == 'pptq_help_i_help'] <- 'pptq_10_left'
  names(pptq_data)[names(pptq_data) == 'pptq_help_i_dont_help'] <- 'pptq_10_right'
  names(pptq_data)[names(pptq_data) == 'pptq_help_depends'] <- 'pptq_10_middle'
  names(pptq_data)[names(pptq_data) == 'pptq_jokes_no_laugh'] <- 'pptq_11_left'
  names(pptq_data)[names(pptq_data) == 'pptq_jokes_laugh'] <- 'pptq_11_right'
  names(pptq_data)[names(pptq_data) == 'pptq_jokes_depends'] <- 'pptq_11_middle'
  names(pptq_data)[names(pptq_data) == 'pptq_usually_worried'] <- 'pptq_12_left'
  names(pptq_data)[names(pptq_data) == 'pptq_usually_not_worried'] <- 'pptq_12_right'
  names(pptq_data)[names(pptq_data) == 'pptq_usually_depends'] <- 'pptq_12_middle'
  names(pptq_data)[names(pptq_data) == 'pptq_learning_not_enjoyed'] <- 'pptq_13_left'
  names(pptq_data)[names(pptq_data) == 'pptq_learning_is_enjoyed'] <- 'pptq_13_right'
  names(pptq_data)[names(pptq_data) == 'pptq_learning_depends'] <- 'pptq_13_middle'
  names(pptq_data)[names(pptq_data) == 'pptq_money_save_it'] <- 'pptq_14_left'
  names(pptq_data)[names(pptq_data) == 'pptq_money_spend_it'] <- 'pptq_14_right'
  names(pptq_data)[names(pptq_data) == 'pptq_money_depends'] <- 'pptq_14_middle'
  names(pptq_data)[names(pptq_data) == 'pptq_smthng_new_dont_lend'] <- 'pptq_15_left'
  names(pptq_data)[names(pptq_data) == 'pptq_smthng_new_lend_it'] <- 'pptq_15_right'
  names(pptq_data)[names(pptq_data) == 'pptq_smthng_new_depends'] <- 'pptq_15_middle'

  # update pptq values
  pptq_update_vals <- function(q_num){
    left_var = paste('pptq_', q_num, '_left', sep = '')
    right_var = paste('pptq_', q_num, '_right', sep = '')
    mid_var = paste('pptq_', q_num, '_middle', sep = '')

    q_val <- ifelse(sapply(pptq_data[[left_var]] == 0, isTRUE) & is.na(pptq_data[[mid_var]]) & is.na(pptq_data[[right_var]]), 1,ifelse(sapply(pptq_data[[mid_var]] == 0, isTRUE) & is.na(pptq_data[[left_var]]) & is.na(pptq_data[[right_var]]), 2,ifelse(sapply(pptq_data[[right_var]] == 0, isTRUE) & is.na(pptq_data[[left_var]]) & is.na(pptq_data[[mid_var]]), 3, NA)))

    return(q_val)
  }

  pptq_data[grepl('pptq', names(pptq_data))] <- sapply(names(pptq_data)[grepl('pptq', names(pptq_data))], function(x) as.numeric(pptq_data[[x]]))

  pptq_update_data <- as.data.frame(sapply(seq(1, 15, 1), function(x) pptq_update_vals(x)))
  names(pptq_update_data) <- sapply(seq(1, 15, 1), function(x) paste0('pptq_', x))

  pptq_data_update <- data.frame(c(pptq_data, pptq_update_data))

  # return data
  return(pptq_data_update)

}

