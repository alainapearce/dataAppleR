#' util_calc_intake: Computes intake variables
#'
#' This function computes intake variables
#'
#' @param intake_data Intake data (i.e., pre and post weights) extracted double entry data
#' @param generate_ed_data (logical) re-generate food ED database, default = FALSE
#'
#' @return A dataframe of intake_data (input) variables and calculated intake variables
#'
#' @examples
#'
#' # process data
#' intake_calc <- util_calc_intake(intake_data)
#'
#' @seealso [proc_redcap()]
#'
#' @export

util_calc_intake <- function(intake_data, generate_ed_data = FALSE) {

  # convert all intake columns to numeric
  intake_data[!grepl('id|visit|notes', names(intake_data))] <- sapply(intake_data[!grepl('id|visit|notes', names(intake_data))], as.numeric)

  # make dataframe with energy density data
  load("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/cogNEATO_tools/dataAppleR/data/ed_data.rda")
  if (isTRUE(generate_ed_data) | !exists('ed_data')) {
    #make dataframe with energy density data
    ed_data <- util_gen_ed_data()
  }

  #### calculate grilled cheese energy density ####

  #### calculate item amounts consumed ####

  consumed_fn <- function(food_str){

    pre_var <- paste(food_str, '_pre_w_plate', sep = '')
    post_var <- paste(food_str, '_post_w_plate', sep = '')
    consumed_g_var <- paste(food_str, '_g_consumed', sep = '')
    consumed_kcal_var <- paste(food_str, '_kcal_consumed', sep = '')

    food_data <- data.frame(matrix(ncol = 0, nrow = nrow(intake_data)))


    food_data[[consumed_g_var]] <- intake_data[[pre_var]] - intake_data[[post_var]]

    # if less than 0 g (post > pre), set to 0
    food_data[[consumed_g_var]] <- sapply(food_data[[consumed_g_var]], function(x) ifelse(!is.na(x), ifelse(x < 0, 0, x), NA))

    # calculate kcal using EDs in ed_data
    food_data[[consumed_kcal_var]] <- food_data[[consumed_g_var]] * ed_data[ed_data['food'] == food_str, 'ed']


    return(food_data)
  }


  foods <- ed_data[['food']]

  consumed_data <- cbind.data.frame(sapply(foods, function(x) consumed_fn(x), USE.NAMES = FALSE, simplify = FALSE))

  # merge with intake data
  intake_names <- names(intake_data)
  consumed_names <- names(consumed_data)
  intake_data <- cbind.data.frame(c(intake_data, consumed_data))

  names(intake_data) <- c(intake_names, consumed_names)

  #### calculate total amounts consumed ####
  # note: by using na.rm = FALSE -- total amounts will only be calculated if there is data for all food items to be summed

  ## meal

  # sum across meal_foods_g_vars columns
  intake_data['meal_g_consumed'] <- rowSums(intake_data[grepl('mac_g|broccoli_g|grapes_g|carrots_g|graham_g', names(intake_data))], na.rm = FALSE)

  #intake_data['meal_g_consumed_inc_water'] <- rowSums(intake_data[grepl('mac_g|broccoli_g|grapes_g|carrots_g|graham_g', names(intake_data))], na.rm = FALSE)

  intake_data['meal_kcal_consumed'] <- rowSums(intake_data[grepl('mac_k|broccoli_k|grapes_k|carrots_k|graham_k', names(intake_data))], na.rm = FALSE)

  ## EAH

  intake_data['eah_g_consumed'] <- rowSums(intake_data[grepl('chips_g|mms_g|brownies_g|cookies_g|starburst_g|fritos_g', names(intake_data))], na.rm = FALSE)

  #intake_data['eah_g_consumed_inc_water'] <- rowSums(intake_data[grepl('chips_g|mms_g|brownies_g|cookies_g|starburst_g|fritos_g', names(intake_data))], na.rm = FALSE)

  intake_data['eah_kcal_consumed'] <- rowSums(intake_data[grepl('chips_k|mms_k|brownies_k|cookies_k|starburst_k|fritos_k', names(intake_data))], na.rm = FALSE)


  ## total (meal + eah)
  intake_data['total_g_consumed'] <- rowSums(intake_data[c('meal_g_consumed', 'eah_g_consumed')], na.rm = FALSE)

  #intake_data['total_g_consumed_inc_water'] <- rowSums(intake_data[c('meal_g_consumed_inc_water', 'eah_g_consumed_inc_water')], na.rm = FALSE)

  intake_data['total_kcal_consumed'] <- rowSums(intake_data[c('meal_kcal_consumed', 'eah_kcal_consumed')], na.rm = FALSE)

  # return data
  return(intake_data)

}
