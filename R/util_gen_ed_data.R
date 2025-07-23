#' util_gen_ed_data: Generate a dataframe with energy density data
#'
#' This function generates a dataframe with energy density data for processing intake data with util_redcap_de()
#'
#' @return A dataframe with energy density data
#'
#' @examples
#'
#' # process data
#' ed_data <- util_gen_ed_data(intake_data)
#'
#' @seealso [util_calc_intake()]
#'
#' @export


util_gen_ed_data <- function() {

  # calculate energy densities (kcal/g) according to nutrition label
  bread_ed <- 130/49
  butter_ed <- 50/14
  cheese_ed <- 70/19

  grilled_cheese_ed <- NA # this will remain NA -- as this will be calculated for each person

  # EDs taken from redcap -- need to confirm
  mac_ed <- 1.76
  broccoli_ed <- 0.28
  grapes_ed <- 0.69
  carrots_ed <- 0.40
  graham_ed <- 4.29
  chips_ed <- 5.71
  mms_ed <- 5.00
  brownies_ed <- 4.73
  cookies_ed <- 4.85
  starburst_ed <- 4.14
  fritos_ed <- 5.71
  water_ed <- 0.0

  # create vectors
  food <- c('mac', 'broccoli', 'grapes', 'carrots', 'graham', 'chips', 'mms', 'brownies', 'cookies', 'starburst', 'fritos', 'water')
  ed <- c(mac_ed, broccoli_ed, grapes_ed, carrots_ed, graham_ed, chips_ed, mms_ed, brownies_ed, cookies_ed, starburst_ed, fritos_ed, water_ed)

  # generate dataframe
  ed_data <- data.frame(food, ed)

  #write out to raw-data
  write.csv(ed_data, "data_raw/apple_foodED_ref.csv", row.names = FALSE)

  #make a database for the package
  usethis::use_data(ed_data, overwrite = TRUE)

}
