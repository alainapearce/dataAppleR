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
  tender_ed <- 2.371 # consistent with label: 230/97 kcal/g
  carrot_ed <- 0.353 ## use label or USDA (Energy (Atwater Specific Factors))?
  chips_ed <- 5.714 # consistent with label: 160/28 kcal/g
  fruit_ed <- 0.427 # consistent with label: 100/234 kcal/g
  water_ed <- 0.0
  ranch_ed <- 4.577 # how to calculate/verify when label is in mL??
  ketchup_ed <- 1.176 # consistent with label: 20/17 kcal/g
  brownie_ed <- 4.363 # consistent with label: 240/55 kcal/g
  corn_chip_ed <- 5.714 # consistent with label: 160/28 kcal/g
  kiss_ed <- 5.000 # consistent with label: 160/32 kcal/g
  ice_cream_ed <- 2.184 # consistent with label: 190/87 kcal/g
  oreo_ed <- 4.706 # consistent with label: 160/34 kcal/g
  popcorn_ed <- 5.000 # consistent with label: 140/28 kcal/g
  pretzel_ed <- 3.929 # consistent with label: 110/28 kcal/g
  skittle_ed <- 3.929 # consistent with label: 110/28 kcal/g
  starburst_ed <- 4.138 # consistent with label: 120/29 kcal/g
  water_eah_ed <- 0.0

  # create vectors
  food <- c('bread', 'butter', 'cheese', 'grilled_cheese', 'tender', 'carrot', 'chips', 'fruit', 'water', 'ranch', 'ketchup',
            'brownie', 'corn_chip', 'kiss', 'ice_cream', 'oreo', 'popcorn', 'pretzel', 'skittle', 'starburst', 'water_eah')
  ed <- c(bread_ed, butter_ed, cheese_ed, grilled_cheese_ed, tender_ed, carrot_ed, chips_ed, fruit_ed, water_ed, ranch_ed, ketchup_ed,
          brownie_ed, corn_chip_ed, kiss_ed, ice_cream_ed, oreo_ed, popcorn_ed, pretzel_ed, skittle_ed, starburst_ed, water_eah_ed)

  # generate dataframe
  ed_data <- data.frame(food, ed)

  #write out to raw-data
  write.csv(ed_data, "data-raw/reach_foodED_ref.csv", row.names = FALSE)

  #make a database for the package
  usethis::use_data(ed_data, overwrite = TRUE)

}
