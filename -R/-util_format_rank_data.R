#' util_format_rank_data: process Ranking Food Item Questionnaire data for REACH visit 1 and 5
#'
#' This function process Ranking Food Item Questionnaire data
#'
#'
#' @param rank_data rank extracted from data from REDCap events
#'
#' @examples
#'
#' # process data
#' rank_data_formatted <- util_format_rank_data(rank_data)
#'
#' @seealso [util_redcap_parent_v1()], [util_redcap_parent_v5()]
#'
#' @export


util_format_rank_data <- function(rank_data) {

  # rename columns
  names(rank_data)[names(rank_data) == 'rank_1'] <- 'rank_package_breads'
  names(rank_data)[names(rank_data) == 'rank_2'] <- 'rank_bakery'
  names(rank_data)[names(rank_data) == 'rank_3'] <- 'rank_saltysnacks'
  names(rank_data)[names(rank_data) == 'rank_4'] <- 'rank_sweetsnacks'
  names(rank_data)[names(rank_data) == 'rank_5'] <- 'rank_cheese'
  names(rank_data)[names(rank_data) == 'rank_6'] <- 'rank_milk'
  names(rank_data)[names(rank_data) == 'rank_7'] <- 'rank_yogurt'
  names(rank_data)[names(rank_data) == 'rank_8'] <- 'rank_butter'
  names(rank_data)[names(rank_data) == 'rank_9'] <- 'rank_eggs'
  names(rank_data)[names(rank_data) == 'rank_10'] <- 'rank_otherdairy'
  names(rank_data)[names(rank_data) == 'rank_11'] <- 'rank_coffee_tea'
  names(rank_data)[names(rank_data) == 'rank_12'] <- 'rank_carbonated_bev_soda'
  names(rank_data)[names(rank_data) == 'rank_13'] <- 'rank_fruitjuice'
  names(rank_data)[names(rank_data) == 'rank_14'] <- 'rank_sportsdrinks'
  names(rank_data)[names(rank_data) == 'rank_15'] <- 'rank_alcohol'
  names(rank_data)[names(rank_data) == 'rank_16'] <- 'rank_redmeat'
  names(rank_data)[names(rank_data) == 'rank_17'] <- 'rank_poultry'
  names(rank_data)[names(rank_data) == 'rank_18'] <- 'rank_seafood'
  names(rank_data)[names(rank_data) == 'rank_19'] <- 'rank_pasta_rice'
  names(rank_data)[names(rank_data) == 'rank_20'] <- 'rank_soup'
  names(rank_data)[names(rank_data) == 'rank_21'] <- 'rank_nuts_seeds'
  names(rank_data)[names(rank_data) == 'rank_22'] <- 'rank_nut_fruit_spreads'
  names(rank_data)[names(rank_data) == 'rank_23'] <- 'rank_breakfast_cereals'
  names(rank_data)[names(rank_data) == 'rank_24'] <- 'rank_protein_bars'
  names(rank_data)[names(rank_data) == 'rank_25'] <- 'rank_protein_shakes'
  names(rank_data)[names(rank_data) == 'rank_26'] <- 'rank_preparedfoods'
  names(rank_data)[names(rank_data) == 'rank_27'] <- 'rank_veg_fresh'
  names(rank_data)[names(rank_data) == 'rank_28'] <- 'rank_fruit_fresh'
  names(rank_data)[names(rank_data) == 'rank_29'] <- 'rank_veg_can'
  names(rank_data)[names(rank_data) == 'rank_30'] <- 'rank_fruit_can'
  names(rank_data)[names(rank_data) == 'rank_31'] <- 'rank_veg_frozen'
  names(rank_data)[names(rank_data) == 'rank_32'] <- 'rank_fruit_frozen'
  names(rank_data)[names(rank_data) == 'rank_33'] <- 'rank_dinner_frozen'
  names(rank_data)[names(rank_data) == 'rank_34'] <- 'rank_breakfast_frozen'
  names(rank_data)[names(rank_data) == 'rank_35'] <- 'rank_desserts_frozen'
  names(rank_data)[names(rank_data) == 'rank_36'] <- 'rank_candy_gum'
  names(rank_data)[names(rank_data) == 'rank_37'] <- 'rank_baking_supplies'
  names(rank_data)[names(rank_data) == 'rank_38'] <- 'rank_condiments'
  names(rank_data)[names(rank_data) == 'rank_39'] <- 'rank_spreads_dips'
  names(rank_data)[names(rank_data) == 'rank_40'] <- 'rank_salad_dressings'
  names(rank_data)[names(rank_data) == 'rank_41'] <- 'rank_spices_seasonings'
  names(rank_data)[names(rank_data) == 'rank_42'] <- 'rank_cooking_oils'


  # return data
  return(rank_data)

}
