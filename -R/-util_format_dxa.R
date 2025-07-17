#' util_format_dxa: process double-entered DXA data for REACH visit 1 and 5
#'
#' This function process Ranking Food Item Questionnaire data
#'
#'
#' @param dxa_data dxa extracted from data from REDCap events
#'
#' @examples
#'
#' # process data
#' dxa_formatted <- util_format_dxa(dxa_data)
#'
#' @seealso [util_redcap_de()]
#'
#' @export


util_format_dxa <- function(dxa_data) {

  # rename columns to align better with prior studies
  names(dxa_data)[!grepl('_id|visit', names(dxa_data))] <- paste0("dxa_", names(dxa_data)[!grepl('_id|visit', names(dxa_data))])

  names(dxa_data) <- gsub("left", "l", names(dxa_data))
  names(dxa_data) <- gsub("right", "r", names(dxa_data))
  names(dxa_data) <- gsub("_am", "_ptile", names(dxa_data))
  names(dxa_data) <- gsub("_am", "_ptile", names(dxa_data))
  names(dxa_data) <- gsub("fat_trunk_over_leg", "percfat_trunk_legs_ratio", names(dxa_data))
  names(dxa_data) <- gsub("lean_over_height", "lean_height_ratio", names(dxa_data))
  names(dxa_data) <- gsub("fat_mass_over_height", "fatmass_height_ratio", names(dxa_data))

  # name wont match Food and Brain which uses leg instead of limb for these vars, but limb is more accurate based on description
  names(dxa_data) <- gsub("trunk_over_limb_fat", "fatmass_trunk_limb_ratio", names(dxa_data))
  names(dxa_data) <- gsub("z_score", "zscore", names(dxa_data))
  names(dxa_data) <- gsub("lean_and_bmc", "lean_bmc_comb", names(dxa_data))

  # return data
  return(dxa_data)

}
