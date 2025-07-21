#' util_format_infant_data: process infant birth weight data
#'
#' This function process nfant birth weight data
#'
#'
#' @param infancy_data infancy data extracted from data from REDCap events
#'
#' @examples
#'
#' # process data
#' infancy_data_formatted <- util_format_infant_data(infancy_data)
#'
#' @seealso [util_redcap_child_v1()]
#'
#' @export


util_format_infant_data <- function(infancy_data) {

  # rename columns
  'demo_birth_length'
  names(infancy_data) <- gsub('_child', '', names(infancy_data))

  names(infancy_data)[names(infancy_data) == 'demo_bl'] <- 'demo_birth_length'
  names(infancy_data)[names(infancy_data) == 'demo_bf_months'] <- 'demo_exclusive_feeding'

  # fix birth weight
  infancy_data['demo_birthweight_pounds_component'] <- c(8, 8, 9, 6, 8, 7, 9, 8, 7, 7, 8, 8, 7, 9,NA, 7, 6, 9, 99, 5, 5, NA, 8, 7, 9, 7, 7, 7, 7, 7, 8, 6, 7, 8, 8, 8, 7, 8, 6, 7, 6, 7, 7, 7, 7, 8, 6, 7, 7, 9, 8, 6, 8, 9, 7, 8, 9, 9, 7, 6, 5, 7, 6, 6, 6, 6, 6, 7, 7, 5)

  infancy_data['demo_birthweight_ounces_component'] <- c(6, 6, 6, 9, 12, 11, 0, 0, 7, 7, 8, 9, 4, 1, NA, 3, 7, 0, 99, 15, 15, NA, 3, 14, 14, 3, 2, 6, 9, 4, 3, 1, 0, 0, 0, 14, 0, 6, 13, 1, 11, 1, 4, 11, 6, 9, 12, 7, 11, 1, 11, 12, 1, 4, 0, 4, 2, 9.5, 13, 9, 15, 8, 3, 9, 9, 11, 9, 9, 12, 4)

  infancy_data['birthweight_ounces_total'] <- infancy_data['demo_birthweight_pounds_component']*16 + infancy_data['demo_birthweight_ounces_component']



  # return data
  return(infancy_data)

}
