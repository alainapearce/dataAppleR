#' util_format_hfi_data: Prepare Fulkerson HFI data for scoring
#'
#' This function prepares Fulkerson HFI data for scoring with dataprepr::score_hfi()
#'
#'
#' @param hfi_data Fulkerson HFI extracted from data from REDCap event
#'
#' @examples
#'
#' # process data
#' hfi_data_formatted <- util_format_hfi_data(hfi_data)
#'
#' @seealso [util_redcap_parent_v4()]
#'
#' @export
#'


util_format_hfi_data <- function(hfi_data) {

  # update names
  names(hfi_data) <- gsub('a___', '_type', names(hfi_data))
  names(hfi_data) <- gsub('milk', 'dairy', names(hfi_data))
  names(hfi_data) <- gsub('visible', 'accessible', names(hfi_data))
  names(hfi_data) <- gsub('hfi_dessert', 'hfi_frozen', names(hfi_data))
  names(hfi_data) <- gsub('butter', 'oils', names(hfi_data))
  names(hfi_data) <- gsub('vegetable', 'veg', names(hfi_data))
  names(hfi_data) <- gsub('meat', 'protein', names(hfi_data))
  names(hfi_data) <- gsub('prepared_dessert', 'dessert', names(hfi_data))
  names(hfi_data) <- gsub('chips', 'snacks', names(hfi_data))
  names(hfi_data) <- gsub('microwave', 'quick', names(hfi_data))
  names(hfi_data) <- gsub('condo', 'cond', names(hfi_data))
  names(hfi_data) <- gsub('chips', 'snacks', names(hfi_data))

  names(hfi_data)[names(hfi_data) == 'hfi_prepackaged'] <- 'hfi_16'
  names(hfi_data)[names(hfi_data) == 'hfi_cereal_1'] <- 'hfi_17'
  names(hfi_data)[names(hfi_data) == 'hfi_cereal_2'] <- 'hfi_18'
  names(hfi_data)[names(hfi_data) == 'hfi_cereal_3'] <- 'hfi_19'

  # define visible fridge columns: items hfi_visible_13 through hfi_visible_28
  visible_fridge_vars <- c(paste0('hfi_accessible_', seq(13, 28)))
  names(hfi_data)[names(hfi_data) %in% visible_fridge_vars] <- sapply(visible_fridge_vars, function(x) gsub('hfi_accessible', 'hfi_accessible_fridge', x), USE.NAMES = FALSE)

  # these columns were added the hfi in REACH and not part of the original scale
  names(hfi_data)[names(hfi_data) == 'hfi_dairy_11'] <- 'hfi_extra_nondairy'
  names(hfi_data)[names(hfi_data) == 'hfi_cond_5'] <- 'hfi_6'
  names(hfi_data)[names(hfi_data) == 'hfi_accessible_fridge_16'] <- 'hfi_extra_accessible_fridge'

  # Convert numbers to letters ----
  names(hfi_data)[names(hfi_data) == 'hfi_accessible_fridge_13'] <- 'hfi_accessible_fridge_a'
  names(hfi_data)[names(hfi_data) == 'hfi_accessible_fridge_14'] <- 'hfi_accessible_fridge_b'
  names(hfi_data)[names(hfi_data) == 'hfi_accessible_fridge_15'] <- 'hfi_accessible_fridge_c'
  names(hfi_data)[names(hfi_data) == 'hfi_accessible_fridge_17'] <- 'hfi_accessible_fridge_d'
  names(hfi_data)[names(hfi_data) == 'hfi_accessible_fridge_18'] <- 'hfi_accessible_fridge_e'
  names(hfi_data)[names(hfi_data) == 'hfi_accessible_fridge_19'] <- 'hfi_accessible_fridge_f'
  names(hfi_data)[names(hfi_data) == 'hfi_accessible_fridge_20'] <- 'hfi_accessible_fridge_g'
  names(hfi_data)[names(hfi_data) == 'hfi_accessible_fridge_21'] <- 'hfi_accessible_fridge_h'
  names(hfi_data)[names(hfi_data) == 'hfi_accessible_fridge_22'] <- 'hfi_accessible_fridge_i'
  names(hfi_data)[names(hfi_data) == 'hfi_accessible_fridge_23'] <- 'hfi_accessible_fridge_j'
  names(hfi_data)[names(hfi_data) == 'hfi_accessible_fridge_24'] <- 'hfi_accessible_fridge_k'
  names(hfi_data)[names(hfi_data) == 'hfi_accessible_fridge_25'] <- 'hfi_accessible_fridge_l'
  names(hfi_data)[names(hfi_data) == 'hfi_accessible_fridge_26'] <- 'hfi_accessible_fridge_m'
  names(hfi_data)[names(hfi_data) == 'hfi_accessible_fridge_27'] <- 'hfi_accessible_fridge_n'
  names(hfi_data)[names(hfi_data) == 'hfi_accessible_fridge_28'] <- 'hfi_accessible_fridge_o'


  # switch number for letter
  num_letter_swap <- function(var_name){

    split_str <- unlist(strsplit(var_name, '_'))

    new_name <- gsub(paste0('_', split_str[3]), paste0('_', letters[as.numeric(split_str[3])]), var_name)

    return(new_name)
  }

  name_list <- names(hfi_data)[grepl('hfi', names(hfi_data))]
  name_list <- name_list[!grepl('fridge|extra|hfi_6|hfi_16|hfi_17|hfi_18|hfi_19', name_list)]

  names(hfi_data)[names(hfi_data) %in% name_list] <- sapply(name_list, function (x) num_letter_swap(x), USE.NAMES = FALSE)


  # return data
  return(hfi_data)

}
