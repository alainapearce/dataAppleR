#' util_format_kbas_data: format Kids Brand Awareness Survey data data
#'
#' This function formats Kids Brand Awareness Survey data
#'
#' @param kbas_data Kids Brand Awareness Survey extracted from data from REDCap events
#'
#' @return formatted kbas_data
#'
#' @examples
#'
#' # process kbas data
#' kbas_data_formatted <- util_format_kbas_data(kbas_data)
#'
#' @seealso [util_redcap_child_v1()], [util_redcap_child_v5()]
#'
#' @export

util_format_kbas_data <- function(kbas_data) {

  # relabel version a and b items
  names(kbas_data)[!grepl('_b|_vb|_id|visit_date', names(kbas_data))] <- paste0('va_', names(kbas_data)[!grepl('_b|_vb|_id|visit_date', names(kbas_data))])
  names(kbas_data)[grepl('_b|_vb', names(kbas_data))] <- paste0('vb_', names(kbas_data)[grepl('_b|_vb', names(kbas_data))])

  # remove _b and _vb from end of column names
  names(kbas_data) <- gsub('_b|_vb', '', names(kbas_data))

  # add version column
  ## define version items
  va_items <- names(kbas_data)[grepl('va', names(kbas_data)) & !grepl('score', names(kbas_data))]
  vb_items <- names(kbas_data)[grepl('vb', names(kbas_data)) & !grepl('score', names(kbas_data))]

  ## assign version -- requires having responses to a given version & no responses to the other version
  kbas_data[['kbas_version']] <- ifelse(rowSums(!is.na(kbas_data[va_items])) > 45 & rowSums(is.na(kbas_data[vb_items])) == 50, 'A', ifelse(rowSums(is.na(kbas_data[va_items])) > 45 & rowSums(!is.na(kbas_data[vb_items])) == 50, 'B', NA))

  #### combine kbas score columns between versions ####

  # combine va and vb total scored columns
  kbas_data$kbas_food_score <- ifelse(kbas_data[['kbas_version']] == 'A', kbas_data[['va_food_score_kbas']], ifelse(kbas_data[['kbas_version']] == 'B', kbas_data[['vb_food_score_kbas']], NA))

  kbas_data$kbas_toy_score <- ifelse(kbas_data[['kbas_version']] == 'A', kbas_data[['va_toy_score_kbas']], ifelse(kbas_data[['kbas_version']] == 'B', kbas_data[['vb_toy_score_kbas']], NA))

  #remove columns
  kbas_data <- kbas_data[, !grepl('va_toy_score|vb_toy_score|va_food_score|vb_food_score|va_q16_score_2', names(kbas_data))]

  #### combine q_*score columns between versions ####

  # define ways to re-name score columns
  food_qs <- paste0('food_', seq(1, 25), '_score')
  toy_qs <- paste0('toy_', seq(1, 25), '_score')

  new_col_va <- c(food_qs, toy_qs) #version a order: food then toys
  new_col_vb <- c(toy_qs, food_qs) #version b order: toys then food

  # process version A data - data[select version A rows, remove 'vb_' columns]
  kbas_va_data <- kbas_data[kbas_data[['kbas_version']] == 'A' & !is.na(kbas_data[['kbas_version']]), -grep('^vb_', names(kbas_data))]

  # remove 'va_' from column names
  names(kbas_va_data) <- gsub('va_', '', names(kbas_va_data))

  # get column names that start with q and end with score
  q_cols_va <- names(kbas_va_data)[grep('^q.*score$', names(kbas_va_data))]

  #rename columns
  names(kbas_va_data)[names(kbas_va_data) %in% q_cols_va] <- new_col_va

  # process version B data - data[select version B rows, remove 'va_' columns]
  kbas_vb_data <- kbas_data[kbas_data[['kbas_version']] == 'B' & !is.na(kbas_data[['kbas_version']]), -grep('^va_', names(kbas_data))]

  # remove 'vb_' from column names
  names(kbas_vb_data) <- gsub('vb_', '', names(kbas_vb_data))

  # get column names that start with q and end with score
  q_cols_vb <- names(kbas_vb_data)[grep('^q.*score$', names(kbas_vb_data))]

  #rename columns
  names(kbas_vb_data)[names(kbas_vb_data) %in% q_cols_vb] <- new_col_vb

  # combine versions A and B with new columns names
  kbas_data <- rbind(kbas_va_data, kbas_vb_data)

  #reorder columns
  kbas_data <- kbas_data[c('participant_id', 'session_id', 'visit_date', 'kbas_version', 'kbas_food_score', 'kbas_toy_score', names(kbas_data)[grepl('^toy_|^food_', names(kbas_data))])]

  # fix naming
  names(kbas_data)[!grepl('_id|visit_date|kbas_', names(kbas_data))] <- sapply(names(kbas_data)[!grepl('_id|visit_date|kbas_', names(kbas_data))], function(x) paste0('kbas_', x), USE.NAMES = FALSE)

  # return data
  return(kbas_data)

}
