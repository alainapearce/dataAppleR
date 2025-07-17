#' util_format_intake_data: process preload and meal intake data
#'
#' This function process Ranking Food Item Questionnaire data
#'
#'
#' @param intake_data intake data extracted from data from REDCap events
#'
#' @examples
#'
#' # process data
#' intake_data_formatted <- util_format_intake_data(intake_data)
#'
#' @seealso [util_redcap_child_v1()], [util_redcap_child_v2()], [util_redcap_child_v3()], [util_redcap_child_v4()], [util_redcap_child_v5()]
#'
#' @export


util_format_intake_data <- function(intake_data) {

  # rename columns
  names(intake_data)[names(intake_data) == 'preload_container'] <- 'preload_pre_w_plate'
  names(intake_data)[names(intake_data) == 'soufflecup_preweight'] <- 'preload_servecup_weight'
  names(intake_data)[names(intake_data) == 'preload_postweight'] <- 'preload_post_w_cup_plate'

  names(intake_data)[!grepl('soufflecup',  names(intake_data))] <- gsub('preweight', 'pre_w_o_plate', names(intake_data)[!grepl('soufflecup',  names(intake_data))])

  names(intake_data) <- gsub('bowl', 'pre_w_plate', names(intake_data))
  names(intake_data) <- gsub('postweight', 'post_w_plate', names(intake_data))

  names(intake_data) <- gsub('plate_1', 'plate_serv1', names(intake_data))
  names(intake_data) <- gsub('plate_2', 'plate_serv2', names(intake_data))
  names(intake_data) <- gsub('plate_3', 'plate_serv3', names(intake_data))
  names(intake_data) <- gsub('plate_4', 'plate_serv4', names(intake_data))

  # fix additional servings
  intake_data['mac_serving2'] <- ifelse(intake_data[['second_servings___0']] == 1, 1, 0)
  intake_data['mac_serving3'] <- ifelse(intake_data[['third_servings___0']] == 1, 1, 0)
  intake_data['mac_serving4'] <- ifelse(intake_data[['fourth_servings___0']] == 1, 1, 0)

  intake_data['broccoli_serving2'] <- ifelse(intake_data[['second_servings___1']] == 1, 1, 0)
  intake_data['broccoli_serving3'] <- ifelse(intake_data[['third_servings___1']] == 1, 1, 0)
  intake_data['broccoli_serving4'] <- ifelse(intake_data[['fourth_servings___1']] == 1, 1, 0)

  intake_data['grapes_serving2'] <- ifelse(intake_data[['second_servings___2']] == 1, 1, 0)
  intake_data['grapes_serving3'] <- ifelse(intake_data[['third_servings___2']] == 1, 1, 0)
  intake_data['grapes_serving4'] <- ifelse(intake_data[['fourth_servings___2']] == 1, 1, 0)

  intake_data['carrots_serving2'] <- ifelse(intake_data[['second_servings___3']] == 1, 1, 0)
  intake_data['carrots_serving3'] <- ifelse(intake_data[['third_servings___3']] == 1, 1, 0)
  intake_data['carrots_serving4'] <- ifelse(intake_data[['fourth_servings___3']] == 1, 1, 0)

  intake_data['graham_serving2'] <- ifelse(intake_data[['second_servings___4']] == 1, 1, 0)
  intake_data['graham_serving3'] <- ifelse(intake_data[['third_servings___4']] == 1, 1, 0)
  intake_data['graham_serving4'] <- ifelse(intake_data[['fourth_servings___4']] == 1, 1, 0)

  intake_data['water_serving2'] <- ifelse(intake_data[['second_servings___5']] == 1, 1, 0)
  intake_data['water_serving3'] <- ifelse(intake_data[['third_servings___5']] == 1, 1, 0)
  intake_data['water_serving4'] <- ifelse(intake_data[['fourth_servings___5']] == 1, 1, 0)

  # remove old serving data
  intake_data <- intake_data[!grepl('___', names(intake_data))]

  #total values
  ## pre without plate
  intake_data['mac_pre_w_o_plate'] <- rowSums(intake_data[grepl('mac_pre_w_o_plate', names(intake_data))], na.rm = TRUE)

  intake_data['broccoli_pre_w_o_plate'] <- rowSums(intake_data[grepl('broccoli_pre_w_o_plate', names(intake_data))], na.rm = TRUE)

  intake_data['grapes_pre_w_o_plate'] <- rowSums(intake_data[grepl('grapes_pre_w_o_plate', names(intake_data))], na.rm = TRUE)

  intake_data['carrots_pre_w_o_plate'] <- rowSums(intake_data[grepl('carrots_pre_w_o_plate', names(intake_data))], na.rm = TRUE)

  intake_data['graham_pre_w_o_plate'] <- rowSums(intake_data[grepl('graham_pre_w_o_plate', names(intake_data))], na.rm = TRUE)

  intake_data['water_pre_w_o_plate'] <- rowSums(intake_data[grepl('water_pre_w_o_plate', names(intake_data))], na.rm = TRUE)

  ## pre with plate
  intake_data['mac_pre_w_plate'] <- rowSums(intake_data[grepl('mac_pre_w_plate', names(intake_data))], na.rm = TRUE)

  intake_data['broccoli_pre_w_plate'] <- rowSums(intake_data[grepl('broccoli_pre_w_plate', names(intake_data))], na.rm = TRUE)

  intake_data['grapes_pre_w_plate'] <- rowSums(intake_data[grepl('grapes_pre_w_plate', names(intake_data))], na.rm = TRUE)

  intake_data['carrots_pre_w_plate'] <- rowSums(intake_data[grepl('carrots_pre_w_plate', names(intake_data))], na.rm = TRUE)

  intake_data['graham_pre_w_plate'] <- rowSums(intake_data[grepl('graham_pre_w_plate', names(intake_data))], na.rm = TRUE)

  intake_data['water_pre_w_plate'] <- rowSums(intake_data[grepl('water_pre_w_plate', names(intake_data))], na.rm = TRUE)

  ## post with plate
  intake_data['mac_post_w_plate'] <- rowSums(intake_data[grepl('mac_post_w_plate', names(intake_data))], na.rm = TRUE)

  intake_data['broccoli_post_w_plate'] <- rowSums(intake_data[grepl('broccoli_post_w_plate', names(intake_data))], na.rm = TRUE)

  intake_data['grapes_post_w_plate'] <- rowSums(intake_data[grepl('grapes_post_w_plate', names(intake_data))], na.rm = TRUE)

  intake_data['carrots_post_w_plate'] <- rowSums(intake_data[grepl('carrots_post_w_plate', names(intake_data))], na.rm = TRUE)

  intake_data['graham_post_w_plate'] <- rowSums(intake_data[grepl('graham_post_w_plate', names(intake_data))], na.rm = TRUE)

  intake_data['water_post_w_plate'] <- rowSums(intake_data[grepl('water_post_w_plate', names(intake_data))], na.rm = TRUE)


  # return data
  return(intake_data)

}
