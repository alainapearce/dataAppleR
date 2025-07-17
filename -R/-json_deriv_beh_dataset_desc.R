#' json_deriv_beh_dataset_desc: Generate dataset_description JSON for beh derivatives
#'
#' This function generates a json file for derivatives/dataREACHr
#'
#' @return A string with data stored in JSON format containing meta-data for beh summary datasets derived by dataREACHr
#'
#'
#' @export

json_deriv_beh_dataset_desc <- function() {

  description_list <- list(
    Name = 'Compiled and summary datasets for behavioral tasks',
    DatasetType = 'Derivative',
    GeneratedBy = list( Name = "dataREACHr",
                        Version = paste0("dataREACHr is not versioned. Datasets were generated on ", Sys.Date(), " using the code from the GitHub repository at that time. Thus, this date serves as a reference point for the specific code version employed."),
                        #Version = packageVersion("datareachr"),
                        Description = "Datasets were generated using dataREACHr functions: deriv_rrv.R, json_deriv_rrv.R",
                        URL = "https://github.com/bfuchs18/dataREACHr")
    )

  # convert formatting to JSON
  description_json <- RJSONIO::toJSON(description_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(description_json, asText = TRUE))){
    print('JSON file may be invalid')
  }

  return(description_json)

}
