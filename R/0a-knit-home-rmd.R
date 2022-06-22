#' Run a Test Report
#'
#' @description Test Report Locally with Dummy Test Data
#' and Dummy Test Parameters. Uses library of test cases. Run this function first
#' to ensure everything is working properly and to see an example of the output.
#' Please note that the pameters for test reports are preset.
#'
#' @param report The report name
#' @param dummy_data_dir The Dummy data set which can be considered as a test case
#' @param dummy_params_file The Dummy parameter set - rmd params into report
#'
#' @return an html document printed out to a file and the path of the output file is returned
#'
#' @examples
#' # run_test_report("home", "monkey_a", "params_1.txt")
#' # run_test_report("flexi", "org_full", "map-room.txt")
#' # run_test_report("full", "org_corner1", "map-tenure.txt")
#' # run_test_report("hhi", "org_corner2", "map.txt")
#'
#' @export
run_test_report <- function (
    report            = "home",
    dummy_data_dir    = "monkey_a",
    dummy_params_file = "params_1.txt"
) {

  ## Get test file paths from folder name
  test_params <- make_test_params(
    data_folder = dummy_data_dir,
    params_file = dummy_params_file
  )

  # build params list (is this necessary?)
  par_list <- monkeyr::make_params_list(
    filtered_obs           = test_params$obs,
    filtered_devices       = test_params$dev,
    filtered_weather       = test_params$weather,
    filtered_interventions = test_params$interv,
    fromTimeStamp          = test_params$fromTimeStamp,
    toTimeStamp            = test_params$toTimeStamp
  )

  # run the rmd report
  monkeyr::knit_home_report(
    param_list  = par_list,
    output_dir  = report,
    output_file = dummy_data_dir
  )
}


#' Make a list of parameters for the report
#'
#' @description Create a list of parameters for building the report. Params determine the data and time
#' period used in each report. Review the details below for a list of fields to pass as parameters
#'
#' @param .list A named list of key-value pairs
#' @param ... Unquoted key-value pairs
#'
#' @details Field options are:\cr
#'   * filtered_obs (a dataframe or filepath to a csv),
#'   * filtered_weather (a dataframe or filepath to a csv),
#'   * filtered_devices (a dataframe or filepath to a csv),
#'   * filtered_interventions (a dataframe or filepath to a csv),
#'   * report_params,
#'   * fromTimeStamp,
#'   * toTimeStamp
#'
#' @return A list of RMD parameters that may be passed to a report generating function
#'
#' @examples
#' # convenient for interactive typing
#' make_params_list(filtered_obs = "inst/data/filtered_obs.csv")
#' # convenient for programming
#' make_params_list(.list = list(filtered_obs = "inst/data/filtered_obs.csv"))
#'
#' @export
make_params_list <- function(.list = NULL, ...) {
  fields <- c(
    "filtered_obs",
    "filtered_weather",
    "filtered_devices",
    "filtered_interventions",
    "report_params",
    "fromTimeStamp",
    "toTimeStamp"
  )
  lapply(names(.list), checkmate::assert_choice, unlist(fields))
  lapply(names(list(...)), checkmate::assert_choice, unlist(fields))
  c(.list, list(...))
}



#' Compile the home report
#'
#' @description Create a home report with a given set of pameters.
#'
#' @param param_list named list of parameters, see `?make_param_list`
#' @param output_dir path to folder to save the html (default: current work dir)
#' @param output_file name of the output html (default: home-{timestamp}.html)
#' @param ... additional named arguments passed onto `rmarkdown::render`
#'
#' @return a compiled document written into the output file and the path of the output file is returned
#'
#' @examples
#' obs  <- system.file("data/filtered_obs.csv", package="monkeytronics.reports")
#' dev  <- system.file("data/filtered_devices.csv", package="monkeytronics.reports")
#' wea  <- system.file("data/filtered_weather.csv", package="monkeytronics.reports")
#' int  <- system.file("data/filtered_interventions.csv", package="monkeytronics.reports")
#' from <- 1627639151
#' to   <- 1630317551
#'
#' par_list <- make_params_list(
#'   filtered_obs = obs,
#'   filtered_devices = dev,
#'   filtered_weather = wea,
#'   filtered_interventions = int,
#'   fromTimeStamp = from,
#'   toTimeStamp = to
#' )
#' knit_home_report(par_list)
#'
#' \dontrun{
#' # using the database
#' # assumes DB credentials are set as environmental variables
#' # see `?dynamo_connect`
#' dev <- dynamo_query(
#' .con = dynamo_connect(),
#' .table = "sn-v1-devices",
#' .partition_key = "device_owner",
#' .partition_value = "al@monkeytronics.co.nz"
#' )
#'
#' int <- dynamo_query(
#'   .con = dynamo_connect(),
#'   .table = "sn-v1-interventions",
#'   .partition_key = "device_id",
#'   .partition_value = "W000011"
#' )
#'
#' wea <- dynamo_query(
#'   .con = dynamo_connect(),
#'   .table = "sn-v1-weather-db",
#'   .partition_key = "city",
#'   .partition_value = "Wellington"
#' )
#'
#' obs <- dynamo_query(
#'   .con = dynamo_connect(),
#'   .table = "SensorNodeData",
#'   .partition_key = "device_id",
#'   .partition_value = "W000011"
#' )
#' from <- 1627639151
#' to   <- 1630317551
#'
#' par_list <- make_params_list(
#'   filtered_obs = obs,
#'   filtered_devices = dev,
#'   filtered_weather = wea,
#'   filtered_interventions = int,
#'   fromTimeStamp = from,
#'   toTimeStamp = to
#' )
#' knit_home_report(par_list)
#'
#' }
#'
#' @export
knit_home_report <-
  function(param_list,
           output_file = paste0("home-", as.numeric(Sys.time()), ".html"),
           output_dir = getwd(),
           ...
           ) {
    ## Get directory of report markdown template
    report_rmd <-
      system.file("/rmd/", "home.rmd", package = "monkeyr")

    ## Render report into html
    rmarkdown::render(
      input = report_rmd,
      params = param_list,
      output_file = output_file,
      output_dir  = paste0("html/", output_dir),
      ...
    )
  }
