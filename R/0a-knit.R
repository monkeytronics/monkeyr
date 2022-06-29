#' Run a Test Report
#'
#' @description Test Report Locally with Dummy Test Data
#' and Dummy Test Parameters. Uses library of test cases. Run this function first
#' to ensure everything is working properly and to see an example of the output.
#' Please note that the pameters for test reports are preset.
#'
#' @param report The report name
#' @param dummy_data The Dummy data set which can be considered as a test case
#' @param dummy_params The Dummy parameter set - rmd params into report
#'
#' @return an html document printed out to a file and the path of the output file is returned
#'
#' @examples
#' # run_test_report("home", "monkey_a",    "params_1.txt")
#' # run_test_report("hhi",  "org_corner2", "map.txt")
#'
#' @export
run_test_report <- function (
    report       = "hhi",
    dummy_data   = "monkey_a",
    dummy_params = "params_1.txt"
) {

  ## Get test file paths from folder name
  test_params <- monkeyr::make_test_params(
    dummy_data   = dummy_data,
    dummy_params = dummy_params
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
  monkeyr::knit_report(
    param_list  = par_list,
    output_dir  = paste0("html/", report),
    output_file = dummy_data,
    rmd_file    = paste0(report, ".rmd")
  )
}


#' Create a List of Parameters to Test
#'
#' @description Test Data! Data-sets include obs.csv; devices.csv;
#' weather.csv & intreventions.csv. Plus report params as txt file.
#' We will collect data-sets from real world tests as we go.
#' Example data-sets might be : \cr
#'   * All devices in data-set have no data.
#'   * Only 1-2 devices in data-set. Mimic home user.
#'   * Lots of devices. Lots of data. Mimic DHB data.
#'   * etc... Expand the cases as they arise.
#'
#' @param folder folder located in inst/extdata/{folder}
#' @param params_file params txt file located at inst/extdata/params/{params_file}
#'
#' @return a list of parameters that may be used for generating a report
#'
#' @export
make_test_params <- function(
    dummy_data   = "monkey_a",
    dummy_params = "params_1.txt",
    ...) {

  ## Pull Timestamps out of args file
  # dummy_data   = "moe_1"
  args_file = system.file("dummy-data", paste0(dummy_data, "/args.csv"), package = "monkeyr")
  args <- readr::read_csv(args_file, col_types = "iic")

  list(

    ## May only work in dev
    # obs           = paste0("inst/dummy-data/",   dummy_data, "/obs.csv"),
    # weather       = paste0("inst/dummy-data/",   dummy_data, "/weather.csv"),
    # dev           = paste0("inst/dummy-data/",   dummy_data, "/dev.csv"),
    # interv        = paste0("inst/dummy-data/",   dummy_data, "/interv.csv"),
    # params        = paste0("inst/dummy_params/", dummy_params),

    ## Using sysdata - production - USE THIS CODE!
    obs           = system.file("dummy-data", paste0(dummy_data, "/obs.csv"),     package = "monkeyr"),
    weather       = system.file("dummy-data", paste0(dummy_data, "/weather.csv"), package = "monkeyr"),
    dev           = system.file("dummy-data", paste0(dummy_data, "/dev.csv"),     package = "monkeyr"),
    interv        = system.file("dummy-data", paste0(dummy_data, "/interv.csv"),  package = "monkeyr"),
    params        = system.file("dummy-params", dummy_params,                     package = "monkeyr"),

    # replace these with text files with standard options.
    fromTimeStamp = args$fromTimeStamp[1],
    toTimeStamp   = args$toTimeStamp[1]
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
#' obs  <- system.file("data/filtered_obs.csv", package="monkeyr")
#' dev  <- system.file("data/filtered_devices.csv", package="monkeyr")
#' wea  <- system.file("data/filtered_weather.csv", package="monkeyr")
#' int  <- system.file("data/filtered_interventions.csv", package="monkeyr")
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
#' ) {
#' knit_report(par_list)
#'
#' }
#'
#' @export
knit_report <-
  function(param_list,
           output_file = paste0("home-", as.numeric(Sys.time()), ".html"),
           output_dir  = getwd(),
           rmd_file    = "home.rmd",
           ...
           ) {
    ## Get directory of report markdown template
    report_rmd <-
      system.file("/rmd/", rmd_file, package = "monkeyr")

    ## Render report into html
    rmarkdown::render(
      input  = report_rmd,
      params = param_list,
      output_file = output_file,
      output_dir  = output_dir,
      ...
    )
  }
