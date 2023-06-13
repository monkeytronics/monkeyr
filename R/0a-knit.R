#' Run a Test Report
#'
#' @description Test Report Locally with Dummy Test Data
#' and Dummy Test Parameters. Uses library of test cases. Run this function first
#' to ensure everything is working properly and to see an example of the output.
#' Please note that the parameters for test reports are preset.
#'
#' `Special Case 'Customer'` The idea is to copy the entire stash from a customer report
#' including the args.csv. These parameters are pulled to exactly replicate the cloud run.
#'
#' Remember to rebuild the library after changing the dummy data or it won't work!
#'
#' `How To Use Special Case` Only available on Admin accounts, inside the `s3_report_stash`
#' folder, run `sync_s3_stash.bash`. Then find the test case you're interested in. Copy files
#' into customer folder in inst/dummy_data, then run this function.
#'
#' @param report The report name
#' @param dummy_data The Dummy data set which can be considered as a test case
#' @param dummy_params The Dummy parameter set - rmd params into report
#'
#' @return an html document printed out to a file and the path of the output file is returned
#'
#' @examples
#' run_test_report("home", "customer",    "params_blank.txt")
#' run_test_report("full", "customer",    "params_blank.txt")
#' run_test_report("full", "monkey_32",   "params_full_map.txt")
#' run_test_report("home", "monkey_32",   "params_blank.txt")
#'
#' @export
run_test_report <- function (
    report       = "full",
    dummy_data   = "customer",
    dummy_params = "params_full_all.txt"
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
    report_params          = test_params$params,
    # report_params          = "intro,intervention anlys,show deployment map,direction anlys,room anlys,room_type anlys,tenure anlys,direction anlys",
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
    dummy_params = "params_blank.txt",
    ...) {

  ## Pull customer info out of args file
  args_file = system.file("dummy-data", paste0(dummy_data, "/args.csv"), package = "monkeyr")
  args <- readr::read_csv(args_file, col_types = "iic")


  ## Replicate Customer Report - NB - we save params / args with '-' separator for csv. Decode here.
  if (dummy_data == "customer") {
    params <- args$report_params[1]
    params <- stri_replace_all_fixed(params, '-', ',')
  } else {
    params = system.file("dummy-params", dummy_params, package = "monkeyr")
  }


  ## params object to use!
  monkey_knit_msg(msg = paste0("params = ", params), resource="make_test_params")
  list(
    obs           = system.file("dummy-data", paste0(dummy_data, "/obs.csv"),     package = "monkeyr"),
    weather       = system.file("dummy-data", paste0(dummy_data, "/weather.csv"), package = "monkeyr"),
    dev           = system.file("dummy-data", paste0(dummy_data, "/dev.csv"),     package = "monkeyr"),
    interv        = system.file("dummy-data", paste0(dummy_data, "/interv.csv"),  package = "monkeyr"),
    params        = params, ## See above
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
#'   filtered_weather = weather,
#'   filtered_interventions = interv,
#'   params = report_params,
#'   fromTimeStamp = from,
#'   toTimeStamp = to
#' ) {
#' knit_report(par_list)
#'
#' }
#'
#' @export
knit_report <-
  function(
    param_list,
    output_file = paste0("home-", as.numeric(Sys.time()), ".html"),
    output_dir  = getwd(),
    rmd_file    = "home.rmd",
    ...
  ) {
    ## Get directory of report markdown template
    report_rmd <- system.file("/rmd/", rmd_file, package = "monkeyr")

    ## Render report into html
    rmarkdown::render(
      input       = report_rmd,
      params      = param_list,
      output_file = output_file,
      output_dir  = output_dir,
      ...
    )
  }


#' monkey_error
#'
#' @description R Code crashed. Need to print out message to: \cr
#'   * to html report
#'   * console / terminal
#'   * render
#'
#' @param err error message to report before knitr exits
#' @param resource the function or chunk name where it's called
#'
#' @examples
#' monkey_knit_error(err = "Error Message", resource="CHUNK_NAME")
#'
#' @export
monkey_knit_error <- function(
    err      = "err message",
    resource = "failing r function",
    ...) {
  msg = paste0("error caught in ", resource, " = ", err, "\n")
  cat(paste0(msg, "\n\n"))
  logger::log_debug(msg)
  ls_result <- ls()
  message(paste0("\nMESSAGE OUT : ls() = ", msg))
  knitr::knit_exit("</body></html>")
}


#' monkey_knit_msg
#'
#' @description Debug : print out message to: \cr
#'   * to html report
#'   * console / terminal
#'   * render
#'
#' @param msg string to holler out everywhere
#' @param resource the function or chunk name where it's called
#'
#' @examples
#' monkey_knit_msg(msg = "Thing to output everywhere", resource="CHUNK_NAME")
#' monkey_knit_msg(msg = "Debugging stuff locally", resource="function name etc.", debug = TRUE)
#'
#' @export
monkey_knit_msg <- function(
    msg   = "message",
    resource = "failing r function",
    debug = FALSE,
    ...) {
  msg = paste0("msg from inside of ", resource, " = ", msg, "\n")

  logger::log_debug(msg)
  ls_result <- ls()
  message(paste0("\nMESSAGE OUT : ls() = ", msg))

  if (debug == TRUE) {
    cat(paste0(msg, "\n"))
  }
}


