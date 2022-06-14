#' test_params
#' @description Test Data! Data-sets include obs.csv; devices.csv;
#' weather.csv & intreventions.csv. Plus report params as txt file.
#' We will collect data-sets from real world tests as we go.
#' Example data-sets might be : \cr
#'   * All devices in data-set have no data.
#'   * Only 1-2 devices in data-set. Mimic home user.
#'   * Lots of devices. Lots of data. Mimic DHB data.
#'   * etc... Expand the cases as they arise.
#' @param folder folder located in inst/extdata/{folder}
#' @param params_file params txt file located at inst/extdata/params/{params_file}
#' @export
test_params <- function(data_folder = "monkey_a", params_file = "params_1.txt") {
  list(

    ## May only work in dev
    # obs           = paste0("inst/test-data/",   data_folder, "/obs.csv"),
    # weather       = paste0("inst/test-data/",   data_folder, "/weather.csv"),
    # dev           = paste0("inst/test-data/",   data_folder, "/dev.csv"),
    # interv        = paste0("inst/test-data/",   data_folder, "/interv.csv"),
    # params        = paste0("inst/test-params/", params_file),

    ## Using sysdata - production - USE THIS CODE!
    obs           = system.file("test-data", paste0(data_folder, "/obs.csv"),     package = "monkeyr"),
    weather       = system.file("test-data", paste0(data_folder, "/weather.csv"), package = "monkeyr"),
    dev           = system.file("test-data", paste0(data_folder, "/dev.csv"),     package = "monkeyr"),
    interv        = system.file("test-data", paste0(data_folder, "/interv.csv"),  package = "monkeyr"),
    params        = system.file("test-params", params_file,                       package = "monkeyr"),


    # replace these with text files with standard options.
    fromTimeStamp = 1627639151,  # not necessary
    toTimeStamp = 1630317551     # not necessary
  )
}
