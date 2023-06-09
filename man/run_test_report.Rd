% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0a-knit.R
\name{run_test_report}
\alias{run_test_report}
\title{Run a Test Report}
\usage{
run_test_report(
  report = "home",
  dummy_data = "monkey_a",
  dummy_params = "params_blank.txt"
)
}
\arguments{
\item{report}{The report name}

\item{dummy_data}{The Dummy data set which can be considered as a test case}

\item{dummy_params}{The Dummy parameter set - rmd params into report}
}
\value{
an html document printed out to a file and the path of the output file is returned
}
\description{
Test Report Locally with Dummy Test Data
and Dummy Test Parameters. Uses library of test cases. Run this function first
to ensure everything is working properly and to see an example of the output.
Please note that the parameters for test reports are preset.

\verb{Special Case 'Customer'} The idea is to copy the entire stash from a customer report
including the args.csv. These parameters are pulled to exactly replicate the cloud run.

\verb{How To Use Special Case} Only available on Admin accounts, inside the \code{s3_report_stash}
folder, run \code{sync_s3_stash.bash}. Then find the test case you're interested in. Copy files
into customer folder in inst/dummy_data, then run this function.
}
\examples{
run_test_report("home", "customer",    "params_blank.txt")
run_test_report("full", "customer",    "params_blank.txt")
run_test_report("full", "monkey_32",   "params_full_map.txt")
run_test_report("home", "monkey_32",   "params_blank.txt")

}
