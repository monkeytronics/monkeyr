% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0c-options.R
\name{target_var_list}
\alias{target_var_list}
\title{target_var_list}
\usage{
target_var_list(wrangled_devices, param_list)
}
\arguments{
\item{wrangled_devices}{data.frame, as output by \code{wrangled_devices}}

\item{param_list}{list, report parameters from param_list. The ones we want will take the form
'{param} anlys' - for instance 'room_type anlys'}
}
\description{
matches requested report params to available segments for reporting
}
