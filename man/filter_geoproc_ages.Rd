% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{filter_geoproc_ages}
\alias{filter_geoproc_ages}
\title{filter geoproc by age}
\usage{
filter_geoproc_ages(
  df,
  myr = 2.5,
  age.list = c("PREHISTORIC", "HISTORIC", "HISTORICAL", "RECENT", "QUATERNARY",
    "HOLOCENE", "PLEISTOCENE", "PLIOCENE-PLEISTOCENE", "PLIO-QUATERNARY", "INTERGLACIAL",
    "POSTGLACIAL", "GLACIAL", "PREGLACIAL", "PLINIAN", "MEDIAVAL", "MEDIEVAL"),
  add = NULL
)
}
\arguments{
\item{myr}{upper age limit in millionyears}

\item{age.list}{filter by categories in age list}

\item{add}{additional age categories}
}
\value{

}
\description{
filter geoproc by age
}
