% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reportDemand.R
\name{reportDemand}
\alias{reportDemand}
\title{Read in GDX and calculate (net and gross) annual demand, used in convGDX2MIF.R for the reporting}
\usage{
reportDemand(gdx)
}
\arguments{
\item{gdx}{a GDX object as created by readGDX, or the path to a gdx}
}
\value{
MAgPIE object - contains the capacity variables
}
\description{
Read in (net and gross) demand data from GDX file, information used in convGDX2MIF.R
for the reporting
}
\examples{

\dontrun{reportDemand(gdx)}

}
\seealso{
\code{\link{convGDX2MIF}}
}
\author{
Sebastian Osorio, Renato Rodrigues
}