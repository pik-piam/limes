% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reportHourAvFactor.R
\name{reportHourAvFactor}
\alias{reportHourAvFactor}
\title{Read in GDX and report the max availability factor per hour (for nonRES), used in convGDX2MIF.R for the reporting}
\usage{
reportHourAvFactor(gdx)
}
\arguments{
\item{gdx}{a GDX object as created by readGDX, or the path to a gdx}
}
\value{
MAgPIE object - contains the max availability factors per hour (for nonRES)
}
\description{
Read in max availability factors per hour (for nonRES) information from GDX file, information used in convGDX2MIF.R
for the reporting
}
\examples{

\dontrun{reportHourAvFactor(gdx)}

}
\seealso{
\code{\link{convGDX2MIF}}
}
\author{
Sebastian Osorio, Renato Rodrigues
}
