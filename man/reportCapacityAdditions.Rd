% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reportCapacityAdditions.R
\name{reportCapacityAdditions}
\alias{reportCapacityAdditions}
\title{Read in GDX and calculate capacity additions, used in convGDX2MIF.R for the reporting}
\usage{
reportCapacityAdditions(gdx)
}
\arguments{
\item{gdx}{a GDX object as created by readGDX, or the path to a gdx}
}
\value{
MAgPIE object - contains the capacity variables
}
\description{
Read in capacity additions from GDX file, information used in convGDX2MIF.R
for the reporting
}
\examples{

\dontrun{reportCapacityAdditions(gdx)}

}
\seealso{
\code{\link{convGDX2MIF}}
}
\author{
Sebastian Osorio
}
