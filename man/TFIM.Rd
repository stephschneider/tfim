\name{TFIM}
\alias{TFIM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Runs the Trajectory-Fire Interception Method for specified latitude and longitude
}
\description{
A more specific version of TFIM_NAPS() where the user can specify the dates and the location of the function.

For each 6-hour time average, the model runs 6 HYSPLIT back trajectories for the directed time. The number of fires within 0.5 degrees of the back trajectories are counted, and the FRP of the fires are averaged. The average interception time is calculated from all the FIRMS/HYSPLIT interceptions.


}
\usage{
TFIM(lat, lon, duration, start_date, end_date, met = NULL, satellite = NULL, location = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{
latitude of start location (numeric)
}
  \item{lon}{
longitude of start location (numeric)
}
  \item{start_date}{
string of start date in format "Year/Month/Day"
}
  \item{end_date}{
string of end date in format "Year/Month/Day"
}
 \item{duration}{
  The length in hours of the HYSPLIT back trajectory. Represented as an integer. The maximum time is 72 hours.
%%     ~~Describe \code{x} here~~
}
\item{met}{
  The meteorology used for the HYSPLIT back-trajectory. The default value is narr for 2001-2019 (inclusive), and nam12 for 2020 - current.
%%     ~~Describe \code{x} here~~
}
\item{satellite}{
Can specify which satellite was used for the FIRMS data, will be included in the output file name
%%     ~~Describe \code{x} here~~
}
\item{location}{
Can specify a location name (or anything else) that will be included in the output file name
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Schneider, S.R.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

This will output the interception data for a location in Kamloops, BC, Canada in April 2023.

TFIM(lat = 50.673092,
     lon = -120.324173,
     duration = 72,
     start_date = "2023/04/01",
     end_date = "2023/04/30",
     met = "nam12")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
