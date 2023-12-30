\name{TFIM_NAPS}
\alias{TFIM_NAPS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Runs the Trajectory-Fire Interception Method for NAPS stations
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
TFIM_NAPS() downloads air quality data from the NAPS repository and transforms it into 6-hour averaged data points. Each average point starts at either 00h, 06h, 12h, or 18h UTC.

For each 6-hour time average, the model runs 6 HYSPLIT back trajectories for the directed time. The number of fires within 0.5 degrees of the back trajectories are counted, and the FRP of the fires are averaged. The average interception time is calculated from all the FIRMS/HYSPLIT interceptions.

}
\usage{
TFIM_NAPS(NAPS_ID,
          duration,
          year,
          met)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{NAPS_ID}{
  The unique ID number associated with each AQ station. To see the details of each station, use NAPS_Station_Info()
%%     ~~Describe \code{x} here~~
}
  \item{duration}{
  The length in hours of the HYSPLIT back trajectory. Represented as an integer. The maximum time is 72 hours.
%%     ~~Describe \code{x} here~~
}
  \item{year}{
  The year you are interested in downloading and processing the data for. The earliest year possible is 2001.
%%     ~~Describe \code{x} here~~
}
\item{met}{
  The meteorology used for the HYSPLIT back-trajectory. The default value is narr for 2001-2019 (inclusive), and nam12 for 2020 - current.
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The NAM12 readme is available here: https://www.ready.noaa.gov/data/archives/nam12/README.TXT
The NARR readme is available here: https://www.ready.noaa.gov/data/archives/narr/README.TXT

As usual, always think about the purpose of your analysis when choosing the meteorology.
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
Code for the method outlined in 'Air Quality Data Approach for Defining Wildfire Influence: Impacts on PM2.5, NO2, CO and O3 in Western Canadian Cities'

DOI: https://pubs.acs.org/doi/10.1021/acs.est.1c04042

}
\author{
%%  ~~who you are~~
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
Downloading and processing the data from the air quality station in
Tomahawk, Alberta in 2009.

  TFIM_calc(91301, 72, 2009)

Downloading and processing the data from the air quality station in
Tomahawk, Alberta in 2020.

  TFIM_calc(91301, 72, 2020, met = "nam12")

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
