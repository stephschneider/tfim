\name{TFIM_calc}
\alias{TFIM_calc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Calculate the number of interceptions for a specific AQ station and year
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
TFIM_calc() downloads air quality data from the NAPS repository and transforms it into 6-hour averaged data points. Each average point starts at either 00h, 06h, 12h, or 18h UTC. For each 6-hour time average, the model runs 6 HYSPLIT back trajectories using NARR meteorology for the directed time. The number of fires within 0.5 degrees of the back trajectories are counted, and the FRP of the fires are averaged.
}
\usage{
TFIM_calc(NAPS_ID,
          duration,
          Year)
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
  \item{Year}{
  The year you are interested in downloading and processing the data for. The earliest year possible is 2001.
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
Code for the method outlined in 'Air Quality Data Approach for Defining Wildfire Influence: Impacts on PM2.5, NO2, CO and O3 in Western Canadian Cities'

DOI: https://pubs.acs.org/doi/10.1021/acs.est.1c04042

}
\author{
%%  ~~who you are~~
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
Tomahawk, Alberta in 2009 - 2019.

  TFIM_calc(91301, 72, 2009)

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
