\name{Import_FIRMS}
\alias{Import_FIRMS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Loads satellite data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Imports the correct FIRMS (either MODIS or VIIRS satellite) data into RStudio.
MODIS: filters for confidence over 30%
VIIRS: filters for nominal or high confidence (removes low confidence data points)

}
\usage{
Import_FIRMS(satellite, NameofFile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{NameofFile}{
  Defaults to "FIRMS_data", but can receive other names
%%     ~~Describe \code{x} here~~
}
  \item{satellite}{
  specify which satellite program (MODIS or VIIRS) contained within the file. Treats the data according to each dataset.
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
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
