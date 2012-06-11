\name{parDEoptim}
\alias{parDEoptim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Parallelized Differential Evolution
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Parallel Differential Evolution
}
\usage{
parDEoptim(fn, lower, upper, n=1, control = DEoptim.control(), .packages = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fn}{
%%     ~~Describe \code{fn} here~~
}
  \item{lower}{
%%     ~~Describe \code{lower} here~~
}
  \item{upper}{
%%     ~~Describe \code{upper} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
Number of segments to split the optimization range into.  Upper and lower define this range.
}
  \item{control}{
%%     ~~Describe \code{control} here~~
}
  \item{.packages}{
%%     ~~Describe \code{.packages} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
  Differential Evolution homepage:
  URL \url{http://www.icsi.berkeley.edu/~storn/code.html}.

  Ardia, D., Boudt, K., Carl, P., Mullen, K.M., Peterson, B.G. (2011)
  Differential Evolution with \pkg{DEoptim}. An Application to Non-Convex Portfolio Optimization. 
  \emph{The R Journal}, 3(1), 27-34. 
  URL \url{http://journal.r-project.org/2011-1/}.

  Ardia, D., Ospina Arango, J.D., Giraldo Gomez, N.D. (2011)
  Jump-Diffusion Calibration using Differential Evolution. 
  \emph{Wilmott Magazine}, 55 (September), 76-79.
  URL \url{http://www.wilmott.com}.
  
  Mitchell, M. (1998) 
  \emph{An Introduction to Genetic Algorithms}.
  The MIT Press. ISBN 0262631857.

  Mullen, K.M, Ardia, D., Gil, D., Windover, D., Cline,
  J. (2011). \pkg{DEoptim:} An R Package for Global Optimization by
  Differential Evolution. 
  \emph{Journal of Statistical Software}, 40(6), 1-26.
  URL \url{http://www.jstatsoft.org/v40/i06/}.

  Price, K.V., Storn, R.M., Lampinen J.A. (2006)
  \emph{Differential Evolution - A Practical Approach to Global Optimization}.
  Berlin Heidelberg: Springer-Verlag. ISBN 3540209506.

  Storn, R. and Price, K. (1997) 
  Differential Evolution -- A Simple and Efficient Heuristic for Global Optimization over Continuous Spaces,
  \emph{Journal of Global Optimization}, 11:4, 341--359.  
}

\author{
%%  ~~who you are~~
Zach Mayer \email{zach.mayer@gmail.com}
}

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{DEoptim}}
\code{\link{DEoptim.control}}
\code{\link{DEoptim-methods}}
}
\examples{

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
