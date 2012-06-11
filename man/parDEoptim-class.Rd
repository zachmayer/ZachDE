\name{parDEoptim-class}
\docType{class}
\alias{parDEoptim-class}
\title{Class "parDEoptim"}
\description{The results of a parallel differential evolution optimization}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("parDEoptim", ...)}
  or, more commonly, by a call to \code{\link{parDEoptim}}.
}
\section{Slots}{
  \describe{
    \item{member}{Object of class \code{"list"}, a list of
      the members of the evolved population}
    \item{optim}{Object of class \code{"list"}, the details on the optimal member of the population}
  }
}
\keyword{classes}
