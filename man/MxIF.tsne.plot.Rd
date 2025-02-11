% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MxIF_tSNE_plot.R
\name{MxIF.tsne.plot}
\alias{MxIF.tsne.plot}
\title{MxIF tSNE plot}
\usage{
MxIF.tsne.plot(data, marker, colors = "colorblind", sz = 1.5,
  highQ = 0.98, lowQ = 0.02, legend = FALSE)
}
\arguments{
\item{data}{data frame of cell data from Matlab based segmentation}

\item{marker}{column to display}

\item{colors}{what colors to use, default is "colorblind" friendly, can also choose "threecolor" black-red-blue, or set your own}

\item{sz, }{what size dot to display, default=1.5}

\item{highQ}{max percentile default=.98}

\item{lowQ}{max percentile default=.02}

\item{legend}{whether to show legend, default=FALSE}
}
\description{
This function plots tSNE results. N.B. column names for plotting must be tSNE1 and tSNE2
Eliot McKinley 9/15/17
}
\examples{
MxIF.tsne.plot(data, 3)
MxIF.tsne.plot(data, 3, colors="threecolor")
MxIF.tsne.plot(data, 3, colors=c("#000000","#98EF56"))
MxIF.tsne.plot(data, 3, colors=c("#000000","#98EF56"), sz=2, highQ=0.95, lowQ=0.05)
}
\keyword{MxIF}
\keyword{tSNE}
