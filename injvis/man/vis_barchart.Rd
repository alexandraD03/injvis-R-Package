% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vis_barchart.R
\name{vis_barchart}
\alias{vis_barchart}
\title{Barchart of sports injuries per tissue and pathology type}
\usage{
vis_barchart(data, colourblind_friendly = FALSE, colourOption = "A")
}
\arguments{
\item{data}{Sports injury data, with columns Tissue, Pathology.Type and Frequency}

\item{colourblind_friendly}{Boolean variable indicating whether to use a colourblind friendly colour palette}

\item{colourOption}{Option to choose which colourblind friendly palette from the package viridis, either "A", "B", "C", "E", "F", "G" or "H" (Default is "A")}
}
\value{
Coloured barchart of sports injury data at tissue and pathology type level
}
\description{
Generate a bar chart to show the frequency of sports injuries per tissue and pathology type.
}
\examples{
vis_barchart(injuryTissuePathTable)
vis_barchart(injuryTissuePathTable, colourblind = TRUE)
vis_barchart(injuryTissuePathTable, colourblind = TRUE, colourOption = "D")

}
