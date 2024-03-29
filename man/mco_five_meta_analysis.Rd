% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_mco_five_meta_analysis.R
\name{mco_five_meta_analysis}
\alias{mco_five_meta_analysis}
\title{mco_five_meta_analysis}
\usage{
mco_five_meta_analysis(
  data1 = NULL,
  data2 = NULL,
  data3 = NULL,
  data4 = NULL,
  data5 = NULL,
  NF,
  measurePM_m = NULL,
  q = NULL
)
}
\arguments{
\item{data1}{Gene expression data set.Gene expression data set.}

\item{data2}{Gene expression data set.Gene expression data set.}

\item{data3}{Gene expression data set.Gene expression data set.}

\item{data4}{Gene expression data set.Gene expression data set.}

\item{data5}{Gene expression data set.Gene expression data set.}

\item{NF}{Number of frontiers Pareto-efficient to calculate.}

\item{measurePM_m}{Three performance measures (1 = mean, 2 = median, 3 = quantail).}

\item{q}{if measurePM_m = 3, q take the value of the quantile, for examnple q1 = 75.}
}
\value{
A list with 2 elements.
\itemize{
\item \code{GeneFrontier} is a genes list with maximum expression changes belonging to each Pareto efficient frontier.
\item \code{PlotFrontier} is a 3D plot of the Pareto efficient frontiers with the maximum expression genes.

}
}
\description{
This function models two macroarray or RNA-seq gene expression datasets,
generating a list of genes with maximal changes in expression.
}
\examples{
# Example5: El siguiente ejemplo...
#' m1 <- mco_five_meta_analysis(data1 = dataExample1,
                       data2 = dataExample2,
                       data3 = dataExample3,
                       data4 = dataExample4,
                       data5 = dataExample5,
                       NF = 3,
                       measurePM_m = 1,
                       q = NULL)


}
\references{
[1] Camacho-Cáceres, K. I. et al. Multiple criteria optimization joint analyses of microarray experiments in lung cancer: from existing microarray data to new knowledge. Cancer Med. 4, 1884–1900 (2015).
            [2] Narváez-Bandera, I., Suárez-Gómez, D., Isaza, C. E. & Cabrera-Ríos, M. Multiple Criteria Optimization (MCO): A gene selection deterministic tool in RStudio. PLoS One 17, e0262890 (2022).
}
\author{
Deiver Suárez-Gómez [aut],
        Isis Narváez-Bandera [aut],
        Clara E. Isaza [aut],
        Mauricio Cabrera-Ríos [aut]
}
