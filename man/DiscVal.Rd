\name{DiscVal}
\alias{DiscVal}
\title{Discriminant Validity}
\usage{
DiscVal(Reliability,Lavaan_Model)
}
\description{
Takes as input a lavaan model CFA model and the associcated semTools::reliability() object to calculate discriminant validity as defined by Fornell & Larcker (1981). Variables are said to have discriminant validity if the square of their correlations is smaller than the average variance extracted (AVE) of each factor.
}
\examples{
HS.model = ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit = lavaan::cfa(HS.model, data = HolzingerSwineford1939)
reliablilty  = semTools::reliability(fit)
DiscVal(reliability,fit)

}
