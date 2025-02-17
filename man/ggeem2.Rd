% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{ggeem2}
\alias{ggeem2}
\title{Plot EEMs plots as contour plots}
\usage{
ggeem2(
  eem,
  manualscale = F,
  manualmax = 1.5,
  manualmin = 0,
  scalefunc = "linear",
  plot_type = "contour",
  nbins = 8,
  label_peaks = F,
  prec = 2,
  palette = "parula",
  z_unit = "RU",
  plot_title = FALSE
)
}
\arguments{
\item{eem}{an object of class eem or eemlist, the data you want to plot}

\item{manualscale}{a logical indicating if you want to specify the minimum and maximum of the intensity scale manually, good for comparisons across samples}

\item{manualmax}{maximum value for intensity scale}

\item{manualmin}{minimum value used for intensity scale}

\item{scalefunc}{function of how to scale the colors. log or linear}

\item{plot_type}{string specifying if the user wants a contour or raster plot. default "contour"}

\item{nbins}{the number of bins (and colors) used in the contour plot, maximum of 24}

\item{label_peaks}{a logical indicating if you want the main coble peaks annotated on the plot}

\item{prec}{an integer for the number of significant figures used for binning the intensity}

\item{palette}{a character with the color palette to use. Currently 'parula', 'ocean.haline', 'cubicl', and 'kovesi.rainbow' from the 'pals' package are supported (and custom 'katie_pal')}

\item{z_unit}{either "RU" or "DOC", use "RU" for raman normalized data and "DOC" for raman and DOC normalized data}

\item{plot_title}{logical - set to true if you want the plot title on the plot (mostly for interactive use)}
}
\value{
If class is eem, will return a single plot, if class is an eemlist will return a list of plots
}
\description{
Creates a pretty EEMs contour plot with the ability to manually set the intensity
scale or use an automated one based on the minimum and maximum of the EEM. Option to
add Coble peak labels to the plot (based off peaks from Coble et al. 2014).
}
