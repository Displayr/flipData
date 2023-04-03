% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimationdata.R
\name{EstimationDataTemplate}
\alias{EstimationDataTemplate}
\title{Extract dataframe information for a template}
\usage{
EstimationDataTemplate(x)
}
\arguments{
\item{x}{A \code{data.frame} containing the data to be templated.}
}
\value{
A list of lists. Each sublist contains information about each variable from the input
        data frame. Each sublist describes the variable with the following elements:
\itemize{
   \item type: The type of the variable (numeric or factor)
   \item label: The label of the variable
   \item name: The name of the variable
   \item questiontype: The question type of the variable
   \item question: The question of the variable
   \item dataset: The dataset of the variable
   \item levels: If variable is a factor, the levels of the variable, otherwise not present
   \item observed.levels: If variable is a factor, the observed levels of the variable, otherwise not present
   \item has.unobserved.levels: Boolean indicating whether the variable has unobserved levels
   \item ordered: If variable is a factor, whether the factor is ordered, otherwise not present
}
}
\description{
Inspects the input dataframe and creates a list of lists. Each list element takes the name
             of the variable and contains information about the variable with a sublist. Such information
             includes the type of the variable (numeric or factor), and other metadata attributes such as
             the label of the variable, the name of the variable, question type of the variable,
             the dataset it originates from and others.
}