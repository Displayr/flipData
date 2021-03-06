% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stacking.R
\name{StackData}
\alias{StackData}
\title{Stack data set}
\usage{
StackData(
  input.data.set.name,
  stacked.data.set.name = NULL,
  stack.with.common.labels = "Automatically",
  reference.variables.to.stack = NULL,
  manual.common.labels = NULL,
  specify.by = "Variable",
  manual.stacking = NULL,
  variables.to.include = NULL,
  include.stacked.data.set.in.output = FALSE,
  include.original.case.variable = TRUE,
  include.observation.variable = TRUE
)
}
\arguments{
\item{input.data.set.name}{Name of data file to stack, either as a path to a
local file (when running locally in R) or file in the Displayr Cloud Drive
(when running in Displayr).}

\item{stacked.data.set.name}{Name of the stacked data file to be saved in
the Displayr Cloud Drive (if run from Displayr) or saved locally.}

\item{stack.with.common.labels}{\code{"Automatically"},
\code{"Using a set of variables to stack as reference"},
\code{"Using manually input common labels"},
\code{"Disabled"}.}

\item{reference.variables.to.stack}{A character vector of sets of variables
to stack to be used as a reference to generate the common labels used for
stacking. This can be a combination of comma-separated names, wildcards
and ranges. Variable ranges can be specified by supplying the start and
end variables separated by a dash '-'. If the start or end variables are
left out, then the range is assumed to start from the first variable or
end at the last variable respectively. Wildcards in variable names can be
specified with an asterisk '*'.}

\item{manual.common.labels}{A list of sets of common labels to be used to
identify variables to stack. Only used when \code{stack.with.common.labels}
is \code{"Using manually input common labels"}. To be identified, a set of
variables to be stacked must contain these labels, and have the same prefix
and suffix before and after these labels.}

\item{specify.by}{"Variable" or "Observation". See \code{manual.stacking}.}

\item{manual.stacking}{If \code{specify.by} is "Variable", this is a
character vector where each string corresponds to the names of the
variables to be stacked into a new variable. If \code{specify.by} is
"Observation", this is a character vector where each string corresponds
the names of the variables in an observation in the stacked variables.
The strings in both cases can be a combination of comma-separated names,
wildcards and ranges.
See \code{reference.variables.to.stack} for more details on the format.}

\item{variables.to.include}{Character vector of comma-separated names of
non-stacked variables to include. Each string can be a combination of
comma-separated names, wildcards and ranges.
See \code{reference.variables.to.stack} for more details on the format.}

\item{include.stacked.data.set.in.output}{Whether to include the stacked
data set in the output object.}

\item{include.original.case.variable}{Whether to include the \code{original_case}
variable in the stacked data set.}

\item{include.observation.variable}{Whether to include the \code{observation}
variable in the stacked data set.}
}
\value{
A list with the following elements:
\itemize{
  \item \code{stacked.data.set.metadata} A list containing metadata on the
    the stacked data set such as variable names, labels etc.
  \item \code{unstackable.names} A list of character vectors containing
    names of the variables that could not be stacked using common labels due
    to mismatching types or value attributes.
  \item \code{common.labels.list} A list of character vectors containing
    the common labels used in the stacking. The source of these common
    labels depends on the parameter \code{stack.with.common.labels} and
    are either automatically generated, extracted from reference variables
    or manually supplied.
  \item \code{is.saved.to.cloud} Whether the stacked data set was saved to
    the Displayr cloud drive.
 }
}
\description{
Stacks variables in a SPSS .sav data set that may be located
  locally or on the Displayr cloud drive (if run in Displayr). Stacking may
  be specified manually and/or by identifying common labels that appear in
  variable labels.

  Manual stacking can be specified by variable or by observation. With the
  former, each group of variables to be stacked together is specified. With
  the latter, the variables in each stacked observation are specified (in
  order of the stacked variables). Any stacking can be performed with either
  option but often one is more convenient than the other depending on the
  structure and variable names of the required stacking.

  Common label stacking occurs by stacking together groups of variables
  whose labels match the common labels after removing common prefixes and
  suffixes. Common labels can be specified to be generated automatically,
  inferred from a set of input reference variables or specified manually.

  The stacked data set is saved as an SPSS .sav data set either locally or
  to the Displayr cloud drive (if run in Displayr).
}
\examples{
path <- system.file("examples", "Cola.sav", package = "flipData")

# Automatic common label stacking and manual stacking by variable
print(StackData(path,
                specify.by = "Variable",
                manual.stacking = c("Q6_*, NA", "Q9_A, Q9_B, Q9_C-Q9_F")))

# Common labels from reference variables and included non-stacked variables
print(StackData(path,
                stack.with.common.labels = "Using a set of variables to stack as reference",
                reference.variables.to.stack = c("Q5_5_*", "Q6_A-Q6_F"),
                variables.to.include = c("Q2")))

# Manually specified common labels and manual stacking by observation
common.labels <- list(c("Coke", "Diet Coke", "Coke Zero", "Pepsi",
                        "Diet Pepsi", "Pepsi Max", "None of these"))
print(StackData(path,
                stack.with.common.labels = "Using manually input common labels",
                manual.common.labels = common.labels,
                specify.by = "Observation",
                manual.stacking = c("Q6_A,Q9_A", "Q6_B,Q9_B", "Q6_C,Q9_C",
                                    "Q6_D,Q9_D", "Q6_E,Q9_E", "Q6_F,Q9_F")))
}
