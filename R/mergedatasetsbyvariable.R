#' @title Merge Data Sets by Variable
#' @description Merges multiple data sets by case where the data sets contain
#'  similar variables but different cases, e.g., data sets from different time
#'  periods.
#' @param data.set.names A character vector of names of data sets from the
#'  Displayr cloud drive to merge (if run from Displayr) or file paths of local
#'  data sets.
#' @param id.variable A string containing the name of the optional ID variable.
#' @param include.or.omit.variables A character vector where each element
#'  corresponds to an input data set, and indicates whether variables from the
#'  input data set are to be specified in the merged data set by specifying
#'  the variables to include or the variables to omit.
#' @param variables.to.include A character vector of variables to include,
#'  where each element contains comma-separated variable names. To specify
#'  variables from a specific data set, suffix the name with the data set index
#'  in parentheses, e.g., 'Q2(3)'. Ranges of variables can be specified by
#'  separating variable names by '-'. This parameter only applies to data sets
#'  where variables are specified to be included.
#' @param variables.to.omit A character vector of variables to omit,
#'  where each element contains comma-separated variable names. To specify
#'  variables from a specific data set, suffix the name with the data set index
#'  in parentheses, e.g., 'Q2(3)'. Ranges of variables can be specified by
#'  separating variable names by '-'. This parameter only applies to data sets
#'  where variables are specified to be omitted.
#' @param include.merged.data.set.in.output Whether to include the merged data
#'  set in the output.
#' @return A list with the following elements:
#' \itemize{
#'   \item \code{merged.data.set} If \code{include.merged.data.set.in.output},
#'   is TRUE, this is a data frame of the merged data set.
#'   \item \code{input.data.sets.metadata} A list containing metadata on the
#'     the input data sets such as variable names, labels etc.
#'   \item \code{merged.data.set.metadata} A list containing metadata on the
#'     the merged data set such as variable names, labels etc.
#'   \item \code{is.saved.to.cloud} Whether the merged data set was saved to
#'     the Displayr cloud drive.
#' }
#' @export
MergeDataSetsByVariable <- function(data.set.names, id.variable = NULL,
                                    include.or.exclude.variables = rep("exclude", length(data.set.names)),
                                    variables.to.include = NULL,
                                    variables.to.omit = NULL)
{
    data.sets <- readDataSets(data.set.names, 2)
    input.data.sets.metadata <- metadataFromDataSets(data.sets)

    matched.cases <- matchCases(input.data.sets.metadata, id.variable)
    merged.variable.names <- mergedVariableNames(input.data.sets.metadata,
                                                 include.or.exclude.variables,
                                                 variables.to.include,
                                                 variables.to.omit)
    merged.data.set <- mergedDataSetByVariable(data.sets, matched.cases,
                                               merged.variable.names,
                                               input.data.sets.metadata)
    merged.data.set.name <- cleanMergedDataSetName(merged.data.set.name,
                                                   data.set.names)
    writeDataSet(merged.data.set, merged.data.set.name)

    result <- list()
    if (include.merged.data.set.in.output)
        result$merged.data.set <- merged.data.set

    result$input.data.sets.metadata <- input.data.sets.metadata
    result$merged.data.set.metadata <- metadataFromDataSet(merged.data.set,
                                                           merged.data.set.name)
    result$is.saved.to.cloud <- IsDisplayrCloudDriveAvailable()
    class(result) <- "MergeDataSetByVariable"
    result
}

matchCases <- function(input.data.sets.metadata, id.variable)
{
    if (!is.null(id.variable))
    {
        id.var.name <- parseIDVariable(id.variable, input.data.sets.metadata)

    }
}

parseIDVariable <- function(id.variable, input.data.sets.metadata)
{

}

mergedVariableNames <- function(input.data.sets.metadata,
                                include.or.exclude.variables,
                                variables.to.include, variables.to.omit)
{
    n.data.sets <- input.data.sets.metadata$n.data.sets
    result <- list()
    for (i in seq_len(n.data.sets))
    {

    }
    result
}

mergedDataSetByVariable <- function(data.sets, matched.cases,
                                    merged.variable.names,
                                    input.data.sets.metadata)
{

}
