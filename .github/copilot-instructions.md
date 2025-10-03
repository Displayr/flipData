# Copilot Instructions for flipData

This document provides coding standards and best practices for the flipData R package development.

## General R Coding Standards

### Assignment Operators
- **ALWAYS use `<-` for assignment, never `=`**
- Function parameters should use `=` (this is correct R syntax)
- Examples:
  ```r
  # Correct
  my_variable <- some_function(param = value)
  
  # Incorrect
  my_variable = some_function(param = value)
  ```

### String Quotes
- **Use double quotes `"` for strings, avoid single quotes `'`**
- Exception: Single quotes are acceptable within double-quoted strings
- Examples:
  ```r
  # Correct
  message <- "This is a string"
  pattern <- "Can't use single quotes here"
  
  # Incorrect
  message <- 'This is a string'
  ```

### Line Length
- **Maximum line length: 120 characters**
- Break long lines using appropriate indentation
- Function calls with many parameters should be broken across lines:
  ```r
  # Correct
  result <- some_long_function_name(
      parameter_one = value1,
      parameter_two = value2,
      parameter_three = value3
  )
  
  # Also correct for long expressions
  long_message <- paste0("This is a very long message that exceeds ",
                        "the line length limit and should be broken ",
                        "across multiple lines")
  ```

### Naming Conventions
- **Variables and functions**: Use `snake_case` or `dotted.case` (prefer `dotted.case` for parameters)
- **Constants**: Use `UPPER_CASE`
- **Private functions**: Prefix with `.` (e.g., `.private_function`)

### Indentation and Spacing
- **Use 4 spaces for indentation, never tabs**
- Add spaces around operators (`<-`, `=`, `+`, `-`, etc.)
- No trailing whitespace

## flipData Package Specific Guidelines

### Error Handling
- **Use `flipU::StopForUserError()` for user-facing errors**
- Include clear, actionable error messages
- Example:
  ```r
  if (length(data) == 0) {
      StopForUserError("No data provided. Please supply a valid dataset.")
  }
  ```

### Dependencies
- **Import functions explicitly using `@importFrom`**
- Common imports for flipData:
  ```r
  #' @importFrom flipU StopForUserError
  #' @importFrom flipAPI QLoadData
  #' @importFrom haven read_sav
  ```

### Data Handling
- **Preserve SPSS metadata** when working with .sav files
- Use `attr()` to maintain variable labels and value labels
- Handle missing data appropriately

### Function Documentation
- **All exported functions MUST have complete roxygen2 documentation**
- Include `@param` for all parameters
- Include `@return` describing the return value
- Include `@export` for exported functions
- Use `@noRd` for internal functions
- Example:
  ```r
  #' Merge multiple SPSS data sets
  #'
  #' @param data.set.names A character vector of data set names
  #' @param min.data.sets Minimum number of data sets required
  #' @return A list of data frames
  #' @export
  #' @importFrom flipU StopForUserError
  ```

### SPSS Integration
- **Variable names**: Ensure compatibility with SPSS naming restrictions
- **Reserved keywords**: Handle SPSS reserved words appropriately
- **File handling**: Use proper error handling for file I/O operations

## Code Quality Standards

### Functions
- **Keep functions focused** - one responsibility per function
- **Use descriptive names** that clearly indicate purpose
- **Validate inputs** at the beginning of functions
- **Return consistent types**

### Comments
- **Use `#` for single-line comments**
- **Explain the "why", not the "what"**
- **Document complex algorithms or business logic**
- **Keep comments up-to-date with code changes**

### Testing
- **Write unit tests** for all exported functions
- **Use `testthat` framework**
- **Test edge cases and error conditions**
- **Maintain high test coverage**

## File Organization

### R Files
- **One main function per file** when possible
- **Group related utility functions** together
- **Use clear, descriptive file names**

### Documentation
- **Keep man/ directory up-to-date** with roxygen2
- **Update NAMESPACE automatically** with roxygen2
- **Maintain comprehensive vignettes** for complex functionality

## Git and Version Control

### Commit Messages
- **Use descriptive commit messages**
- **Start with verb in present tense** (e.g., "Add", "Fix", "Update")
- **Reference issues when applicable**

### Branching
- **Use feature branches** for development
- **Keep branches focused** on single features or fixes
- **Delete merged branches** to keep repository clean

## Performance Considerations

### Memory Usage
- **Avoid unnecessary data copying**
- **Use appropriate data structures**
- **Consider memory-efficient alternatives** for large datasets

### Computational Efficiency
- **Vectorize operations** when possible
- **Avoid loops** when vectorized alternatives exist
- **Profile code** for performance bottlenecks

## Security and Privacy

### Data Handling
- **Never commit sensitive data** to version control
- **Use appropriate file permissions**
- **Handle user data responsibly**

## Package Maintenance

### Dependencies
- **Minimize external dependencies**
- **Pin version ranges** for critical dependencies
- **Regularly update dependencies** for security patches

### Documentation
- **Keep README.md current**
- **Update NEWS.md** for each release
- **Maintain clear installation instructions**

## Example Code Patterns

### Typical Function Structure
```r
#' Function title
#'
#' @param data A data frame
#' @param subset Optional subset condition
#' @return Processed data frame
#' @export
#' @importFrom flipU StopForUserError
myFunction <- function(data, subset = NULL) {
    # Validate inputs
    if (missing(data) || !is.data.frame(data)) {
        StopForUserError("Data must be a data frame")
    }
    
    # Process data
    result <- processData(data, subset)
    
    # Return result
    result
}
```

### Error Handling Pattern
```r
# Check for required conditions
if (condition_not_met) {
    StopForUserError("Clear description of what went wrong and how to fix it")
}
```

### SPSS Integration Pattern
```r
# Preserve SPSS attributes
original_attrs <- attributes(data)
data <- process_data(data)
attributes(data) <- original_attrs
```

---

These instructions ensure code quality, maintainability, and consistency across the flipData package. Always prioritize clarity and user experience in your implementations.