# Copilot Instructions for R Development

<!-- ## Project Overview -->

<!-- ## R Package Standards -->

<!-- ### Documentation Standards
- Use roxygen2 documentation format with `#'` comments
- Always include `@title` and `@description` for exported functions
- Use `@param` to document all function parameters
- Use `@return` to describe return values
- Include `@examples` for demonstration
- Use `@export` for functions that should be available to users
- Use `@importFrom package function` for external dependencies
- Reference other functions with `\code{\link{function_name}}`
- Use `@inheritParams` to inherit parameter documentation -->

<!-- ### Function Naming Conventions
- Use PascalCase for exported functions (e.g., `AddFormulaBars`, `Average`, `RegressionTable`)
- Use camelCase for internal/helper functions
- Function names should be descriptive and action-oriented
- Prefer full words over abbreviations -->

### Code Style
- Follow the tidyverse style guide (https://style.tidyverse.org/) unless otherwise specified
- Require `=` for assignment
<!-- - Use 4-space indentation
- Place opening braces on the same line
- Use meaningful variable names with dots for separation (e.g., `source.language`, `target.language`)
- Functions should be well-structured with clear logic flow
- Use early returns for error conditions or simple cases -->

<!-- ### Error Handling
- Use `flipU::StopForUserError()` for user-facing error messages
- Validate inputs at the beginning of functions
- Provide informative error messages -->

<!-- ### Testing Standards
- Use testthat framework for unit tests
- Test files should be in `tests/testthat/` directory
- Test file names should start with `test-` followed by the source file name
- Use `context()` to group related tests
- Use descriptive test names with `test_that()`
- Test both normal and edge cases
- Include tests for error conditions -->

<!-- ### Package Structure
```
package_name/
├── DESCRIPTION          # Package metadata
├── NAMESPACE           # Export/import declarations
├── R/                  # Source code
├── man/                # Generated documentation
├── tests/              # Test files
│   └── testthat/
├── inst/               # Installed files
└── data/               # Package data
``` -->

<!-- ### Dependencies
- Prefer using specific imports with `@importFrom` rather than full package imports
- Common dependencies include:
  - `flipU`: Utility functions
  - `flipFormat`: Formatting functions
  - `formattable`: For creating formatted tables
  - `htmlwidgets`: For HTML widgets -->

<!-- ### Code Organization
- Group related functions in single files
- Use descriptive file names that match the main function
- Keep files focused on a single responsibility
- Order functions logically (exported functions first, then helpers) -->

<!-- ## Best Practices
1. Always validate inputs before processing
2. Use existing flipU utility functions when available
3. Maintain consistency with existing codebase patterns
4. Write comprehensive tests for all exported functions
5. Document all parameters and return values clearly
6. Handle edge cases gracefully
7. Prefer explicit imports over library() calls
8. Use meaningful variable names that indicate data types and purpose
9. Include examples that demonstrate typical usage patterns
10. Follow R package development best practices for CRAN compatibility -->

<!-- ## Example Function Template
```r
#' @title Brief Function Description
#' @description Detailed description of what the function does,
#'   including any important details about behavior or assumptions.
#' @param param1 Description of first parameter.
#' @param param2 Description of second parameter.
#' @return Description of what the function returns.
#' @importFrom package function
#' @export
#' @examples
#' # Example usage
#' result <- FunctionName(param1 = value1, param2 = value2)
FunctionName <- function(param1, param2) {
    # Input validation
    if (missing(param1))
        flipU::StopForUserError("param1 is required")
    
    # Function logic
    result <- process_data(param1, param2)
    
    return(result)
}
``` -->

<!-- When working with R code in this workspace, follow these standards to maintain consistency with the existing codebase and ensure high-quality, maintainable code. -->



<!-- # Copilot Instructions for flipData

## Overview
<!-- Describe the package purpose, main functionality, and architectural patterns -->

<!-- ## Coding Standards -->
<!-- Define coding conventions, naming patterns, and style guidelines -->

<!-- ## Function Documentation -->
<!-- Specify documentation requirements and patterns -->

<!-- ## Error Handling -->
<!-- Define error handling conventions and patterns -->

<!-- ## Testing Guidelines -->
<!-- Specify testing approaches and patterns -->

<!-- ## Package-Specific Patterns -->
<!-- Document any unique patterns or conventions specific to this package -->

<!-- ## Examples -->
<!-- Provide code examples and templates --> -->