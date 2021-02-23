# define any import/importFrom here, see https://kbroman.org/pkg_primer/pages/depends.html
#' @importFrom dplyr %>%
#' @importFrom rlang .data .env
NULL

#' Check for Issues
#'
#' Helper function to verify that there are no issues that will prevent any of the code in this project running.
check_for_issues <- function() {
  errors_encountered <- vector("list", 100)
  error_ctr <- 0

  tryCatch({
    curl::ie_get_proxy_for_url()
    invisible(NULL)
  }, error = function(e) {
    msg <- paste0(
      crayon::underline(crayon::red("Error with proxy configuration")),
      "\n\n",
      "Press the windows button and type proxy settings.",
      "Make sure 'Automatically detect settings' is disabled"
    )
    errors_encountered[[(error_ctr <<- error_ctr + 1)]] <<- msg
  })

  if (error_ctr > 0) {
    stop("Resolve ", error_ctr, " issues before continuing:\n\n",
         paste(errors_encountered[1:error_ctr]))
  }

  invisible(NULL)
}
