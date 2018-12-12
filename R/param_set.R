#' Parameter sets
#' 
#' Group a set of `dials` parameters. 
#' @param ... One or more `param` objects.
#' @return An object of class "param_set". 
#' @examples 
#' param_set(mtry, neighbors, penalty)
#' 
#' @export
param_set <- function(...) {
  res <- new_param_set(...)
  res
}

is_param <- function(x) {
  inherits(x, "param")
}

#' @importFrom purrr map_lgl
#' @importFrom glue glue_collapse
new_param_set <- function(...) {
  params <- list(...)  
  if (length(params) == 0) {
    stop("At least one  `param` object is required.", 
         call. = FALSE)
  }
  are_param <- purrr::map_lgl(params, is_param)
  if(any(!are_param)) {
    stop(
      "Arguments in positions ", 
      glue:::glue_collapse(
        which(!are_param), sep = ", ", last = " and "
      ),
      " are not `param` objects.",
      call. = FALSE
    )
  }
  class(params) <- "param_set"
  params
}

#' @importFrom glue glue_collapse
#' @export
print.param_set <- function(x, ...) {
  cat("A parameter set containing:\n\n")
  labs <- labels(x)
  cat(glue:::glue_collapse(labs, sep = "\n"))
  invisible(x)
}

#' @importFrom purrr map_chr pluck
#' @importFrom glue glue_collapse
#' @export
labels.param_set <- function(object, ...) {
  labs <- paste("-", purrr::map_chr(object, purrr::pluck, "label"))
  final <- !has_unknowns(object)
  final <- ifelse(final, "(finalized)", "")
  labs <- paste(labs, final)
  labs
}

