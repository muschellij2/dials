### For printing

format_range_val <- function(val, ukn = "?", digits = 3) {
  if (!is_unknown(val)) {
    txt <- format(val, digits = digits)
  } else
    txt <- ukn
  txt
}

format_bounds <- function(bnds) {
  res <- c("(", ")")
  if (bnds[1])
    res[1] <- "["
  if (bnds[2])
    res[2] <- "]"
  res
}

# From parsnip:::check_installs
#' @importFrom utils installed.packages
check_installs <- function (x) {
  lib_inst <- rownames(installed.packages())
  is_inst <- x %in% lib_inst
  if (any(!is_inst)) {
    stop(
      "This engine requires some package installs: ",
      paste0("'", x[!is_inst], "'", collapse = ", "),
      call. = FALSE
    )
  }
}

# checking functions -----------------------------------------------------------

check_label <- function(txt) {

  msg <- "`label` should be a single character string or NULL."

  if (! (is.null(txt) | is.character(txt)) ) {
    abort(msg)
  }

  if (is.character(txt) & length(txt) > 1) {
    abort(msg)
  }

  invisible(txt)
}

check_name <- function(txt) {

  if (!rlang::is_scalar_character(txt) ) {
    abort("`name` should be a single character string.")
  }

  invisible(txt)
}

check_finalize <- function(x) {
  if (!is.null(x) & !is.function(x))
    stop("`finalize` should be NULL or a function.", .call = FALSE)
  invisible(x)
}

check_id <- function(x) {
  if (!is.null(x) & !is.character(x)) {
    abort("`id` should be NULL or a character.")
  }
}

# is functions -----------------------------------------------------------------

is_dials_param <- function(x) {
  inherits(x, "param")
}
