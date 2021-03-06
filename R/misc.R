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
check_installs <- function(x) {
  lib_inst <- rownames(installed.packages())
  is_inst <- x %in% lib_inst
  if (any(!is_inst)) {
    rlang::abort(
      paste0(
        "Package(s) not installed: ",
        paste0("'", x[!is_inst], "'", collapse = ", ")
      )
    )
  }
}

# checking functions -----------------------------------------------------------

check_label <- function(txt) {
  if (is.null(txt))
    rlang::abort("`label` should be a single named character string or NULL.")
  if (!is.character(txt) || length(txt) > 1)
    rlang::abort("`label` should be a single named character string or NULL.")
  if (length(names(txt)) != 1)
    rlang::abort("`label` should be a single named character string or NULL.")
  invisible(txt)
}

check_finalize <- function(x) {
  if (!is.null(x) & !is.function(x))
    rlang::abort("`finalize` should be NULL or a function.")
  invisible(x)
}

check_range <- function(x, type, trans) {
  if (!is.null(trans)) {
    return(invisible(x))
  }
  x0 <- 0
  if (!is.list(x)) {
    x <- as.list(x)
  }
  x <- x[!is_unknown(x)]
  right_type <- purrr::map_lgl(x, ~ all(typeof(.x) == type))
  if (length(right_type) > 0 && any(!right_type)) {
    msg <- paste0(
      "Since `type = '", type, "'`, please use that data type for the range."
    )
    rlang::abort(msg)
  }
  invisible(x0)
}
