#' @export
gridr <- function() {
  new_gridr()
}

#' @export
param_regular <- function(gridr, param, levels = 5, id = NA) {
  validate_gridr(gridr)
  gridr_param <- new_gridr_param_regular(param, levels = levels, id = id)
  add_param(gridr, gridr_param)
}

#' @export
param_random <- function(gridr, param, size = 10, id = NA) {
  validate_gridr(gridr)
  gridr_param <- new_gridr_param_random(param, size = size, id = id)
  add_param(gridr, gridr_param)
}

#' @export
param_manual <- function(gridr, name, values, id = NA) {
  validate_gridr(gridr)
  gridr_param <- new_gridr_param_manual(name = name, values = values, id = id)
  add_param(gridr, gridr_param)
}

# ------------------------------------------------------------------------------
# finalize

#' @export
finalize.gridr <- function(object, x, ...) {
  object <- purrr::map(object, finalize, x = x)
  new_gridr(!!! object)
}

#' @export
finalize.gridr_param <- function(object, x, ...) {
  object$param <- finalize(object$param, x)
  object
}

#' @export
finalize.gridr_param_manual <- function(object, x, ...) {
  object
}

# ------------------------------------------------------------------------------
# gridify

#' @export
gridify <- function(x) {
  UseMethod("gridify")
}

#' @export
gridify.tbl_gridr <- function(x) {

  values <- x$values

  x <- simple_unite(x, remove = TRUE)

  names(values) <- x$name_id

  expanded <- do.call(expand.grid, values)
  tibble::as_tibble(expanded, .name_repair = "unique")
}

# tidyr::unite()
simple_unite <- function(x, remove = TRUE) {

  if (any(is.na(x$id))) {
    abort("To include the `id`, no values may be `NA`.")
  }

  name <- x$name
  needs_unite <- name %in% name[duplicated(name)]

  if_yes <- paste(x$name, x$id, sep = "..")
  if_no  <- x$name

  x$name_id <- ifelse(needs_unite, if_yes, if_no)

  if (remove) {
    x$name <- NULL
    x$id <- NULL
  }

  x
}

# ------------------------------------------------------------------------------
# generate

#' @importFrom generics generate
#' @export
generics::generate

#' @export
generate.gridr <- function(x, ...) {
  tbl <- purrr::map_dfr(x, generate)
  new_tbl_gridr(tbl)
}

#' @export
generate.gridr_param_manual <- function(x, ...) {
  gridr_row(name = x$name, id = x$id, values = x$values)
}

#' @export
generate.gridr_param_regular <- function(x, ...) {
  validate_finalized(x$param)
  values <- value_seq(x$param, n = x$levels)
  name <- names(x$param$label)
  gridr_row(name = name, id = x$id, values = values)
}

#' @export
generate.gridr_param_random <- function(x, ...) {
  validate_finalized(x$param)
  values <- value_sample(x$param, n = x$size)
  name <- names(x$param$label)
  gridr_row(name = name, id = x$id, values = values)
}

# ------------------------------------------------------------------------------
# constructors

new_gridr <- function(..., subclass = character()) {

  elems <- rlang::list2(...)

  validate_all_gridr_params(elems)

  structure(elems, class = c(subclass, "gridr"))

}

new_gridr_param <- function(id = NA, ..., subclass = character()) {

  if (! (rlang::is_scalar_character(id) || is.na(id)) ) {
    abort("`id` must be a single character, or NA.")
  }

  elems <- rlang::list2(...)
  validate_unique_names(elems)

  elems <- c(elems, list(id = id))

  structure(elems, class = c(subclass, "gridr_param"))
}

new_gridr_param_manual <- function(name, values, id = NA) {

  if (!rlang::is_scalar_character(name)) {
    abort("`name` must be a single character.")
  }

  new_gridr_param(
    id = id,
    name = name,
    values = values,
    subclass = "gridr_param_manual"
  )
}

new_gridr_param_regular <- function(param, levels, id = NA) {

  if (!rlang::is_scalar_integerish(levels)) {
    abort("`levels` must be a single integer.")
  }

  if (!is_dials_param(param)) {
    abort("`param` must be a dials 'param' object.")
  }

  levels <- as.integer(levels)

  new_gridr_param(
    id = id,
    param = param,
    levels = levels,
    subclass = "gridr_param_regular"
  )
}

new_gridr_param_random <- function(param, size, id = NA) {

  if (!rlang::is_scalar_integerish(size)) {
    abort("`size` must be a single integer.")
  }

  validate_dials_param(param)

  size <- as.integer(size)

  new_gridr_param(
    id = id,
    param = param,
    size = size,
    subclass = "gridr_param_random"
  )
}

new_tbl_gridr <- function(x) {
  structure(x, class = c("tbl_gridr", class(x)))
}

# ------------------------------------------------------------------------------
# helpers

add_param <- function(gridr, gridr_param) {
  new_gridr(!!! gridr, gridr_param)
}

gridr_row <- function(name, id, values) {
  tibble::tibble(name = name, id = id, values = list(values))
}

# ------------------------------------------------------------------------------
# is functions

is_gridr_param <- function(x) {
  inherits(x, "gridr_param")
}

is_gridr <- function(x) {
  inherits(x, "gridr")
}

is_finalized <- function(x) {
  !has_unknowns(x)
}

# ------------------------------------------------------------------------------
# validation

validate_finalized <- function(param) {
  if (!is_finalized(param)) {
    abort("`param` must be finalized. Call `finalize()` first.")
  }
}

validate_gridr <- function(gridr) {
  if (!is_gridr(gridr)) {
    abort("`param` must be a 'gridr' object.")
  }
}

validate_dials_param <- function(param) {
  if (!is_dials_param(param)) {
    abort("`param` must be a 'param' object.")
  }
}

validate_all_gridr_params <- function(x) {
  all_gridr_params <- all(purrr::map_lgl(x, is_gridr_param))

  if (!all_gridr_params) {
    abort("All elements in `...` must be 'gridr_param' object.")
  }
}

validate_unique_names <- function(x) {
  if (!has_unique_names(x)) {
    abort("`...` must have unique names.")
  }
}

has_unique_names <- function(x) {
  nms <- names(x)

  if (length(nms) != length(x)) {
    return(FALSE)
  }

  if (any(is.na(nms) | nms == "")) {
    return(FALSE)
  }

  !anyDuplicated(nms)
}
