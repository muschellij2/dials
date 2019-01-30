# ------------------------------------------------------------------------------
# param_box functions

#' @export
param_box <- function() {
  new_param_box()
}

#' @export
pack_param <- function(param_box, param, id = NA) {

  validate_param_box(param_box)
  pb_param <- new_param_box_param(param = param, id = id)
  pack_one_param(param_box, pb_param)

}

#' @export
pack_values_param <- function(param_box, name, values, id = NA) {

  param <- new_qual_param(
    name = name,
    type = "list",
    values = values,
    label = name
  )

  pack_param(param_box, param, id = id)

}

#' @export
pack_range_param <- function(param_box, name, min, max, id = NA) {

  validate_numeric(min, "min")
  validate_numeric(max, "max")
  validate_min_max_same_class(min, max)
  validate_length(min, 1, "min")
  validate_length(max, 1, "max")
  validate_name(name)

  if (is.integer(min)) {
    type <- "integer"
  }
  else {
    type <- "double"
  }

  range <- list(min, max)

  param <- new_quant_param(
    name = name,
    type = type,
    range = range,
    inclusive = c(TRUE, TRUE),
    label = name
  )

  pack_param(param_box, param, id = id)
}

pack_one_param <- function(x, param) {
  UseMethod("pack_one_param")
}

pack_one_param.param_box <- function(x, param) {
  params <- c(x$params, list(param))
  new_param_box(params = params)
}

pack_one_param.grid_box <- function(x, param) {
  packed_param_box <- NextMethod()
  new_grid_box(grid_tbl = x$grid_tbl, params = packed_param_box$params)
}

# ------------------------------------------------------------------------------
# grid_box functions

#' @export
grid_generate <- function(x, default = "regular", n = 10) {
  UseMethod("grid_generate")
}

#' @export
grid_generate.param_box <- function(x, default = "regular", n = 10) {
  gb <- new_grid_box(params = x$params)
  grid_generate(gb, default = default, n = n)
}

#' @export
grid_generate.grid_box <- function(x, default = "regular", n = 10) {

  validate_all_finalized(x$params)

  param_nms <- purrr::map_chr(x$params, function(.x) .x$name)
  grid_nms <- x$grid_tbl$name

  # Set defaults for any that need it
  unset_nms <- setdiff(param_nms, grid_nms)

  if (length(unset_nms) > 0) {
    n <- as.list(rep(n, times = length(unset_nms)))
    names(n) <- unset_nms
    x <- add_grid_tbl_rows(default, x, !!! n)
  }

  tbl_grid <- purrr::map_dfr(x$params, param_generate, grid_tbl = x$grid_tbl)
  new_tbl_grid(tbl_grid)
}

#' @export
grid_assign_regular <- function(x, ...) {
  UseMethod("grid_assign_regular")
}

#' @export
grid_assign_regular.param_box <- function(x, ...) {
  gb <- new_grid_box(params = x$params)
  grid_assign_regular(gb, ...)
}

#' @export
grid_assign_regular.grid_box <- function(x, ...) {
  add_grid_tbl_rows(type = "regular", x, ...)
}

#' @export
grid_assign_random <- function(x, ...) {
  UseMethod("grid_assign_random")
}

#' @export
grid_assign_random.param_box <- function(x, ...) {
  gb <- new_grid_box(params = x$params)
  grid_assign_random(gb, ...)
}

#' @export
grid_assign_random.grid_box <- function(x, ...) {
  add_grid_tbl_rows(type = "random", x, ...)
}

param_generate <- function(pb_param, grid_tbl) {

  param_info <- grid_tbl[grid_tbl$name == pb_param$name, ]

  if (param_info$type == "regular") {
    value_fn <- value_seq
  }
  else if (param_info$type == "random") {
    value_fn <- value_sample
  }

  vals <- value_fn(pb_param$param, n = param_info$n)

  tbl_grid_row(
    name = pb_param$param$name,
    id = pb_param$id,
    values = vals
  )
}

tbl_grid_row <- function(name, id, values) {
  tibble::tibble(name = name, id = id, values = list(values))
}

new_tbl_grid <- function(x) {
  structure(x, class = c("tbl_grid", class(x)))
}

add_grid_tbl_rows <- function(type, x, ...) {

  name_vals <- rlang::list2(...)

  validate_unique_names(name_vals)

  names <- names(name_vals)
  validate_valid_names(x, names)

  n <- unname(name_vals)
  # Validate integerish, but coerce to double (to allow for Inf)
  validate_all_integerish(n)
  n <- map_dbl(n, as.double)

  grid_tbl <- tibble::add_row(x$grid_tbl, name = names, type = type, n = n)

  new_grid_box(grid_tbl = grid_tbl, params = x$params)

}

# ------------------------------------------------------------------------------
# gridify

#' @export
gridify <- function(x) {
  UseMethod("gridify")
}

#' @export
gridify.tbl_grid <- function(x) {

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
# finalize

#' @export
finalize.param_box <- function(object, x, ...) {
  params <- purrr::map(object$params, finalize, x = x)
  new_param_box(params = params)
}

#' @export
finalize.param_box_param <- function(object, x, ...) {
  object$param <- finalize(object$param, x)
  object
}

#' @export
finalize.grid_box <- function(object, x, ...) {
  finalized_param_box <- NextMethod()
  new_grid_box(grid_tbl = object$grid_tbl, params = finalized_param_box$params)
}

# ------------------------------------------------------------------------------
# constructors

new_param_box <- function(params = list(), ..., subclass = character()) {

  validate_all_param_box_params(params)
  validate_no_duplicate_params(params)

  elems <- rlang::list2(...)
  elems <- c(list(params = params), elems)

  structure(elems, class = c(subclass, "param_box"))

}

new_grid_box <- function(grid_tbl = new_grid_tbl(), params = list(), ..., subclass = character()) {

  validate_grid_tbl_structure(grid_tbl)

  new_param_box(
    params = params,
    grid_tbl = grid_tbl,
    ...,
    subclass = c(subclass, "grid_box")
  )

}

new_param_box_param <- function(param, id = NA, ..., subclass = character()) {

  if (!is_dials_param(param)) {
    abort("`param` must be a dials `param` object.")
  }

  if (! (rlang::is_scalar_character(id) || is.na(id)) ) {
    abort("`id` must be a single character, or NA.")
  }

  name <- param$name

  if (!is.na(id)) {
    name <- paste0(name, "..", id)
  }

  elems <- rlang::list2(...)
  validate_unique_names(elems)

  elems <- c(list(param = param, name = name, id = id), elems)

  structure(elems, class = c(subclass, "param_box_param"))
}

# ------------------------------------------------------------------------------
# helpers

new_grid_tbl <- function(name = character(), type = character(), n = integer()) {
  tibble::tibble(name = name, type = type, n = n)
}

# ------------------------------------------------------------------------------
# is_*()

is_param_box <- function(x) {
  inherits(x, "param_box")
}

is_param_box_param <- function(x) {
  inherits(x, "param_box_param")
}

is_grid_box_param <- function(x) {
  inherits(x, "grid_box_param")
}

is_finalized <- function(x) {
  !has_unknowns(x)
}

# ------------------------------------------------------------------------------
# validation

validate_param_box <- function(x) {
  if (!is_param_box(x)) {
    abort("`param_box` must be a `param_box` object.")
  }

  invisible(x)
}

validate_no_duplicate_params <- function(param_box_params) {
  nms <- purrr::map_chr(param_box_params, function(x) x$name)
  if (anyDuplicated(nms)) {
    abort(glue(
      "Duplicate params cannot be added to a `param_box`. ",
      "Use `id` to differentiate them."
    ))
  }
}

validate_all_param_box_params <- function(x) {
  all_param_box_params <- all(purrr::map_lgl(x, is_param_box_param))

  if (!all_param_box_params) {
    abort("All elements in `...` must be `param_box_param` objects.")
  }

  invisible(x)
}

validate_all_grid_box_params <- function(x) {
  all_grid_box_params <- all(purrr::map_lgl(x, is_grid_box_param))

  if (!all_grid_box_params) {
    abort("All elements in `...` must be `grid_box_param` objects.")
  }

  invisible(x)
}

validate_numeric <- function(x, x_nm) {
  if (!is.numeric(x)) {
    abort(glue("`{x_nm}` must be a numeric value."))
  }

  invisible(x)
}

validate_length <- function(x, n, x_nm = "x") {
  x_n <- length(x)
  if (!(x_n == n)) {
    abort(glue("`{x_nm} should be length {n}, not {x_n}."))
  }
  invisible(x)
}

validate_min_max_same_class <- function(min, max) {
  if (class(min) != class(max)) {
    abort("The class of `min` and `max` must match.")
  }
}

validate_grid_tbl_structure <- function(grid_tbl) {

  if (!is.data.frame(grid_tbl)) {
    abort("`grid_tbl` must be a data frame.")
  }

  if (!identical(colnames(grid_tbl), c("name", "type", "n"))) {
    abort("The column names of the `grid_tbl` must be: 'name', 'type', 'id'.")
  }

  if (!is.character(grid_tbl$name)) {
    abort("The `name` column must be a character vector.")
  }

  if (!is.character(grid_tbl$type)) {
    abort("The `type` column must be a character vector.")
  }

  if (!is.numeric(grid_tbl$n)) {
    abort("The `n` column must be a numeric vector.")
  }

  if (anyDuplicated(grid_tbl$name)) {
    abort("There cannot be duplicated param names in `name`.")
  }

  invisible(grid_tbl)
}

validate_all_integerish <- function(x) {
  all_integerish <- all(purrr::map_lgl(x, rlang::is_integerish, n = 1L))

  if (!all_integerish) {
    abort("All values of `...` must be size 1 integers.")
  }

  invisible(x)
}

validate_valid_names <- function(x, nms) {
  param_nms <- purrr::map_chr(x$params, function(.x) .x$name)

  exists_lgl <- nms %in% param_nms

  if (!all(exists_lgl)) {
    bad_nms <- nms[!exists_lgl]
    bad_nms <- glue::glue_collapse(glue::single_quote(bad_nms), ", ")

    abort(glue(
      "Parameters must exist to set their type. The following do not: {bad_nms}.\n",
      "(If you used an `id`, specify that parameter with the convention '<name>..<id>'.)"
    ))
  }

  grid_nms <- x$grid_tbl$name

  duplicated_lgl <- nms %in% grid_nms

  if (any(duplicated_lgl)) {
    bad_nms <- nms[duplicated_lgl]
    bad_nms <- glue::glue_collapse(glue::single_quote(bad_nms), ", ")

    abort(glue(
      "Parameters can only be set to one grid type. ",
      "The following already exist in the `grid_tbl`: {bad_nms}."
    ))
  }

  invisible(x)
}

validate_all_finalized <- function(param_box_params) {

  finalized_lgl <- purrr::map_lgl(
    .x = param_box_params,
    .f = function(.x) is_finalized(.x$param)
  )

  if (!all(finalized_lgl)) {
    not_finalized <- map_chr(param_box_params[!finalized_lgl], function(.x) .x$name)
    not_finalized <- glue::glue_collapse(glue::single_quote(not_finalized), ", ")

    abort(glue(
      "All parameters must be finalized. ",
      "The following are not: {not_finalized}. ",
      "Please call `finalize()` before generating the grid."
    ))
  }

  invisible(param_box_params)
}

validate_name <- check_name

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
