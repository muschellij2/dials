# ------------------------------------------------------------------------------
# param_set functions

#' Create a new `param_set`
#'
#' A `param_set` is the container that stores multiple param objects. These are
#' used to generate hyperparameter grids and other more complex
#' parameter structures.
#'
#' @examples
#'
#' # An empty param_set
#' param_set()
#'
#' # A param set with 1 param, cost()
#' param_set() %>%
#'   attach_param(cost())
#'
#' @family parameter set functions
#'
#' @export
param_set <- function() {
  new_param_set()
}

#' Attach a parameter to a `param_set`
#'
#' @description
#'
#' * `attach_param()` adds a dials `param` object to a `param_set`.
#' * `attach_values_param()` constructs a custom qualitative dials parameter
#' on the fly, and adds it to the `param_set`.
#' * `attach_range_param()` constructs a custom quantitative dials parameter
#' on the fly, and adds it to the `param_set`.
#'
#' @details
#'
#' `attach_values_param()` constructs a [new_qual_param()] with `type = "list"`.
#' This is very flexible, and any type of value can be stored inside the list.
#'
#' `attach_range_param()` constructs a [new_quant_param()] with the type
#' determined from the `min` and `max` arguments. The `inclusive` argument
#' is set to `TRUE`, and there is no `trans` allowed. If you require that
#' flexibility, construct a full dials parameter yourself using
#' [new_quant_param()] and use that instead.
#'
#' @param param_set A `param_set`.
#'
#' @param param A single dials `param` object (such as [mtry()]) to add to the
#' `param_set`.
#'
#' @param id An optional single `character` `id`. This is generally _not_ needed,
#' and is only useful if you have multiple parameters of the same name in the
#' `param_set` (for example, it might be useful to have a [num_comp()] parameter
#' that will be used with a PCA recipes step, and a [num_comp()] parameter
#' that will be used with a PLS recipes step all in the same `param_set`).
#'
#' @param name A single `character` used for the name (and label) of the parameter
#' generated dynamically.
#'
#' @param values A `list` of values that should be used as values of the
#' parameter.
#'
#' @param min A single `integer` or `double` value used to set the minimum
#' value of the range of the new parameter object. Should be of the same class
#' as `max`.
#'
#' @param max A single `integer` or `double` value used to set the maximum
#' value of the range of the new parameter object. Should be of the same class
#' as `min`.
#'
#' @examples
#'
#' ps <- param_set()
#'
#' # Attaching dials parameters
#' ps_2 <- ps %>%
#'   attach_param(penalty()) %>%
#'   attach_param(mixture())
#'
#' # Generate a `tbl_grid` using the defaults (type = "regular" and 10 levels
#' # per parameter)
#' ps_2 %>%
#'   grid_generate()
#'
#' @family parameter set functions
#'
#' @export
attach_param <- function(param_set, param, id = NA) {

  validate_param_set(param_set)
  pb_param <- new_param_set_param(param = param, id = id)
  attach_one_param(param_set, pb_param)

}

#' @rdname attach_param
#' @export
attach_values_param <- function(param_set, name, values, id = NA) {

  param <- new_qual_param(
    name = name,
    type = "list",
    values = values,
    label = name
  )

  attach_param(param_set, param, id = id)

}

#' @rdname attach_param
#' @export
attach_range_param <- function(param_set, name, min, max, id = NA) {

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

  attach_param(param_set, param, id = id)
}

attach_one_param <- function(x, param) {
  UseMethod("attach_one_param")
}

attach_one_param.param_set <- function(x, param) {
  params <- c(x$params, list(param))
  new_param_set(params = params)
}

attach_one_param.grid_set <- function(x, param) {
  refined_param_set <- NextMethod()
  new_grid_set(grid_tbl = x$grid_tbl, params = refined_param_set$params)
}

# ------------------------------------------------------------------------------
# grid_set functions

#' Generate a compact parameter grid from a `param_set`
#'
#' `grid_generate()` constructs a `tbl_grid` from a `param_set`. This is a
#' compact representation of a parameter grid that can be used in other tuning
#' functions.
#'
#' Remember that before a grid can be generated, none of the parameters can
#' contain `unknown()` values. These are generally set explicitly by
#' specifying the `range` when creating quantitative parameters, or by calling
#' [finalize()] with the training data.
#'
#' @param x A `param_set` or `grid_set` to generate a `tbl_grid` from.
#'
#' @param default A single character. Either `"regular"` or `"random"`. This is
#' the default type of grid to generate for parameters who's type has not
#' been set with [grid_assign_random()] or [grid_assign_regular()].
#'
#' @param n A single integer. The value that goes along with `default`. If
#' `"regular"`, this is the number of `levels` for each parameter. If
#' `"random"`, this is the `size` for each parameter.
#'
#' @return
#'
#' A `tbl_grid` with columns for `name`, `id` and `values`. The `values` column
#' is a compact list column that contains the value for each hyperparameter.
#'
#' @family parameter set functions
#'
#' @examples
#'
#' # Default grid, 1 param
#' param_set() %>%
#'   attach_param(cost()) %>%
#'   grid_generate()
#'
#' # 100 random draws for the custom numeric parameter
#' set.seed(123)
#'
#' param_set() %>%
#'   attach_param(cost()) %>%
#'   attach_range_param("custom_param", 1, 5) %>%
#'   grid_assign_random(custom_param = 100) %>%
#'   grid_generate()
#'
#' # mtry() requires training data to finalize the range
#' mtry()
#'
#' param_set() %>%
#'   attach_param(mtry()) %>%
#'   finalize(x = iris) %>%
#'   grid_generate()
#'
#' @export
grid_generate <- function(x, default = "regular", n = 10) {
  UseMethod("grid_generate")
}

#' @export
grid_generate.param_set <- function(x, default = "regular", n = 10) {
  gb <- new_grid_set(params = x$params)
  grid_generate(gb, default = default, n = n)
}

#' @export
grid_generate.grid_set <- function(x, default = "regular", n = 10) {

  validate_default(default)
  validate_integerish(n, "n")

  if (length(x$params) == 0L) {
    return(new_tbl_grid(tbl_grid_row()))
  }

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

#' Assign a grid type to a parameter
#'
#' @description
#'
#' These functions take a `param_set` and add meta data about how the grid
#' should be generated for each parameter in the set.
#'
#' * `grid_assign_regular()` states that the variables specified in `...`
#' should be generated in a `"regular"` fashion.
#' * `grid_assign_random()` states that the variables specified in `...` should
#' be generated in a `"random"` fashion.
#'
#' @details
#'
#' Neither of these functions generate the grid themselves, that is the job
#' of [grid_generate()].
#'
#' When an `id` is set in [attach_param()] (or other similar `attach_*()`
#' functions), you _must_ specify the name of that parameter using the
#' convention `<name>..<id>`. Generally, `id` should not be set manually,
#' and is useful only when there are multiple parameters of the same name
#' in the set. See the examples for more information.
#'
#' @param x A `param_set`.
#'
#' @param ... Name-value pairs. The name matches the name of the parameter
#' you are modifying. For `grid_assign_regular()`, the value is the number of
#' `levels` to generate for that parameter. For `grid_assign_random()`, the
#' value is the `size` of each parameter to generate.
#'
#' @examples
#'
#' # Use `grid_assign_regular()` to specify that you want
#' # a regular sequence of size 20 across the range of cost
#' param_set() %>%
#'   attach_param(cost()) %>%
#'   grid_assign_regular(cost = 20) %>%
#'   grid_generate()
#'
#' # Use `grid_assign_random()` to specify that you want
#' # 30 random uniform values across the range of cost
#' set.seed(123)
#'
#' param_set() %>%
#'   attach_param(cost()) %>%
#'   grid_assign_random(cost = 30) %>%
#'   grid_generate()
#'
#' # When there are conflicting parameters, use `id`
#' # and specify the name as `<name>..<id>`
#' param_set() %>%
#'   attach_param(cost(), id = "1") %>%
#'   attach_param(cost(), id = "2") %>%
#'   grid_assign_regular(cost..1 = 5, cost..2 = 10) %>%
#'   grid_generate()
#'
#' @family parameter set functions
#'
#' @export
grid_assign_regular <- function(x, ...) {
  UseMethod("grid_assign_regular")
}

#' @export
grid_assign_regular.param_set <- function(x, ...) {
  gb <- new_grid_set(params = x$params)
  grid_assign_regular(gb, ...)
}

#' @export
grid_assign_regular.grid_set <- function(x, ...) {
  add_grid_tbl_rows(type = "regular", x, ...)
}

#' @rdname grid_assign_regular
#' @export
grid_assign_random <- function(x, ...) {
  UseMethod("grid_assign_random")
}

#' @export
grid_assign_random.param_set <- function(x, ...) {
  gb <- new_grid_set(params = x$params)
  grid_assign_random(gb, ...)
}

#' @export
grid_assign_random.grid_set <- function(x, ...) {
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

tbl_grid_row <- function(name = character(), id = character(), values = list()) {
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

  new_grid_set(grid_tbl = grid_tbl, params = x$params)

}

# ------------------------------------------------------------------------------
# gridify

#' Expand a `tbl_grid` into the full parameter grid
#'
#' Most of the time, a `tbl_grid` suffices as the object that can be passed
#' off to a tuning function, but sometimes it is helpful to generate the full
#' grid explicitly. `gridify()` does exactly this.
#'
#' When there are identical `name` columns in the `tbl_grid`, the `id` is used
#' in the column name of the expanded grid using the convention `<name>..<id>`
#' to tell them apart.
#'
#' @param x A `tbl_grid`.
#'
#' @return
#'
#' A `tibble` with as many columns as there were rows in the `tbl_grid`
#' (one per parameter). There are as many rows as the cartesian product of
#' all of the parameters.
#'
#' @examples
#'
#' # Defaults to 10 values per parameter
#' # 10 x 10 = 100 rows
#' param_set() %>%
#'   attach_param(cost()) %>%
#'   attach_param(penalty()) %>%
#'   grid_generate() %>%
#'   gridify()
#'
#' # If an ID is set, and there are no conflicts,
#' # it is not used in the naming of the columns
#' param_set() %>%
#'   attach_param(cost(), id = "1") %>%
#'   grid_generate() %>%
#'   gridify()
#'
#' # But when there are conflicts, the ID
#' # is used to tell them apart
#' param_set() %>%
#'   attach_param(cost(), id = "1") %>%
#'   attach_param(cost(), id = "2") %>%
#'   grid_generate() %>%
#'   gridify()
#'
#' @family parameter set functions
#'
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

  name <- x$name
  needs_unite <- name %in% name[duplicated(name)]

  if_yes <- paste(name, x$id, sep = "..")
  if_no  <- name

  x$name_id <- ifelse(needs_unite, if_yes, if_no)

  if (remove) {
    x$name <- NULL
    x$id <- NULL
  }

  x
}

# ------------------------------------------------------------------------------
# finalize

#' @rdname finalize
#' @export
finalize.param_set <- function(object, x, ...) {
  params <- purrr::map(object$params, finalize, x = x)
  new_param_set(params = params)
}

#' @export
finalize.param_set_param <- function(object, x, ...) {
  object$param <- finalize(object$param, x)
  object
}

#' @rdname finalize
#' @export
finalize.grid_set <- function(object, x, ...) {
  finalized_param_set <- NextMethod()
  new_grid_set(grid_tbl = object$grid_tbl, params = finalized_param_set$params)
}

# ------------------------------------------------------------------------------
# constructors

new_param_set <- function(params = list(), ..., subclass = character()) {

  validate_all_param_set_params(params)
  validate_no_duplicate_params(params)

  elems <- rlang::list2(...)
  elems <- c(list(params = params), elems)

  structure(elems, class = c(subclass, "param_set"))

}

new_grid_set <- function(grid_tbl = new_grid_tbl(), params = list(), ..., subclass = character()) {

  validate_grid_tbl_structure(grid_tbl)

  new_param_set(
    params = params,
    grid_tbl = grid_tbl,
    ...,
    subclass = c(subclass, "grid_set")
  )

}

new_param_set_param <- function(param, id = NA, ..., subclass = character()) {

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

  structure(elems, class = c(subclass, "param_set_param"))
}

# ------------------------------------------------------------------------------
# helpers

new_grid_tbl <- function(name = character(), type = character(), n = integer()) {
  tibble::tibble(name = name, type = type, n = n)
}

# ------------------------------------------------------------------------------
# is_*()

#' Test if an object is a `param_set` or `grid_set`
#'
#' @param x An object to test.
#'
#' @examples
#'
#' is_param_set(param_set())
#'
#' # `param_set`s become `grid_set`s after a `grid_assign_*()` function
#' # has been called.
#' param_set() %>%
#'   attach_param(cost()) %>%
#'   grid_assign_regular(cost = 5) %>%
#'   is_grid_set()
#'
#' @export
is_param_set <- function(x) {
  inherits(x, "param_set")
}

#' @rdname is_param_set
#' @export
is_grid_set <- function(x) {
  inherits(x, "grid_set")
}

is_param_set_param <- function(x) {
  inherits(x, "param_set_param")
}

is_finalized <- function(x) {
  !has_unknowns(x)
}

# ------------------------------------------------------------------------------
# validation

validate_param_set <- function(x) {
  if (!is_param_set(x)) {
    abort("`param_set` must be a `param_set` object.")
  }

  invisible(x)
}

validate_no_duplicate_params <- function(param_set_params) {
  nms <- purrr::map_chr(param_set_params, function(x) x$name)
  if (anyDuplicated(nms)) {
    abort(glue(
      "Duplicate params cannot be added to a `param_set`. ",
      "Use `id` to differentiate them."
    ))
  }
}

validate_all_param_set_params <- function(x) {
  all_param_set_params <- all(purrr::map_lgl(x, is_param_set_param))

  if (!all_param_set_params) {
    abort("All elements in `...` must be `param_set_param` objects.")
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

validate_all_finalized <- function(param_set_params) {

  finalized_lgl <- purrr::map_lgl(
    .x = param_set_params,
    .f = function(.x) is_finalized(.x$param)
  )

  if (!all(finalized_lgl)) {
    not_finalized <- map_chr(param_set_params[!finalized_lgl], function(.x) .x$name)
    not_finalized <- glue::glue_collapse(glue::single_quote(not_finalized), ", ")

    abort(glue(
      "All parameters must be finalized. ",
      "The following are not: {not_finalized}. ",
      "Please call `finalize()` before generating the grid."
    ))
  }

  invisible(param_set_params)
}

validate_name <- check_name

validate_integerish <- function(x, x_nm) {
  if (!rlang::is_integerish(x, n = 1)) {
    abort(glue("`{x_nm}` must be a single integer."))
  }
}

validate_default <- function(x) {

  if (!rlang::is_scalar_character(x)) {
    abort(glue(
      "`default` must be a `character` of length 1, ",
      "not a `{class(x)[1]}` of length {length(x)}."
    ))
  }

  default_values <- c("regular", "random")

  if (! (x %in% default_values) ) {
    default_values <- glue::glue_collapse(glue::single_quote(default_values), ", ")
    abort(glue("`default` must be one of: {default_values}. Not '{x}'."))
  }

  invisible(x)
}

validate_unique_names <- function(x) {
  if (!has_unique_names(x)) {
    abort("`...` must have unique names.")
  }

  invisible(x)
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
