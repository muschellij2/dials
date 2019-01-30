#' Merge parameter grid values into a parsnip object
#'
#'
#' @description
#'
#' \pkg{parsnip} contains model objects that have consistent names with
#' \pkg{dials}. `merge()` can be used to easily update any of the main parameters
#' in a \pkg{parsnip} model.
#'
#' For more information and examples, see the article,
#' [Using dials with parsnip](https://tidymodels.github.io/dials/articles/articles/Dials_and_Parsnip.html).
#'
#' @param x,y A combination of one \pkg{parsnip} model object (that has class
#' `"model_spec"`) and one parameter grid resulting from [grid_regular()] or
#' [grid_random()]. As long as this combination is present, the order that
#' they are provided in does not matter.
#'
#' @param ... Not currently used.
#'
#' @return
#'
#' A list containing updated model objects.
#'
#' @importFrom utils getS3method
#' @export merge.model_spec
#' @method merge model_spec
#' @export
merge.model_spec <- function(x, y, ...) {
  UseMethod("merge.model_spec", y)
}


#' @method merge.model_spec default
#' @export
merge.model_spec.default <- function(x, y, ...) {
  abort("`x` is a 'model_spec', but `y` is not a known mergable type.")
}

#' @rdname merge.model_spec
#' @method merge.model_spec tbl_grid
#' @export
merge.model_spec.tbl_grid <- function(x, y, ...) {

  update_method <- get_update_function(class(x)[1])

  vry <- varying_args(x, full = FALSE)

  y <- fill_missing_ids(y, vry)

  validate_varying_are_in_grid(y, vry)

  vry_grd <- left_join_vry_and_grd(vry = vry, grd = y)

  # for a spec, shouldnt be any need to worry about identical IDs
  grid <- gridify(vry_grd)

  nrow_seq <- seq_len(nrow(grid))

  param_obj <- list(object = x)

  grid$specs <- purrr::map(nrow_seq, ~{

    param_lst <- as.list(grid[.x, , drop = FALSE])
    param_lst <- c(param_lst, param_obj)
    do.call(update_method, param_lst)

  })

  grid
}

#' @rdname merge.model_spec
#' @export merge.recipe
#' @method merge recipe
#' @export
merge.recipe <- function(x, y, ...) {
  UseMethod("merge.recipe", y)
}

#' @method merge.recipe default
#' @export
merge.recipe.default <- function(x, y, ...) {
  abort("`x` is a 'recipe', but `y` is not a known mergable type.")
}

#' @rdname merge.model_spec
#' @method merge.recipe tbl_grid
#' @export
merge.recipe.tbl_grid <- function(x, y, ...) {

  vry <- varying_args(x, full = FALSE)

  y <- fill_missing_ids(y, vry)

  validate_varying_are_in_grid(y, vry)

  vry_grd <- left_join_vry_and_grd(vry = vry, grd = y)

  # for a recipe, we do need to worry about identical params
  grid <- gridify(vry_grd)

  grid_t <- purrr::transpose(grid)

  vry <- simple_unite(vry, remove = FALSE)

  grid$recipes <- purrr::map(grid_t, update_recipe, rec = x, vry_tbl = vry)

  grid
}

#' @rdname merge.model_spec
#' @export merge.tbl_grid
#' @method merge tbl_grid
#' @export
merge.tbl_grid <- function(x, y, ...) {
  UseMethod("merge.tbl_grid", y)
}

#' @method merge.tbl_grid default
#' @export
merge.tbl_grid.default <- function(x, y, ...) {
  abort("`x` is a 'tbl_grid', but `y` is not a known mergable type.")
}

#' @rdname merge.model_spec
#' @method merge.tbl_grid recipe
#' @export
merge.tbl_grid.recipe <- function(x, y, ...) {
  merge(y, x, ...)
}

#' @rdname merge.model_spec
#' @method merge.tbl_grid model_spec
#' @export
merge.tbl_grid.model_spec <- function(x, y, ...) {
  merge(y, x, ...)
}

# ------------------------------------------------------------------------------

fill_missing_ids <- function(grd, vry) {

  update_id <- function(name, id) {

    # user supplied ID
    if (!is.na(id)) {
      return(id)
    }

    # can't update anything (not a varying param)
    if (!(name %in% vry$name)) {
      return(NA)
    }

    pos <- which(vry$name == name)[1]
    vry$id[pos]

  }

  # fill in any missing IDs with the varying tbl info
  grd$id <- purrr::map2_chr(grd$name, grd$id, update_id)

  grd
}

validate_varying_are_in_grid <- function(grd, vry) {
  vry_nms <- paste(vry$name, vry$id, sep = "..")
  grd_nms <- paste(grd$name, grd$id, sep = "..")

  validate_unique_name_id_pairs(grd_nms)

  vry_exist <- vry_nms %in% grd_nms

  if (!all(vry_exist)) {
    dont_exist <- glue::glue_collapse(glue::single_quote(vry_nms[!vry_exist]), ", ")
    abort(glue(
      "All varying 'name..id' combinations must be specified in the 'tbl_grid'. ",
      "The following don't exist: {dont_exist}."
    ))
  }
}

validate_unique_name_id_pairs <- function(x) {

  dups <- duplicated(x)

  if (any(dups)) {
    dup <- glue::glue_collapse(x[dups], ", ")

    abort(glue(
      "There is at least one duplicated 'name..id' pair present ({dup}). ",
      "Do you need to specify an `id` to preserve uniqueness?"
    ))
  }
}

left_join_vry_and_grd <- function(vry, grd) {

  merged <- merge.data.frame(
    x = vry,
    y = grd,
    by = c("name", "id"),
    all.x = TRUE
  )

  # it is a tbl_grid (it has name, id, values columns)
  vry_with_vals <- as_tibble(merged)
  class(vry_with_vals) <- c("tbl_grid", class(vry_with_vals))

  vry_with_vals
}

get_update_function <- function(cls) {

  update_fn <- try(getS3method("update", cls), silent = TRUE)

  if (inherits(update_fn, "try-error")) {
    abort(glue("No `update` method for class '{cls}'."))
  }

  update_fn
}

# For each step, fill in hyperparameters where needed
update_recipe <- function(grd_single, rec, vry_tbl) {

  update_fn <- get_update_function("step")

  rec$steps <- map(
    .x = rec$steps,
    .f = update_step,
    grd_single = grd_single,
    vry_tbl = vry_tbl,
    update_fn = update_fn
  )

  rec
}

update_step <- function(stp, grd_single, vry_tbl, update_fn) {

  # Find the varying args specific to this recipe step
  vry_step <- vry_tbl[vry_tbl$id == stp$id, , drop = FALSE]

  # Find the matching grid columns
  grd_rec_specific <- grd_single[vry_step$name_id]

  # Undo the `..` uniqueness
  dot_dot_anything <- "\\.\\..*"
  names(grd_rec_specific) <- gsub(dot_dot_anything, "", names(grd_rec_specific))

  # Update the step
  #update(stp, !!! grd_rec_specific)
  do.call(update_fn, rlang::list2(object = stp, !!! grd_rec_specific))

}


