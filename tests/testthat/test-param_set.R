# ------------------------------------------------------------------------------
# param set creation

context("param_set creation")

test_that("can create an empty param_set", {

  ps <- structure(list(params = list()), class = "param_set")

  expect_equal(param_set(), ps)

})

test_that("can attach parameters", {

  ps <- param_set() %>%
    attach_param(cost())

  expect_equal(ps$params[[1]]$param, cost())
  expect_equal(ps$params[[1]]$name, "cost")
  expect_equal(ps$params[[1]]$id, NA)

  ps <- ps %>%
    attach_param(penalty())

  # retest to ensure no breakage
  expect_equal(ps$params[[1]]$param, cost())
  expect_equal(ps$params[[1]]$name, "cost")

  # test new param
  expect_equal(ps$params[[2]]$param, penalty())
  expect_equal(ps$params[[2]]$name, "penalty")

})

test_that("can attach with ID", {
  ps <- param_set() %>%
    attach_param(cost(), id = "id")

  expect_equal(ps$params[[1]]$id, "id")
  expect_equal(ps$params[[1]]$name, "cost..id")
})

test_that("duplicate parameters are not allowed", {

  ps <- param_set() %>%
    attach_param(cost())

  expect_error(attach_param(ps, cost()), "Duplicate params")
})

test_that("can add duplicate parameters using IDs", {

  ps <- param_set() %>%
    attach_param(cost(), id = "1")

  expect_error(
    attach_param(ps, cost(), id = "2"),
    NA
  )
})

test_that("can only add dials parameters to a param_set", {
  expect_error(
    param_set() %>% attach_param("x"),
    "`param` must be a dials `param`"
  )
})

test_that("can create quant params on the fly", {

  ps <- attach_range_param(param_set(), "test", 1L, 5L)

  test <- new_quant_param(
    name = "test",
    type = "integer",
    range = c(1L, 5L),
    inclusive = c(TRUE, TRUE),
    label = "test"
  )

  expect_equal(ps$params[[1]]$param, test)
})

test_that("can create qual params on the fly", {

  ps <- attach_values_param(param_set(), "test", values = list(1, 2, 3))

  test <- new_qual_param(
    name = "test",
    type = "list",
    values = list(1, 2, 3),
    label = "test"
  )

  expect_equal(ps$params[[1]]$param, test)
})

test_that("min/max must be of the same class", {

  expect_error(
    attach_range_param(param_set(), "test", 1L, 5),
    "The class of"
  )

  expect_error(
    attach_range_param(param_set(), "test", 1, 5L),
    "The class of"
  )

})

test_that("min/max must be numeric", {

  expect_error(
    attach_range_param(param_set(), "test", min = "x", max = 5),
    "`min` must be a numeric value."
  )

})

test_that("attach_values_param() `values` must be a list", {

  expect_error(
    attach_values_param(param_set(), "test", values = c(1, 2, 3)),
    "`values` must be a list."
  )

})

# ------------------------------------------------------------------------------
# grid set functionality

context("grid_set manipulation")

test_that("can generate basic grids", {

  grid <- param_set() %>%
    attach_param(cost()) %>%
    grid_generate()

  vals <- list(value_seq(cost(), 10))

  expect_is(grid, "tbl_grid")

  expect_equal(grid$name, "cost")
  expect_equal(grid$id, NA)
  expect_equal(grid$values, vals)

})

test_that("must finalize before grid creation", {

  expect_error(
    param_set() %>%
      attach_param(mtry()) %>%
      grid_generate(),
    "All parameters must be finalized."
  )

  expect_error(
    param_set() %>%
      attach_param(mtry()) %>%
      finalize(x = iris) %>%
      grid_generate(),
    NA
  )

})

test_that("can change default grid", {

  ps1 <- param_set() %>%
    attach_param(cost()) %>%
    grid_generate(n = 20)

  set.seed(123)
  ps2 <- param_set() %>%
    attach_param(cost()) %>%
    grid_generate(default = "random", n = 20)

  set.seed(123)
  vals <- value_sample(cost(), 20)

  expect_equal(
    length(ps1$values[[1]]),
    20
  )

  expect_equal(
    length(ps2$values[[1]]),
    20
  )

  expect_equal(
    ps2$values[[1]],
    vals
  )

})

test_that("can only use regular/random default", {

  expect_error(
    grid_generate(param_set(), default = "x"),
    "`default` must be one of:"
  )

  expect_error(
    grid_generate(param_set(), default = 1),
    "`default` must be a `character` of length 1, not a `numeric` of length 1."
  )
})

test_that("can only use single integerish n", {

  expect_error(
    grid_generate(param_set(), n = 1.5),
    "`n` must be a single integer."
  )

  expect_error(
    grid_generate(param_set(), n = c(1, 2)),
    "`n` must be a single integer."
  )

  # Can use Inf, useful for all levels of qual param
  expect_error(
    ps_inf <- param_set() %>%
      attach_param(activation()) %>%
      grid_generate(n = Inf),
    NA
  )

  expect_equal(
    ps_inf$values[[1]],
    values_activation
  )
})

test_that("can set param grid types individually", {

  set.seed(123)
  grid <- param_set() %>%
    attach_param(cost()) %>%
    attach_param(activation()) %>%
    grid_assign_regular(cost = 5) %>%
    grid_assign_random(activation = 20) %>%
    grid_generate()

  expect_equal(
    length(grid$values[[1]]),
    5
  )

  expect_equal(
    length(grid$values[[2]]),
    20
  )

})

test_that("can set params using 'name..id'", {

  grid <- param_set() %>%
    attach_param(cost(), id = "1") %>%
    attach_param(cost(), id = "2") %>%
    grid_assign_regular(cost..1 = 5, cost..2 = 20) %>%
    grid_generate()

  expect_equal(
    grid$id,
    c("1", "2")
  )

  expect_equal(
    length(grid$values[[1]]),
    5
  )

  expect_equal(
    length(grid$values[[2]]),
    20
  )

})

test_that("gridify() can generate expanded grids", {

  full_grid <- param_set() %>%
    attach_param(cost()) %>%
    attach_param(activation()) %>%
    grid_generate() %>%
    gridify()

  expect_equal(
    colnames(full_grid),
    c("cost", "activation")
  )

  expect_equal(
    nrow(full_grid),
    40
  )

})

test_that("gridify() with duplicate names generate name..id columns", {

  full_grid <- param_set() %>%
    attach_param(cost(), id = "1") %>%
    attach_param(cost(), id = "2") %>%
    grid_generate() %>%
    gridify()

  expect_equal(
    colnames(full_grid),
    c("cost..1", "cost..2")
  )

})

test_that("gridify() does not use the id unless there is a duplicate name", {

  full_grid <- param_set() %>%
    attach_param(cost(), id = "1") %>%
    grid_generate() %>%
    gridify()

  expect_equal(
    colnames(full_grid),
    "cost"
  )

})

test_that("empty param_sets still return a tbl_grid", {

  grid <- grid_generate(param_set())

  expect_is(grid, "tbl_grid")
  expect_equal(colnames(grid), c("name", "id", "values"))

})
