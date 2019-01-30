context("test-validate")

context("qualitative parameter values")

test_param_1 <- new_qual_param(
  name = "test_param_1",
  type = "list",
  values = list(1, 2, 3, 4, 5),
  label = "param"
)

test_param_2 <- new_qual_param(
  name = "test_param_2",
  type = "list",
  values = list(list(x = 1, y = 2), list(x = 2, y = 2), list(x = 2, y = 3)),
  label = "param"
)

test_that("validation - list", {

  expect_equal(
    value_validate(test_param_1, list(1)),
    TRUE
  )

  expect_equal(
    value_validate(test_param_1, list(1, 2)),
    c(TRUE, TRUE)
  )

  expect_equal(
    value_validate(test_param_2, list(list(x = 1, y = 2))),
    TRUE
  )

  expect_equal(
    value_validate(test_param_2, list(list(x = 1, y = 2), list(x = 2, y = 3))),
    c(TRUE, TRUE)
  )

  # Not wrapped in list(), so treated as two separate things
  # when used with `%in%`
  expect_equal(
    value_validate(test_param_2, list(x = 1, y = 2)),
    c(FALSE, FALSE)
  )

})
