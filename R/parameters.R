#' Default parameter values
#'
#' These are default parameter value objects for parameters that take a `values`
#' argument. When optimizing over qualitative parameter values, you can subset
#' the default arguments listed here and supply the result to `values` if you
#' want to optimize a different set of `values`.
#'
#' @format A character vector of parameter values.
#'
#' @name default_parameters
NULL

# ------------------------------------------------------------------------------

#' Parameter functions related to tree- and rule-based models.
#'
#' These are parameter generating functions that can be used for modeling,
#' especially in conjunction with the \pkg{parsnip} package.
#'
#' @param range A two-element vector holding the smallest and largest possible
#' values, respectively. If these cannot be set when the parameter is created,
#' `unknown()` can be used instead of a particular value. If a transformation
#' is specified, these should be in the _transformed units_. The default is
#' an educated guess at the range of each parameter.
#'
#' @param values A vector of possible parameter values. For qualitative
#' parameters, character and logical defaults have been set, but can be modified
#' as needed.
#'
#' @param trans A `trans` object from the `scales` package, such as
#' `scales::log10_trans()` or `scales::reciprocal_trans()`. If not provided,
#' the default is used which matches the units used in `range`. If no
#' transformation, `NULL`.
#'
#' @inheritParams new_quant_param
#'
#' @details
#' These functions generate parameters that are useful when the model is
#' based on trees or rules.
#'
#' * `mtry()` and `mtry_long()`: The number of predictors that will be randomly
#'   sampled at each split when creating tree models. The latter uses a
#'   log transformation and is helpful when the data set has a large number of
#'   columns. (See `parsnip::rand_forest()`).
#'
#' * `trees()`: The number of trees contained in a random forest or boosted
#'   ensemble. In the latter case, this is equal to the number of boosting
#'   iterations. (See `parsnip::rand_forest()` and `parsnip::boost_tree()`).
#'
#' * `min_n()`: The minimum number of data points in a node that are required
#'   for the node to be split further. (See `parsnip::rand_forest()` and
#'   `parsnip::boost_tree()`).
#'
#' * `sample_size()`: The size of the data set used for modeling within an
#'   iteration of the modeling algorithm, such as stochastic gradient boosting.
#'   (See `parsnip::boost_tree()`).
#'
#' * `learn_rate()`: The rate at which the boosting algorithm adapts from
#'   iteration-to-iteration. (See `parsnip::boost_tree()`).
#'
#' * `loss_reduction()`: The reduction in the loss function required to split
#'   further. (See `parsnip::boost_tree()`).
#'
#' * `tree_depth()`: The maximum depth of the tree (i.e. number of splits).
#'   (See `parsnip::boost_tree()`).
#'
#' * `prune()`: A logical for whether a tree or set of rules should be pruned.
#'
#' * `Cp()`: The cost-complexity parameter in classical CART models.
#'
#' @return
#'
#' A `param` object. The underlying functions that generate the parameters are
#' [new_quant_param()] and [new_qual_param()].
#'
#' @aliases tree_parameters
#' @export
#' @rdname tree_parameters
mtry <- function(range = c(1L, unknown()), trans = NULL, id = NULL) {
  new_quant_param(
    name = "mtry",
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "# Randomly Selected Predictors",
    id = id,
    finalize = get_p
  )
}

#' @export
#' @rdname tree_parameters
#' @importFrom scales log10_trans
mtry_long <- function(range = c(0L, unknown()), trans = log10_trans(), id = NULL) {
  new_quant_param(
    name = "mtry_long",
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "# Randomly Selected Predictors",
    id = id,
    finalize = get_log_p
  )
}

#' @rdname tree_parameters
#' @export
trees <- function(range = c(1L, 2000L), trans = NULL, id = NULL) {
  new_quant_param(
    name = "trees",
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "# Trees",
    id = id,
    finalize = NULL
  )
}

#' @rdname tree_parameters
#' @export
min_n <- function(range = c(2L, unknown()), trans = NULL, id = NULL) {
  new_quant_param(
    name = "min_n",
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "Minimal Node Size",
    id = id,
    finalize = get_n_frac
  )
}

#' @rdname tree_parameters
#' @export
sample_size <- function(range = c(unknown(), unknown()), trans = NULL, id = NULL) {
  new_quant_param(
    name = "sample_size",
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "# Observations Sampled",
    id = id,
    finalize = get_n_frac_range
  )
}

#' @rdname tree_parameters
#' @export
learn_rate <- function(range = c(unknown(), unknown()), trans = NULL, id = NULL) {
  new_quant_param(
    name = "learn_rate",
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "Learning Rate",
    id = id,
    finalize = NULL
  )
}


#' @rdname tree_parameters
#' @export
loss_reduction <- function(range = c(unknown(), unknown()), trans = NULL, id = NULL) {
  new_quant_param(
    name = "loss_reduction",
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "Minimum Loss Reduction",
    id = id,
    finalize = NULL
  )
}

#' @rdname tree_parameters
#' @export
tree_depth <- function(range = c(2L, 15L), trans = NULL, id = NULL) {
  new_quant_param(
    name = "tree_depth",
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "Tree Depth",
    id = id,
    finalize = NULL
  )
}

#' @export
#' @rdname tree_parameters
prune <- function(values = c(TRUE, FALSE), id = NULL) {
  new_qual_param(
    name = "prune",
    type = "logical",
    values = values,
    label = "Pruning",
    id = id,
    finalize = NULL
  )
}

#' @export
#' @rdname tree_parameters
Cp <- function(range = c(-10, -1), trans = log10_trans(), id = NULL) {
  new_quant_param(
    name = "Cp",
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "Cost-Complexity Parameter",
    id = id,
    finalize = NULL
  )
}

###################################################################

#' Parameter functions related to parametric models.
#'
#' @inherit mtry description
#'
#' @inheritParams mtry
#'
#' @details
#' These functions generate parameters that are useful when the model is based
#' on some type of slope/intercept model.
#'
#' * `penalty()`: The total amount of regularization used.
#' (See `parsnip::linear_reg()` and `parsnip::logistic_reg()`
#' with glmnet models).
#'
#' * `mixture()`: The proportion of L1 regularization in the model.
#' (See `parsnip::linear_reg()` and `parsnip::logistic_reg()`).
#'
#' * `dropout()`: The parameter dropout rate. (See `parsnip:::mlp()`).
#'
#' * `epochs()`: The number of iterations of training. (See `parsnip:::mlp()`).
#'
#' * `activation()`: The type of activation function between network layers.
#' (See `parsnip:::mlp()`).
#'
#' * `hidden_units()`: The number of hidden units in a network layer.
#' (See `parsnip:::mlp()`).
#'
#' * `batch_size()`: The mini-batch size for neural networks.
#'
#' * `rbf_sigma()`: The sigma parameters of a radial basis function.
#'
#' * `cost()`: A cost value for SVM models.
#'
#' * `scale_factor()`: The polynomial and hyperbolic tangent kernel
#' scaling factor.
#'
#' * `margin()`: The SVM margin parameter (e.g. epsilon in the insensitive-loss
#' function for regression).
#'
#' * `degree()`: The polynomial degree.
#'
#' * `prod_degree()`: The number of terms to combine into interactions.
#' A value of 1 implies an additive model. Useful for MARS models.
#'
#' * `num_terms()`: A nonspecific parameter for the number of terms in a model.
#' This can be used with models that include feature selection, such as MARS.
#'
#' * `num_comp()`: The number of components in a model
#' (e.g. PCA or PLS components).
#'
#' * `deg_free()`: A parameter for the degrees of freedom.
#'
#' * `prune_method()`: A parameter for pruming methods for MARS.
#'
#' @inherit mtry return
#'
#' @aliases para_parameters
#' @rdname para_parameters
#' @export
dropout <- function(range = c(0, 1), trans = NULL, id = NULL) {
  new_quant_param(
    name = "dropout",
    type = "double",
    range = range,
    inclusive = c(TRUE, FALSE),
    trans = trans,
    label = "Dropout Rate",
    id = id,
    finalize = NULL
  )
}

#' @rdname para_parameters
#' @export
epochs <- function(range = c(1L, 1000L), trans = NULL, id = NULL) {
  new_quant_param(
    name = "epochs",
    type = "integer",
    range = c(1L, 1000L),
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "# Epochs",
    id = id,
    finalize = NULL
  )
}

#' @rdname para_parameters
#' @export
activation <- function(values = values_activation, id = NULL) {
  new_qual_param(
    name = "activation",
    type = "character",
    values = values,
    label = "Activation Function",
    id = id,
    finalize = NULL
  )
}

#' @rdname default_parameters
#' @export
values_activation <- c("linear", "softmax", "relu", "elu")

#' @rdname para_parameters
#' @export
mixture <- function(range = c(0, 1), trans = NULL, id = NULL) {
  new_quant_param(
    name = "mixture",
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "% lasso Penalty",
    id = id,
    finalize = NULL
  )
}

#' @rdname para_parameters
#' @export
penalty <- function(range = c(-10, 0), trans = log10_trans(), id = NULL) {
  new_quant_param(
    name = "penalty",
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "Amount of Regularization",
    id = id,
    finalize = NULL
  )
}

#' @export
#' @rdname para_parameters
rbf_sigma <- function(range = c(-10, 0), trans = log10_trans(), id = NULL) {
  new_quant_param(
    name = "rbf_sigma",
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "Radial Basis Function sigma",
    id = id,
    finalize = get_rbf_range
  )
}

#' @export
#' @rdname para_parameters
prod_degree <- function(range = c(1L, 2L), trans = NULL, id = NULL) {
  new_quant_param(
    name = "prod_degree",
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "Degree of Interaction",
    id = id,
    finalize = NULL
  )
}

#' @export
#' @rdname para_parameters
num_terms <- function(range = c(1L, unknown()), trans = NULL, id = NULL) {
  new_quant_param(
    name = "num_terms",
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "# Model Terms",
    id = id,
    finalize = get_p
  )
}

#' @export
#' @rdname para_parameters
num_comp <- function(range = c(1L, unknown()), trans = NULL, id = NULL) {
  new_quant_param(
    name = "num_comp",
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "# Components",
    id = id,
    finalize = get_p
  )
}

#' @export
#' @rdname para_parameters
cost <- function(range = c(-10, -1), trans = log2_trans(), id = NULL) {
  new_quant_param(
    name = "cost",
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "Cost",
    id = id,
    finalize = NULL
  )
}

#' @export
#' @rdname para_parameters
scale_factor <- function(range = c(-10, -1), trans = log2_trans(), id = NULL) {
  new_quant_param(
    name = "scale_factor",
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "Scale Factor",
    id = id,
    finalize = NULL
  )
}

#' @export
#' @rdname para_parameters
margin <- function(range = c(0, .2), trans = NULL, id = NULL) {
  new_quant_param(
    name = "margin",
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "Insensitivity Margin",
    id = id,
    finalize = NULL
  )
}

#' @export
#' @rdname para_parameters
degree <- function(range = c(1, 3), trans = NULL, id = NULL) {
  new_quant_param(
    name = "degree",
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "Polynomial Degree",
    id = id,
    finalize = NULL
  )
}

#' @export
#' @rdname para_parameters
deg_free <- function(range = c(1, 5), trans = NULL, id = NULL) {
  new_quant_param(
    name = "deg_free",
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "Degrees of Freedom",
    id = id,
    finalize = NULL
  )
}

#' @export
#' @rdname para_parameters
hidden_units <- function(range = c(1L, 10), trans = NULL, id = NULL) {
  new_quant_param(
    name = "hidden_units",
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "# Hidden Units",
    id = id,
    finalize = NULL
  )
}

#' @export
#' @rdname para_parameters
#' @importFrom scales log2_trans
batch_size <- function(range = c(unknown(), unknown()), trans = log2_trans(), id = NULL) {
  new_quant_param(
    name = "batch_size",
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "Batch Size",
    id = id,
    finalize = get_batch_sizes
  )
}

#' @export
#' @rdname para_parameters
prune_method <- function(values = values_prune_method, id = NULL) {
  new_qual_param(
    name = "prune_method",
    type = c("character"),
    values = values,
    default = "backward",
    label = "Pruning Method",
    id = id,
    finalize = NULL
  )
}

#' @rdname default_parameters
#' @export
values_prune_method <- c(
  "backward", "none", "exhaustive",
  "forward", "seqrep", "cv"
)

###################################################################

#' Parameter functions related to miscellaneous models.
#'
#' @inherit mtry description
#'
#' @inheritParams mtry
#'
#' @details
#' These functions generate parameters that are useful in a variety of
#' models.
#'
#' * `weight_func()`: The type of kernel function that weights the distances
#' between samples (e.g. in a K-nearest neighbors model).
#'
#' * `surv_dist()`: The statistical distribution of the data in a survival
#'   analysis model. (See `parsnip::surv_reg()`) .
#'
#' * `Laplace()`: The Laplace correction used to smooth low-frequency counts.
#'
#' * `neighbors()`: A parameter for the number of neighbors used in a prototype
#' model.
#'
#' * `dist_power()`: The order parameter used in calculating a
#' Minkowski distance.
#'
#' * `threshold()`: A general thresholding parameter for values
#' between `[0, 1]`.
#'
#' @inherit mtry return
#'
#' @aliases misc_parameters
#' @rdname misc_parameters
#' @export
weight_func <- function(values = values_weight_func, id = NULL) {
  new_qual_param(
    name = "weight_func",
    type = "character",
    values = values,
    label = "Distance Weighting Function",
    id = id,
    finalize = NULL
  )
}

#' @rdname default_parameters
#' @export
values_weight_func <- c("rectangular", "triangular", "epanechnikov",
                        "biweight", "triweight", "cos", "inv",
                        "gaussian", "rank", "optimal")

# in reference to survival::survreg
#' @rdname misc_parameters
#' @export
surv_dist <- function(values = values_surv_dist, id = NULL) {
  new_qual_param(
    name = "surv_dist",
    type = "character",
    values = values,
    label = "Distribution",
    id = id,
    finalize = NULL
  )
}

#' @rdname default_parameters
#' @export
values_surv_dist <- c("weibull", "exponential", "gaussian",
                      "logistic", "lognormal", "loglogistic")

#' @export
#' @rdname misc_parameters
Laplace <- function(range = c(0, 3), trans = NULL, id = NULL) {
  new_quant_param(
    name = "Laplace",
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    default = 0,
    label = "Laplace Correction",
    id = id,
    finalize = NULL
  )
}


#' @export
#' @rdname misc_parameters
neighbors <- function(range = c(1L, unknown()), trans = NULL, id = NULL) {
  new_quant_param(
    name = "neighbors",
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "# Nearest Neighbors",
    id = id,
    finalize = get_n_frac
  )
}

#' @export
#' @rdname misc_parameters
dist_power <- function(range = c(1, 2), trans = NULL, id = NULL) {
  new_quant_param(
    name = "dist_power",
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "Minkowski Distance Order",
    id = id,
    finalize = NULL
  )
}


#' @export
#' @rdname misc_parameters
threshold <- function(range = c(0, 1), trans = NULL, id = NULL) {
  new_quant_param(
    name = "threshold",
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    default = 0.5,
    label = "Threshold",
    id = id,
    finalize = NULL
  )
}


###################################################################

#' Parameter functions related to text analysis.
#'
#' These are parameter generating functions that can be used for modeling,
#' especially in conjunction with the \pkg{textrecipes} package.
#'
#' @inheritParams mtry
#'
#' @details
#' These functions generate parameters that are useful in a variety of
#' text models.
#'
#' * `min_times()`, `max_times()`: The frequency of word occurances for removal.
#' (See `textrecipes::step_tokenfilter()`).
#'
#' * `max_tokens()`: The number of tokens that will be retained. (See
#' `textrecipes::step_tokenfilter`).
#'
#' * `weight()`: A parameter for `"double normalization"` when creating token
#' counts. (See `textrecipes::step_tf()`).
#'
#' * `weight_scheme()`: The method for term frequency calculations.
#' (See `textrecipes::step_tf()`).
#'
#' * `token()`: The type of token. (See `textrecipes::step_tokenize()`).
#'
#' @return Each object is generated by either `new_quant_param()` or
#' `new_qual_param()`.
#'
#' @name text_parameters
#'
NULL

#' @export
#' @rdname text_parameters
weight <- function(range = c(-10, 0), trans = log10_trans(), id = NULL) {
  new_quant_param(
    name = "weight",
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "Weight",
    id = id,
    finalize = NULL
  )
}

#' @export
#' @rdname text_parameters
weight_scheme <- function(values = values_weight_scheme, id = NULL) {
  new_qual_param(
    name = "weight_scheme",
    type = "character",
    values = values,
    label = "Term Frequency Weight Method",
    id = id,
    finalize = NULL
  )
}

#' @rdname default_parameters
#' @export
values_weight_scheme <- c("raw count", "binary",
                          "term frequency", "log normalization",
                          "double normalization")

#' @export
#' @rdname text_parameters
token <- function(values = values_token, id = NULL) {
  new_qual_param(
    name = "token",
    type = "character",
    values = values,
    label = "Token Unit",
    id = id,
    finalize = NULL
  )
}

#' @rdname default_parameters
#' @export
values_token <- c("words", "characters", "character_shingle",
                  "lines", "ngrams", "paragraphs", "ptb", "regex",
                  "sentences", "skip_ngrams", "tweets",
                  "word_stems")

#' @export
#' @rdname text_parameters
max_times <- function(range = c(1L, as.integer(10^5)), trans = NULL, id = NULL) {
  new_quant_param(
    name = "max_times",
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "Maximum Token Frequency",
    id = id,
    finalize = NULL
  )
}


#' @export
#' @rdname text_parameters
min_times <- function(range = c(0L, 1000L), trans = NULL, id = NULL) {
  new_quant_param(
    name = "min_times",
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "Minimum Token Frequency",
    id = id,
    finalize = NULL
  )
}


#' @export
#' @rdname text_parameters
max_tokens <- function(range = c(0L, as.integer(10^5)), trans = NULL, id = NULL) {
  new_quant_param(
    name = "max_tokens",
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = "# Retained Tokens",
    id = id,
    finalize = NULL
  )
}




