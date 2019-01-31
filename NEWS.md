# dials 0.0.2.9000

## Parameter sets

`param_set`s are a new feature in dials that are used to create parameter sets
from multiple individual dials parameters. These parameter sets can then be
used to generate hyperparameter grids. The grids can be regular, random, or
a mix of both. To learn more, read the documentation (and examples!) for the 
functions: `param_set()`, `attach_param()`, and `grid_generate()`.

## Breaking changes

* All parameter _objects_ are now parameter _functions_. For example, the 
pre-configured object `mtry` is now a function, `mtry()`, with arguments for the
`range` and the `trans`form. This provides greater flexibility in parameter
creation, and should feel more natural.

* The `label` argument to `new_quant_param()` and `new_qual_param()` has been separated into `name` and `label` for clarity. This is a developer-facing change and only matters if you create your own custom parameters.

## Other changes

* The pipe `%>%` is now re-exported.

* There is a new `"list"` type for custom qualitative parameters. This is 
extremely flexible and accepts any input for `values` as long as it is wrapped
in `list()`. This can be useful for complex varying parameters.

* A few parameters used an incorrect `name`. These have been fixed.

* `scales` has been moved from Depends to Imports and the functions 
`scales::log10_trans()` and `scales::log2_trans()` have been re-exported here.

* `dplyr` has been moved to Suggests.

# dials 0.0.2

* Parameter objects now contain code to finalize their values and a number of helper functions for certain data-specific parameters. A `force` option can be used to avoid updating the values.  
* Parameter objects are printed differently inside of tibbles. 
* `regularization` was changed to `penalty` in a few models to be consistent with [this change](tidymodels/model-implementation-principles@08d3afd). 
* `batch_size` and `threshold` were added.
* Added a set of parameters for the `textrecipes` package [issue 16](https://github.com/tidymodels/dials/issues/16). 

# dials 0.0.1

* First CRAN version
