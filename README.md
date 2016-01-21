# MU Models

This is an R package to make it easier to quickly and automatically build predictive models for Millikin University's Alumni & Development Center.

The package provides three functions:

- `buildAFpredictors()` - builds a data frame of data to do predictions (and/or) training with.
- `buildAFmodels()`  - builds a list of models. By defaults, builds lm, glm and rf models.
- `predictAF()` - builds a data frame of predictions from a list of models (as generated by `buildAFmodels()`).

This package is not suited for general use--field names specific to our data are hard coded.