library(testthat)
library(cynthiayu8010)

test_that("OR_95CI function works correctly", {
  # load file
  data(toydata)

  # fit a logistic regression model
  mod <- glm(y ~ x1 + x2, data = toydata, family = binomial)

  # extract coefficient and standard error
  coef <- coef(mod)
  se <- sqrt(diag(vcov(mod)))

  # run the OR_95CI function
  result <- OR_95CI(coef, se, siglevel = 0.05, roundto = 2)

  # check the output format
  expect_type(result, "character")
  expect_length(result, length(coef))
  expect_equal(result, c("0.96(0.58,1.58)",
                         "2.73(1.66,4.49)",
                         "2.44(0.76,7.84)"))

  # other tests
  expect_equal(OR_95CI(0, 0.5, siglevel = 0.05, roundto = 2),
               "1.00(0.38,2.66)")
})
