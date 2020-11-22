gmm_1 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 2, 4, 1, 3, 5), 3,
                          dimnames = list(c("A", "B", "C"), NULL)),
              sigma = list(matrix(c(1, 1, 1, 1, 2, 2, 1, 2, 3), 3,
                                  dimnames = list(c("A", "B", "C"),
                                                  c("A", "B", "C"))),
                           matrix(c(2, 3, 1, 3, 5, 2, 1, 2, 4), 3,
                                  dimnames = list(c("A", "B", "C"),
                                                  c("A", "B", "C")))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = 1,
              mu = matrix(0, 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(1, 2, 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_2) <- "gmm"

test_that("conditionalize a gmm object", {
  expect_equal(conditional(gmm_1, "C"),
               list(alpha = c(0.4, 0.6),
                    mu_x = matrix(c(0, 2, 1, 3), 2,
                                  dimnames = list(c("A", "B"), NULL)),
                    sigma_x = list(matrix(c(1, 1, 1, 2), 2,
                                          dimnames = list(c("A", "B"),
                                                          c("A", "B"))),
                                   matrix(c(2, 3, 3, 5), 2,
                                          dimnames = list(c("A", "B"),
                                                          c("A", "B")))),
                    coeff = list(matrix(c(2, 0, 1),
                                        dimnames = list(c("(Intercept)", "A",
                                                          "B"),
                                                        "C")),
                                 matrix(c(3, - 1, 1),
                                        dimnames = list(c("(Intercept)", "A",
                                                          "B"),
                                                        "C"))),
                    sigma_c = list(matrix(1, dimnames = list("C", "C")),
                                   matrix(3, dimnames = list("C", "C")))))
})

test_that("conditionalize a gmm object with unordered dependent variables", {
  expect_equal(conditional(gmm_1, c("C", "B")),
               list(alpha = c(0.4, 0.6),
                    mu_x = matrix(c(0, 1), 1, dimnames = list("A", NULL)),
                    sigma_x = list(matrix(1, dimnames = list("A", "A")),
                                   matrix(2, dimnames = list("A", "A"))),
                    coeff = list(matrix(c(2, 1, 4, 1), 2,
                                        dimnames = list(c("(Intercept)", "A"),
                                                        c("B", "C"))),
                                 matrix(c(1.5, 1.5, 4.5, 0.5), 2,
                                        dimnames = list(c("(Intercept)", "A"),
                                                        c("B", "C")))),
                    sigma_c = list(matrix(c(1, 1, 1, 2), 2,
                                          dimnames = list(c("B", "C"),
                                                          c("B", "C"))),
                                   matrix(c(0.5, 0.5, 0.5, 3.5), 2,
                                          dimnames = list(c("B", "C"),
                                                          c("B", "C"))))))
})

test_that("conditionalize a gmm object with duplicated dependent variables", {
  expect_equal(conditional(gmm_1, c("C", "C")),
               list(alpha = c(0.4, 0.6),
                    mu_x = matrix(c(0, 2, 1, 3), 2,
                                  dimnames = list(c("A", "B"), NULL)),
                    sigma_x = list(matrix(c(1, 1, 1, 2), 2,
                                          dimnames = list(c("A", "B"),
                                                          c("A", "B"))),
                                   matrix(c(2, 3, 3, 5), 2,
                                          dimnames = list(c("A", "B"),
                                                          c("A", "B")))),
                    coeff = list(matrix(c(2, 0, 1),
                                        dimnames = list(c("(Intercept)", "A",
                                                          "B"),
                                                        "C")),
                                 matrix(c(3, - 1, 1),
                                        dimnames = list(c("(Intercept)", "A",
                                                          "B"),
                                                        "C"))),
                    sigma_c = list(matrix(1, dimnames = list("C", "C")),
                                   matrix(3, dimnames = list("C", "C")))))
})

test_that("conditionalize a gmm object with no explanatory variable", {
  expect_equal(conditional(gmm_1, c("A", "B", "C")),
               list(alpha = c(0.4, 0.6),
                    mu_x = matrix(numeric(), ncol = 2,
                                  dimnames = list(character(), NULL)),
                    sigma_x = list(matrix(numeric(), 0, 0,
                                          dimnames = list(character(),
                                                          character())),
                                   matrix(numeric(), 0, 0,
                                          dimnames = list(character(),
                                                          character()))),
                    coeff = list(matrix(c(0, 2, 4), 1,
                                        dimnames = list("(Intercept)",
                                                        c("A", "B", "C"))),
                                 matrix(c(1, 3, 5), 1,
                                        dimnames = list("(Intercept)",
                                                        c("A", "B", "C")))),
                    sigma_c = list(matrix(c(1, 1, 1, 1, 2, 2, 1, 2, 3), 3,
                                          dimnames = list(c("A", "B", "C"),
                                                          c("A", "B", "C"))),
                                   matrix(c(2, 3, 1, 3, 5, 2, 1, 2, 4), 3,
                                          dimnames = list(c("A", "B", "C"),
                                                          c("A", "B", "C"))))))
})

test_that("conditionalize a gmm object with non-positive definite covariance matrices", {
  expect_equal(conditional(gmm_2, "B"),
               list(alpha = 1, mu_x = matrix(0, dimnames = list("A", NULL)),
                    sigma_x = list(matrix(1, dimnames = list("A", "A"))),
                    coeff = list(matrix(c(0, 1),
                                        dimnames = list(c("(Intercept)", "A"),
                                                        "B"))),
                    sigma_c = list(matrix(2.225074e-308,
                                          dimnames = list("B", "B")))),
               tolerance = 0.01)
})
