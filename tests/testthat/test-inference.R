gmm_1 <- list(alpha = 1, mu = matrix(0, dimnames = list("A", NULL)),
              sigma = list(matrix(1, dimnames = list("A", "A"))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(2, 0, 3, 1), 2, dimnames = list(c("B", "A"), NULL)),
              sigma = list(matrix(c(2, 1, 1, 1), 2,
                                  dimnames = list(c("B", "A"), c("B", "A"))),
                           matrix(c(5, 3, 3, 2), 2,
                                  dimnames = list(c("B", "A"), c("B", "A")))))
class(gmm_2) <- "gmm"

gmbn_1 <- list(A = gmm_1, B = gmm_2)
class(gmbn_1) <- "gmbn"

test_that("perform inference", {
  set.seed(0)
  expect_equal(inference(gmbn_1, data.frame(A = c(0, NA, 6), B = c(NA, 7, NA)),
                         n_part = 2),
               tibble(A = c(0, 1.33, 6), B = c(0.627, 7, 10.4)),
               tolerance = 1e-02)
})

test_that("perform inference with extra columns", {
  set.seed(0)
  expect_equal(inference(gmbn_1,
                         data.frame(A = c(0, NA, 6), B = c(NA, 7, NA),
                                    C = c(0, NA, 0)),
                         n_part = 2),
               tibble(A = c(0, 1.33, 6), B = c(0.627, 7, 10.4)),
               tolerance = 1e-02)
})

test_that("perform inference with missing columns", {
  set.seed(0)
  expect_equal(inference(gmbn_1, data.frame(A = c(0, NA, 6)), n_part = 2),
               tibble(A = c(0, 0.502, 6), B = c(1.07, 3.1, 10.5)),
               tolerance = 1e-02)
})

test_that("perform inference with no column", {
  set.seed(0)
  expect_equal(inference(gmbn_1, data.frame(c(NA, NA, NA))[, FALSE],
                         n_part = 2),
               tibble(A = c(0.844, - 1.23, - 0.15), B = c(2.23, - 0.408, 1.14)),
               tolerance = 1e-02)
})

test_that("perform inference with no row", {
  expect_equal(inference(gmbn_1, data.frame(A = numeric(), B = numeric())),
               tibble(A = numeric(), B = numeric()))
})

test_that("perform inference with no row and no column", {
  expect_equal(inference(gmbn_1, data.frame()),
               tibble(A = numeric(), B = numeric()))
})

test_that("perform inference for unordered nodes", {
  set.seed(0)
  expect_equal(inference(gmbn_1, data.frame(A = c(0, NA, 6), B = c(NA, 7, NA)),
                         nodes = c("B", "A"), n_part = 2),
               tibble(A = c(0, 1.33, 6), B = c(0.627, 7, 10.4)),
               tolerance = 1e-02)
})

test_that("perform inference for duplicated nodes", {
  set.seed(0)
  expect_equal(inference(gmbn_1, data.frame(A = c(0, NA, 6), B = c(NA, 7, NA)),
                         nodes = c("A", "B", "B"), n_part = 2),
               tibble(A = c(0, 1.33, 6), B = c(0.627, 7, 10.4)),
               tolerance = 1e-02)
})

test_that("perform inference for not all the nodes", {
  set.seed(0)
  expect_equal(inference(gmbn_1, data.frame(A = c(0, NA, 6), B = c(NA, 7, NA)),
                         nodes = "B", n_part = 2),
               tibble(B = c(0.627, 7, 10.4)), tolerance = 1e-02)
})

test_that("perform inference with several subsets of particles", {
  set.seed(0)
  expect_equal(inference(gmbn_1, data.frame(A = c(0, NA, 6), B = c(NA, 7, NA)),
                         n_part = 2, max_part_sim = 2),
               tibble(A = c(0, - 0.563, 6), B = c(2.06, 7, 10.4)),
               tolerance = 1e-02)
})

test_that("perform inference with verbose", {
  set.seed(0)
  expect_equal(inference(gmbn_1, data.frame(A = c(0, NA, 6), B = c(NA, 7, NA)),
                         n_part = 2, verbose = TRUE),
               tibble(A = c(0, 1.33, 6), B = c(0.627, 7, 10.4)),
               tolerance = 1e-02)
})
