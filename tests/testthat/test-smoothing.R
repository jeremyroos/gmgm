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
gmm_3 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(2, 0, 4, 3, 1, 5), 3,
                          dimnames = list(c("B", "A", "A.1"), NULL)),
              sigma = list(matrix(c(2, 1, 2, 1, 1, 1, 2, 1, 3), 3,
                                  dimnames = list(c("B", "A", "A.1"),
                                                  c("B", "A", "A.1"))),
                           matrix(c(5, 3, 2, 3, 2, 1, 2, 1, 4), 3,
                                  dimnames = list(c("B", "A", "A.1"),
                                                  c("B", "A", "A.1")))))
class(gmm_3) <- "gmm"
gmm_4 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 2, 1, 3), 2,
                          dimnames = list(c("A", "A.1"), NULL)),
              sigma = list(matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("A", "A.1"),
                                                  c("A", "A.1"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "A.1"),
                                                  c("A", "A.1")))))
class(gmm_4) <- "gmm"

gmbn_1 <- list(A = gmm_1, B = gmm_2)
class(gmbn_1) <- "gmbn"
gmbn_2 <- list(A = gmm_1, B = gmm_3)
class(gmbn_2) <- "gmbn"
gmbn_3 <- list(A = gmm_4, B = gmm_3)
class(gmbn_3) <- "gmbn"

gmdbn_1 <- list(b_1 = gmbn_1, b_2 = gmbn_2, b_3 = gmbn_3)
class(gmdbn_1) <- "gmdbn"
gmdbn_2 <- list(b_1 = gmbn_1)
class(gmdbn_2) <- "gmdbn"

test_that("perform smoothing inference", {
  set.seed(0)
  expect_equal(smoothing(gmdbn_1,
                         data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                    A = c(0, NA, 6, NA, 3, NA),
                                    B = c(NA, 7, NA, 6, NA, 9)),
                         col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(0, 2.4, 6, 1.33, 3, 1.11),
                      B = c(0.411, 7, 9.51, 6, 4.58, 9)),
               tolerance = 1e-02)
})

test_that("perform smoothing inference with unordered rows", {
  set.seed(0)
  expect_equal(smoothing(gmdbn_1,
                         data.frame(seq = c(2, 1, 2, 1, 2, 1),
                                    A = c(NA, 0, 3, NA, NA, 6),
                                    B = c(6, NA, NA, 7, 9, NA)),
                         col_seq = "seq", n_part = 2),
               tibble(seq = c(2, 1, 2, 1, 2, 1),
                      A = c(1.33, 0, 3, 2.4, 1.11, 6),
                      B = c(6, 0.411, 4.58, 7, 9, 9.51)),
               tolerance = 1e-02)
})

test_that("perform smoothing inference with extra columns", {
  set.seed(0)
  expect_equal(smoothing(gmdbn_1,
                         data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                    A = c(0, NA, 6, NA, 3, NA),
                                    B = c(NA, 7, NA, 6, NA, 9),
                                    C = c(0, NA, 0, NA, 0, NA)),
                         col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(0, 2.4, 6, 1.33, 3, 1.11),
                      B = c(0.411, 7, 9.51, 6, 4.58, 9)),
               tolerance = 1e-02)
})

test_that("perform smoothing inference with missing columns", {
  set.seed(0)
  expect_equal(smoothing(gmdbn_1,
                         data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                    A = c(0, NA, 6, NA, 3, NA)),
                         col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(0, 0.764, 6, 0.502, 3, 0.673),
                      B = c(0.411, 1.68, 10.1, 2.15, 5.01, 2.77)),
               tolerance = 1e-02)
})

test_that("perform smoothing inference with one observation sequence", {
  set.seed(0)
  expect_equal(smoothing(gmdbn_1, data.frame(A = c(0, NA, 6), B = c(NA, 7, NA)),
                         n_part = 2),
               tibble(A = c(0, 0.415, 6), B = c(2.44, 7, 9.29)),
               tolerance = 1e-02)
})

test_that("perform smoothing inference with no row", {
  expect_equal(smoothing(gmdbn_1,
                         data.frame(seq = numeric(), A = numeric(),
                                    B = numeric()),
                         col_seq = "seq"),
               tibble(seq = numeric(), A = numeric(), B = numeric()))
  expect_equal(smoothing(gmdbn_1, data.frame(A = numeric(), B = numeric())),
               tibble(A = numeric(), B = numeric()))
})

test_that("perform smoothing inference with no column", {
  set.seed(0)
  expect_equal(smoothing(gmdbn_1, data.frame(c(NA, NA, NA))[, FALSE],
                         n_part = 2),
               tibble(A = c(0.502, - 0.15, - 1.28),
                      B = c(1.85, 0.534, - 0.943)),
               tolerance = 1e-02)
})

test_that("perform smoothing inference with no row and no column", {
  expect_equal(smoothing(gmdbn_1, data.frame()),
               tibble(A = numeric(), B = numeric()))
})

test_that("perform smoothing inference for unordered nodes", {
  set.seed(0)
  expect_equal(smoothing(gmdbn_1,
                         data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                    A = c(0, NA, 6, NA, 3, NA),
                                    B = c(NA, 7, NA, 6, NA, 9)),
                         nodes = c("B", "A"), col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(0, 2.4, 6, 1.33, 3, 1.11),
                      B = c(0.411, 7, 9.51, 6, 4.58, 9)),
               tolerance = 1e-02)
})

test_that("perform smoothing inference for duplicated nodes", {
  set.seed(0)
  expect_equal(smoothing(gmdbn_1,
                         data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                    A = c(0, NA, 6, NA, 3, NA),
                                    B = c(NA, 7, NA, 6, NA, 9)),
                         nodes = c("A", "B", "B"), col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(0, 2.4, 6, 1.33, 3, 1.11),
                      B = c(0.411, 7, 9.51, 6, 4.58, 9)),
               tolerance = 1e-02)
})

test_that("perform smoothing inference for not all the nodes", {
  set.seed(0)
  expect_equal(smoothing(gmdbn_1,
                         data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                    A = c(0, NA, 6, NA, 3, NA),
                                    B = c(NA, 7, NA, 6, NA, 9)),
                         nodes = "B", col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      B = c(0.411, 7, 9.51, 6, 4.58, 9)),
               tolerance = 1e-02)
})

test_that("perform smoothing inference with duplicated observation sequence column names", {
  set.seed(0)
  expect_equal(smoothing(gmdbn_1,
                         data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                    A = c(0, NA, 6, NA, 3, NA),
                                    B = c(NA, 7, NA, 6, NA, 9)),
                         col_seq = c("seq", "seq"), n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(0, 2.4, 6, 1.33, 3, 1.11),
                      B = c(0.411, 7, 9.51, 6, 4.58, 9)),
               tolerance = 1e-02)
})

test_that("perform smoothing inference in a gmdbn object with one gmbn element", {
  set.seed(0)
  expect_equal(smoothing(gmdbn_2,
                         data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                    A = c(0, NA, 6, NA, 3, NA),
                                    B = c(NA, 7, NA, 6, NA, 9)),
                         col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(0, 1.24, 6, - 0.952, 3, - 0.0398),
                      B = c(1.07, 7, 10.2, 6, 5.77, 9)),
               tolerance = 1e-02)
  set.seed(0)
})

test_that("perform smoothing inference with several subsets of particles", {
  set.seed(0)
  expect_equal(smoothing(gmdbn_1,
                         data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                    A = c(0, NA, 6, NA, 3, NA),
                                    B = c(NA, 7, NA, 6, NA, 9)),
                         col_seq = "seq", n_part = 2, max_part_sim = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(0, 0.415, 6, - 0.0177, 3, 0.64),
                      B = c(2.44, 7, 9.29, 6, 4.95, 9)),
               tolerance = 1e-02)
})

test_that("perform smoothing inference with verbose", {
  set.seed(0)
  expect_equal(smoothing(gmdbn_1,
                         data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                    A = c(0, NA, 6, NA, 3, NA),
                                    B = c(NA, 7, NA, 6, NA, 9)),
                         col_seq = "seq", n_part = 2, verbose = TRUE),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(0, 2.4, 6, 1.33, 3, 1.11),
                      B = c(0.411, 7, 9.51, 6, 4.58, 9)),
               tolerance = 1e-02)
})
