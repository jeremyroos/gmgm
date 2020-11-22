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

test_that("perform predictive inference", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9)),
                          col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(1.3, 0.255, - 0.488, - 0.563, 0.374, 0.329),
                      B = c(3.64, 0.575, 0.822, 1.72, 1.29, 1.65)),
               tolerance = 1e-02)
})

test_that("perform predictive inference with unordered rows in the evidence", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = c(2, 1, 2, 1, 2, 1),
                                     A = c(NA, 0, 3, NA, NA, 6),
                                     B = c(6, NA, NA, 7, 9, NA)),
                          col_seq = "seq", n_part = 2),
               tibble(seq = c(2, 1, 2, 1, 2, 1),
                      A = c(- 0.563, 1.3, 0.374, 0.255, 0.329, - 0.488),
                      B = c(1.72, 3.64, 1.29, 0.575, 1.65, 0.822)),
               tolerance = 1e-02)
})

test_that("perform predictive inference with extra columns in the evidence", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9),
                                     C = c(0, NA, 0, NA, 0, NA)),
                          col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(1.3, 0.255, - 0.488, - 0.563, 0.374, 0.329),
                      B = c(3.64, 0.575, 0.822, 1.72, 1.29, 1.65)),
               tolerance = 1e-02)
})

test_that("perform predictive inference with missing columns in the evidence", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA)),
                          col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(1.3, 0.374, - 1.13, - 0.563, 0.795, 0.632),
                      B = c(3.64, 0.828, 0.416, 1.72, 1.01, 2.02)),
               tolerance = 1e-02)
})

test_that("perform predictive inference with one observation sequence", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(A = c(0, NA, 6), B = c(NA, 7, NA)),
                          n_part = 2),
               tibble(A = c(0.502, - 0.0177, - 1.32),
                      B = c(1.85, 0.574, - 0.588)),
               tolerance = 1e-02)
})

test_that("perform predictive inference with no row in the evidence", {
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = numeric(), A = numeric(),
                                     B = numeric()),
                          col_seq = "seq"),
               tibble(seq = numeric(), A = numeric(), B = numeric()))
  expect_equal(prediction(gmdbn_1, data.frame(A = numeric(), B = numeric())),
               tibble(A = numeric(), B = numeric()))
})

test_that("perform predictive inference with no column in the evidence", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1, data.frame(c(NA, NA, NA))[, FALSE],
                          n_part = 2),
               tibble(A = c(0.502, - 0.294, - 0.883),
                      B = c(1.85, - 0.134, - 1.24)),
               tolerance = 1e-02)
})

test_that("perform predictive inference with no row and no column in the evidence", {
  expect_equal(prediction(gmdbn_1, data.frame()),
               tibble(A = numeric(), B = numeric()))
})

test_that("perform predictive inference with a predicted evidence", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9)),
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9)),
                          col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(0, - 0.317, 6, 1.32, 3, 0.532),
                      B = c(1.1, 7, 9.16, 6, 4.99, 9)),
               tolerance = 1e-02)
})

test_that("perform predictive inference with unordered rows in the predicted evidence", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9)),
                          data.frame(seq = c(2, 1, 2, 1, 2, 1),
                                     A = c(NA, 0, 3, NA, NA, 6),
                                     B = c(6, NA, NA, 7, 9, NA)),
                          col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(0, - 0.317, 6, 1.32, 3, 0.532),
                      B = c(1.1, 7, 9.16, 6, 4.99, 9)),
               tolerance = 1e-02)
})

test_that("perform predictive inference with extra rows in the predicted evidence", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9)),
                          data.frame(seq = c(1, 1, 1, 1, 2, 2, 2, 2),
                                     A = c(0, NA, 6, NA, NA, 3, NA, 0),
                                     B = c(NA, 7, NA, 0, 6, NA, 9, NA)),
                          col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(0, - 0.317, 6, 1.32, 3, 0.532),
                      B = c(1.1, 7, 9.16, 6, 4.99, 9)),
               tolerance = 1e-02)
})

test_that("perform predictive inference with missing rows in the predicted evidence", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9)),
                          data.frame(seq = c(1, 1, 2, 2), A = c(0, NA, NA, 3),
                                     B = c(NA, 7, 6, NA)),
                          col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(0, - 0.317, - 0.878, 1.32, 3, 0.155),
                      B = c(1.1, 7, 0.508, 6, 4.99, 1.78)),
               tolerance = 1e-02)
})

test_that("perform predictive inference with extra observation sequences in the predicted evidence", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9)),
                          data.frame(seq = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
                                     A = c(0, NA, 6, NA, 3, NA, 0, NA, 0),
                                     B = c(NA, 7, NA, 6, NA, 9, NA, 0, NA)),
                          col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(0, - 0.317, 6, 1.32, 3, 0.532),
                      B = c(1.1, 7, 9.16, 6, 4.99, 9)),
               tolerance = 1e-02)
})

test_that("perform predictive inference with missing observation sequences in the predicted evidence", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9)),
                          data.frame(seq = c(1, 1, 1), A = c(0, NA, 6),
                                     B = c(NA, 7, NA)),
                          col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(0, 0.436, 6, 0.502, 0.0766, 1.26),
                      B = c(0.627, 7, 9.64, 2.15, 1.32, 3.85)),
               tolerance = 1e-02)
})

test_that("perform predictive inference with extra columns in the predicted evidence", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9)),
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9),
                                     C = c(0, NA, 0, NA, 0, NA)),
                          col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(0, - 0.317, 6, 1.32, 3, 0.532),
                      B = c(1.1, 7, 9.16, 6, 4.99, 9)),
               tolerance = 1e-02)
})

test_that("perform predictive inference with missing columns in the predicted evidence", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9)),
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA)),
                          col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(0, - 0.228, 6, 0.502, 3, 1.26),
                      B = c(0.627, 0.694, 9.64, 2.15, 5.5, 3.85)),
               tolerance = 1e-02)
})

test_that("perform predictive inference with no row in the predicted evidence", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9)),
                          data.frame(seq = numeric(), A = numeric(),
                                     B = numeric()),
                          col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(1.3, 0.255, - 0.488, - 0.563, 0.374, 0.329),
                      B = c(3.64, 0.575, 0.822, 1.72, 1.29, 1.65)),
               tolerance = 1e-02)
})

test_that("perform predictive inference with no column in the predicted evidence", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(A = c(0, NA, 6), B = c(NA, 7, NA)),
                          data.frame(c(NA, NA, NA))[, FALSE], n_part = 2),
               tibble(A = c(0.502, - 0.0177, - 1.32),
                      B = c(1.85, 0.574, - 0.588)),
               tolerance = 1e-02)
})

test_that("perform predictive inference with no row and no column in the predicted evidence", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(A = c(0, NA, 6), B = c(NA, 7, NA)),
                          data.frame(), n_part = 2),
               tibble(A = c(0.502, - 0.0177, - 1.32),
                      B = c(1.85, 0.574, - 0.588)),
               tolerance = 1e-02)
})

test_that("perform predictive inference for unordered nodes", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9)),
                          nodes = c("B", "A"), col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(1.3, 0.255, - 0.488, - 0.563, 0.374, 0.329),
                      B = c(3.64, 0.575, 0.822, 1.72, 1.29, 1.65)),
               tolerance = 1e-02)
})

test_that("perform predictive inference for duplicated nodes", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9)),
                          nodes = c("A", "B", "B"), col_seq = "seq",
                          n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(1.3, 0.255, - 0.488, - 0.563, 0.374, 0.329),
                      B = c(3.64, 0.575, 0.822, 1.72, 1.29, 1.65)),
               tolerance = 1e-02)
})

test_that("perform predictive inference for not all the nodes", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9)),
                          nodes = "B", col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      B = c(3.64, 0.575, 0.822, 1.72, 1.29, 1.65)),
               tolerance = 1e-02)
})

test_that("perform predictive inference with duplicated observation sequence column names", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9)),
                          col_seq = c("seq", "seq"), n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(1.3, 0.255, - 0.488, - 0.563, 0.374, 0.329),
                      B = c(3.64, 0.575, 0.822, 1.72, 1.29, 1.65)),
               tolerance = 1e-02)
})

test_that("perform predictive inference for several time horizons", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9)),
                          col_seq = "seq", horizon = c(1, 2), n_part = 2),
               list(hor_1 = tibble(seq = c(1, 1, 1, 2, 2, 2),
                                   A = c(1.3, 0.0386, - 0.854, - 0.563, 1.07,
                                         0.126),
                                   B = c(3.64, 1.26, - 0.37, 1.72, 2.54, 1.8)),
                    hor_2 = tibble(seq = c(1, 1, 1, 2, 2, 2),
                                   A = c(NA, - 0.355, - 1.04, NA, - 0.32,
                                         - 0.608),
                                   B = c(NA, 0.703, - 0.589, NA, 0.0321,
                                         1.47))),
               tolerance = 1e-02)
})

test_that("perform predictive inference for several time horizons with no row in the evidence", {
  expect_equal(prediction(gmdbn_1,
                         data.frame(seq = numeric(), A = numeric(),
                                    B = numeric()),
                         col_seq = "seq", horizon = c(1, 2)),
               list(hor_1 = tibble(seq = numeric(), A = numeric(),
                                   B = numeric()),
                    hor_2 = tibble(seq = numeric(), A = numeric(),
                                   B = numeric())),
               tolerance = 1e-02)
})

test_that("perform predictive inference for duplicated time horizons", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                         data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                    A = c(0, NA, 6, NA, 3, NA),
                                    B = c(NA, 7, NA, 6, NA, 9)),
                         col_seq = "seq", horizon = c(1, 1), n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(1.3, 0.255, - 0.488, - 0.563, 0.374, 0.329),
                      B = c(3.64, 0.575, 0.822, 1.72, 1.29, 1.65)),
               tolerance = 1e-02)
})

test_that("perform predictive inference in a gmdbn object with one gmbn element", {
  set.seed(0)
  expect_equal(prediction(gmdbn_2,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9)),
                          col_seq = "seq", n_part = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(- 0.612, 1.2, - 0.0177, - 0.719, - 0.355, - 0.32),
                      B = c(0.516, 3.33, 1.63, 0.449, 1.84, 1.26)),
               tolerance = 1e-02)
})

test_that("perform predictive inference in a gmdbn object with one gmbn element and several time horizons", {
  set.seed(0)
  expect_equal(prediction(gmdbn_2,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9)),
                          col_seq = "seq", horizon = c(1, 2), n_part = 2),
               list(hor_1 = tibble(seq = c(1, 1, 1, 2, 2, 2),
                                   A = c(- 0.612, 1.2, - 0.0177, - 0.719,
                                         - 0.355, - 0.32),
                                   B = c(0.516, 3.33, 1.63, 0.449, 1.84, 1.26)),
                    hor_2 = tibble(seq = c(1, 1, 1, 2, 2, 2),
                                   A = c(NA, 1.2, - 0.0177, NA, - 0.355,
                                         - 0.32),
                                   B = c(NA, 3.33, 1.63, NA, 1.84, 1.26))),
               tolerance = 1e-02)
})

test_that("perform predictive inference with several subset of particles", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9)),
                          col_seq = "seq", n_part = 2, max_part_sim = 2),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(0.502, - 0.0177, - 1.32, 0.795, 0.939, 0.195),
                      B = c(1.85, 0.574, - 0.588, 2.18, 2.39, 1.45)),
               tolerance = 1e-02)
})

test_that("perform predictive inference", {
  set.seed(0)
  expect_equal(prediction(gmdbn_1,
                          data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                     A = c(0, NA, 6, NA, 3, NA),
                                     B = c(NA, 7, NA, 6, NA, 9)),
                          col_seq = "seq", n_part = 2, verbose = TRUE),
               tibble(seq = c(1, 1, 1, 2, 2, 2),
                      A = c(1.3, 0.255, - 0.488, - 0.563, 0.374, 0.329),
                      B = c(3.64, 0.575, 0.822, 1.72, 1.29, 1.65)),
               tolerance = 1e-02)
})
