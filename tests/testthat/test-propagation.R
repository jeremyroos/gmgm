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

test_that("propagate initial particles with a gmbn object", {
  set.seed(0)
  expect_equal(propagation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5)),
                           gmbn_1,
                           data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                      A = c(0, NA, 6, NA, 3, NA),
                                      B = c(NA, 7, NA, 6, NA, 9)),
                           col_seq = "seq", n_times = 3),
               tibble(seq = c(1, 1, 2, 2), weight = c(0.5, 0.5, 0.0103, 0.99),
                      A.2 = c(0, 0, 1.33, 1.33), B.2 = c(0.411, 0.411, 6, 6),
                      A.1 = c(2.4, 2.4, 3, 3), B.1 = c(7, 7, 5.44, 5.19),
                      A = c(6, 6, - 0.412, 0.252), B = c(10.8, 9.62, 9, 9)),
               tolerance = 0.01)
})

test_that("propagate particles with a temporal gmbn object", {
  set.seed(0)
  expect_equal(propagation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(0, 0, 9, 9), B = c(4, 4, 6, 6)),
                           gmbn_2,
                           data.frame(seq = c(1, 1, 2, 2), A = c(NA, 6, 3, NA),
                                      B = c(7, NA, NA, 9)),
                           col_seq = "seq", n_times = 2),
               tibble(seq = c(1, 1, 2, 2), weight = c(0.5, 0.5, 4.03e-16, 1),
                      A.2 = c(0, 0, 9, 9), B.2 = c(4, 4, 6, 6),
                      A.1 = c(1.33, 1.33, 3, 3), B.1 = c(7, 7, 6.7, 5.42),
                      A = c(6, 6, - 0.00577, 2.4), B = c(9.1, 8.87, 9, 9)),
               tolerance = 0.01)
})

test_that("propagate unordered particles with a temporal gmbn object", {
  set.seed(0)
  expect_equal(propagation(data.frame(seq = c(2, 1, 2, 1),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(9, 0, 9, 0), B = c(6, 4, 6, 4)),
                           gmbn_2,
                           data.frame(seq = c(1, 1, 2, 2), A = c(NA, 6, 3, NA),
                                      B = c(7, NA, NA, 9)),
                           col_seq = "seq", n_times = 2),
               tibble(seq = c(1, 1, 2, 2), weight = c(0.5, 0.5, 4.03e-16, 1),
                      A.2 = c(0, 0, 9, 9), B.2 = c(4, 4, 6, 6),
                      A.1 = c(1.33, 1.33, 3, 3), B.1 = c(7, 7, 6.7, 5.42),
                      A = c(6, 6, - 0.00577, 2.4), B = c(9.1, 8.87, 9, 9)),
               tolerance = 0.01)
})

test_that("propagate particles with unordered columns with a temporal gmbn object", {
  set.seed(0)
  expect_equal(propagation(data.frame(B = c(4, 4, 6, 6),
                                      seq = c(1, 1, 2, 2),
                                      A = c(0, 0, 9, 9),
                                      weight = c(0.5, 0.5, 0.5, 0.5)),
                           gmbn_2,
                           data.frame(seq = c(1, 1, 2, 2), A = c(NA, 6, 3, NA),
                                      B = c(7, NA, NA, 9)),
                           col_seq = "seq", n_times = 2),
               tibble(B.2 = c(4, 4, 6, 6), seq = c(1, 1, 2, 2),
                      A.2 = c(0, 0, 9, 9), weight = c(0.5, 0.5, 4.03e-16, 1),
                      A.1 = c(1.33, 1.33, 3, 3), B.1 = c(7, 7, 6.7, 5.42),
                      A = c(6, 6, - 0.00577, 2.4), B = c(9.1, 8.87, 9, 9)),
               tolerance = 0.01)
})

test_that("propagate particles with extra columns with a temporal gmbn object", {
  set.seed(0)
  expect_equal(propagation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(0, 0, 9, 9), B = c(4, 4, 6, 6),
                                      C = c(0, 0, 0, 0)),
                           gmbn_2,
                           data.frame(seq = c(1, 1, 2, 2), A = c(NA, 6, 3, NA),
                                      B = c(7, NA, NA, 9)),
                           col_seq = "seq", n_times = 2),
               tibble(seq = c(1, 1, 2, 2), weight = c(0.5, 0.5, 4.03e-16, 1),
                      A.2 = c(0, 0, 9, 9), B.2 = c(4, 4, 6, 6),
                      C = c(0, 0, 0, 0), A.1 = c(1.33, 1.33, 3, 3),
                      B.1 = c(7, 7, 6.7, 5.42), A = c(6, 6, - 0.00577, 2.4),
                      B = c(9.1, 8.87, 9, 9)),
               tolerance = 0.01)
})

test_that("propagate particles with a temporal gmbn object and extra rows in the evidence", {
  set.seed(0)
  expect_equal(propagation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(0, 0, 9, 9), B = c(4, 4, 6, 6)),
                           gmbn_2,
                           data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                      A = c(NA, 6, NA, 3, NA, 0),
                                      B = c(7, NA, 0, NA, 9, NA)),
                           col_seq = "seq", n_times = 2),
               tibble(seq = c(1, 1, 2, 2), weight = c(0.5, 0.5, 4.03e-16, 1),
                      A.2 = c(0, 0, 9, 9), B.2 = c(4, 4, 6, 6),
                      A.1 = c(1.33, 1.33, 3, 3), B.1 = c(7, 7, 6.7, 5.42),
                      A = c(6, 6, - 0.00577, 2.4), B = c(9.1, 8.87, 9, 9)),
               tolerance = 0.01)
})

test_that("propagate particles with a temporal gmbn object and missing rows in the evidence", {
  set.seed(0)
  expect_equal(propagation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(0, 0, 9, 9), B = c(4, 4, 6, 6)),
                           gmbn_2,
                           data.frame(seq = c(1, 2), A = c(NA, 3),
                                      B = c(7, NA)),
                           col_seq = "seq", n_times = 2),
               tibble(seq = c(1, 1, 2, 2), weight = c(0.5, 0.5, 0.5, 0.5),
                      A.2 = c(0, 0, 9, 9), B.2 = c(4, 4, 6, 6),
                      A.1 = c(1.33, 1.33, 3, 3), B.1 = c(7, 7, 6.7, 5.42),
                      A = c(2.4, 0.764, - 0.799, - 1.15),
                      B = c(3.9, 0.756, 1.28, - 0.0686)),
               tolerance = 0.01)
})

test_that("propagate particles with a temporal gmbn object and extra observation sequences in the evidence", {
  set.seed(0)
  expect_equal(propagation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(0, 0, 9, 9), B = c(4, 4, 6, 6)),
                           gmbn_2,
                           data.frame(seq = c(1, 1, 2, 2, 3, 3),
                                      A = c(NA, 6, 3, NA, 0, NA),
                                      B = c(7, NA, NA, 9, NA, 0)),
                           col_seq = "seq", n_times = 2),
               tibble(seq = c(1, 1, 2, 2), weight = c(0.5, 0.5, 4.03e-16, 1),
                      A.2 = c(0, 0, 9, 9), B.2 = c(4, 4, 6, 6),
                      A.1 = c(1.33, 1.33, 3, 3), B.1 = c(7, 7, 6.7, 5.42),
                      A = c(6, 6, - 0.00577, 2.4), B = c(9.1, 8.87, 9, 9)),
               tolerance = 0.01)
})

test_that("propagate particles with a temporal gmbn object and missing observation sequences in the evidence", {
  set.seed(0)
  expect_equal(propagation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(0, 0, 9, 9), B = c(4, 4, 6, 6)),
                           gmbn_2,
                           data.frame(seq = c(1, 1), A = c(NA, 6),
                                      B = c(7, NA)),
                           col_seq = "seq", n_times = 2),
               tibble(seq = c(1, 1, 2, 2), weight = c(0.5, 0.5, 0.5, 0.5),
                      A.2 = c(0, 0, 9, 9), B.2 = c(4, 4, 6, 6),
                      A.1 = c(1.27, 1.33, 0.415, - 1.54),
                      B.1 = c(7, 7, 2.54, - 0.0608),
                      A = c(6, 6, - 0.799, - 1.15),
                      B = c(9.78, 9.03, - 0.483, - 0.717)),
               tolerance = 0.01)
})

test_that("propagate particles with a temporal gmbn object and extra columns in the evidence", {
  set.seed(0)
  expect_equal(propagation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(0, 0, 9, 9), B = c(4, 4, 6, 6)),
                           gmbn_2,
                           data.frame(seq = c(1, 1, 2, 2), A = c(NA, 6, 3, NA),
                                      B = c(7, NA, NA, 9), C = c(NA, 0, 0, NA)),
                           col_seq = "seq", n_times = 2),
               tibble(seq = c(1, 1, 2, 2), weight = c(0.5, 0.5, 4.03e-16, 1),
                      A.2 = c(0, 0, 9, 9), B.2 = c(4, 4, 6, 6),
                      A.1 = c(1.33, 1.33, 3, 3), B.1 = c(7, 7, 6.7, 5.42),
                      A = c(6, 6, - 0.00577, 2.4), B = c(9.1, 8.87, 9, 9)),
               tolerance = 0.01)
})

test_that("propagate particles with a temporal gmbn object and missing columns in the evidence", {
  set.seed(0)
  expect_equal(propagation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(0, 0, 9, 9), B = c(4, 4, 6, 6)),
                           gmbn_2,
                           data.frame(seq = c(1, 1, 2, 2), A = c(NA, 6, 3, NA)),
                           col_seq = "seq", n_times = 2),
               tibble(seq = c(1, 1, 2, 2), weight = c(0.5, 0.5, 0.5, 0.5),
                      A.2 = c(0, 0, 9, 9), B.2 = c(4, 4, 6, 6),
                      A.1 = c(- 0.326, 1.33, 3, 3),
                      B.1 = c(- 0.617, 2.15, 6.24, 6.42),
                      A = c(6, 6, 0.764, - 0.799),
                      B = c(9.55, 9.03, 1.67, 0.81)),
               tolerance = 0.01)
})

test_that("propagate particles with a temporal gmbn object and one observation sequence", {
  set.seed(0)
  expect_equal(propagation(data.frame(weight = c(0.5, 0.5), A = c(0, 0),
                                      B = c(4, 4)),
                           gmbn_2, data.frame(A = c(NA, 6), B = c(7, NA)),
                           n_times = 2),
               tibble(weight = c(0.5, 0.5), A.2 = c(0, 0), B.2 = c(4, 4),
                      A.1 = c(- 0.326, 1.33), B.1 = c(7, 7), A = c(6, 6),
                      B = c(9.65, 8.61)),
               tolerance = 0.01)
  set.seed(0)
  expect_equal(propagation(data.frame(weight = c(0.5, 0.5), A = c(0, 0),
                                      B = c(4, 4)),
                           gmbn_2, data.frame(A = c(NA, 6), B = c(7, NA)),
                           col_seq = character(), n_times = 2),
               tibble(weight = c(0.5, 0.5), A.2 = c(0, 0), B.2 = c(4, 4),
                      A.1 = c(- 0.326, 1.33), B.1 = c(7, 7), A = c(6, 6),
                      B = c(9.65, 8.61)),
               tolerance = 0.01)
})

test_that("propagate particles with a temporal gmbn object and no evidence", {
  set.seed(0)
  expect_equal(propagation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(0, 0, 9, 9), B = c(4, 4, 6, 6)),
                           gmbn_2, col_seq = "seq", n_times = 2),
               tibble(seq = c(1, 1, 2, 2), weight = c(0.5, 0.5, 0.5, 0.5),
                      A.2 = c(0, 0, 9, 9), B.2 = c(4, 4, 6, 6),
                      A.1 = c(1.33, 1.27, 0.415, - 1.54),
                      B.1 = c(2.75, 4.25, 3.24, - 0.58),
                      A = c(- 0.299, - 0.412, 0.252, - 0.892),
                      B = c(0.867, 0.538, 0.175, - 0.111)),
               tolerance = 0.01)
  set.seed(0)
  expect_equal(propagation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(0, 0, 9, 9), B = c(4, 4, 6, 6)),
                           gmbn_2,
                           data.frame(seq = numeric(), A = numeric(),
                                      B = numeric()),
                           col_seq = "seq", n_times = 2),
               tibble(seq = c(1, 1, 2, 2), weight = c(0.5, 0.5, 0.5, 0.5),
                      A.2 = c(0, 0, 9, 9), B.2 = c(4, 4, 6, 6),
                      A.1 = c(1.33, 1.27, 0.415, - 1.54),
                      B.1 = c(2.75, 4.25, 3.24, - 0.58),
                      A = c(- 0.299, - 0.412, 0.252, - 0.892),
                      B = c(0.867, 0.538, 0.175, - 0.111)),
               tolerance = 0.01)
})

test_that("propagate particles with a temporal gmbn object and no column in the evidence", {
  set.seed(0)
  expect_equal(propagation(data.frame(weight = c(0.5, 0.5), A = c(0, 0),
                                      B = c(4, 4)),
                           gmbn_2, data.frame(c(NA, NA))[, FALSE], n_times = 2),
               tibble(weight = c(0.5, 0.5), A.2 = c(0, 0), B.2 = c(4, 4),
                      A.1 = c(- 0.326, 1.33), B.1 = c(0.663, 1.75),
                      A = c(- 0.295, - 0.00577), B = c(- 0.134, 1.2)),
               tolerance = 0.01)
  set.seed(0)
})

test_that("propagate particles with a temporal gmbn object, no row and no column in the evidence", {
  set.seed(0)
  expect_equal(propagation(data.frame(weight = c(0.5, 0.5), A = c(0, 0),
                                      B = c(4, 4)),
                           gmbn_2, data.frame(), n_times = 2),
               tibble(weight = c(0.5, 0.5), A.2 = c(0, 0), B.2 = c(4, 4),
                      A.1 = c(- 0.326, 1.33), B.1 = c(0.663, 1.75),
                      A = c(- 0.295, - 0.00577), B = c(- 0.134, 1.2)),
               tolerance = 0.01)
})

test_that("propagate particles with a temporal gmbn object with duplicated observation sequence column names", {
  set.seed(0)
  expect_equal(propagation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(0, 0, 9, 9), B = c(4, 4, 6, 6)),
                           gmbn_2,
                           data.frame(seq = c(1, 1, 2, 2), A = c(NA, 6, 3, NA),
                                      B = c(7, NA, NA, 9)),
                           col_seq = c("seq", "seq"), n_times = 2),
               tibble(seq = c(1, 1, 2, 2), weight = c(0.5, 0.5, 4.03e-16, 1),
                      A.2 = c(0, 0, 9, 9), B.2 = c(4, 4, 6, 6),
                      A.1 = c(1.33, 1.33, 3, 3), B.1 = c(7, 7, 6.7, 5.42),
                      A = c(6, 6, - 0.00577, 2.4), B = c(9.1, 8.87, 9, 9)),
               tolerance = 0.01)
})

test_that("propagate particles with a temporal gmbn object over no time slice", {
  expect_equal(propagation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(0, 0, 9, 9), B = c(4, 4, 6, 6)),
                           gmbn_2,
                           data.frame(seq = c(1, 1, 2, 2), A = c(NA, 6, 3, NA),
                                      B = c(7, NA, NA, 9)),
                           col_seq = "seq", n_times = 0),
               tibble(seq = c(1, 1, 2, 2), weight = c(0.5, 0.5, 0.5, 0.5),
                      A = c(0, 0, 9, 9), B = c(4, 4, 6, 6)))
})

test_that("propagate particles with a temporal gmbn object and no renewal step", {
  set.seed(0)
  expect_equal(propagation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(0, 0, 9, 9), B = c(4, 4, 6, 6)),
                           gmbn_2,
                           data.frame(seq = c(1, 1, 2, 2), A = c(NA, 6, 3, NA),
                                      B = c(7, NA, NA, 9)),
                           col_seq = "seq", n_times = 2, min_ess = 0),
               tibble(seq = c(1, 1, 2, 2),
                      weight = c(8.33e-14, 1, 0.107, 0.893),
                      A.2 = c(0, 0, 9, 9), B.2 = c(4, 4, 6, 6),
                      A.1 = c(- 0.326, 1.33, 3, 3), B.1 = c(7, 7, 6.7, 5.42),
                      A = c(6, 6, - 0.295, - 0.00577), B = c(9.88, 9.1, 9, 9)),
               tolerance = 0.01)
})

test_that("propagate initial particles with a gmdbn object", {
  set.seed(0)
  expect_equal(propagation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5)),
                           gmdbn_1,
                           data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                      A = c(0, NA, 6, NA, 3, NA),
                                      B = c(NA, 7, NA, 6, NA, 9)),
                           col_seq = "seq", n_times = 3),
               tibble(seq = c(1, 1, 2, 2), weight = c(0.5, 0.5, 0.0000136, 1),
                      A.2 = c(0, 0, 1.33, 1.33), B.2 = c(0.411, 0.411, 6, 6),
                      A.1 = c(2.4, 2.4, 3, 3), B.1 = c(7, 7, 4.81, 4.58),
                      A = c(6, 6, 0.209, 1.11), B = c(10.1, 8.96, 9, 9)),
               tolerance = 0.01)
})
