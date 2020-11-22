gmm_1 <- list(alpha = 1, mu = matrix(0, dimnames = list("A", NULL)),
              sigma = list(matrix(1, dimnames = list("A", "A"))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = 1, mu = matrix(0, dimnames = list("B", NULL)),
              sigma = list(matrix(1, dimnames = list("B", "B"))))
class(gmm_2) <- "gmm"
gmm_3 <- list(alpha = 1, mu = matrix(0, dimnames = list("C", NULL)),
              sigma = list(matrix(1, dimnames = list("C", "C"))))
class(gmm_3) <- "gmm"
gmm_4 <- list(alpha = c(0.1661748, 0.8338252),
              mu = matrix(c(6.650029e-140, 3.948111), 1,
                          dimnames = list("A", NULL)),
              sigma = list(matrix(0.005007389, dimnames = list("A", "A")),
                           matrix(1.726536, dimnames = list("A", "A"))))
class(gmm_4) <- "gmm"
gmm_5 <- list(alpha = c(0.1666667, 0.1666667, 0.5, 0.1666667),
              mu = matrix(c(7, 9, 6.094894, 3.530951), 1,
                          dimnames = list("B", NULL)),
              sigma = list(matrix(0.005, dimnames = list("B", "B")),
                           matrix(0.005, dimnames = list("B", "B")),
                           matrix(0.01450626, dimnames = list("B", "B")),
                           matrix(0.005, dimnames = list("B", "B"))))
class(gmm_5) <- "gmm"
gmm_6 <- list(alpha = c(0.3333333, 0.6666667),
              mu = matrix(c(6.7903344, 0.9002508, 3.763442, 4.487926), 2,
                          dimnames = list(c("C", "A"), NULL)),
              sigma = list(matrix(c(0.9788607, - 0.7260016, - 0.7260016,
                                    0.5436343),
                                  2, dimnames = list(c("C", "A"), c("C", "A"))),
                           matrix(c(0.9597421, 0.9010994, 0.9010994, 0.9080968),
                                  2,
                                  dimnames = list(c("C", "A"), c("C", "A")))))
class(gmm_6) <- "gmm"
gmm_7 <- list(alpha = 1, mu = matrix(2.304809, dimnames = list("A", NULL)),
              sigma = list(matrix(4.271348, dimnames = list("A", "A"))))
class(gmm_7) <- "gmm"
gmm_8 <- list(alpha = c(0.3333333, 0.3333242, 0.3333425),
              mu = matrix(c(- 0.3334605, 4.5, 7.7765522, 0.4400948, 7.499986,
                            1.974289),
                          2, dimnames = list(c("B", "A"), NULL)),
              sigma = list(matrix(c(0.01385594, 0.12563401, 0.12563401,
                                    1.503333),
                                  2, dimnames = list(c("B", "A"), c("B", "A"))),
                           matrix(c(0.4053299, - 0.2278356, - 0.2278356,
                                    0.1324616),
                                  2, dimnames = list(c("B", "A"), c("B", "A"))),
                           matrix(c(1.50331, 2.023787, 2.023787, 2.733853),
                                  2,
                                  dimnames = list(c("B", "A"), c("B", "A")))))
class(gmm_8) <- "gmm"
gmm_9 <- list(alpha = c(0.5, 0.5),
              mu = matrix(c(1.243403, 1.276906, 6.316625, 3.332711), 2,
                          dimnames = list(c("C", "A"), NULL)),
              sigma = list(matrix(c(0.2909673, 0.5781801, 0.5781801, 1.2239391),
                                  2, dimnames = list(c("C", "A"), c("C", "A"))),
                           matrix(c(1.17794, - 2.341516, - 2.341516, 4.668545),
                                  2,
                                  dimnames = list(c("C", "A"), c("C", "A")))))
class(gmm_9) <- "gmm"
gmm_10 <- list(alpha = 1, mu = matrix(0.02661075, dimnames = list("A", NULL)),
              sigma = list(matrix(0.003805421, dimnames = list("A", "A"))))
class(gmm_10) <- "gmm"
gmm_11 <- list(alpha = c(0.5, 0.5),
              mu = matrix(c(5.017594, 6), 1, dimnames = list("B", NULL)),
              sigma = list(matrix(0.005, dimnames = list("B", "B")),
                           matrix(0.005, dimnames = list("B", "B"))))
class(gmm_11) <- "gmm"
gmm_12 <- list(alpha = c(0.75, 0.25),
              mu = matrix(c(3.058225, 6), 1, dimnames = list("A", NULL)),
              sigma = list(matrix(0.1007646, dimnames = list("A", "A")),
                           matrix(0.005, dimnames = list("A", "A"))))
class(gmm_12) <- "gmm"
gmm_13 <- list(alpha = c(0.25, 0.25, 0.5),
               mu = matrix(c(1.60097, 3, 9, 3.527776, 7.175548, 4.323449), 2,
                           dimnames = list(c("B", "A"), NULL)),
               sigma = list(matrix(c(0.005, 0, 0, 0.005), 2,
                                   dimnames = list(c("B", "A"), c("B", "A"))),
                            matrix(c(5e-03, 5.674337e-207, 5.674337e-207,
                                     5e-03),
                                   2,
                                   dimnames = list(c("B", "A"), c("B", "A"))),
                            matrix(c(0.02387817, 0.1962106, 0.1962106,
                                     1.8772153),
                                   2,
                                   dimnames = list(c("B", "A"), c("B", "A")))))
class(gmm_13) <- "gmm"
gmm_14 <- list(alpha = 1,
               mu = matrix(0.01677451, dimnames = list("A", NULL)),
               sigma = list(matrix(0.003520923, dimnames = list("A", "A"))))
class(gmm_14) <- "gmm"
gmm_15 <- list(alpha = 1, mu = matrix(6.003919, dimnames = list("B", NULL)),
               sigma = list(matrix(0.003343572, dimnames = list("B", "B"))))
class(gmm_15) <- "gmm"
gmm_16 <- list(alpha = c(0.25, 0.25, 0.25, 0.25),
               mu = matrix(c(- 0.3212006, 1.448437, 3, 6), 1,
                           dimnames = list("A", NULL)),
               sigma = list(matrix(0.005, dimnames = list("A", "A")),
                            matrix(0.005, dimnames = list("A", "A")),
                            matrix(0.005, dimnames = list("A", "A")),
                            matrix(0.005, dimnames = list("A", "A"))))
class(gmm_16) <- "gmm"
gmm_17 <- list(alpha = c(0.5, 0.5),
               mu = matrix(c(- 0.1647741, 6.5, 8, 2.98028), 2,
                           dimnames = list(c("B", "B.1"), NULL)),
               sigma = list(matrix(c(0.0125369, - 0.0391654, - 0.0391654, 0.17),
                                   2,
                                   dimnames = list(c("B", "B.1"),
                                                   c("B", "B.1"))),
                            matrix(c(0.67, - 2.018372, - 2.018372, 6.114071), 2,
                                   dimnames = list(c("B", "B.1"),
                                                   c("B", "B.1")))))
class(gmm_17) <- "gmm"

gmbn_1 <- list(A = gmm_1, B = gmm_2, C = gmm_3)
class(gmbn_1) <- "gmbn"
gmbn_2 <- list(A = gmm_4, B = gmm_5, C = gmm_6)
class(gmbn_2) <- "gmbn"
gmbn_3 <- list(A = gmm_7, B = gmm_8, C = gmm_9)
class(gmbn_3) <- "gmbn"
gmbn_4 <- list(A = gmm_1, B = gmm_2)
class(gmbn_4) <- "gmbn"
gmbn_5 <- list(A = gmm_10, B = gmm_11)
class(gmbn_5) <- "gmbn"
gmbn_6 <- list(A = gmm_12, B = gmm_13)
class(gmbn_6) <- "gmbn"
gmbn_7 <- list(A = gmm_14, B = gmm_15)
class(gmbn_7) <- "gmbn"
gmbn_8 <- list(A = gmm_16, B = gmm_17)
class(gmbn_8) <- "gmbn"

gmdbn_1 <- list(b_1 = gmbn_4, b_2 = gmbn_4)
class(gmdbn_1) <- "gmdbn"
gmdbn_2 <- list(b_1 = gmbn_5, b_2 = gmbn_6)
class(gmdbn_2) <- "gmdbn"
gmdbn_3 <- list(b_1 = gmbn_7, b_2 = gmbn_8)
class(gmdbn_3) <- "gmdbn"

test_that("perform the structural EM algorithm for a gmbn object", {
  set.seed(0)
  expect_equal(struct_em(gmbn_1,
                         data.frame(A = c(0, NA, 6, NA, 3, NA),
                                    B = c(NA, 7, NA, 6, NA, 9),
                                    C = c(8, NA, 5, NA, 2, NA)),
                         arcs_cand = data.frame(from = c("A", "A", "B"),
                                                to = c("B", "C", "C")),
                         n_part = 2, max_iter_sem = 2, max_iter_pem = 2,
                         max_iter_step = 2, max_iter_smem = 2, regul = 0.01,
                         max_iter_em = 5),
               list(gmgm = gmbn_2,
                    data = tibble(A = c(0, 4.35, 6, 4.6, 3, 1.8),
                                  B = c(6.27, 7, 3.53, 6, 6.01, 9),
                                  C = c(8, 3.95, 5, 4.1, 2, 5.58)),
                    seq_score = matrix(c(- 46.2276, - 37.24874, - 44.13652,
                                         - 32.27129),
                                       2, dimnames = list(c("E", "M"), NULL))),
               tolerance = 0.01)
})

test_that("perform the structural EM algorithm for a gmbn object with explicit arguments", {
  set.seed(0)
  expect_equal(struct_em(gmbn_1,
                         data.frame(A = c(0, NA, 6, NA, 3, NA),
                                    B = c(NA, 7, NA, 6, NA, 9),
                                    C = c(8, NA, 5, NA, 2, NA)),
                         c("A", "B","C"),
                         data.frame(from = c("A", "A", "B"),
                                    to = c("B", "C", "C")),
                         NULL, "bic", 2, 1e06, 1, 2, 2, FALSE, TRUE, TRUE, 0,
                         Inf, 2, TRUE, TRUE, 1, Inf, 0.5, Inf, 2, 0.01, 1e-06,
                         5),
               list(gmgm = gmbn_2,
                    data = tibble(A = c(0, 4.35, 6, 4.6, 3, 1.8),
                                  B = c(6.27, 7, 3.53, 6, 6.01, 9),
                                  C = c(8, 3.95, 5, 4.1, 2, 5.58)),
                    seq_score = matrix(c(- 46.2276, - 37.24874, - 44.13652,
                                         - 32.27129),
                                       2, dimnames = list(c("E", "M"), NULL))),
               tolerance = 0.01)
})

test_that("perform the structural EM algorithm for a gmbn object with no iteration of the parametric EM algorithm", {
  set.seed(0)
  expect_equal(struct_em(gmbn_1,
                         data.frame(A = c(0, NA, 6, NA, 3, NA),
                                    B = c(NA, 7, NA, 6, NA, 9),
                                    C = c(8, NA, 5, NA, 2, NA)),
                         arcs_cand = data.frame(from = c("A", "A", "B"),
                                                to = c("B", "C", "C")),
                         n_part = 2, max_iter_sem = 3, max_iter_pem = 0,
                         max_iter_step = 2, max_iter_smem = 2, regul = 0.01,
                         max_iter_em = 5),
               list(gmgm = gmbn_3,
                    data = tibble(A = c(0, 0.88, 6, - 0.0495, 3, 4),
                                  B = c(8.55, 7, - 0.208, 6, - 0.459, 9),
                                  C = c(8, 1.25, 5, 0.481, 2, 5.95)),
                    seq_score = matrix(c(- 175.83553, - 38.83331, - 143.6634,
                                         - 30.7931, - 99.07375, - 31.68701),
                                       2, dimnames = list(c("E", "M"), NULL))),
               tolerance = 0.01)
})

test_that("perform no iteration of the structural EM algorithm for a gmbn object", {
  expect_equal(struct_em(gmbn_1,
                         data.frame(A = c(0, NA, 6, NA, 3, NA),
                                    B = c(NA, 7, NA, 6, NA, 9)),
                         max_iter_sem = 0),
               list(gmgm = gmbn_1, data = NULL,
                    seq_score = matrix(numeric(), 2,
                                       dimnames = list(c("E", "M"), NULL))),
               tolerance = 0.01)
})

test_that("perform the structural EM algorithm for a gmbn object with verbose", {
  set.seed(0)
  expect_equal(struct_em(gmbn_1,
                         data.frame(A = c(0, NA, 6, NA, 3, NA),
                                    B = c(NA, 7, NA, 6, NA, 9),
                                    C = c(8, NA, 5, NA, 2, NA)),
                         arcs_cand = data.frame(from = c("A", "A", "B"),
                                                to = c("B", "C", "C")),
                         n_part = 2, max_iter_sem = 2, max_iter_pem = 2,
                         verbose = TRUE, max_iter_step = 2, max_iter_smem = 2,
                         regul = 0.01, max_iter_em = 5),
               list(gmgm = gmbn_2,
                    data = tibble(A = c(0, 4.35, 6, 4.6, 3, 1.8),
                                  B = c(6.27, 7, 3.53, 6, 6.01, 9),
                                  C = c(8, 3.95, 5, 4.1, 2, 5.58)),
                    seq_score = matrix(c(- 46.2276, - 37.24874, - 44.13652,
                                         - 32.27129),
                                       2, dimnames = list(c("E", "M"), NULL))),
               tolerance = 0.01)
})

test_that("perform the structural EM algorithm for a gmdbn object", {
  set.seed(0)
  expect_equal(struct_em(gmdbn_1,
                         data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                    A = c(0, NA, 6, NA, 3, NA),
                                    B = c(NA, 7, NA, 6, NA, 9)),
                         arcs_cand = data.frame(from = c("A", "A", "A", "B"),
                                                to = c("A", "B", "B", "B"),
                                                lag = c(0, 0, 1, 1)),
                         col_seq = "seq", n_part = 2, max_iter_sem = 2,
                         max_iter_pem = 2, max_iter_step = 2, max_iter_smem = 2,
                         regul = 0.01, max_iter_em = 5),
               list(gmgm = gmdbn_2,
                    data = tibble(seq = c(1, 1, 1, 2, 2, 2),
                                  A = c(0, 2.65, 6, 0.0532, 3, 3.53),
                                  B = c(5.02, 7, 7.35, 6, 1.6, 9)),
                    seq_score = matrix(c(- 21.98519, - 11.29735, - 9.619716,
                                         - 6.668585),
                                       2, dimnames = list(c("E", "M"), NULL))),
               tolerance = 0.01)
})

test_that("perform the structural EM algorithm for a gmdbn object with no iteration of the parametric EM algorithm", {
  set.seed(0)
  expect_equal(struct_em(gmdbn_1,
                         data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                    A = c(0, NA, 6, NA, 3, NA),
                                    B = c(NA, 7, NA, 6, NA, 9)),
                         arcs_cand = data.frame(from = c("A", "A", "A", "B"),
                                                to = c("A", "B", "B", "B"),
                                                lag = c(0, 0, 1, 1)),
                         col_seq = "seq", n_part = 2, max_iter_sem = 3,
                         max_iter_pem = 0, max_iter_step = 2, max_iter_smem = 2,
                         regul = 0.01, max_iter_em = 5),
               list(gmgm = gmdbn_3,
                    data = tibble(seq = c(1, 1, 1, 2, 2, 2),
                                  A = c(0, 1.45, 6, 0.0335, 3, - 0.321),
                                  B = c(6.01, 7, - 0.282, 6, - 0.0473, 9)),
                    seq_score = matrix(c(- 121.076241, - 9.587937,
                                         - 6043.824772, - 2.740334,
                                         - 193.299192, - 2.315253),
                                       2, dimnames = list(c("E", "M"), NULL))),
               tolerance = 0.01)
})
