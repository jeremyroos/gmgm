gmm_1 <- list(alpha = 1, mu = matrix(0, dimnames = list("A", NULL)),
              sigma = list(matrix(1, dimnames = list("A", "A"))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = 1, mu = matrix(0, dimnames = list("B", NULL)),
              sigma = list(matrix(1, dimnames = list("B", "B"))))
class(gmm_2) <- "gmm"
gmm_3 <- list(alpha = 1, mu = matrix(0, dimnames = list("C", NULL)),
              sigma = list(matrix(1, dimnames = list("C", "C"))))
class(gmm_3) <- "gmm"
gmm_4 <- list(alpha = 1, mu = matrix(4.833333, dimnames = list("A", NULL)),
              sigma = list(matrix(8.40619, dimnames = list("A", "A"))))
class(gmm_4) <- "gmm"
gmm_5 <- list(alpha = 1, mu = matrix(5.166667, dimnames = list("B", NULL)),
              sigma = list(matrix(5.549048, dimnames = list("B", "B"))))
class(gmm_5) <- "gmm"
gmm_6 <- list(alpha = c(0.3335526, 0.6664474),
              mu = matrix(c(6.497041, 2.500987, 1.249754, 6.500822), 2,
                          dimnames = list(c("C", "B"), NULL)),
              sigma = list(matrix(c(1.511549, 1.496384, 1.496384, 1.50366), 2,
                                  dimnames = list(c("C", "B"), c("C", "B"))),
                           matrix(c(0.5519983, - 0.2995869, - 0.2995869,
                                    2.6010413),
                                  2,
                                  dimnames = list(c("C", "B"), c("C", "B")))))
class(gmm_6) <- "gmm"
gmm_7 <- list(alpha = c(0.3333185, 0.6666815),
              mu = matrix(c(8.499974, 7.500061, 3.000135, 4.000047), 2,
                          dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(0.1700083, - 0.4999829, - 0.4999829,
                                    1.5033315),
                                  2, dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(3.602587, - 1.799744, - 1.799744, 3.602009),
                                  2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_7) <- "gmm"
gmm_8 <- list(alpha = c(0.6664474, 0.3335526),
              mu = matrix(c(6.500822, 1.249754, 2.500987, 6.497041), 2,
                          dimnames = list(c("B", "C"), NULL)),
              sigma = list(matrix(c(2.6010413, - 0.2995869, - 0.2995869,
                                    0.5519983),
                                  2, dimnames = list(c("B", "C"), c("B", "C"))),
                           matrix(c(1.503660, 1.496384, 1.496384, 1.511549), 2,
                                  dimnames = list(c("B", "C"), c("B", "C")))))
class(gmm_8) <- "gmm"
gmm_9 <- list(alpha = 1, mu = matrix(3, dimnames = list("C", NULL)),
              sigma = list(matrix(6.287143, dimnames = list("C", "C"))))
class(gmm_9) <- "gmm"
gmm_10 <- list(alpha = c(0.5071482, 0.4928518),
              mu = matrix(c(2.056722, 7.690487), 1, dimnames = list("A", NULL)),
              sigma = list(matrix(1.655347, dimnames = list("A", "A")),
                           matrix(1.152791, dimnames = list("A", "A"))))
class(gmm_10) <- "gmm"
gmm_11 <- list(alpha = c(0.5, 0.1666667, 0.3333333),
              mu = matrix(c(3, 3, 7, 3, 7.5, 8.5), 2,
                          dimnames = list(c("B", "A"), NULL)),
              sigma = list(matrix(c(1.5025, - 2.25, - 2.25, 4.5025), 2,
                                  dimnames = list(c("B", "A"), c("B", "A"))),
                           matrix(c(0.005, 0, 0, 0.005), 2,
                                  dimnames = list(c("B", "A"), c("B", "A"))),
                           matrix(c(1.503333, - 0.5, - 0.5, 0.17), 2,
                                  dimnames = list(c("B", "A"), c("B", "A")))))
class(gmm_11) <- "gmm"
gmm_12 <- list(alpha = c(0.3333333, 0.3333333, 0.3333333),
               mu = matrix(c(2, 3, 0.5, 8.5, 6.5, 3), 2,
                           dimnames = list(c("C", "A"), NULL)),
               sigma = list(matrix(c(0.003333333, - 2.460655e-14,
                                     - 2.460655e-14, 0.003333333),
                                   2,
                                   dimnames = list(c("C", "A"), c("C", "A"))),
                            matrix(c(0.17, - 0.1666667, - 0.1666667, 0.17), 2,
                                   dimnames = list(c("C", "A"), c("C", "A"))),
                            matrix(c(1.503333, - 3, - 3, 6.003333), 2,
                                   dimnames = list(c("C", "A"), c("C", "A")))))
class(gmm_12) <- "gmm"
gmm_13 <- list(alpha = c(0.4000843, 0.5999157),
               mu = matrix(c(3.000717, 7.666844), 1,
                           dimnames = list("A", NULL)),
               sigma = list(matrix(0.00476687, dimnames = list("A", "A")),
                            matrix(1.169268, dimnames = list("A", "A"))))
class(gmm_13) <- "gmm"
gmm_14 <- list(alpha = c(0.6, 0.4),
               mu = matrix(c(7.333333, 3, 2.5, 6.5), 2,
                          dimnames = list(c("B", "B.1"), NULL)),
               sigma = list(matrix(c(1.169167, 1, 1, 1.5025), 2,
                                   dimnames = list(c("B", "B.1"),
                                                   c("B", "B.1"))),
                            matrix(c(1.503333, - 0.5, - 0.5, 0.17), 2,
                                   dimnames = list(c("B", "B.1"),
                                                   c("B", "B.1")))))
class(gmm_14) <- "gmm"
gmm_15 <- list(alpha = c(0.4999998, 0.5000002),
               mu = matrix(c(3, 6.999998), 1, dimnames = list("A", NULL)),
               sigma = list(matrix(0.003333334, dimnames = list("A", "A")),
                            matrix(0.6700047, dimnames = list("A", "A"))))
class(gmm_15) <- "gmm"
gmm_16 <- list(alpha = c(0.5, 0.5),
               mu = matrix(c(8, 1.5, 2.5, 6), 2,
                           dimnames = list(c("B", "A.1"), NULL)),
               sigma = list(matrix(c(0.67, 1, 1, 1.503333), 2,
                                   dimnames = list(c("B", "A.1"),
                                                   c("B", "A.1"))),
                            matrix(c(1.503333, 3, 3, 6.003333), 2,
                                   dimnames = list(c("B", "A.1"),
                                                   c("B", "A.1")))))
class(gmm_16) <- "gmm"
gmm_17 <- list(alpha = c(0.5, 0.5),
               mu = matrix(c(0, 9), 1, dimnames = list("A", NULL)),
               sigma = list(matrix(0.005, dimnames = list("A", "A")),
                            matrix(0.005, dimnames = list("A", "A"))))
class(gmm_17) <- "gmm"
gmm_18 <- list(alpha = 1,
               mu = matrix(c(5, 4.5), dimnames = list(c("B", "A"), NULL)),
               sigma = list(matrix(c(0.67, 3, 3, 13.50333), 2,
                                   dimnames = list(c("B", "A"), c("B", "A")))))
class(gmm_18) <- "gmm"
gmm_19 <- list(alpha = c(0.4999499, 0.5000501),
               mu = matrix(c(8.0001, 5.50025, 2.500451, 4.49985), 2,
                           dimnames = list(c("B", "A"), NULL)),
               sigma = list(matrix(c(0.669978, 1.666611, 1.666611, 4.169861), 2,
                                   dimnames = list(c("B", "A"), c("B", "A"))),
                            matrix(c(1.504585, - 1.50035, - 1.50035, 1.503383),
                                   2,
                                   dimnames = list(c("B", "A"), c("B", "A")))))
class(gmm_19) <- "gmm"
gmm_20 <- list(alpha = c(0.5, 0.5),
               mu = matrix(c(4, 6), 1, dimnames = list("B", NULL)),
               sigma = list(matrix(0.005, dimnames = list("B", "B")),
                            matrix(0.005, dimnames = list("B", "B"))))
class(gmm_20) <- "gmm"

gmbn_1 <- list(A = gmm_1, B = gmm_2, C = gmm_3)
class(gmbn_1) <- "gmbn"
gmbn_2 <- list(A = gmm_4, B = gmm_5, C = gmm_6)
class(gmbn_2) <- "gmbn"
gmbn_3 <- list(A = gmm_4, B = gmm_2, C = gmm_6)
class(gmbn_3) <- "gmbn"
gmbn_4 <- list(A = gmm_7, B = gmm_8, C = gmm_9)
class(gmbn_4) <- "gmbn"
gmbn_5 <- list(A = gmm_4, B = gmm_5, C = gmm_9)
class(gmbn_5) <- "gmbn"
gmbn_6 <- list(A = gmm_10, B = gmm_11, C = gmm_12)
class(gmbn_6) <- "gmbn"
gmbn_7 <- list(A = gmm_1, B = gmm_2)
class(gmbn_7) <- "gmbn"
gmbn_8 <- list(A = gmm_13, B = gmm_14)
class(gmbn_8) <- "gmbn"
gmbn_9 <- list(A = gmm_15, B = gmm_16)
class(gmbn_9) <- "gmbn"
gmbn_10 <- list(A = gmm_17, B = gmm_18)
class(gmbn_10) <- "gmbn"
gmbn_11 <- list(A = gmm_15, B = gmm_19)
class(gmbn_11) <- "gmbn"
gmbn_12 <- list(A = gmm_17, B = gmm_20)
class(gmbn_12) <- "gmbn"

gmdbn_1 <- list(b_1 = gmbn_7, b_2 = gmbn_7)
class(gmdbn_1) <- "gmdbn"
gmdbn_2 <- list(b_1 = gmbn_10, b_2 = gmbn_11)
class(gmdbn_2) <- "gmdbn"
gmdbn_3 <- list(b_1 = gmbn_10, b_2 = gmbn_9)
class(gmdbn_3) <- "gmdbn"
gmdbn_4 <- list(b_1 = gmbn_7, b_2 = gmbn_9)
class(gmdbn_4) <- "gmdbn"
gmdbn_5 <- list(b_1 = gmbn_12, b_2 = gmbn_9)
class(gmdbn_5) <- "gmdbn"

test_that("learn the structure of a gmbn object", {
  expect_equal(struct_learn(gmbn_1,
                            data.frame(A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9),
                                       C = c(8, 2, 5, 0, 2, 1)),
                            arcs_cand = data.frame(from = c("A", "A", "B"),
                                                   to = c("B", "C", "C")),
                            max_iter_step = 2, max_iter_smem = 2, regul = 0.01,
                            max_iter_em = 5),
               list(gmgm = gmbn_2,
                    evol_score =
                      list(global = c(old = - 269.91617, new = - 47.29099),
                           local = matrix(c(- 106.8054, - 17.1917, - 106.80539,
                                            - 15.94537, - 56.30539, - 14.15392),
                                          2,
                                          dimnames = list(c("old", "new"),
                                                          c("A", "B", "C"))))),
               tolerance = 0.01)
})

test_that("learn the structure of a gmbn object with extra columns", {
  expect_equal(struct_learn(gmbn_1,
                            data.frame(A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9),
                                       C = c(8, 2, 5, 0, 2, 1),
                                       D = c(0, 0, 0, 0, 0, 0)),
                            arcs_cand = data.frame(from = c("A", "A", "B"),
                                                   to = c("B", "C", "C")),
                            max_iter_step = 2, max_iter_smem = 2, regul = 0.01,
                            max_iter_em = 5),
               list(gmgm = gmbn_2,
                    evol_score =
                      list(global = c(old = - 269.91617, new = - 47.29099),
                           local = matrix(c(- 106.8054, - 17.1917, - 106.80539,
                                            - 15.94537, - 56.30539, - 14.15392),
                                          2,
                                          dimnames = list(c("old", "new"),
                                                          c("A", "B", "C"))))),
               tolerance = 0.01)
})

test_that("learn unordered local structures of a gmbn object", {
  expect_equal(struct_learn(gmbn_1,
                            data.frame(A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9),
                                       C = c(8, 2, 5, 0, 2, 1)),
                            nodes = c("C", "B", "A"),
                            arcs_cand = data.frame(from = c("A", "A", "B"),
                                                   to = c("B", "C", "C")),
                            max_iter_step = 2, max_iter_smem = 2, regul = 0.01,
                            max_iter_em = 5),
               list(gmgm = gmbn_2,
                    evol_score =
                      list(global = c(old = - 269.91617, new = - 47.29099),
                           local = matrix(c(- 106.8054, - 17.1917, - 106.80539,
                                            - 15.94537, - 56.30539, - 14.15392),
                                          2,
                                          dimnames = list(c("old", "new"),
                                                          c("A", "B", "C"))))),
               tolerance = 0.01)
})

test_that("learn duplicated local structures of a gmbn object", {
  expect_equal(struct_learn(gmbn_1,
                            data.frame(A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9),
                                       C = c(8, 2, 5, 0, 2, 1)),
                            nodes = c("A", "B", "C", "C"),
                            arcs_cand = data.frame(from = c("A", "A", "B"),
                                                   to = c("B", "C", "C")),
                            max_iter_step = 2, max_iter_smem = 2, regul = 0.01,
                            max_iter_em = 5),
               list(gmgm = gmbn_2,
                    evol_score =
                      list(global = c(old = - 269.91617, new = - 47.29099),
                           local = matrix(c(- 106.8054, - 17.1917, - 106.80539,
                                            - 15.94537, - 56.30539, - 14.15392),
                                          2,
                                          dimnames = list(c("old", "new"),
                                                          c("A", "B", "C"))))),
               tolerance = 0.01)
})

test_that("learn not all the local structures of a gmbn object", {
  expect_equal(struct_learn(gmbn_1,
                            data.frame(A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9),
                                       C = c(8, 2, 5, 0, 2, 1)),
                            nodes = c("A", "C"),
                            arcs_cand = data.frame(from = c("A", "A", "B"),
                                                   to = c("B", "C", "C")),
                            max_iter_step = 2, max_iter_smem = 2, regul = 0.01,
                            max_iter_em = 5),
               list(gmgm = gmbn_3,
                    evol_score =
                      list(global = c(old = - 269.91617, new = - 138.151),
                           local = matrix(c(- 106.8054, - 17.1917, - 106.80539,
                                            - 106.80539, - 56.30539,
                                            - 14.15392),
                                          2,
                                          dimnames = list(c("old", "new"),
                                                          c("A", "B", "C"))))),
               tolerance = 0.01)
})

test_that("learn no local structure of a gmbn object", {
  expect_equal(struct_learn(gmbn_1,
                            data.frame(A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9),
                                       C = c(8, 2, 5, 0, 2, 1)),
                            nodes = NULL),
               list(gmgm = gmbn_1,
                    evol_score =
                      list(global = c(old = - 269.9162, new = - 269.9162),
                           local = matrix(c(- 106.8054, - 106.8054, - 106.8054,
                                            - 106.8054, - 56.30539, - 56.30539),
                                          2,
                                          dimnames = list(c("old", "new"),
                                                          c("A", "B", "C"))))),
               tolerance = 0.01)
  expect_equal(struct_learn(gmbn_1,
                            data.frame(A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9),
                                       C = c(8, 2, 5, 0, 2, 1)),
                            nodes = character()),
               list(gmgm = gmbn_1,
                    evol_score =
                      list(global = c(old = - 269.9162, new = - 269.9162),
                           local = matrix(c(- 106.8054, - 106.8054, - 106.8054,
                                            - 106.8054, - 56.30539, - 56.30539),
                                          2,
                                          dimnames = list(c("old", "new"),
                                                          c("A", "B", "C"))))),
               tolerance = 0.01)
})

test_that("learn the structure of a gmbn object with all possible candidate arcs", {
  expect_equal(struct_learn(gmbn_1,
                            data.frame(A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9),
                                       C = c(8, 2, 5, 0, 2, 1)),
                            max_iter_step = 2, max_iter_smem = 2, regul = 0.01,
                            max_iter_em = 5),
               list(gmgm = gmbn_4,
                    evol_score =
                      list(global = c(old = - 269.91617, new = - 48.73736),
                           local = matrix(c(- 106.8054, - 16.55987, - 106.80539,
                                            - 15.85737, - 56.30539, - 16.32012),
                                          2,
                                          dimnames = list(c("old", "new"),
                                                          c("A", "B", "C"))))),
               tolerance = 0.01)
})

test_that("learn the structure of a gmbn object with duplicated candidate arcs", {
  expect_equal(struct_learn(gmbn_1,
                            data.frame(A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9),
                                       C = c(8, 2, 5, 0, 2, 1)),
                            arcs_cand = data.frame(from = c("A", "A", "B", "B"),
                                                   to = c("B", "C", "C", "C")),
                            max_iter_step = 2, max_iter_smem = 2, regul = 0.01,
                            max_iter_em = 5),
               list(gmgm = gmbn_2,
                    evol_score =
                      list(global = c(old = - 269.91617, new = - 47.29099),
                           local = matrix(c(- 106.8054, - 17.1917, - 106.80539,
                                            - 15.94537, - 56.30539, - 14.15392),
                                          2,
                                          dimnames = list(c("old", "new"),
                                                          c("A", "B", "C"))))),
               tolerance = 0.01)
})

test_that("learn the structure of a gmbn object with no candidate arc", {
  expect_equal(struct_learn(gmbn_1,
                            data.frame(A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9),
                                       C = c(8, 2, 5, 0, 2, 1)),
                            arcs_cand = NULL, max_iter_step = 2,
                            max_iter_smem = 2, regul = 0.01, max_iter_em = 5),
               list(gmgm = gmbn_5,
                    evol_score =
                      list(global = c(old = - 269.91617, new = - 49.45719),
                           local = matrix(c(- 106.8054, - 17.1917, - 106.80539,
                                            - 15.94537, - 56.30539, - 16.32012),
                                          2,
                                          dimnames = list(c("old", "new"),
                                                          c("A", "B", "C"))))),
               tolerance = 0.01)
  expect_equal(struct_learn(gmbn_1,
                            data.frame(A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9),
                                       C = c(8, 2, 5, 0, 2, 1)),
                            arcs_cand = data.frame(from = character(),
                                                   to = character(),
                                                   lag = numeric()),
                            max_iter_step = 2, max_iter_smem = 2, regul = 0.01,
                            max_iter_em = 5),
               list(gmgm = gmbn_5,
                    evol_score =
                      list(global = c(old = - 269.91617, new = - 49.45719),
                           local = matrix(c(- 106.8054, - 17.1917, - 106.80539,
                                            - 15.94537, - 56.30539, - 16.32012),
                                          2,
                                          dimnames = list(c("old", "new"),
                                                          c("A", "B", "C"))))),
               tolerance = 0.01)
  expect_equal(struct_learn(gmbn_1,
                            data.frame(A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9),
                                       C = c(8, 2, 5, 0, 2, 1)),
                            arcs_cand = data.frame(from = logical(),
                                                   to = logical(),
                                                   lag = logical()),
                            max_iter_step = 2, max_iter_smem = 2, regul = 0.01,
                            max_iter_em = 5),
               list(gmgm = gmbn_5,
                    evol_score =
                      list(global = c(old = - 269.91617, new = - 49.45719),
                           local = matrix(c(- 106.8054, - 17.1917, - 106.80539,
                                            - 15.94537, - 56.30539, - 16.32012),
                                          2,
                                          dimnames = list(c("old", "new"),
                                                          c("A", "B", "C"))))),
               tolerance = 0.01)
})

test_that("learn the structure of a gmbn object with the AIC", {
  expect_equal(struct_learn(gmbn_1,
                            data.frame(A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9),
                                       C = c(8, 2, 5, 0, 2, 1)),
                            arcs_cand = data.frame(from = c("A", "A", "B"),
                                                   to = c("B", "C", "C")),
                            score = "aic", max_iter_step = 2, max_iter_smem = 2,
                            regul = 0.01, max_iter_em = 5),
               list(gmgm = gmbn_2,
                    evol_score =
                      list(global = c(old = - 270.54089, new = - 48.85279),
                           local = matrix(c(- 107.01363, - 17.39994,
                                            - 107.01363, - 16.15361, - 56.51363,
                                            - 15.29924),
                                          2,
                                          dimnames = list(c("old", "new"),
                                                          c("A", "B", "C"))))),
               tolerance = 0.01)
})

test_that("learn the structure of a gmbn object with the log-likelihood", {
  expect_equal(struct_learn(gmbn_1,
                            data.frame(A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9),
                                       C = c(8, 2, 5, 0, 2, 1)),
                            arcs_cand = data.frame(from = c("A", "A", "B"),
                                                   to = c("B", "C", "C")),
                            score = "loglik", max_iter_step = 2,
                            max_iter_smem = 2, regul = 0.01, max_iter_em = 5),
               list(gmgm = gmbn_6,
                    evol_score =
                      list(global = c(old = - 264.54089, new = - 6.42388),
                           local = matrix(c(- 105.01363, - 14.45551,
                                            - 105.013631, - 2.538262,
                                            - 54.51363, 10.56990),
                                          2,
                                          dimnames = list(c("old", "new"),
                                                          c("A", "B", "C"))))),
               tolerance = 0.01)
})

test_that("learn the structure of a temporal gmbn with one observation sequence", {
  expect_equal(struct_learn(gmbn_7,
                            data.frame(A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9)),
                            arcs_cand = data.frame(from = c("A", "A", "A", "B"),
                                                   to = c("A", "B", "B", "B"),
                                                   lag = c(0, 0, 1, 1)),
                            max_iter_step = 2, max_iter_smem = 2, regul = 0.01,
                            max_iter_em = 5),
               list(gmgm = gmbn_8,
                    evol_score =
                      list(global = c(old = - 203.40826, new = - 19.93469),
                           local = matrix(c(- 105.704131, - 8.867489,
                                            - 97.70413, - 11.06720),
                                          2,
                                          dimnames = list(c("old", "new"),
                                                          c("A", "B"))))),
               tolerance = 0.01)
  expect_equal(struct_learn(gmbn_7,
                            data.frame(A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9)),
                            arcs_cand = data.frame(from = c("A", "A", "A", "B"),
                                                   to = c("A", "B", "B", "B"),
                                                   lag = c(0, 0, 1, 1)),
                            col_seq = character(), max_iter_step = 2,
                            max_iter_smem = 2, regul = 0.01, max_iter_em = 5),
               list(gmgm = gmbn_8,
                    evol_score =
                      list(global = c(old = - 203.40826, new = - 19.93469),
                           local = matrix(c(- 105.704131, - 8.867489,
                                            - 97.70413, - 11.06720),
                                          2,
                                          dimnames = list(c("old", "new"),
                                                          c("A", "B"))))),
               tolerance = 0.01)
})

test_that("learn the structure of a temporal gmbn with several observation sequences", {
  expect_equal(struct_learn(gmbn_7,
                            data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                       A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9)),
                            arcs_cand = data.frame(from = c("A", "A", "A", "B"),
                                                   to = c("A", "B", "B", "B"),
                                                   lag = c(0, 0, 1, 1)),
                            col_seq = "seq", max_iter_step = 2,
                            max_iter_smem = 2, regul = 0.01, max_iter_em = 5),
               list(gmgm = gmbn_9,
                    evol_score =
                      list(global = c(old = - 142.624097, new = - 7.342039),
                           local = matrix(c(- 64.062048, - 5.302352,
                                            - 78.562048, - 2.039687),
                                          2,
                                          dimnames = list(c("old", "new"),
                                                          c("A", "B"))))),
               tolerance = 0.01)
})

test_that("learn the structure of a temporal gmbn with duplicated observation sequence column names", {
  expect_equal(struct_learn(gmbn_7,
                            data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                       A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9)),
                            arcs_cand = data.frame(from = c("A", "A", "A", "B"),
                                                   to = c("A", "B", "B", "B"),
                                                   lag = c(0, 0, 1, 1)),
                            col_seq = c("seq", "seq"), max_iter_step = 2,
                            max_iter_smem = 2, regul = 0.01, max_iter_em = 5),
               list(gmgm = gmbn_9,
                    evol_score =
                      list(global = c(old = - 142.624097, new = - 7.342039),
                           local = matrix(c(- 64.062048, - 5.302352,
                                            - 78.562048, - 2.039687),
                                          2,
                                          dimnames = list(c("old", "new"),
                                                          c("A", "B"))))),
               tolerance = 0.01)
})

test_that("learn the structure of a gmdbn object with non-temporal gmbn elements", {
  expect_equal(struct_learn(gmdbn_1,
                            data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                       A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9)),
                            arcs_cand = data.frame(from = "A", to = "B"),
                            col_seq = "seq", max_iter_step = 2,
                            max_iter_smem = 2, regul = 0.01, max_iter_em = 5),
               list(gmgm = gmdbn_2,
                    evol_score =
                      list(global = c(old = - 214.186145, new = - 6.009289),
                           local =
                             list(b_1 = matrix(c(- 43.031024, 0.341278,
                                                 - 28.53102, 2.08483),
                                               2,
                                               dimnames = list(c("old", "new"),
                                                               c("A", "B"))),
                                  b_2 = matrix(c(- 64.062048, - 5.302352,
                                                 - 78.562048, - 3.133045),
                                               2,
                                               dimnames = list(c("old", "new"),
                                                               c("A", "B")))))),
               tolerance = 0.01)
})

test_that("learn the structure of a gmdbn object with temporal gmbn elements", {
  expect_equal(struct_learn(gmdbn_1,
                            data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                       A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9)),
                            arcs_cand = data.frame(from = c("A", "A", "A", "B"),
                                                   to = c("A", "B", "B", "B"),
                                                   lag = c(0, 0, 1, 1)),
                            col_seq = "seq", max_iter_step = 2,
                            max_iter_smem = 2, regul = 0.01, max_iter_em = 5),
               list(gmgm = gmdbn_3,
                    evol_score =
                      list(global = c(old = - 214.186145, new = - 4.915931),
                           local =
                             list(b_1 = matrix(c(- 43.031024, 0.341278,
                                                 - 28.53102, 2.08483),
                                               2,
                                               dimnames = list(c("old", "new"),
                                                               c("A", "B"))),
                                  b_2 = matrix(c(- 64.062048, - 5.302352,
                                                 - 78.562048, - 2.039687),
                                               2,
                                               dimnames = list(c("old", "new"),
                                                               c("A", "B")))))),
               tolerance = 0.01)
})

test_that("learn the structure of gmbn elements of a gmdbn object", {
  expect_equal(struct_learn(gmdbn_1,
                            data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                       A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9)),
                            nodes = list(b_2 = c("A", "B")),
                            arcs_cand = data.frame(from = c("A", "A", "A", "B"),
                                                   to = c("A", "B", "B", "B"),
                                                   lag = c(0, 0, 1, 1)),
                            col_seq = "seq", max_iter_step = 2,
                            max_iter_smem = 2, regul = 0.01, max_iter_em = 5),
               list(gmgm = gmdbn_4,
                    evol_score =
                      list(global = c(old = - 214.186145, new = - 78.90409),
                           local =
                             list(b_1 = matrix(c(- 43.031024, - 43.03102,
                                                 - 28.53102, - 28.53102),
                                               2,
                                               dimnames = list(c("old", "new"),
                                                               c("A", "B"))),
                                  b_2 = matrix(c(- 64.062048, - 5.302352,
                                                 - 78.562048, - 2.039687),
                                               2,
                                               dimnames = list(c("old", "new"),
                                                               c("A", "B")))))),
               tolerance = 0.01)
})

test_that("learn the structure of a gmdbn object with candidate arcs only for some gmbn elements", {
  expect_equal(struct_learn(gmdbn_1,
                            data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                       A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9)),
                            arcs_cand = list(b_2 = data.frame(from = c("A", "A",
                                                                       "A",
                                                                       "B"),
                                                              to = c("A", "B",
                                                                     "B", "B"),
                                                              lag = c(0, 0, 1,
                                                                      1))),
                            col_seq = "seq", max_iter_step = 2,
                            max_iter_smem = 2, regul = 0.01, max_iter_em = 5),
               list(gmgm = gmdbn_5,
                    evol_score =
                      list(global = c(old = - 214.186145, new = - 6.659483),
                           local =
                             list(b_1 = matrix(c(- 43.031024, 0.341278,
                                                 - 28.53102, 0.341278),
                                               2,
                                               dimnames = list(c("old", "new"),
                                                               c("A", "B"))),
                                  b_2 = matrix(c(- 64.062048, - 5.302352,
                                                 - 78.562048, - 2.039687),
                                               2,
                                               dimnames = list(c("old", "new"),
                                                               c("A", "B")))))),
               tolerance = 0.01)
})

test_that("learn the structure of a gmdbn object with verbose", {
  expect_equal(struct_learn(gmdbn_1,
                            data.frame(seq = c(1, 1, 1, 2, 2, 2),
                                       A = c(0, 3, 6, 9, 3, 8),
                                       B = c(4, 7, 1, 6, 4, 9)),
                            arcs_cand = data.frame(from = c("A", "A", "A", "B"),
                                                   to = c("A", "B", "B", "B"),
                                                   lag = c(0, 0, 1, 1)),
                            col_seq = "seq", verbose = TRUE, max_iter_step = 2,
                            max_iter_smem = 2, regul = 0.01, max_iter_em = 5),
               list(gmgm = gmdbn_3,
                    evol_score =
                      list(global = c(old = - 214.186145, new = - 4.915931),
                           local =
                             list(b_1 = matrix(c(- 43.031024, 0.341278,
                                                 - 28.53102, 2.08483),
                                               2,
                                               dimnames = list(c("old", "new"),
                                                               c("A", "B"))),
                                  b_2 = matrix(c(- 64.062048, - 5.302352,
                                                 - 78.562048, - 2.039687),
                                               2,
                                               dimnames = list(c("old", "new"),
                                                               c("A", "B")))))),
               tolerance = 0.01)
})
