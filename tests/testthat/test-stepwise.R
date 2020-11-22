gmm_1 <- list(alpha = 1,
              mu = matrix(0, dimnames = list("C", NULL)),
              sigma = list(matrix(1, dimnames = list("C", "C"))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = c(0.3335526, 0.6664474),
              mu = matrix(c(6.497041, 2.500987, 1.249754, 6.500822), 2,
                          dimnames = list(c("C", "B"), NULL)),
              sigma = list(matrix(c(1.511549, 1.496384, 1.496384, 1.50366),
                                  2, dimnames = list(c("C", "B"), c("C", "B"))),
                           matrix(c(0.5519983, - 0.2995869, - 0.2995869,
                                    2.6010413),
                                  2,
                                  dimnames = list(c("C", "B"), c("C", "B")))))
class(gmm_2) <- "gmm"
gmm_3 <- list(alpha = 1,
              mu = matrix(c(0, 0, 0), dimnames = list(c("C", "A", "B"), NULL)),
              sigma = list(matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), 3,
                                  dimnames = list(c("C", "A", "B"),
                                                  c("C", "A", "B")))))
class(gmm_3) <- "gmm"
gmm_4 <- list(alpha = c(0.3333333, 0.3333333, 0.3333333),
              mu = matrix(c(2, 3, 0.5, 8.5, 6.5, 3), 2,
                          dimnames = list(c("C", "A"), NULL)),
              sigma = list(matrix(c(0.003333333, - 2.460655e-14, - 2.460655e-14,
                                    0.003333333),
                                  2,
                                  dimnames = list(c("C", "A"), c("C", "A"))),
                           matrix(c(0.17, - 0.1666667, - 0.1666667, 0.17), 2,
                                  dimnames = list(c("C", "A"), c("C", "A"))),
                           matrix(c(1.503333, - 3, - 3, 6.003333), 2,
                                  dimnames = list(c("C", "A"), c("C", "A")))))
class(gmm_4) <- "gmm"
gmm_5 <- list(alpha = c(0.3333367, 0.3333333, 0.33333),
              mu = matrix(c(1.00004, 4.99996, 1.5, 8, 6.500015, 2.500015), 2,
                          dimnames = list(c("C", "B"), NULL)),
              sigma = list(matrix(c(0.6701023, - 0.666769, - 0.666769,
                                    0.6701023),
                                  2, dimnames = list(c("C", "B"), c("C", "B"))),
                           matrix(c(0.17, - 0.3333333, - 0.3333333, 0.67), 2,
                                  dimnames = list(c("C", "B"), c("C", "B"))),
                           matrix(c(1.503328, 1.499995, 1.499995, 1.503328), 2,
                                  dimnames = list(c("C", "B"), c("C", "B")))))
class(gmm_5) <- "gmm"
gmm_6 <- list(alpha = 1,
              mu = matrix(3, dimnames = list("C", NULL)),
              sigma = list(matrix(6.287143, dimnames = list("C", "C"))))
class(gmm_6) <- "gmm"
gmm_7 <- list(alpha = c(0.6666667, 0.3333333),
              mu = matrix(c(1.25, 5.75, 6.5, 6.5, 3, 2.5), 3,
                          dimnames = list(c("C", "A", "B"), NULL)),
              sigma = list(matrix(c(0.552, - 1.75, - 0.3, - 1.75, 6.152, 1.9,
                                    - 0.3, 1.9, 2.602), 3,
                                  dimnames = list(c("C", "A", "B"),
                                                  c("C", "A", "B"))),
                           matrix(c(1.503333, - 3, 1.5, - 3, 6.003333, - 3, 1.5,
                                    - 3, 1.503333), 3,
                                  dimnames = list(c("C", "A", "B"),
                                                  c("C", "A", "B")))))
class(gmm_7) <- "gmm"

test_that("perform the stepwise algorithm (add operations)", {
  expect_equal(stepwise(gmm_1,
                        data.frame(A = c(0, 3, 6, 9, 3, 8),
                                   B = c(4, 7, 1, 6, 4, 9),
                                   C = c(8, 2, 5, 0, 2, 1)),
                        max_iter_step = 2, max_iter_smem = 2, regul = 0.01,
                        max_iter_em = 5),
               list(gmm = gmm_2,
                    posterior = matrix(c(1, 0, 0.9999998, 0, 0, 0, 6.029636e-19,
                                         1, 1.671704e-07, 1, 1, 1),
                                       6),
                    seq_score = c(- 56.30539, - 16.32012, - 14.15392),
                    seq_oper = c("SMEM", "add B + SMEM")),
               tolerance = 0.01)
  expect_equal(stepwise(gmm_1,
                        matrix(c(0, 3, 6, 9, 3, 8, 4, 7, 1, 6, 4, 9, 8, 2, 5, 0,
                                 2, 1),
                               6, dimnames = list(NULL, c("A", "B", "C"))),
                        max_iter_step = 2, max_iter_smem = 2, regul = 0.01,
                        max_iter_em = 5),
               list(gmm = gmm_2,
                    posterior = matrix(c(1, 0, 0.9999998, 0, 0, 0, 6.029636e-19,
                                         1, 1.671704e-07, 1, 1, 1),
                                       6),
                    seq_score = c(- 56.30539, - 16.32012, - 14.15392),
                    seq_oper = c("SMEM", "add B + SMEM")),
               tolerance = 0.01)
})

test_that("perform the stepwise algorithm (remove operations)", {
  expect_equal(stepwise(gmm_3,
                        data.frame(A = c(0, 3, 6, 9, 3, 8),
                                   B = c(4, 7, 1, 6, 4, 9),
                                   C = c(8, 2, 5, 0, 2, 1)),
                        max_iter_step = 2, max_iter_smem = 2, regul = 0.01,
                        max_iter_em = 5),
               list(gmm = gmm_4,
                    posterior = matrix(c(0, 1, 0, 0, 1, 0, 7.388666e-115, 0,
                                         8.108853e-146, 1, 0, 1, 1, 0, 1, 0, 0,
                                         0),
                                       6),
                    seq_score = c(- 62.57655, - 11.9679, - 4.66006),
                    seq_oper = c("SMEM", "remove B + SMEM")),
               tolerance = 0.01)
})

test_that("perform the stepwise algorithm with row names", {
  expect_equal(stepwise(gmm_1,
                        data.frame(A = c(0, 3, 6, 9, 3, 8),
                                   B = c(4, 7, 1, 6, 4, 9),
                                   C = c(8, 2, 5, 0, 2, 1),
                                   row.names = c("row_1", "row_2", "row_3",
                                                 "row_4", "row_5", "row_6")),
                        max_iter_step = 2, max_iter_smem = 2, regul = 0.01,
                        max_iter_em = 5),
               list(gmm = gmm_2,
                    posterior = matrix(c(1, 0, 0.9999998, 0, 0, 0, 6.029636e-19,
                                         1, 1.671704e-07, 1, 1, 1),
                                       6,
                                       dimnames = list(c("row_1", "row_2",
                                                         "row_3", "row_4",
                                                         "row_5", "row_6"),
                                                       NULL)),
                    seq_score = c(- 56.30539, - 16.32012, - 14.15392),
                    seq_oper = c("SMEM", "add B + SMEM")),
               tolerance = 0.01)
})

test_that("perform the stepwise algorithm with extra columns", {
  expect_equal(stepwise(gmm_1,
                        data.frame(A = c(0, 3, 6, 9, 3, 8),
                                   B = c(4, 7, 1, 6, 4, 9),
                                   C = c(8, 2, 5, 0, 2, 1),
                                   D = c(0, 0, 0, 0, 0, 0)),
                        x_cand = c("A", "B"), max_iter_step = 2,
                        max_iter_smem = 2, regul = 0.01, max_iter_em = 5),
               list(gmm = gmm_2,
                    posterior = matrix(c(1, 0, 0.9999998, 0, 0, 0, 6.029636e-19,
                                         1, 1.671704e-07, 1, 1, 1),
                                       6),
                    seq_score = c(- 56.30539, - 16.32012, - 14.15392),
                    seq_oper = c("SMEM", "add B + SMEM")),
               tolerance = 0.01)
})

test_that("perform the stepwise algorithm with non-candidate explanatory variables", {
  expect_equal(stepwise(gmm_3,
                        data.frame(A = c(0, 3, 6, 9, 3, 8),
                                   B = c(4, 7, 1, 6, 4, 9),
                                   C = c(8, 2, 5, 0, 2, 1)),
                        x_cand = "A", max_iter_step = 2, max_iter_smem = 2,
                        regul = 0.01, max_iter_em = 5),
               list(gmm = gmm_5,
                    posterior = matrix(c(0, 9.788173e-294, 2.011352e-05, 1, 1,
                                         0, 0, 1, 5.083391e-16, 0,
                                         2.610680e-121, 1, 1, 0, 0.9999799, 0,
                                         0, 0),
                                       6),
                    seq_score = c(- 62.57655, - 11.9679, - 6.89676),
                    seq_oper = c("SMEM", "remove A + SMEM")),
               tolerance = 0.01)
})

test_that("perform the stepwise algorithm with duplicated candidate explanatory variables", {
  expect_equal(stepwise(gmm_1,
                        data.frame(A = c(0, 3, 6, 9, 3, 8),
                                   B = c(4, 7, 1, 6, 4, 9),
                                   C = c(8, 2, 5, 0, 2, 1)),
                        x_cand = c("A", "B", "B"), max_iter_step = 2,
                        max_iter_smem = 2, regul = 0.01, max_iter_em = 5),
               list(gmm = gmm_2,
                    posterior = matrix(c(1, 0, 0.9999998, 0, 0, 0, 6.029636e-19,
                                         1, 1.671704e-07, 1, 1, 1),
                                       6),
                    seq_score = c(- 56.30539, - 16.32012, - 14.15392),
                    seq_oper = c("SMEM", "add B + SMEM")),
               tolerance = 0.01)
})

test_that("perform the stepwise algorithm with no candidate explanatory variable", {
  expect_equal(stepwise(gmm_1, data.frame(C = c(8, 2, 5, 0, 2, 1)),
                        x_cand = NULL, max_iter_step = 2, max_iter_smem = 2,
                        regul = 0.01, max_iter_em = 5),
               list(gmm = gmm_6, posterior = matrix(1, 6),
                    seq_score = c(- 56.30539, - 16.32012, - 16.32012),
                    seq_oper = c("SMEM", "none")),
               tolerance = 0.01)
  expect_equal(stepwise(gmm_1, data.frame(C = c(8, 2, 5, 0, 2, 1)),
                        max_iter_step = 2, max_iter_smem = 2, regul = 0.01,
                        max_iter_em = 5),
               list(gmm = gmm_6, posterior = matrix(1, 6),
                    seq_score = c(- 56.30539, - 16.32012, - 16.32012),
                    seq_oper = c("SMEM", "none")),
               tolerance = 0.01)
})

test_that("perform the stepwise algorithm with the AIC", {
  expect_equal(stepwise(gmm_1,
                        data.frame(A = c(0, 3, 6, 9, 3, 8),
                                   B = c(4, 7, 1, 6, 4, 9),
                                   C = c(8, 2, 5, 0, 2, 1)),
                        score = "aic", max_iter_step = 2, max_iter_smem = 2,
                        regul = 0.01, max_iter_em = 5),
               list(gmm = gmm_2,
                    posterior = matrix(c(1, 0, 0.9999998, 0, 0, 0, 6.029636e-19,
                                         1, 1.671704e-07, 1, 1, 1),
                                       6),
                    seq_score = c(- 56.51363, - 16.52836, - 15.29924),
                    seq_oper = c("SMEM", "add B + SMEM")),
               tolerance = 0.01)
})

test_that("perform the stepwise algorithm with the log-likelihood", {
  expect_equal(stepwise(gmm_1,
                        data.frame(A = c(0, 3, 6, 9, 3, 8),
                                   B = c(4, 7, 1, 6, 4, 9),
                                   C = c(8, 2, 5, 0, 2, 1)),
                        score = "loglik", max_iter_step = 2, max_iter_smem = 2,
                        regul = 0.01, max_iter_em = 5),
               list(gmm = gmm_4,
                    posterior = matrix(c(0, 1, 0, 0, 1, 0, 7.388666e-115, 0,
                                         8.108853e-146, 1, 0, 1, 1, 0, 1, 0, 0,
                                         0),
                                       6),
                    seq_score = c(- 54.51363, - 12.47493, 10.5699),
                    seq_oper = c("SMEM", "add A + SMEM")),
               tolerance = 0.01)
})

test_that("perform the stepwise algorithm with no add operation", {
  expect_equal(stepwise(gmm_1,
                        data.frame(A = c(0, 3, 6, 9, 3, 8),
                                   B = c(4, 7, 1, 6, 4, 9),
                                   C = c(8, 2, 5, 0, 2, 1)),
                        add = FALSE, max_iter_smem = 2, regul = 0.01,
                        max_iter_em = 5),
               list(gmm = gmm_6, posterior = matrix(1, 6),
                    seq_score = c(- 56.30539, - 16.32012, - 16.32012),
                    seq_oper = c("SMEM", "none")),
               tolerance = 0.01)
})

test_that("perform the stepwise algorithm with no remove operation", {
  expect_equal(stepwise(gmm_3,
                        data.frame(A = c(0, 3, 6, 9, 3, 8),
                                   B = c(4, 7, 1, 6, 4, 9),
                                   C = c(8, 2, 5, 0, 2, 1)),
                        remove = FALSE, max_iter_smem = 2, regul = 0.01,
                        max_iter_em = 5),
               list(gmm = gmm_7,
                    posterior = matrix(c(2.042583e-233, 1, 1.241414e-176, 1, 1,
                                         1, 1, 0, 1, 0, 0, 0),
                                       6),
                    seq_score = c(- 62.57655, - 11.9679, - 11.9679),
                    seq_oper = c("SMEM", "none")),
               tolerance = 0.01)
})

test_that("perform the stepwise algorithm with a minimum number of explanatory variables", {
  expect_equal(stepwise(gmm_3,
                        data.frame(A = c(0, 3, 6, 9, 3, 8),
                                   B = c(4, 7, 1, 6, 4, 9),
                                   C = c(8, 2, 5, 0, 2, 1)),
                        min_x = 2, max_iter_smem = 2, regul = 0.01,
                        max_iter_em = 5),
               list(gmm = gmm_7,
                    posterior = matrix(c(2.042583e-233, 1, 1.241414e-176, 1, 1,
                                         1, 1, 0, 1, 0, 0, 0),
                                       6),
                    seq_score = c(- 62.57655, - 11.9679, - 11.9679),
                    seq_oper = c("SMEM", "none")),
               tolerance = 0.01)
})

test_that("perform the stepwise algorithm with a maximum number of explanatory variables", {
  expect_equal(stepwise(gmm_1,
                        data.frame(A = c(0, 3, 6, 9, 3, 8),
                                   B = c(4, 7, 1, 6, 4, 9),
                                   C = c(8, 2, 5, 0, 2, 1)),
                        max_x = 0, max_iter_smem = 2, regul = 0.01,
                        max_iter_em = 5),
               list(gmm = gmm_6, posterior = matrix(1, 6),
                    seq_score = c(- 56.30539, - 16.32012, - 16.32012),
                    seq_oper = c("SMEM", "none")),
               tolerance = 0.01)
})

test_that("perform no iteration of the stepwise algorithm", {
  expect_equal(stepwise(gmm_1,
                        data.frame(A = c(0, 3, 6, 9, 3, 8),
                                   B = c(4, 7, 1, 6, 4, 9),
                                   C = c(8, 2, 5, 0, 2, 1)),
                        max_iter_step = 0),
               list(gmm = gmm_1, posterior = matrix(1, 6),
                    seq_score = - 56.30539, seq_oper = character()),
               tolerance = 0.01)
})

test_that("perform the stepwise algorithm with verbose", {
  expect_equal(stepwise(gmm_1,
                        data.frame(A = c(0, 3, 6, 9, 3, 8),
                                   B = c(4, 7, 1, 6, 4, 9),
                                   C = c(8, 2, 5, 0, 2, 1)),
                        max_iter_step = 2, verbose = TRUE, max_iter_smem = 2,
                        regul = 0.01, max_iter_em = 5),
               list(gmm = gmm_2,
                    posterior = matrix(c(1, 0, 0.9999998, 0, 0, 0, 6.029636e-19,
                                         1, 1.671704e-07, 1, 1, 1),
                                       6),
                    seq_score = c(- 56.30539, - 16.32012, - 14.15392),
                    seq_oper = c("SMEM", "add B + SMEM")),
               tolerance = 0.01)
})
