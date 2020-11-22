gmm_1 <- list(alpha = 1,
              mu = matrix(c(0, 2), dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 0, 0, 1), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = c(0.3333185, 0.6666815),
              mu = matrix(c(8.499974, 7.500061, 3.000135, 4.000047), 2,
                          dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(0.1700083, - 0.4999829, - 0.4999829,
                                    1.5033315),
                                  2, dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(3.602587, - 1.799744, - 1.799744, 3.602009),
                                  2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_2) <- "gmm"
gmm_3 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 2, 1, 3), 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_3) <- "gmm"
gmm_4 <- list(alpha = 1,
              mu = matrix(c(4.833333, 5.166667),
                          dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(8.547619, 2.166667, 2.166667, 5.690476), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_4) <- "gmm"
gmm_5 <- list(alpha = 1,
              mu = matrix(c(4.833333, 5.166667),
                          dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(8.40619, 2.166667, 2.166667, 5.549048), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_5) <- "gmm"
gmm_6 <- list(alpha = c(0.5316266, 0.4683734),
              mu = matrix(c(5.86814, 3.923245, 3.658777, 6.578011), 2,
                          dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(5.4443, 1.405555, 1.405555, 4.086618), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(8.06521, 4.734752, 4.734752, 3.459628), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_6) <- "gmm"

test_that("perform the SMEM algorithm (split operations)", {
  expect_equal(smem(gmm_1,
                    data.frame(A = c(0, 3, 6, 9, 3, 8),
                               B = c(4, 7, 1, 6, 4, 9)),
                    max_iter_smem = 2, regul = 0.01, max_iter_em = 5),
               list(gmm = gmm_2,
                    posterior = matrix(c(0, 0, 0, 0.9999932, 0, 0.9999999, 1, 1,
                                         1, 6.757123e-06, 1, 9.609848e-08),
                                       6),
                    seq_score = c(- 164.51666, - 35.58449, - 30.60377),
                    seq_oper = c("EM", "split 1 + EM")),
               tolerance = 0.01)
  expect_equal(smem(gmm_1,
                    matrix(c(0, 3, 6, 9, 3, 8, 4, 7, 1, 6, 4, 9), 6,
                           dimnames = list(NULL, c("A", "B"))),
                    max_iter_smem = 2, regul = 0.01, max_iter_em = 5),
               list(gmm = gmm_2,
                    posterior = matrix(c(0, 0, 0, 0.9999932, 0, 0.9999999, 1, 1,
                                         1, 6.757123e-06, 1, 9.609848e-08),
                                       6),
                    seq_score = c(- 164.51666, - 35.58449, - 30.60377),
                    seq_oper = c("EM", "split 1 + EM")),
               tolerance = 0.01)
})

test_that("perform the SMEM algorithm (merge operations)", {
  expect_equal(smem(gmm_3,
                    data.frame(A = c(0, 3, 6, 9, 3, 8),
                               B = c(4, 7, 1, 6, 4, 9)),
                    max_iter_smem = 2, regul = 1, max_iter_em = 5),
               list(gmm = gmm_4, posterior = matrix(1, 6),
                    seq_score = c(- 166.11773, - 41.25962, - 35.74699),
                    seq_oper = c("EM", "merge 1 and 2 + EM")),
               tolerance = 0.01)
})

test_that("perform the SMEM algorithm with row names", {
  expect_equal(smem(gmm_1,
                    data.frame(A = c(0, 3, 6, 9, 3, 8), B = c(4, 7, 1, 6, 4, 9),
                               row.names = c("row_1", "row_2", "row_3", "row_4",
                                             "row_5", "row_6")),
                    max_iter_smem = 2, regul = 0.01, max_iter_em = 5),
               list(gmm = gmm_2,
                    posterior = matrix(c(0, 0, 0, 0.9999932, 0, 0.9999999, 1, 1,
                                         1, 6.757123e-06, 1, 9.609848e-08),
                                       6,
                                       dimnames = list(c("row_1", "row_2",
                                                         "row_3", "row_4",
                                                         "row_5", "row_6"),
                                                       NULL)),
                    seq_score = c(- 164.51666, - 35.58449, - 30.60377),
                    seq_oper = c("EM", "split 1 + EM")),
               tolerance = 0.01)
})

test_that("perform the SMEM algorithm with extra columns", {
  expect_equal(smem(gmm_1,
                    data.frame(A = c(0, 3, 6, 9, 3, 8), B = c(4, 7, 1, 6, 4, 9),
                               C = c(0, 0, 0, 0, 0, 0)),
                    max_iter_smem = 2, regul = 0.01, max_iter_em = 5),
               list(gmm = gmm_2,
                    posterior = matrix(c(0, 0, 0, 0.9999932, 0, 0.9999999, 1, 1,
                                         1, 6.757123e-06, 1, 9.609848e-08),
                                       6),
                    seq_score = c(- 164.51666, - 35.58449, - 30.60377),
                    seq_oper = c("EM", "split 1 + EM")),
               tolerance = 0.01)
})

test_that("perform the SMEM algorithm for a conditional gmm object", {
  expect_equal(smem(gmm_1,
                    data.frame(A = c(0, 3, 6, 9, 3, 8), B = c(4, 7, 1, 6, 4, 9),
                               C = c(0, 0, 0, 0, 0, 0)),
                    y = "B", max_iter_smem = 2, regul = 0.01, max_iter_em = 5),
               list(gmm = gmm_2,
                    posterior = matrix(c(0, 0, 0, 0.9999932, 0, 0.9999999, 1, 1,
                                         1, 6.757123e-06, 1, 9.609848e-08),
                                       6),
                    seq_score = c(- 59.49303, - 18.31463, - 16.50089),
                    seq_oper = c("EM", "split 1 + EM")),
               tolerance = 0.01)
})

test_that("perform the SMEM algorithm with the AIC", {
  expect_equal(smem(gmm_1,
                    data.frame(A = c(0, 3, 6, 9, 3, 8),
                               B = c(4, 7, 1, 6, 4, 9)),
                    score = "aic", max_iter_smem = 2, regul = 0.01,
                    max_iter_em = 5),
               list(gmm = gmm_2,
                    posterior = matrix(c(0, 0, 0, 0.9999932, 0, 0.9999999, 1, 1,
                                         1, 6.757123e-06, 1, 9.609848e-08),
                                       6),
                    seq_score = c(- 165.03726, - 36.1051, - 31.74909),
                    seq_oper = c("EM", "split 1 + EM")),
               tolerance = 0.01)
})

test_that("perform the SMEM algorithm with the log-likelihood", {
  expect_equal(smem(gmm_1,
                    data.frame(A = c(0, 3, 6, 9, 3, 8),
                               B = c(4, 7, 1, 6, 4, 9)),
                    score = "loglik", max_iter_smem = 2, regul = 0.01,
                    max_iter_em = 5),
               list(gmm = gmm_2,
                    posterior = matrix(c(0, 0, 0, 0.9999932, 0, 0.9999999, 1, 1,
                                         1, 6.757123e-06, 1, 9.609848e-08),
                                       6),
                    seq_score = c(- 160.03726, - 31.1051, - 20.74909),
                    seq_oper = c("EM", "split 1 + EM")),
               tolerance = 0.01)
})

test_that("perform the SMEM algorithm with no split operation", {
  expect_equal(smem(gmm_1,
                    data.frame(A = c(0, 3, 6, 9, 3, 8),
                               B = c(4, 7, 1, 6, 4, 9)),
                    split = FALSE, regul = 0.01, max_iter_em = 5),
               list(gmm = gmm_5, posterior = matrix(1, 6),
                    seq_score = c(- 164.51666, - 35.58449, - 35.58449),
                    seq_oper = c("EM", "none")),
               tolerance = 0.01)
})

test_that("perform the SMEM algorithm with no merge operation", {
  expect_equal(smem(gmm_3,
                    data.frame(A = c(0, 3, 6, 9, 3, 8),
                               B = c(4, 7, 1, 6, 4, 9)),
                    merge = FALSE, regul = 1, max_iter_em = 5),
               list(gmm = gmm_6,
                    posterior = matrix(c(0.0446268, 0.06115622, 1, 0.99996515,
                                         0.89890579, 0.07479677, 0.9553732,
                                         0.9388438, 1.437539e-15, 3.484545e-05,
                                         0.1010942, 0.9252032),
                                       6),
                    seq_score = c(- 166.11773, - 41.25962, - 41.25962),
                    seq_oper = c("EM", "none")),
               tolerance = 0.01)
})

test_that("perform the SMEM algorithm with a minimum number of mixture components", {
  expect_equal(smem(gmm_3,
                    data.frame(A = c(0, 3, 6, 9, 3, 8),
                               B = c(4, 7, 1, 6, 4, 9)),
                    min_comp = 2, regul = 1, max_iter_em = 5),
               list(gmm = gmm_6,
                    posterior = matrix(c(0.0446268, 0.06115622, 1, 0.99996515,
                                         0.89890579, 0.07479677, 0.9553732,
                                         0.9388438, 1.437539e-15, 3.484545e-05,
                                         0.1010942, 0.9252032),
                                       6),
                    seq_score = c(- 166.11773, - 41.25962, - 41.25962),
                    seq_oper = c("EM", "none")),
               tolerance = 0.01)
})

test_that("perform the SMEM algorithm with a maximum number of mixture components", {
  expect_equal(smem(gmm_1,
                    data.frame(A = c(0, 3, 6, 9, 3, 8),
                               B = c(4, 7, 1, 6, 4, 9)),
                    max_comp = 1, regul = 0.01, max_iter_em = 5),
               list(gmm = gmm_5, posterior = matrix(1, 6),
                    seq_score = c(- 164.51666, - 35.58449, - 35.58449),
                    seq_oper = c("EM", "none")),
               tolerance = 0.01)
})

test_that("perform the SMEM algorithm with a maximum rank", {
  expect_equal(smem(gmm_1,
                    data.frame(A = c(0, 3, 6, 9, 3, 8),
                               B = c(4, 7, 1, 6, 4, 9)),
                    max_iter_smem = 2, regul = 0.01, max_iter_em = 5),
               list(gmm = gmm_2,
                    posterior = matrix(c(0, 0, 0, 0.9999932, 0, 0.9999999, 1, 1,
                                         1, 6.757123e-06, 1, 9.609848e-08),
                                       6),
                    seq_score = c(- 164.51666, - 35.58449, - 30.60377),
                    seq_oper = c("EM", "split 1 + EM")),
               tolerance = 0.01)
})

test_that("perform no iteration of the SMEM algorithm", {
  expect_equal(smem(gmm_1,
                    data.frame(A = c(0, 3, 6, 9, 3, 8),
                               B = c(4, 7, 1, 6, 4, 9)),
                    max_iter_smem = 0),
               list(gmm = gmm_1, posterior = matrix(1, 6),
                    seq_score = - 164.5067, seq_oper = character()),
               tolerance = 0.01)
})

test_that("perform the SMEM algorithm with verbose", {
  expect_equal(smem(gmm_1,
                    data.frame(A = c(0, 3, 6, 9, 3, 8),
                               B = c(4, 7, 1, 6, 4, 9)),
                    max_iter_smem = 2, regul = 0.01, max_iter_em = 5,
                    verbose = TRUE),
               list(gmm = gmm_2,
                    posterior = matrix(c(0, 0, 0, 0.9999932, 0, 0.9999999, 1, 1,
                                         1, 6.757123e-06, 1, 9.609848e-08),
                                       6),
                    seq_score = c(- 164.51666, - 35.58449, - 30.60377),
                    seq_oper = c("EM", "split 1 + EM")),
               tolerance = 0.01)
})
