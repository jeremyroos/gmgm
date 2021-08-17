gmm_1 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 2, 1, 3), 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = c(0.6397767, 0.3602233),
              mu = matrix(c(5.16342, 4.231158, 4.24708, 6.828183), 2,
                          dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(8.045724, 1.491022, 1.491022, 4.324193), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(5.934738, 3.556337, 3.556337, 2.721583), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_2) <- "gmm"
gmm_3 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(100, 100, 1, 3), 2,
                          dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_3) <- "gmm"
gmm_4 <- list(alpha = 1,
              mu = matrix(c(4.833333, 5.166667),
                          dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(8.40619, 2.166667, 2.166667, 5.549048), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_4) <- "gmm"
gmm_5 <- list(alpha = c(0.6148867, 0.3851133),
              mu = matrix(c(5.329828, 4.272223, 4.040611, 6.59477), 2,
                          dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(9.754278, 1.869673, 1.869673, 5.765295), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(8.865438, 5.419667, 5.419667, 4.284093), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_5) <- "gmm"

test_that("perform the EM algorithm", {
  expect_equal(em(gmm_1,
                  data.frame(A = c(0, 3, 6, 9, 3, 8), B = c(4, 7, 1, 6, 4, 9)),
                  max_iter_em = 2),
               list(gmm = gmm_2,
                    posterior = matrix(c(0.3374681, 0.2269094, 1, 0.9999921,
                                         0.9508694, 0.1117884, 0.6625319,
                                         0.7730906, 2.271e-17, 7.854876e-06,
                                         0.04913058, 0.8882116),
                                       6),
                    seq_loglik = c(- 151.31306, - 31.62107, - 30.51398)),
               tolerance = 0.01)
  expect_equal(em(gmm_1,
                  matrix(c(0, 3, 6, 9, 3, 8, 4, 7, 1, 6, 4, 9), 6,
                         dimnames = list(NULL, c("A", "B"))),
                  max_iter_em = 2),
               list(gmm = gmm_2,
                    posterior = matrix(c(0.3374681, 0.2269094, 1, 0.9999921,
                                         0.9508694, 0.1117884, 0.6625319,
                                         0.7730906, 2.271e-17, 7.854876e-06,
                                         0.04913058, 0.8882116),
                                       6),
                    seq_loglik = c(- 151.31306, - 31.62107, - 30.51398)),
               tolerance = 0.01)
})

test_that("perform the EM algorithm with row names", {
  expect_equal(em(gmm_1,
                  data.frame(A = c(0, 3, 6, 9, 3, 8), B = c(4, 7, 1, 6, 4, 9),
                             row.names = c("row_1", "row_2", "row_3", "row_4",
                                           "row_5", "row_6")),
                  max_iter_em = 2),
               list(gmm = gmm_2,
                    posterior = matrix(c(0.3374681, 0.2269094, 1, 0.9999921,
                                         0.9508694, 0.1117884, 0.6625319,
                                         0.7730906, 2.271e-17, 7.854876e-06,
                                         0.04913058, 0.8882116),
                                       6,
                                       dimnames = list(c("row_1", "row_2",
                                                         "row_3", "row_4",
                                                         "row_5", "row_6"),
                                                       NULL)),
                    seq_loglik = c(- 151.31306, - 31.62107, - 30.51398)),
               tolerance = 0.01)
})

test_that("perform the EM algorithm with extra columns", {
  expect_equal(em(gmm_1,
                  data.frame(A = c(0, 3, 6, 9, 3, 8), B = c(4, 7, 1, 6, 4, 9),
                             C = c(0, 0, 0, 0, 0, 0)),
                  max_iter_em = 2),
               list(gmm = gmm_2,
                    posterior = matrix(c(0.3374681, 0.2269094, 1, 0.9999921,
                                         0.9508694, 0.1117884, 0.6625319,
                                         0.7730906, 2.271e-17, 7.854876e-06,
                                         0.04913058, 0.8882116),
                                       6),
                    seq_loglik = c(- 151.31306, - 31.62107, - 30.51398)),
               tolerance = 0.01)
})

test_that("perform the EM algorithm with removed mixture components", {
  expect_equal(em(gmm_3,
                  data.frame(A = c(0, 3, 6, 9, 3, 8), B = c(4, 7, 1, 6, 4, 9)),
                  max_iter_em = 2),
               list(gmm = gmm_4, posterior = matrix(1, 6),
                    seq_loglik = c(- 253.6422, - 31.1051, - 31.1051)),
               tolerance = 0.01)
})

test_that("perform the EM algorithm with no regularization", {
  expect_equal(em(gmm_1,
                  data.frame(A = c(0, 3, 6, 9, 3, 8), B = c(4, 7, 1, 6, 4, 9)),
                  regul = NULL, max_iter_em = 2),
               list(gmm = gmm_5,
                    posterior = matrix(c(0.2654682, 0.2632832, 1, 0.999027,
                                         0.7886992, 0.1731678, 0.7345318,
                                         0.7367168, 1.785065e-10, 9.729525e-04,
                                         0.2113008, 0.8268322),
                                       6),
                    seq_loglik = c(- 151.26306, - 28.73719, - 28.07696)),
               tolerance = 0.01)
})

test_that("perform no iteration of the EM algorithm", {
  expect_equal(em(gmm_1,
                  data.frame(A = c(0, 3, 6, 9, 3, 8), B = c(4, 7, 1, 6, 4, 9)),
                  max_iter_em = 0),
               list(gmm = gmm_1,
                    posterior = matrix(c(0.983609626, 0.007351552, 1, 1, 0.4,
                                         0.4, 0.01639037, 0.9926484,
                                         5.298943e-24, 1.16717e-19, 0.6, 0.6),
                                       6),
                    seq_loglik = - 151.26306),
               tolerance = 0.01)
})

test_that("perform the EM algorithm with verbose", {
  expect_equal(em(gmm_1,
                  data.frame(A = c(0, 3, 6, 9, 3, 8), B = c(4, 7, 1, 6, 4, 9)),
                  max_iter_em = 2, verbose = TRUE),
               list(gmm = gmm_2,
                    posterior = matrix(c(0.3374681, 0.2269094, 1, 0.9999921,
                                         0.9508694, 0.1117884, 0.6625319,
                                         0.7730906, 2.271e-17, 7.854876e-06,
                                         0.04913058, 0.8882116),
                                       6),
                    seq_loglik = c(- 151.31306, - 31.62107, - 30.51398)),
               tolerance = 0.01)
})
