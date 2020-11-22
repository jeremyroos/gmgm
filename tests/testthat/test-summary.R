gmm_1 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(2, 0, 3, 1), 2, dimnames = list(c("B", "A"), NULL)),
              sigma = list(matrix(c(2, 1, 1, 1), 2,
                                  dimnames = list(c("B", "A"), c("B", "A"))),
                           matrix(c(5, 3, 3, 2), 2,
                                  dimnames = list(c("B", "A"), c("B", "A")))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 1), 1, dimnames = list("A", NULL)),
              sigma = list(matrix(1, dimnames = list("A", "A")),
                           matrix(2, dimnames = list("A", "A"))))
class(gmm_2) <- "gmm"
gmm_3 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(2, 0, 3, 1), 2,
                          dimnames = list(c("B", "A.1"), NULL)),
              sigma = list(matrix(c(2, 1, 1, 1), 2,
                                  dimnames = list(c("B", "A.1"),
                                                  c("B", "A.1"))),
                           matrix(c(5, 3, 3, 2), 2,
                                  dimnames = list(c("B", "A.1"),
                                                  c("B", "A.1")))))
class(gmm_3) <- "gmm"

gmbn_1 <- list(A = gmm_2, B = gmm_1)
class(gmbn_1) <- "gmbn"
gmbn_2 <- list(A = gmm_2, B = gmm_3)
class(gmbn_2) <- "gmbn"

gmdbn_1 <- list(b_1 = gmbn_1, b_2 = gmbn_2)
class(gmdbn_1) <- "gmdbn"

test_that("summarise a gmm object", {
  expect_equal(summary(gmm_1), c(n_var = 2, n_comp = 2, n_param = 11))
})

test_that("summarise a gmbn object", {
  expect_equal(summary(gmbn_1),
               list(global = c(n_nodes = 2, n_arcs = 1, n_comp = 4,
                               n_param = 16),
                    local = matrix(c(0, 2, 5, 1, 2, 11), 3,
                                   dimnames = list(c("n_arcs", "n_comp",
                                                     "n_param"),
                                                   c("A", "B")))))
})

test_that("summarise a gmdbn object", {
  expect_equal(summary(gmdbn_1),
               list(global = c(n_gmbn = 2, n_nodes = 4, n_arcs = 2, n_comp = 8,
                               n_param = 32),
                    local = list(b_1 = matrix(c(0, 2, 5, 1, 2, 11), 3,
                                              dimnames = list(c("n_arcs",
                                                                "n_comp",
                                                                "n_param"),
                                                              c("A", "B"))),
                                 b_2 = matrix(c(0, 2, 5, 1, 2, 11), 3,
                                              dimnames = list(c("n_arcs",
                                                                "n_comp",
                                                                "n_param"),
                                                              c("A", "B"))))))
})
