gmm_1 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 2, 1, 3), 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 1), 1, dimnames = list("A", NULL)),
              sigma = list(matrix(1, dimnames = list("A", "A")),
                           matrix(2, dimnames = list("A", "A"))))
class(gmm_2) <- "gmm"
gmm_3 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(2, 0, 3, 1), 2, dimnames = list(c("B", "A"), NULL)),
              sigma = list(matrix(c(2, 1, 1, 1), 2,
                                  dimnames = list(c("B", "A"), c("B", "A"))),
                           matrix(c(5, 3, 3, 2), 2,
                                  dimnames = list(c("B", "A"), c("B", "A")))))
class(gmm_3) <- "gmm"
gmm_4 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(4, 0, 2, 5, 1, 3), 3,
                          dimnames = list(c("A.10", "A.2", "B"), NULL)),
              sigma = list(matrix(c(3, 1, 2, 1, 1, 1, 2, 1, 2), 3,
                                  dimnames = list(c("A.10", "A.2", "B"),
                                                  c("A.10", "A.2", "B"))),
                           matrix(c(4, 1, 2, 1, 2, 3, 2, 3, 5), 3,
                                  dimnames = list(c("A.10", "A.2", "B"),
                                                  c("A.10", "A.2", "B")))))
class(gmm_4) <- "gmm"
gmm_5 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(2, 0, 4, 3, 1, 5), 3,
                          dimnames = list(c("B", "A.2", "A.10"), NULL)),
              sigma = list(matrix(c(2, 1, 2, 1, 1, 1, 2, 1, 3), 3,
                                  dimnames = list(c("B", "A.2", "A.10"),
                                                  c("B", "A.2", "A.10"))),
                           matrix(c(5, 3, 2, 3, 2, 1, 2, 1, 4), 3,
                                  dimnames = list(c("B", "A.2", "A.10"),
                                                  c("B", "A.2", "A.10")))))
class(gmm_5) <- "gmm"

gmbn_1 <- list(A = gmm_2, B = gmm_3)
class(gmbn_1) <- "gmbn"
gmbn_2 <- list(A = gmm_2, B = gmm_5)
class(gmbn_2) <- "gmbn"

test_that("create a gmbn object", {
  expect_equal(gmbn(B = gmm_1, A = gmm_2), gmbn_1)
})

test_that("create a temporal gmbn object", {
  expect_equal(gmbn(A = gmm_2, B = gmm_4), gmbn_2)
})
