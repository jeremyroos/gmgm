gmm_1 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 1), 1, dimnames = list("A", NULL)),
              sigma = list(matrix(1, dimnames = list("A", "A")),
                           matrix(2, dimnames = list("A", "A"))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(2, 0, 3, 1), 2, dimnames = list(c("B", "A"), NULL)),
              sigma = list(matrix(c(2, 1, 1, 1), 2,
                                  dimnames = list(c("B", "A"), c("B", "A"))),
                           matrix(c(5, 3, 3, 2), 2,
                                  dimnames = list(c("B", "A"), c("B", "A")))))
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
gmm_4 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(2, 0, 4, 3, 1, 5), 3,
                          dimnames = list(c("B", "A", "A.1"), NULL)),
              sigma = list(matrix(c(2, 1, 2, 1, 1, 1, 2, 1, 3), 3,
                                  dimnames = list(c("B", "A", "A.1"),
                                                  c("B", "A", "A.1"))),
                           matrix(c(5, 3, 2, 3, 2, 1, 2, 1, 4), 3,
                                  dimnames = list(c("B", "A", "A.1"),
                                                  c("B", "A", "A.1")))))
class(gmm_4) <- "gmm"

gmbn_1 <- list(A = gmm_1, B = gmm_2)
class(gmbn_1) <- "gmbn"
gmbn_2 <- list(A = gmm_1, B = gmm_3)
class(gmbn_2) <- "gmbn"
gmbn_3 <- list(A = gmm_1, B = gmm_4)
class(gmbn_3) <- "gmbn"

gmdbn_1 <- list(b_1 = gmbn_1, b_2 = gmbn_2)
class(gmdbn_1) <- "gmdbn"
gmdbn_2 <- list(b_1 = gmbn_1, b_2 = gmbn_3, b_3 = gmbn_3, b_10 = gmbn_2)
class(gmdbn_2) <- "gmdbn"

test_that("create a gmdbn object with an explicit gmbn element b_1", {
  expect_equal(gmdbn(b_1 = gmbn_1, b_2 = gmbn_2), gmdbn_1)
})

test_that("create a gmdbn object with no explicit gmbn element b_1", {
  expect_equal(gmdbn(b_10 = gmbn_2, b_3 = gmbn_3), gmdbn_2)
})
