gmm_1 <- list(alpha = 1,
              mu = matrix(0, 2, dimnames = list(c("A", "A.1"), NULL)),
              sigma = list(matrix(c(1, 0, 0, 1), 2,
                                  dimnames = list(c("A", "A.1"),
                                                  c("A", "A.1")))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = 1,
              mu = matrix(0, 4,
                          dimnames = list(c("B", "A", "A.1", "B.1"), NULL)),
              sigma = list(matrix(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                                    1),
                                  4,
                                  dimnames = list(c("B", "A", "A.1", "B.1"),
                                                  c("B", "A", "A.1", "B.1")))))
class(gmm_2) <- "gmm"

gmbn_1 <- list(A = gmm_1, B = gmm_2)
class(gmbn_1) <- "gmbn"

test_that("display the graphical structure of a gmbn object", {
  expect_is(network(gmbn_1), "visNetwork")
})
