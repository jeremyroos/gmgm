gmm_1 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 2, 1, 3), 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 2, 1, 3), 2, dimnames = list(c("B", "C"), NULL)),
              sigma = list(matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("B", "C"), c("B", "C"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("B", "C"), c("B", "C")))))
class(gmm_2) <- "gmm"
gmm_3 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 1), 1, dimnames = list("A", NULL)),
              sigma = list(matrix(1, dimnames = list("A", "A")),
                           matrix(2, dimnames = list("A", "A"))))
class(gmm_3) <- "gmm"

test_that("rename variables", {
  expect_equal(rename_var(gmm_1, c("A", "B"), c("B", "C")), gmm_2)
})

test_that("rename no variable", {
  expect_equal(rename_var(gmm_3, NULL, NULL), gmm_3)
  expect_equal(rename_var(gmm_3, character(), character()), gmm_3)
})
