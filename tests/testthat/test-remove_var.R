gmm_1 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 2, 4, 1, 3, 5), 3,
                          dimnames = list(c("A", "B", "C"), NULL)),
              sigma = list(matrix(c(1, 1, 1, 1, 2, 2, 1, 2, 3), 3,
                                  dimnames = list(c("A", "B", "C"),
                                                  c("A", "B", "C"))),
                           matrix(c(2, 3, 1, 3, 5, 2, 1, 2, 4), 3,
                                  dimnames = list(c("A", "B", "C"),
                                                  c("A", "B", "C")))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(2, 3), 1, dimnames = list("B", NULL)),
              sigma = list(matrix(2, dimnames = list("B", "B")),
                           matrix(5, dimnames = list("B", "B"))))
class(gmm_2) <- "gmm"
gmm_3 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 2, 1, 3), 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_3) <- "gmm"

test_that("remove variables", {
  expect_equal(remove_var(gmm_1, c("A", "C")), gmm_2)
})

test_that("remove duplicated variables", {
  expect_equal(remove_var(gmm_3, c("A", "A")), gmm_2)
})

test_that("remove non-existent variables", {
  expect_equal(remove_var(gmm_3, "C"), gmm_3)
})

test_that("remove no variable", {
  expect_equal(remove_var(gmm_3, NULL), gmm_3)
  expect_equal(remove_var(gmm_3, character()), gmm_3)
})
