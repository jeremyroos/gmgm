gmm_1 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 2, 1, 3), 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_1) <- "gmm"

gmm_2 <- list(alpha = 1, mu = matrix(0, 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 0, 0, 1), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_2) <- "gmm"

test_that("create a gmm object", {
  expect_equal(gmm(alpha = c(0.4, 0.6),
                   mu = matrix(c(0, 2, 1, 3), 2,
                               dimnames = list(c("A", "B"), NULL)),
                   sigma = list(matrix(c(1, 1, 1, 2), 2),
                                matrix(c(2, 3, 3, 5), 2))),
               gmm_1)
})

test_that("create a gmm object with one mixture component", {
  expect_equal(gmm(alpha = 1,
                   mu = matrix(0, 2, dimnames = list(c("A", "B"), NULL)),
                   sigma = list(matrix(c(1, 0, 0, 1), 2))),
               gmm_2)
})

test_that("create a gmm object whith non-normalized mixture proportions", {
  expect_equal(gmm(alpha = c(0.8, 1.2),
                   mu = matrix(c(0, 2, 1, 3), 2,
                               dimnames = list(c("A", "B"), NULL)),
                   sigma = list(matrix(c(1, 1, 1, 2), 2),
                                matrix(c(2, 3, 3, 5), 2))),
               gmm_1)
})

test_that("create a gmm object with explicit variable names", {
  expect_equal(gmm(alpha = c(comp_1 = 0.4, comp_2 = 0.6),
                   mu = matrix(c(0, 2, 1, 3), 2,
                               dimnames = list(c("C", "D"),
                                               c("comp_1", "comp_2"))),
                   sigma = list(comp_1 = matrix(c(1, 1, 1, 2), 2,
                                                dimnames = list(c("C", "D"),
                                                                c("C", "D"))),
                                comp_2 = matrix(c(2, 3, 3, 5), 2,
                                                dimnames = list(c("C", "D"),
                                                                c("C", "D")))),
                   var = c("A", "B")),
               gmm_1)
})
