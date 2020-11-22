gmm_1 <- list(alpha = 1, mu = matrix(0, dimnames = list("A", NULL)),
              sigma = list(matrix(1, dimnames = list("A", "A"))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = 1, mu = matrix(0, 2, dimnames = list(c("B", "A"), NULL)),
              sigma = list(matrix(c(1, 0, 0, 1), 2,
                                  dimnames = list(c("B", "A"), c("B", "A")))))
class(gmm_2) <- "gmm"
gmm_3 <- list(alpha = 1, mu = matrix(0, dimnames = list("B", NULL)),
              sigma = list(matrix(1, dimnames = list("B", "B"))))
class(gmm_3) <- "gmm"
gmm_4 <- list(alpha = 1,
              mu = matrix(0, 2, dimnames = list(c("A", "A.1"), NULL)),
              sigma = list(matrix(c(1, 0, 0, 1), 2,
                                  dimnames = list(c("A", "A.1"),
                                                  c("A", "A.1")))))
class(gmm_4) <- "gmm"

gmbn_1 <- list(A = gmm_1, B = gmm_2)
class(gmbn_1) <- "gmbn"
gmbn_2 <- list(A = gmm_1, B = gmm_3)
class(gmbn_2) <- "gmbn"
gmbn_3 <- list(A = gmm_4, B = gmm_2)
class(gmbn_3) <- "gmbn"

gmdbn_1 <- list(b_1 = gmbn_1, b_2 = gmbn_3)
class(gmdbn_1) <- "gmdbn"

test_that("provide the graphical structure of a gmbn object", {
  expect_equal(structure(gmbn_1),
               list(nodes = c("A", "B"),
                    arcs = tibble(from = "A", to = "B", lag = 0)))
})

test_that("provide the graphical structure of a gmbn object with no arc", {
  expect_equal(structure(gmbn_2),
               list(nodes = c("A", "B"),
                    arcs = tibble(from = character(), to = character(),
                                  lag = integer())))
})

test_that("provide the graphical structure of a gmdbn object", {
  expect_equal(structure(gmdbn_1),
               list(nodes = c("A", "B"),
                    arcs = list(b_1 = tibble(from = "A", to = "B", lag = 0),
                                b_2 = tibble(from = c("A", "A"),
                                             to = c("A", "B"), lag = c(1, 0)))))
})
