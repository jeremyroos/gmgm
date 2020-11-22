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
              mu = matrix(0, 4,
                          dimnames = list(c("B", "A", "A.1", "B.1"), NULL)),
              sigma = list(matrix(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                                    1),
                                  4,
                                  dimnames = list(c("B", "A", "A.1", "B.1"),
                                                  c("B", "A", "A.1", "B.1")))))
class(gmm_4) <- "gmm"
gmm_5 <- list(alpha = 1,
              mu = matrix(0, 2, dimnames = list(c("A", "A.1"), NULL)),
              sigma = list(matrix(c(1, 0, 0, 1), 2,
                                  dimnames = list(c("A", "A.1"),
                                                  c("A", "A.1")))))
class(gmm_5) <- "gmm"
gmm_6 <- list(alpha = 1,
              mu = matrix(0, 3, dimnames = list(c("B", "A", "A.1"), NULL)),
              sigma = list(matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), 3,
                                  dimnames = list(c("B", "A", "A.1"),
                                                  c("B", "A", "A.1")))))
class(gmm_6) <- "gmm"

gmbn_1 <- list(A = gmm_1, B = gmm_2)
class(gmbn_1) <- "gmbn"
gmbn_2 <- list(A = gmm_1, B = gmm_3)
class(gmbn_2) <- "gmbn"
gmbn_3 <- list(A = gmm_1, B = gmm_4)
class(gmbn_3) <- "gmbn"
gmbn_4 <- list(A = gmm_5, B = gmm_6)
class(gmbn_4) <- "gmbn"
gmbn_5 <- list(A = gmm_5, B = gmm_2)
class(gmbn_5) <- "gmbn"

gmdbn_1 <- list(b_1 = gmbn_1, b_2 = gmbn_5)
class(gmdbn_1) <- "gmdbn"
gmdbn_2 <- list(b_1 = gmbn_2, b_2 = gmbn_2)
class(gmdbn_2) <- "gmdbn"
gmdbn_3 <- list(b_1 = gmbn_1, b_2 = gmbn_1)
class(gmdbn_3) <- "gmdbn"
gmdbn_4 <- list(b_1 = gmbn_1, b_2 = gmbn_2)
class(gmdbn_4) <- "gmdbn"

test_that("remove arcs from a gmbn object", {
  expect_equal(remove_arcs(gmbn_1, data.frame(from = "A", to = "B", lag = 0)),
               gmbn_2)
})

test_that("remove arcs from a gmbn object with missing column from", {
  expect_equal(remove_arcs(gmbn_3, data.frame(to = c("B", "B"), lag = c(0, 1))),
               gmbn_2)
  expect_equal(remove_arcs(gmbn_3,
                           data.frame(from = c(NA, NA), to = c("B", "B"),
                                      lag = c(0, 1))),
               gmbn_2)
})

test_that("remove arcs from a gmbn object with missing column to", {
  expect_equal(remove_arcs(gmbn_4,
                           data.frame(from = c("A", "A"), lag = c(0, 1))),
               gmbn_2)
  expect_equal(remove_arcs(gmbn_4,
                           data.frame(from = c("A", "A"), to = c(NA, NA),
                                      lag = c(0, 1))),
               gmbn_2)
})

test_that("remove arcs from a gmbn object with missing column lag", {
  expect_equal(remove_arcs(gmbn_1, data.frame(from = "A", to = "B")), gmbn_2)
  expect_equal(remove_arcs(gmbn_1, data.frame(from = "A", to = "B", lag = NA)),
               gmbn_2)
})

test_that("remove duplicated arcs from a gmbn object", {
  expect_equal(remove_arcs(gmbn_1,
                           data.frame(from = c("A", "A"), to = c("B", "B"),
                                      lag = c(0, 0))),
               gmbn_2)
})

test_that("remove non-existent arcs from a gmbn object", {
  expect_equal(remove_arcs(gmbn_2, data.frame(from = "A", to = "B", lag = 0)),
               gmbn_2)
})

test_that("remove no arc from a gmbn object", {
  expect_equal(remove_arcs(gmbn_1, NULL), gmbn_1)
  expect_equal(remove_arcs(gmbn_1,
                           data.frame(from = character(), to = character(),
                                      lag = numeric())),
               gmbn_1)
  expect_equal(remove_arcs(gmbn_1,
                           data.frame(from = logical(), to = logical(),
                                      lag = logical())),
               gmbn_1)
})

test_that("remove arcs from a gmdbn object", {
  expect_equal(remove_arcs(gmdbn_1,
                           data.frame(from = c("A", "A"), to = c("A", "B"),
                                      lag = c(1, 0))),
               gmdbn_2)
})

test_that("remove arcs from gmbn elements of a gmdbn object", {
  expect_equal(remove_arcs(gmdbn_3,
                           list(b_2 = data.frame(from = "A", to = "B",
                                                 lag = 0))),
               gmdbn_4)
})
