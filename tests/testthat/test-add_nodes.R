gmm_1 <- list(alpha = 1, mu = matrix(0, dimnames = list("A", NULL)),
              sigma = list(matrix(1, dimnames = list("A", "A"))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = 1, mu = matrix(0, dimnames = list("B", NULL)),
              sigma = list(matrix(1, dimnames = list("B", "B"))))
class(gmm_2) <- "gmm"

gmbn_1 <- list(A = gmm_1)
class(gmbn_1) <- "gmbn"
gmbn_2 <- list(A = gmm_1, B = gmm_2)
class(gmbn_2) <- "gmbn"

gmdbn_1 <- list(b_1 = gmbn_1, b_2 = gmbn_1)
class(gmdbn_1) <- "gmdbn"
gmdbn_2 <- list(b_1 = gmbn_2, b_2 = gmbn_2)
class(gmdbn_2) <- "gmdbn"

test_that("add nodes to a gmbn object", {
  expect_equal(add_nodes(gmbn_1, "B"), gmbn_2)
})

test_that("define the nodes of a new gmbn object", {
  expect_equal(add_nodes(NULL, c("A", "B")), gmbn_2)
})

test_that("add duplicated nodes to a gmbn object", {
  expect_equal(add_nodes(gmbn_1, c("B", "B")), gmbn_2)
})

test_that("add existent nodes to a gmbn object", {
  expect_equal(add_nodes(gmbn_1, "A"), gmbn_1)
})

test_that("add no node to a gmbn object", {
  expect_equal(add_nodes(gmbn_1, NULL), gmbn_1)
  expect_equal(add_nodes(gmbn_1, character()), gmbn_1)
})

test_that("add nodes to a gmdbn object", {
  expect_equal(add_nodes(gmdbn_1, "B"), gmdbn_2)
})
