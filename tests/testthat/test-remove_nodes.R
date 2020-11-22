gmm_1 <- list(alpha = 1, mu = matrix(0, dimnames = list("A", NULL)),
              sigma = list(matrix(1, dimnames = list("A", "A"))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = 1, mu = matrix(0, 2, dimnames = list(c("B", "A"), NULL)),
              sigma = list(matrix(c(1, 0, 0, 1), 2,
                                  dimnames = list(c("B", "A"), c("B", "A")))))
class(gmm_2) <- "gmm"
gmm_3 <- list(alpha = 1, mu = matrix(0, dimnames = list("C", NULL)),
              sigma = list(matrix(1, dimnames = list("C", "C"))))
class(gmm_3) <- "gmm"
gmm_4 <- list(alpha = 1, mu = matrix(0, dimnames = list("B", NULL)),
              sigma = list(matrix(1, dimnames = list("B", "B"))))
class(gmm_4) <- "gmm"

gmbn_1 <- list(A = gmm_1, B = gmm_2, C = gmm_3)
class(gmbn_1) <- "gmbn"
gmbn_2 <- list(B = gmm_4)
class(gmbn_2) <- "gmbn"
gmbn_3 <- list(A = gmm_1, B = gmm_2)
class(gmbn_3) <- "gmbn"

gmdbn_1 <- list(b_1 = gmbn_3, b_2 = gmbn_3)
class(gmdbn_1) <- "gmdbn"
gmdbn_2 <- list(b_1 = gmbn_2, b_2 = gmbn_2)
class(gmdbn_2) <- "gmdbn"

test_that("remove nodes from a gmbn object", {
  expect_equal(remove_nodes(gmbn_1, c("A", "C")), gmbn_2)
})

test_that("remove duplicated nodes from a gmbn object", {
  expect_equal(remove_nodes(gmbn_3, c("A", "A")), gmbn_2)
})

test_that("remove non-existent nodes from a gmbn object", {
  expect_equal(remove_nodes(gmbn_3, "C"), gmbn_3)
})

test_that("remove no node from a gmbn object", {
  expect_equal(remove_nodes(gmbn_3, NULL), gmbn_3)
  expect_equal(remove_nodes(gmbn_3, character()), gmbn_3)
})

test_that("remove nodes from a gmdbn object", {
  expect_equal(remove_nodes(gmdbn_1, "A"), gmdbn_2)
})
