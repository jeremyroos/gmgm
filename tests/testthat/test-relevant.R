gmm_1 <- list(alpha = 1, mu = matrix(0, dimnames = list("A", NULL)),
              sigma = list(matrix(1, dimnames = list("A", "A"))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = 1, mu = matrix(0, 2, dimnames = list(c("B", "A"), NULL)),
              sigma = list(matrix(c(1, 0, 0, 1), 2,
                                  dimnames = list(c("B", "A"), c("B", "A")))))
class(gmm_2) <- "gmm"
gmm_3 <- list(alpha = 1, mu = matrix(0, 2, dimnames = list(c("C", "B"), NULL)),
              sigma = list(matrix(c(1, 0, 0, 1), 2,
                                  dimnames = list(c("C", "B"), c("C", "B")))))
class(gmm_3) <- "gmm"
gmm_4 <- list(alpha = 1, mu = matrix(0, 2, dimnames = list(c("D", "C"), NULL)),
              sigma = list(matrix(c(1, 0, 0, 1), 2,
                                  dimnames = list(c("D", "C"), c("D", "C")))))
class(gmm_4) <- "gmm"
gmm_5 <- list(alpha = 1, mu = matrix(0, dimnames = list("B", NULL)),
              sigma = list(matrix(1, dimnames = list("B", "B"))))
class(gmm_5) <- "gmm"

gmbn_1 <- list(A = gmm_1, B = gmm_2, C = gmm_3, D = gmm_4)
class(gmbn_1) <- "gmbn"
gmbn_2 <- list(B = gmm_5, C = gmm_3, D = gmm_4)
class(gmbn_2) <- "gmbn"
gmbn_3 <- list(A = gmm_1, B = gmm_2, C = gmm_3)
class(gmbn_3) <- "gmbn"
gmbn_4 <- list(B = gmm_5, C = gmm_3)
class(gmbn_4) <- "gmbn"

gmdbn_1 <- list(b_1 = gmbn_1, b_2 = gmbn_1)
class(gmdbn_1) <- "gmdbn"
gmdbn_2 <- list(b_1 = gmbn_4, b_2 = gmbn_4)
class(gmdbn_2) <- "gmdbn"

test_that("extract a relevant gmbn object with observed nodes", {
  expect_equal(relevant(gmbn_1, "C", nodes_obs = c("A", "B")), gmbn_2)
})

test_that("extract a relevant gmbn object with duplicated observed nodes", {
  expect_equal(relevant(gmbn_1, "C", nodes_obs = c("B", "B")), gmbn_2)
})

test_that("extract a relevant gmbn object with missing nodes", {
  expect_equal(relevant(gmbn_1, "C", nodes_miss = c("C", "D")), gmbn_3)
})

test_that("extract a relevant gmbn object with duplicated missing nodes", {
  expect_equal(relevant(gmbn_1, "C", nodes_miss = c("D", "D")), gmbn_3)
})

test_that("extract a relevant gmbn object with no observed or missing node", {
  expect_equal(relevant(gmbn_1, "C"), gmbn_1)
  expect_equal(relevant(gmbn_1, "C", nodes_obs = character(),
                        nodes_miss = character()),
               gmbn_1)
})

test_that("extract a relevant gmdbn object with observed and missing nodes", {
  expect_equal(relevant(gmdbn_1, "C", nodes_obs = "B", nodes_miss = "D"),
               gmdbn_2)
})
