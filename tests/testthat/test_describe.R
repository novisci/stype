testthat::context("describe functions")

test_that(
  "grouped summaries work",
  {
    skip_on_ci() # until smd package installed in r/pkgdev
    x1 <- c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
    g  <- factor(rep(LETTERS[1:2], each = 3))
    desc <- describe(x1, g = g)
    expect_true("smd" %in% names(desc))
    
  }
)

test_that(
  "grouped AND weighted summaries work",
  {   
    skip_on_ci() # until smd package installed in r/pkgdev
    x1 <- c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
    g  <- factor(rep(LETTERS[1:2], each = 3))
    w  <- rep(1:2, each = 3)
    desc <- describe(x1, g = g, w = w)
    expect_true(all(c("smd", "weighted_proportion") %in% names(desc)))
    
    
  }
)