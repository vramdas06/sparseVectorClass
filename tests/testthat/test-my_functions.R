test_that("check validity method exists", {
  expect_false({
    validity_method <- getValidity(getClassDef("sparse_numeric"))
    is.null(validity_method)
  })
})

test_that("check validity method", {
  expect_true({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    validObject(x)
  })
})

test_that("check validity method 2", {
  expect_error({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    x@length <- 2L
    validObject(x)
  })
})

test_that("check coercion return class", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  }, "sparse_numeric")
})

test_that("check for show method", {
  expect_no_error({
    getMethod("show", "sparse_numeric")
  })
})

test_that("check for plot method", {
  expect_no_error({
    getMethod("plot", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for + method", {
  expect_no_error({
    getMethod("+", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for - method", {
  expect_no_error({
    getMethod("-", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for * method", {
  expect_no_error({
    getMethod("*", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("sparse add generic", {expect_true(isGeneric("sparse_add"))})
test_that("sparse mult generic", {expect_true(isGeneric("sparse_mult"))})
test_that("sparse sub generic", {expect_true(isGeneric("sparse_sub"))})
test_that("sparse crossprod generic", {expect_true(isGeneric("sparse_crossprod"))})

test_that("sparse add formals", {
  expect_true(length(formals(sparse_add)) >= 2L)
})

test_that("sparse mult formals", {
  expect_true(length(formals(sparse_mult)) >= 2L)
})

test_that("sparse sub formals", {
  expect_true(length(formals(sparse_sub)) >= 2L)
})

test_that("sparse crossprod formals", {
  expect_true(length(formals(sparse_crossprod)) >= 2L)
})

test_that("check returned class for add", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, "sparse_numeric")
})

test_that("sparse_add", {
  result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("sparse add dense", {
  result <- as(c(2, 4, 6, 10, 12), "sparse_numeric")
  expect_equal({
    x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("all zero wrong length", {
  expect_error({
    x <- as(rep(0, 10), "sparse_numeric")
    y <- as(rep(0, 9), "sparse_numeric")
    sparse_add(x, y)
  })
})

test_that("check returned class for sub", {
  expect_s4_class({
    x <- as(c(5, 0, 3, 0, 2), "sparse_numeric")
    y <- as(c(1, 0, 3, 0, 0), "sparse_numeric")
    sparse_sub(x, y)
  }, "sparse_numeric")
})

test_that("sparse_sub", {
  result <- as(c(4, 0, 0, 0, 2), "sparse_numeric")
  expect_equal({
    x <- as(c(5, 0, 3, 0, 2), "sparse_numeric")
    y <- as(c(1, 0, 3, 0, 0), "sparse_numeric")
    sparse_sub(x, y)
  }, result)
})

test_that("sparse_sub wrong length", {
  expect_error({
    x <- as(c(1, 0, 3), "sparse_numeric")
    y <- as(c(1, 0), "sparse_numeric")
    sparse_sub(x, y)
  })
})

test_that("check returned class for mult", {
  expect_s4_class({
    x <- as(c(2, 0, 3, 0, 5), "sparse_numeric")
    y <- as(c(1, 2, 3, 0, 0), "sparse_numeric")
    sparse_mult(x, y)
  }, "sparse_numeric")
})

test_that("sparse_mult", {
  result <- as(c(2, 0, 9, 0, 0), "sparse_numeric")
  expect_equal({
    x <- as(c(2, 0, 3, 0, 5), "sparse_numeric")
    y <- as(c(1, 2, 3, 0, 0), "sparse_numeric")
    sparse_mult(x, y)
  }, result)
})

test_that("sparse_mult wrong length", {
  expect_error({
    x <- as(c(1, 0, 3), "sparse_numeric")
    y <- as(c(1, 0), "sparse_numeric")
    sparse_mult(x, y)
  })
})

test_that("sparse_crossprod", {
  expect_equal({
    x <- as(c(1, 0, 3, 0, 5), "sparse_numeric")
    y <- as(c(2, 0, 3, 0, 1), "sparse_numeric")
    sparse_crossprod(x, y)
  }, 16)
})

test_that("sparse_crossprod wrong length", {
  expect_error({
    x <- as(c(1, 0, 3), "sparse_numeric")
    y <- as(c(1, 0), "sparse_numeric")
    sparse_crossprod(x, y)
  })
})

test_that("coercion numeric to sparse", {
  expect_equal({
    sparse <- as(c(1, 0, 3, 0, 5), "sparse_numeric")
    list(sparse@value, sparse@pos, sparse@length)
  }, list(c(1, 3, 5), c(1L, 3L, 5L), 5L))
})

test_that("coercion numeric to sparse all zeros", {
  expect_equal({
    sparse <- as(c(0, 0, 0), "sparse_numeric")
    list(sparse@value, sparse@pos, sparse@length)
  }, list(numeric(0), integer(0), 3L))
})

test_that("coercion sparse to numeric", {
  expect_equal({
    sparse <- new("sparse_numeric",
                  value = c(1, 3, 5),
                  pos = c(1L, 3L, 5L),
                  length = 5L)
    as(sparse, "numeric")
  }, c(1, 0, 3, 0, 5))
})

test_that("coercion sparse to numeric all zeros", {
  expect_equal({
    sparse <- new("sparse_numeric",
                  value = numeric(0),
                  pos = integer(0),
                  length = 3L)
    as(sparse, "numeric")
  }, c(0, 0, 0))
})

test_that("check for show method", {
  expect_no_error({
    getMethod("show", "sparse_numeric")
  })
})

test_that("show method prints", {
  expect_output({
    x <- as(c(1, 0, 3, 0, 5), "sparse_numeric")
    show(x)
  }, "Sparse numeric vector of length 5")
})

test_that("check for plot method", {
  expect_no_error({
    getMethod("plot", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("plot with overlap", {
  expect_no_error({
    x <- as(c(1, 0, 3, 4, 5, 0, 7), "sparse_numeric")
    y <- as(c(2, 0, 3, 0, 6, 0, 8), "sparse_numeric")
    plot(x, y)
  })
})

test_that("plot no overlap", {
  expect_no_error({
    x <- as(c(1, 0, 0, 4), "sparse_numeric")
    y <- as(c(0, 2, 3, 0), "sparse_numeric")
    plot(x, y)
  })
})

test_that("plot wrong length", {
  expect_error({
    x <- as(c(1, 0, 3), "sparse_numeric")
    y <- as(c(1, 0), "sparse_numeric")
    plot(x, y)
  })
})

test_that("mean calculation", {
  expect_equal({
    x <- as(c(2, 0, 6, 0, 0), "sparse_numeric")
    mean(x)
  }, 1.6)
})

test_that("mean all zeros", {
  expect_equal({
    x <- as(c(0, 0, 0), "sparse_numeric")
    mean(x)
  }, 0)
})

test_that("norm calculation", {
  expect_equal({
    x <- as(c(3, 0, 4, 0, 0), "sparse_numeric")
    norm(x)
  }, 5)
})

test_that("check for standardize method", {
  expect_no_error({
    getMethod("standardize", "sparse_numeric")
  })
})

test_that("check returned class for standardize", {
  expect_s4_class({
    x <- as(c(1, 0, 3, 0, 5, 0, 0), "sparse_numeric")
    standardize(x)
  }, "sparse_numeric")
})

test_that("standardize mean zero", {
  expect_equal({
    x <- as(c(1, 0, 3, 0, 5, 0, 0), "sparse_numeric")
    x_std <- standardize(x)
    x_dense <- as(x_std, "numeric")
    mean(x_dense)
  }, 0, tolerance = 1e-10)
})

test_that("standardize zero variance error", {
  expect_error({
    x <- as(c(5, 5, 5), "sparse_numeric")
    standardize(x)
  })
})
