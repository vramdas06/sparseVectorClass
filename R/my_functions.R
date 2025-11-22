#' @importFrom methods show
#' @importFrom graphics plot points legend
#' @importFrom grDevices rgb
NULL

#' Sparse Numeric Class
#'
#' An S4 class that stores a sparse numeric vector using value/position
#' pairs.
#'
#' @slot value A numeric vector of non-zero values.
#' @slot pos An integer vector of positions corresponding to the values.
#' @slot length An integer giving the total logical length of the vector.
#'
#' @examples
#' x <- methods::new("sparse_numeric", value = c(1, 3), pos = c(1L, 4L), length = 5L)
#' y <- methods::new("sparse_numeric", value = c(2), pos = c(4L), length = 5L)
#' x + y
#'
#' @name sparse_numeric
#' @docType class
methods::setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)

### Validity Method ###

methods::setValidity("sparse_numeric", function(object) {
  if (length(object@value) != length(object@pos)) {
    return("Lengths of 'value' and 'pos' must be equal.")
  }
  if (any(object@pos < 1L) || any(object@pos > object@length)) {
    return("'pos' values must be between 1 and 'length'.")
  }
  if (anyDuplicated(object@pos)) {
    return("'pos' must not contain duplicates.")
  }
  if (!is.numeric(object@value)) {
    return("'value' must be numeric.")
  }
  if (!is.integer(object@length)) {
    return("'length' must be an integer")
  }
  TRUE
})

### Addition Method ###

#' Add Two Sparse Numeric Vectors
#'
#' Performs element-wise addition of two sparse numeric vectors of equal length.
#'
#' @param x A sparse_numeric object
#' @param y A sparse_numeric object
#' @param ... Additional arguments (currently unused)
#'
#' @return A sparse_numeric object containing the element-wise sum
#'
#' @examples
#' x <- as(c(1, 0, 3, 0, 5), "sparse_numeric")
#' y <- as(c(0, 2, 3, 0, 0), "sparse_numeric")
#' result <- sparse_add(x, y)
#' # Can also use: result <- x + y
#'
#' @export
methods::setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))

#' @rdname sparse_add
#' @export
methods::setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"),
  function(x, y) {
    # Error when x and y are different lengths
    if (x@length != y@length)
      stop("Vectors must have the same length.")

    # combine and sort all positions
    all_pos <- sort(unique(c(x@pos, y@pos)))
    if (length(all_pos) == 0) { # No non-zero elements in either x or y
      return(methods::new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
    }

    # get indices of all_pos where x is non 0
    ix <- match(all_pos, x@pos)
    # get indices of all_pos where y is non 0
    iy <- match(all_pos, y@pos)

    # get values where X and y are non-zero, otherwise fill with 0
    xvals <- x@value[ix]
    xvals[is.na(ix)] <- 0
    yvals <- y@value[iy]
    yvals[is.na(iy)] <- 0

    # Sum the shortened sparse vectors
    res_vals <- xvals + yvals
    keep <- res_vals != 0 # drop any 0s that are created
    if (!any(keep)) { # if all previously non-zero elements become 0, return an all 0 sparse vector
      return(methods::new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
    }

    # Return the resulting sparse vector
    methods::new("sparse_numeric",
        value = res_vals[keep],
        pos = as.integer(all_pos[keep]),
        length = x@length)
  })


### sparse_sub ###

#' Subtract Two Sparse Numeric Vectors
#'
#' Performs element-wise subtraction of two sparse numeric vectors of equal length.
#'
#' @param x A sparse_numeric object
#' @param y A sparse_numeric object
#' @param ... Additional arguments (currently unused)
#'
#' @return A sparse_numeric object containing the element-wise difference (x - y)
#'
#' @examples
#' x <- as(c(5, 0, 3, 0, 2), "sparse_numeric")
#' y <- as(c(1, 0, 3, 0, 0), "sparse_numeric")
#' result <- sparse_sub(x, y)
#' # Can also use: result <- x - y
#'
#' @export
methods::setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))

#' @rdname sparse_sub
#' @export
methods::setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            # Error when x and y are different lengths
            if (x@length != y@length)
              stop("Vectors must have the same length.")

            # combine and sort all positions
            all_pos <- sort(unique(c(x@pos, y@pos)))

            # create all 0 value vectors for x and y separately
            x_values <- rep(0, length(all_pos))
            y_values <- rep(0, length(all_pos))

            #fill in proper values where x and y are non-zero
            x_values[match(x@pos, all_pos)] <- x@value
            y_values[match(y@pos, all_pos)] <- y@value

            # subtract the vectors
            result_vals <- x_values - y_values
            keep <- result_vals != 0 # index of non-zeroes in resulting value vector

            # create the resulting vector
            methods::new("sparse_numeric",
                value = result_vals[keep],
                pos = as.integer(all_pos[keep]),
                length = x@length)
          })

### sparse_mult ###

#' Multiply Two Sparse Numeric Vectors Element-wise
#'
#' Performs element-wise multiplication of two sparse numeric vectors of equal length.
#'
#' @param x A sparse_numeric object
#' @param y A sparse_numeric object
#' @param ... Additional arguments (currently unused)
#'
#' @return A sparse_numeric object containing the element-wise product
#'
#' @examples
#' x <- as(c(2, 0, 3, 0, 5), "sparse_numeric")
#' y <- as(c(1, 2, 3, 0, 0), "sparse_numeric")
#' result <- sparse_mult(x, y)
#' # Can also use: result <- x * y
#'
#' @export
methods::setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))

#' @rdname sparse_mult
#' @export
methods::setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            # Error when x and y are different lengths
            if (x@length != y@length)
              stop("Vectors must have the same length.")

            # only find overlapping positions
            common_pos <- intersect(x@pos, y@pos)

            # all elements will be 0 if x and y have no common positions
            if (length(common_pos) == 0) {
              return(methods::new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
            }

            # make small vector with values at the common positions for x and y
            x_vals <- x@value[match(common_pos, x@pos)]
            y_vals <- y@value[match(common_pos, y@pos)]

            # multiply and keep non-zero results
            result_vals <- x_vals * y_vals
            keep <- result_vals != 0

            # create the resulting sparse vector
            methods::new("sparse_numeric",
                value = result_vals[keep],
                pos = as.integer(common_pos[keep]),
                length = x@length)
          })

### sparse_crossprod ##

#' Compute Cross Product of Two Sparse Numeric Vectors
#'
#' Computes the cross product of two sparse numeric vectors of equal length.
#'
#' @param x A sparse_numeric object
#' @param y A sparse_numeric object
#' @param ... Additional arguments (currently unused)
#'
#' @return A numeric scalar representing the cross product
#'
#' @examples
#' x <- as(c(1, 0, 3, 0, 5), "sparse_numeric")
#' y <- as(c(2, 0, 3, 0, 1), "sparse_numeric")
#' result <- sparse_crossprod(x, y)
#' # Result: 1*2 + 3*3 + 5*1 = 16
#'
#' @export
methods::setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

#' @rdname sparse_crossprod
#' @export
methods::setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            # Error when x and y are different lengths
            if (x@length != y@length)
              stop("Vectors must have the same length.")
            # only find overlapping positions
            common <- intersect(x@pos, y@pos)

            # sum the product of overlapping position values
            sum(x@value[match(common, x@pos)] * y@value[match(common, y@pos)])
          })

### Operator overloading ###
#' Add two sparse_numeric objects
#'
#' @param e1,e2 sparse_numeric objects
#' @return A sparse_numeric object
#'
#' @exportMethod +
methods::setMethod("+", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_add(e1, e2))

#' Subtract two sparse_numeric objects
#'
#' @param e1,e2 sparse_numeric objects
#' @return A sparse_numeric object
#'
#' @exportMethod -
methods::setMethod("-", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_sub(e1, e2))

#' Multiply two sparse_numeric objects
#'
#' @param e1,e2 sparse_numeric objects
#' @return A sparse_numeric object
#'
#' @exportMethod *
methods::setMethod("*", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_mult(e1, e2))


### Coercion Methods ###
#' Coerce numeric to sparse_numeric
#'
#' Converts a numeric vector to sparse format, storing only non-zero elements.
#'
#' @param from Numeric vector to convert.
#'
#' @return A \code{sparse_numeric} object.
#'
#' @examples
#' x <- c(0, 2.5, 0, 0, 3.7, 0, 1.2)
#' sparse_x <- as(x, "sparse_numeric")
#'
#' @name coerce-numeric-sparse_numeric

methods::setAs("numeric", "sparse_numeric", function(from) {
  nonzero_idx <- which(from != 0) # get indices of non-zeor elements
  methods::new("sparse_numeric",
      value = from[nonzero_idx],
      pos = as.integer(nonzero_idx),
      length = as.integer(length(from)))
})

#' Coerce sparse_numeric to numeric
#'
#' Converts a sparse_numeric object back to a standard numeric vector.
#'
#' @param from A \code{sparse_numeric} object to convert.
#'
#' @return A numeric vector with zeros in positions that were not stored.
#'
#' @examples
#' x <- c(0, 2.5, 0, 0, 3.7, 0, 1.2)
#' sparse_x <- as(x, "sparse_numeric")
#' y <- as(sparse_x, "numeric")
#'
#' @name coerce-sparse_numeric-numeric

methods::setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length) # create numeric vector of length l
  out[from@pos] <- from@value # replace non-zero elements with values
  out
})

### Show method ###

#' Show Method for Sparse Numeric Vectors
#'
#' Displays a sparse numeric vector in a readable format, showing the total length
#' and a data frame of non-zero positions and their values.
#'
#' @param object A sparse_numeric object
#'
#' @return Invisibly returns the object; called for side effects (printing)
#'
#' @examples
#' x <- as(c(1, 0, 3, 0, 5), "sparse_numeric")
#' methods::show(x)
#' # Or simply: x
#'
#' @rdname show-sparse_numeric
#' @export
methods::setMethod("show", "sparse_numeric", function(object) {
  cat("Sparse numeric vector of length", object@length, "\n")
  cat("Non-zero elements:\n")
  print(data.frame(pos = object@pos, value = object@value))
})

### Plot Method ###

#' Plot Two Sparse Numeric Vectors
#'
#' Creates a scatter plot showing the overlapping non-zero elements of two sparse
#' numeric vectors. The x-axis represents position, and the y-axis represents value.
#'
#' @param x A sparse_numeric object
#' @param y A sparse_numeric object
#' @param ... Additional graphical parameters passed to plot()
#'
#' @return returns NULL, but creates a plot
#'
#' @examples
#' x <- as(c(1, 0, 3, 4, 5, 0, 7), "sparse_numeric")
#' y <- as(c(2, 0, 3, 0, 6, 0, 8), "sparse_numeric")
#' plot(x, y)
#'
#' @export
methods::setMethod("plot", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
  if (x@length != y@length)
    stop("Vectors must have the same length.")

  # Find overlapping nonzero positions
  common_pos <- intersect(x@pos, y@pos)

  # when there are no overlapping positions
  if (length(common_pos) == 0) {
    plot(0, 0, type = "n", xlab = "Position", ylab = "Value",
         main = "No overlapping non-zero elements")
    return(invisible(NULL))
  }

  # get values for overlapping positions
  ix <- match(common_pos, x@pos)
  iy <- match(common_pos, y@pos)
  xvals <- x@value[ix]
  yvals <- y@value[iy]

  # Plot setup
  plot(range(common_pos), range(c(xvals, yvals)),
       type = "n",
       xlab = "Position",
       ylab = "Value",
       main = "Overlapping Non-Zero Elements by Position")

  # Add x and y points (different color, size, and shape + transparency to see overlap better)
  points(common_pos, xvals, pch = 17, col = rgb(0, 0, 1, 0.5), cex = 1.5)
  points(common_pos, yvals, pch = 16, col = rgb(1, 0, 0, 0.5))

  # Add legend
  legend("topright",
         legend = c("x values", "y values"),
         col = c(rgb(0, 0, 1, 0.6), rgb(1, 0, 0, 0.6)),
         pch = c(17, 16),
         bty = "n")
})

### mean() ###

#' Calculate Mean of Sparse Numeric Vector
#'
#' Computes the mean of a sparse numeric vector, accounting for all elements
#' including zeros.
#'
#' @param x A sparse_numeric object
#' @param ... Additional arguments (currently unused)
#'
#' @return A numeric scalar representing the mean
#'
#' @examples
#' x <- as(c(2, 0, 6, 0, 0), "sparse_numeric")
#' mean(x)
#' # Result: (2 + 0 + 6 + 0 + 0) / 5 = 1.6
#'
#' @rdname sparse_numeric-methods
#' @export
methods::setMethod("mean", "sparse_numeric",
  function(x, ...) {
  # take mean including 0
  sum(x@value) / x@length
})

### norm() ###

#' Calculate Norm of Sparse Numeric Vector
#'
#' Computes the squared norm of the vector, which is the square root of the sum of the squared individual elements of a vector.
#'
#' @param x A sparse_numeric object
#'
#' @return A numeric scalar representing the Euclidean norm
#'
#' @examples
#' x <- as(c(3, 0, 4, 0, 0), "sparse_numeric")
#' norm(x)
#' # Result: sqrt(3^2 + 4^2) = 5
#'
#' @export
methods::setGeneric("norm", function(x) standardGeneric("norm"))

#' @rdname norm
#' @export
methods::setMethod("norm", "sparse_numeric",
          function(x) {
            # squared sum of values
            sum_sq <- sum(x@value^2)
            # take square root of sum
            sqrt(sum_sq)
          }
)

### standardize() ###

#' Standardize a Sparse Numeric Vector
#'
#' Standardizes a sparse numeric vector by subtracting the mean and dividing by the
#' standard deviation The result is returned as a sparse_numeric
#' object.
#'
#' @param x A sparse_numeric object
#'
#' @return A sparse_numeric object containing the standardized values
#'
#' @examples
#' x <- as(c(1, 0, 3, 0, 5, 0, 0), "sparse_numeric")
#' x_std <- standardize(x)
#' # Result has mean ≈ 0 and standard deviation = 1
#'
#' @export
methods::setGeneric("standardize", function(x) standardGeneric("standardize"))

#' @rdname standardize
#' @export
methods::setMethod("standardize", "sparse_numeric",
  function(x) {

    # collect values for calculating variance
    n <- x@length
    vals <- x@value
    k <- length(vals)

    # calculate mean
    mu <- sum(vals) / n

    # calculate variance (adding numerator term for 0s also)
    var_x <- (sum((vals - mu)^2) + (n - k) * (mu^2)) / n

    if (var_x == 0) {
      stop("Cannot standardize a vector with 0 standard deviation")
    }

    sd_x <- sqrt(var_x)

    # Standardize the non-zero entries
    new_vals <- (vals - mu) / sd_x

    # Calc the standardized value for 0s
    zero_standardized <- (-mu) / sd_x

    # put new standardized value in sparse vector if zero_standardized isn't 0
    new_pos <- x@pos
    if (zero_standardized != 0) {
      zero_positions <- setdiff(seq_len(n), x@pos)
      new_vals <- c(new_vals, rep(zero_standardized, length(zero_positions)))
      new_pos  <- c(new_pos, zero_positions)
    }

    # Remove any resulting zeros to maintain sparsity
    keep <- new_vals != 0
    new_vals <- new_vals[keep]
    new_pos  <- new_pos[keep]

    # reorder to put positions in order
    o <- order(new_pos)
    new_vals <- new_vals[o]
    new_pos  <- new_pos[o]

    methods::new("sparse_numeric",
        value = new_vals[keep],
        pos   = as.integer(new_pos[keep]),
        length = n)
    }
  )
