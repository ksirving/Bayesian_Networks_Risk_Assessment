compute_ci <- function(mat_U, mat_F, sample_size, FUN, ...,
                       n_sim = 1000, dist.out = FALSE) {
  # Validation of inputs
  # Check input matrices
  if (!is.matrix(mat_U) || !is.matrix(mat_F)) {
    stop("mat_U and mat_F must be matrices.")
  }
  if (!identical(dim(mat_U), dim(mat_F))) {
    stop("mat_U and mat_F must have the same dimensions.")
  }
  
  if (!dim(mat_F)[1] == dim(mat_F)[2]) {
    stop("mat_F must be a square matrix.")
  }
  if (!dim(mat_U)[1] == dim(mat_U)[2]) {
    stop("mat_U must be square matrix.")
  }
  
  # Sample size validation
  if (!(inherits(sample_size, "list") || inherits(sample_size, "matrix") ||
        length(sample_size) == 1)) {
    stop("sample_size needs to be a matrix, a list of two matrices,
         or an integer with length 1")
  }
  
  # When sample_size is a single matrix.
  if (inherits(sample_size, "matrix")) {
    if (nrow(sample_size) != nrow(mat_U)) {
      stop("if sample_size is a matrix,
           it should be the same dimension as mat_U")
    }
  }
  
  # When sample_size is a list of two matrices.
  if (inherits(sample_size, "list")) {
    if (!identical(
      lapply(sample_size, dim)[[1]],
      lapply(sample_size, dim)[[2]]
    )) {
      stop("if sample_size is a list of matrices,
           they should both be the same dimensions.")
    }
    if (!identical(lapply(sample_size, dim)[[1]], dim(mat_U))) {
      stop("if sample_size is a list of matrices,
           they should be the same dimension as mat_U")
    }
    if (!sum(names(sample_size) %in% c("mat_F_ss", "mat_U_ss")) == 2) {
      stop("if sample_size is a list of matrices,
           the names of the list entries need to be named
           'mat_F_ss' and 'mat_U_ss'")
    }
  }
  
  unlisted_sample_size <- unlist(sample_size)
  
  if (!min(abs(c(unlisted_sample_size %% 1, unlisted_sample_size %% 1 - 1))) <
      .Machine$double.eps^0.5) {
    stop("sample_size must be integer value(s)")
  }
  
  if (min(unlisted_sample_size) < 0) {
    stop("sample_size must be >= 0.")
  }
  
  # Check FUN argument
  if (!is.function(FUN)) {
    stop("FUN must be a function.")
  }
  
  # replicate the simulation of MPMs
  sim_out <- replicate(n_sim, add_mpm_error(mat_U,
                                            mat_F,
                                            sample_size,
                                            split = FALSE,
                                            by_type = FALSE
  ),
  simplify = FALSE
  )
  
  # apply the function FUN to each matrix
  estimates <- sapply(sim_out, FUN, ...)
  
  # Check the estimates and use warnings if necessary
  if (any(is.infinite(estimates))) {
    warning("Some estimates are Inf. \n
            Try running with argument `dist.out = TRUE`
            and examine the estimates.")
  }
  
  emp_quantiles <- quantile(estimates, c(0.025, 0.975), na.rm = TRUE)
  
  if (dist.out == FALSE) {
    return(emp_quantiles)
  }
  if (dist.out == TRUE) {
    out <- list(
      "quantiles" = emp_quantiles, "estimates" = estimates,
      "matrices" = sim_out
    )
    return(out)
  }
}
