#' Reconstruct pulses
#' @param x the object to reconstruct the pulse
#' @param ... further arguments used by some methods
#' @export
reconstruct <- function(x, ...) {
  UseMethod("reconstruct", x)
}

#' @rdname reconstruct
#' @export
#' @importFrom raster raster resample
reconstruct.list <- function(x, ...) {
  pulse <- raster(
    x$shape[[1]] * (x$peak_amplitude - x$start_amplitude) + x$start_amplitude,
    xmn = x$start_time, xmx = x$end_time,
    ymn = x$start_frequency, ymx = x$end_frequency
  )
  target <- raster(
    ncols = 4 * (ceiling(x$end_time) - floor(x$start_time)),
    nrows = 4 * (ceiling(x$end_frequency) - floor(x$start_frequency)),
    xmn = floor(x$start_time), xmx = ceiling(x$end_time),
    ymn = floor(x$start_frequency), ymx = ceiling(x$end_frequency),
    crs = NA
  )
  resample(pulse, target)
}

#' @rdname reconstruct
#' @export
#' @importFrom assertthat assert_that
reconstruct.data.frame <- function(x, ...) {
  assert_that(
    has_name(x, "peak_amplitude"),
    has_name(x, "start_amplitude"),
    has_name(x, "start_time"),
    has_name(x, "end_time"),
    has_name(x, "start_frequency"),
    has_name(x, "end_frequency"),
    has_name(x, "shape")
  )
  pulse <- lapply(
    seq_len(nrow(x)),
    function(i) {
      reconstruct(as.list(x[i, ]))
    }
  )
  if (has_name(x, "fingerprint")) {
    names(pulse) <- x$fingerprint
  }
  return(pulse)
}

#' @rdname reconstruct
#' @export
#' @importFrom methods validObject
#' @importFrom assertthat assert_that is.string
#' @param spectrogram the fingerprint of the selected spectrogram
reconstruct.soundPulse <- function(x, ..., spectrogram) {
  validObject(x)
  assert_that(is.string(spectrogram))
  relevant <- x@Pulse[x@Pulse$spectrogram == spectrogram, ]
  start_time <- floor(min(relevant$start_time))
  end_time <- ceiling(max(relevant$end_time))
  start_frequency <- floor(min(relevant$start_frequency))
  end_frequency <- ceiling(max(relevant$end_frequency))
  pulse <- reconstruct(relevant)
  while (length(pulse) > 1) {
    half <- floor(length(pulse) / 2)
    for (i in seq_len(half)) {
      pulse[[i]] <- merge(pulse[[i]], pulse[[half + 1]])
      pulse[[half + 1]] <- NULL
    }
  }
  pulse[[1]]
}


#' @rdname reconstruct
#' @export
#' @importFrom methods validObject
reconstruct.soundPyramide <- function(x, ...) {
  validObject(x)
  cols <- colnames(x@Pyramid)
  rescaled <- t(x@Pyramid) * x@Scaling[cols, "sd"] + x@Scaling[cols, "center"]
  pulse <- data.frame(
    peak_amplitude = rescaled["peak_amplitude", ],
    start_amplitude = rescaled["peak_amplitude", ] -
      rescaled["amplitude_range", ],
    start_time = 0,
    end_time = rescaled["duration", ],
    start_frequency = rescaled["peak_frequency", ] -
      rescaled["start_frequency", ] * rescaled["frequency_range", ],
    end_frequency = rescaled["peak_frequency", ] +
      (1 - rescaled["start_frequency", ]) * rescaled["frequency_range", ]
  )
  relevant <- t(rescaled[grep("^P[0-3]*$", cols), ])
  pulse$shape <- lapply(
    seq_len(nrow(relevant)),
    function(i) {
      pyramid2shape(relevant[i, ])
    }
  )
  reconstruct(pulse)
}

pyramid2shape <- function(z) {
  grand_mean <- z[grep("^P[0]*$", names(z))]
  s0 <- z[grep("^P[0-3]*0$", names(z))]
  s1 <- z[grep("^P[0-3]*1$", names(z))]
  s2 <- z[grep("^P[0-3]*2$", names(z))]
  s3 <- z[grep("^P[0-3]*3$", names(z))]
  names(s0) <- substr(names(s0), 1, nchar(names(s0)) - 1)
  names(s1) <- substr(names(s0), 1, nchar(names(s1)) - 1)
  names(s2) <- substr(names(s0), 1, nchar(names(s2)) - 1)
  names(s3) <- substr(names(s0), 1, nchar(names(s3)) - 1)
  s1[1] <- grand_mean + s1[1]
  s2[1] <- grand_mean + s2[1]
  s3[1] <- grand_mean + s3[1]
  s0[1] <- 4 * grand_mean - s1[1] - s2[1] - s3[1]
  if (length(s0) > 1) {
    cbind(
      rbind(pyramid2shape(s0), pyramid2shape(s1)),
      rbind(pyramid2shape(s2), pyramid2shape(s3))
    )
  } else {
    matrix(c(s0, s1, s2, s3), ncol = 2)
  }
}

#' @rdname reconstruct
#' @export
#' @importFrom methods validObject
reconstruct.soundCluster <- function(x, ...) {
  validObject(x)
  z <- x@Network$codes[[1]]
  cols <- colnames(z)
  rescaled <- t(z) * x@Scaling[cols, "sd"] + x@Scaling[cols, "center"]
  pulse <- data.frame(
    peak_amplitude = rescaled["peak_amplitude", ],
    start_amplitude = rescaled["peak_amplitude", ] -
      rescaled["amplitude_range", ],
    start_time = 0,
    end_time = rescaled["duration", ],
    start_frequency = rescaled["peak_frequency", ] -
      rescaled["start_frequency", ] * rescaled["frequency_range", ],
    end_frequency = rescaled["peak_frequency", ] +
      (1 - rescaled["start_frequency", ]) * rescaled["frequency_range", ]
  )
  relevant <- t(rescaled[grep("^P[0-3]*$", cols), ])
  pulse$shape <- lapply(
    seq_len(nrow(relevant)),
    function(i) {
      pyramid2shape(relevant[i, ])
    }
  )
  reconstruct(pulse)
}
