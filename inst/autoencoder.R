library(Matrix)
library(abind)
library(keras)
library(tfprobability)

test_paths <- "~/opnames/thuis"
train_paths <- c(
  "~/opnames/kasteel_beersel", "~/opnames/provinciaal_domein_huizingen"
)

read_chunks <- function(
  file, chunk_size = 4, threshold = 10 / 60, overlap = 0, size
) {
  message(file)
  x <- readRDS(file)
  width <- chunk_size / attr(x, "resolution")[1]
  signal <- which(apply(x, 2, max) >= threshold * 255)
  start <- signal[c(1, which(diff(signal) > 1) + 1)]
  end <- signal[c(which(diff(signal) > 1), length(signal))]
  start <- unlist(lapply(
    which(start + width - 1 <= end),
    function(i) {
      seq(
        start[i],
        end[i] - width + 1,
        by = pmin(width, pmax(1, ceiling((1 - overlap) * width)))
      )
    }
  ))
  if (!missing(size) && size < length(start)) {
    start <- sample(start, size)
  }
  x <- t(x[, as.vector(outer(seq_len(width) - 1, start, "+")), ])
  z <- array(
    as.vector(x),
    dim = c(width, nrow(x) / width, ncol(x))
  )
  aperm(z, c(2, 1, 3))
}

rds2keras <- function(
  paths, batch_size = 10, chunk_size = 4, threshold = 10/60, overlap = 0, size
) {
  files <- lapply(paths, list.files, pattern = "_1dconv.rds$",
                  full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  z <- lapply(
    sample(unlist(files), batch_size),
    read_chunks,
    chunk_size = chunk_size, threshold = threshold, overlap = overlap,
    size = size
  )
  z <- abind(z, along = 1) / 255
  list(inputs = z, targets = z)
}

test <- rds2keras(paths = test_paths, batch_size = 1e3, overlap = 0.5)

input_shape <- dim(test$inputs)[-1]
encoded_size <- 2L
filters <- 10L
encoder_model <- keras_model_sequential() %>%
  layer_conv_1d(
    input_shape = input_shape,
    filters = filters, kernel_size = 2L, strides = 1L,
    padding = "same", activation = "relu"
  ) %>%
  layer_max_pooling_1d(pool_size = 2L) %>%
  layer_dropout(rate = 0.5) %>%
  layer_conv_1d(
    filters = filters, kernel_size = 2L, strides = 1L,
    padding = "same", activation = "relu"
  ) %>%
  layer_max_pooling_1d(pool_size = 2L) %>%
  layer_dropout(rate = 0.5) %>%
  layer_conv_1d(
    filters = filters, kernel_size = 2L, strides = 1L,
    padding = "same", activation = "relu"
  ) %>%
  layer_max_pooling_1d(pool_size = 2L) %>%
  layer_dropout(rate = 0.5) %>%
  layer_conv_1d(
    filters = filters, kernel_size = 2L, strides = 1L,
    padding = "same", activation = "relu"
  ) %>%
  layer_max_pooling_1d(pool_size = 2L) %>%
  layer_dropout(rate = 0.5) %>%
  layer_flatten() %>%
  layer_dense(units = params_size_multivariate_normal_tri_l(encoded_size)) %>%
  layer_multivariate_normal_tri_l(event_size = encoded_size) %>%
  # last layer adds KL divergence loss
  layer_kl_divergence_add_loss(
    distribution = tfd_independent(
      tfd_normal(loc = c(0, 0), scale = 1),
      reinterpreted_batch_ndims = 1
    )
  )

decoder_model <- keras_model_sequential() %>%
  layer_reshape(input_shape = encoded_size, target_shape = c(2, 1)) %>%
  layer_upsampling_1d(size = 2L) %>%
  layer_dropout(rate = 0.5) %>%
  layer_conv_1d(
    filters = filters, kernel_size = 2L, strides = 1L,
    padding = "same", activation = "relu"
  ) %>%
  layer_upsampling_1d(size = 2L) %>%
  layer_dropout(rate = 0.5) %>%
  layer_conv_1d(
    filters = filters, kernel_size = 2L, strides = 1L,
    padding = "same", activation = "relu"
  ) %>%
  layer_upsampling_1d(size = 2L) %>%
  layer_dropout(rate = 0.5) %>%
  layer_flatten() %>%
  layer_dense(params_size_independent_bernoulli(input_shape)) %>%
  layer_independent_bernoulli(
    event_shape = input_shape,
    convert_to_tensor_fn = tfp$distributions$Bernoulli$logits
  )
decoder_input <- layer_input(shape = encoded_size)

vae_loss <- function(x, rv_x) {
  -tfd_log_prob(rv_x, x)
}

vae_model <- keras_model(
  inputs = encoder_model$inputs,
  outputs = decoder_model(encoder_model$outputs[1])
) %>% compile(
  optimizer = "adam",
  loss = vae_loss
)

vae_model %>%
  fit_generator(
    generator = rds2keras(
      paths = train_paths, overlap = 0.1, size = 10
    ),
    batch_size = 100, epochs = 10, view_metrics = TRUE,
    validation_data = test
  )


predict(decoder_model, matrix(c(2, 2), ncol = 2))

library(ggplot2)

pred_encoder <- predict(encoder_model, train)
as.data.frame(pred_encoder) %>%
  ggplot(aes(x = V1, y = V2)) +
  geom_point(alpha = 0.1) +
  geom_density_2d() +
  coord_fixed()



i <- which.min(sqrt((pred_encoder[, 1] + 2) ^ 2 + (pred_encoder[, 2] + 0) ^ 2))
image(train[i, , 64:1], asp = 4)

pred_encoder %*% matrix(c(-2, ))


d


predict(generator, matrix(2, ncol = 2))
