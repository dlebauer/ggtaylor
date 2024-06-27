# Tests for geom_taylor and coord_taylor
test_that("geom_taylor creates the expected layer", {
  obs <- rnorm(100)
  mod1 <- obs + rnorm(100, sd = 0.5)
  mod2 <- obs + rnorm(100, sd = 1.0)

  obsmod <- data.frame(
    obs = rep(obs, 2),
    mod = c(mod1, mod2),
    covariate = rep(c("Model 1", "Model 2"), each = 100)
  )

  p <- ggplot(data = obsmod) +
    geom_taylor(aes(obs, mod, color = covariate)) +
    coord_taylor() +
    scale_x_continuous(name = "Correlation", breaks = seq(0, 1, by = 0.2),
                       labels = function(x) round(cos(x * pi/2), 2)) +
    scale_y_continuous(name = "Standard Deviation") +
    theme_minimal()

  expect_s3_class(p, "ggplot")
  expect_true("GeomTaylor" %in% sapply(p$layers, function(layer) class(layer$geom)[1]))
  expect_true("CoordTaylor" %in% class(p$coordinates))
})

