poismean <- dnorm(1:100, 50, 15) * 500 + 1
y <- c(1, rpois(150, c(poismean, poismean[1:50])))
n <- length(y)

library(glmgen)
mod0 <- trendfilter(x = y, k = 0L, lambda = seq(10, 100, 10), family = "poisson")

lambda <- seq(10, 100, 10)
k <- length(lambda)
res0 <- data.frame(
  Pois_mean = c(exp(mod0$beta)),
  lambda = rep(lambda, each = n),
  Time = rep(1:n, k)
)

mod1 <- trendfilter(x = y, k = 1L, lambda = seq(10, 100, 10), family = "poisson")
lambda <- seq(10, 100, 10)
res1 <- data.frame(
  Pois_mean = c(exp(mod1$beta)),
  lambda = rep(lambda, each = n),
  Time = rep(1:n, k)
)

mod2 <- trendfilter(x = y, k = 2L, lambda = seq(10, 100, 10), family = "poisson")
lambda <- seq(10, 100, 10)
res2 <- data.frame(
  Pois_mean = c(exp(mod2$beta)),
  lambda = rep(mod2$lambda, each = n),
  Time = rep(1:n, k)
)

res <- rbind(res0, res1, res2)

library(ggplot2)
fig1 <- ggplot(
  res0,
  aes(.data$Time, .data$Pois_mean,
      colour = .data$lambda,
      group = .data$lambda
  )
) +
  geom_line() +
  scale_colour_viridis_c(trans = "log10") +
  labs(x = "", y = "") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#002145")) +
  theme(plot.background = element_rect(fill = "#002145")) +
  theme(legend.position = "none") +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "white")
  )

fig2 <- ggplot(
  res1,
  aes(.data$Time, .data$Pois_mean,
      colour = .data$lambda,
      group = .data$lambda
  )
) +
  geom_line() +
  scale_colour_viridis_c(trans = "log10") +
  labs(x = "", y = "") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#002145")) +
  theme(plot.background = element_rect(fill = "#002145")) +
  theme(legend.position = "none") +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "white")
  )


fig3 <- ggplot(
  res2,
  aes(.data$Time, .data$Pois_mean,
      colour = .data$lambda,
      group = .data$lambda
  )
) +
  geom_line() +
  scale_colour_viridis_c(trans = "log10") +
  labs(x = "", y = "") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#002145")) +
  theme(plot.background = element_rect(fill = "#002145")) +
  theme(legend.position = "none") +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "white")
  )

library(gridExtra)
ptf_graph <- grid.arrange(fig1, fig2, fig3, nrow = 1)

# 12*3.375
#ggsave("gfx/ptf-graph.png", ptf_graph, width=1152, height=324, units="px")
