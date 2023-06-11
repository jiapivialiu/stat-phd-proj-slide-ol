poismean <- dnorm(1:100, 50, 15) * 500 + 1
y <- c(1, rpois(150, c(poismean, poismean[1:50])))
n <- length(y)

library(glmgen)
mod0 <- trendfilter(x = y, k = 0L, lambda = seq(10, 100, length.out=10), family = "poisson")

lambda <- seq(10, 100, length.out=10)
k <- length(lambda)
res0 <- data.frame(
  Pois_mean = c(exp(mod0$beta)),
  lambda = rep(lambda, each = n),
  Time = rep(1:n, k)
)

mod1 <- trendfilter(x = y, k = 1L, lambda = seq(.1, 20, length.out=10), family = "poisson")
lambda <- seq(.1, 20, length.out=10)
res1 <- data.frame(
  Pois_mean = c(exp(mod1$beta)),
  lambda = rep(lambda, each = n),
  Time = rep(1:n, k)
)

mod2 <- trendfilter(x = y, k = 2L, lambda = seq(.1, 20, length.out=10), family = "poisson")
lambda <- seq(.1, 20, length.out=10)
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
    axis.text = element_blank()
    #axis.text = element_text(color = "white")
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
    #axis.text = element_text(color = "white")
    axis.text = element_blank()
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
    #axis.text = element_text(color = "white"),
    axis.text = element_blank()
  )

library(gridExtra)
ptf_graph <- grid.arrange(fig1, fig2, fig3, nrow = 1)

# 12*3.375
ggsave("gfx/ptf-graph.png", ptf_graph, width=10.76, height=3.67, units="in")


# another plot
my_palette <- c("#DB0B5B", "#6495ed","#ffa319")
set.seed(123)
n <- 100
x <- seq(0, 1, length.out = n)
y <- sin(x * 2 * pi) + rnorm(n, sd = 0.1)
#plot(x, y)
library(glmgen)
est1 <- trendfilter(x = y, k = 2L, lambda=10)
#lines(x, est1$beta, col = "#6495ed", lwd=1.5)
est2 <- trendfilter(x = y, k = 1L, lambda=10)
#lines(x, est2$beta, col = "#ffa319", lwd=1.5)
est3 <- trendfilter(x = y, k = 0L, lambda=3)
#lines(x, est3$beta, col = "#DB0B5B", lwd=1.5)
df <- data.table(x = x,
                 tf1 = est3$beta,
                 tf2 = est2$beta,
                 tf3 = est1$beta,
                 y=y)
df %>% 
  pivot_longer(!c(x,y), names_to = "curves", values_to = "values") %>%
  group_by(curves) %>%
  ggplot(aes(x=x, y=y)) + 
  geom_point(alpha = .1) + 
  geom_line(aes(y=values, col=curves), lwd=1.3) + 
  scale_color_manual(values = my_palette,
                     labels = c("k=0", "k=1", "k=2")) + 
  labs(color="TF degrees") + 
  theme_bw() + 
  theme(legend.position = "bottom", 
        legend.background = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
  
ggsave("gfx/tfline.png", units="in")



