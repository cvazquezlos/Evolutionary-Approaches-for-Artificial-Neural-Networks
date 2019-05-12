library(ggplot2)
library(extrafont)
font_import()
y
loadfonts(device = "win")

# IDENTITY
identity <- function(x) {
  x
}

identity_df <- data.frame(x = -6:6, y = identity(-6:6), stringsAsFactors = F)
identity_plot <- ggplot(data = identity_df, aes(x = x, y = y)) +
  geom_vline(xintercept = 0, colour = "#B6B6B6", linetype = "dashed") +
  geom_hline(yintercept = 0, colour = "#B6B6B6", linetype = "dashed") +
  geom_line(color = "orange", size = 1) +
  scale_x_continuous(breaks = seq(-6, 6, 2)) +
  scale_y_continuous(breaks = seq(-6, 6, 2)) +
  theme_classic() +
  ggtitle("Identity") +
  labs(x = "", y = "") +
  theme (
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#E8E8E8"),
    text = element_text(size = 35, family = "Cambria"),
    plot.title = element_text(family = "Cambria", size = 45, hjust = 0.5, margin = margin(0, 0, 25, 0))
  )
identity_plot

# BINARY STEP
binary_step <- function(x) {
  if (x < 0) {
    0
  } else {
    1
  }
}

domain <- seq(-6, 6, 0.01)
binary_step_df <- data.frame(x = domain, y = unlist(lapply(domain, binary_step)), stringsAsFactors = F)
binary_step_plot <- ggplot(data = binary_step_df, aes(x = x, y = y)) +
  geom_vline(xintercept = 0, colour = "#B6B6B6", linetype = "dashed") +
  geom_hline(yintercept = 0, colour = "#B6B6B6", linetype = "dashed") +
  geom_line(color = "orange", size = 1) +
  scale_x_continuous(breaks = seq(-6, 6, 2)) +
  scale_y_continuous(breaks = seq(-0.2, 1.2, 0.2)) +
  theme_classic() +
  ggtitle("Binary step") +
  labs(x = "", y = "") +
  theme (
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#E8E8E8"),
    text = element_text(size = 35, family = "Cambria"),
    plot.title = element_text(family = "Cambria", hjust = 0.5, size = 45, margin = margin(0, 0, 25, 0))
  )
binary_step_plot

# SIGMOID
sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

domain <- seq(-6, 6, 0.01)
sigmoid_df <- data.frame(x = domain, y = unlist(lapply(domain, sigmoid)), stringsAsFactors = F)
sigmoid_plot <- ggplot(data = sigmoid_df, aes(x = x, y = y)) +
  geom_vline(xintercept = 0, colour = "#B6B6B6", linetype = "dashed") +
  geom_hline(yintercept = 0, colour = "#B6B6B6", linetype = "dashed") +
  geom_line(color = "orange", size = 1) +
  scale_x_continuous(breaks = seq(-6, 6, 2)) +
  scale_y_continuous(breaks = seq(0, 1, 0.5)) +
  theme_classic() +
  ggtitle("Sigmoid") +
  labs(x = "", y = "") +
  theme (
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#E8E8E8"),
    text = element_text(size = 35, family = "Cambria"),
    plot.title = element_text(family = "Cambria", hjust = 0.5, size = 45, margin = margin(0, 0, 25, 0))
  )
sigmoid_plot

# TANH
domain <- seq(-6, 6, 0.01)
tanh_df <- data.frame(x = domain, y = unlist(lapply(domain, tanh)), stringsAsFactors = F)
tanh_plot <- ggplot(data = tanh_df, aes(x = x, y = y)) +
  geom_vline(xintercept = 0, colour = "#B6B6B6", linetype = "dashed") +
  geom_hline(yintercept = 0, colour = "#B6B6B6", linetype = "dashed") +
  geom_line(color = "orange", size = 1) +
  scale_x_continuous(breaks = seq(-6, 6, 2)) +
  scale_y_continuous(breaks = seq(-1, 1, 0.4)) +
  theme_classic() +
  ggtitle("TanH") +
  labs(x = "", y = "") +
  theme (
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#E8E8E8"),
    text = element_text(size = 35, family = "Cambria"),
    plot.title = element_text(family = "Cambria", hjust = 0.5, size = 45, margin = margin(0, 0, 25, 0))
  )
tanh_plot

# RELU
relu <- function(x) {
  if (x < 0) {
    0
  } else {
    x
  }
}

domain <- seq(-6, 6, 0.01)
relu_df <- data.frame(x = domain, y = unlist(lapply(domain, relu)), stringsAsFactors = F)
relu_plot <- ggplot(data = relu_df, aes(x = x, y = y)) +
  geom_vline(xintercept = 0, colour = "#B6B6B6", linetype = "dashed") +
  geom_hline(yintercept = 0, colour = "#B6B6B6", linetype = "dashed") +
  geom_line(color = "orange", size = 1) +
  scale_x_continuous(breaks = seq(-6, 6, 2)) +
  scale_y_continuous(breaks = seq(-6, 6, 1)) +
  theme_classic() +
  ggtitle("ReLu") +
  labs(x = "", y = "") +
  theme (
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#E8E8E8"),
    text = element_text(size = 35, family = "Cambria"),
    plot.title = element_text(family = "Cambria", hjust = 0.5, size = 45, margin = margin(0, 0, 25, 0))
  )
relu_plot

# LEAKY RELU
leaky_relu <- function(x) {
  if (x < 0) {
    0.1 * x
  } else {
    x
  }
}

domain <- seq(-6, 6, 0.01)
leaky_relu_df <- data.frame(x = domain, y = unlist(lapply(domain, leaky_relu)), stringsAsFactors = F)
leaky_relu_plot <- ggplot(data = leaky_relu_df, aes(x = x, y = y)) +
  geom_vline(xintercept = 0, colour = "#B6B6B6", linetype = "dashed") +
  geom_hline(yintercept = 0, colour = "#B6B6B6", linetype = "dashed") +
  geom_line(color = "orange", size = 1) +
  scale_x_continuous(breaks = seq(-6, 6, 2)) +
  scale_y_continuous(breaks = seq(-6, 6, 1)) +
  theme_classic() +
  ggtitle("Leaky ReLU") +
  labs(x = "", y = "") +
  theme (
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#E8E8E8"),
    text = element_text(size = 35, family = "Cambria"),
    plot.title = element_text(family = "Cambria", hjust = 0.5, size = 45, margin = margin(0, 0, 25, 0))
  )
leaky_relu_plot

# SOFTPLUS
softplus <- function(x) {
  log(1 + exp(x))
}

domain <- seq(-6, 6, 0.01)
softplus_df <- data.frame(x = domain, y = unlist(lapply(domain, softplus)), stringsAsFactors = F)
softplus_plot <- ggplot(data = softplus_df, aes(x = x, y = y)) +
  geom_vline(xintercept = 0, colour = "#B6B6B6", linetype = "dashed") +
  geom_hline(yintercept = 0, colour = "#B6B6B6", linetype = "dashed") +
  geom_line(color = "orange", size = 1) +
  scale_x_continuous(breaks = seq(-6, 6, 2)) +
  scale_y_continuous(breaks = seq(-6, 6, 1)) +
  theme_classic() +
  ggtitle("SoftPlus") +
  labs(x = "", y = "") +
  theme (
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#E8E8E8"),
    text = element_text(size = 35, family = "Cambria"),
    plot.title = element_text(family = "Cambria", hjust = 0.5, size = 45, margin = margin(0, 0, 25, 0))
  )
softplus_plot

# SQUARE NONLINEARITY<>
square_nonlinearity <- function(x) {
  if (x > 2.0) {
    1
  } else if (x >= 0 & x <= 2.0) {
    x - ((x ^ 2) / 4)
  } else if (x >= -2.0 & x < 0) {
    x + ((x ^ 2) / 4)
  } else {
    -1
  }
}

domain <- seq(-2.5, 2.5, 0.01)
square_nonlinearity_df <- data.frame(x = domain, y = unlist(lapply(domain, square_nonlinearity)), stringsAsFactors = F)
square_nonlinearity_plot <- ggplot(data = square_nonlinearity_df, aes(x = x, y = y)) +
  geom_vline(xintercept = 0, colour = "#B6B6B6", linetype = "dashed") +
  geom_hline(yintercept = 0, colour = "#B6B6B6", linetype = "dashed") +
  geom_line(color = "orange", size = 1) +
  scale_x_continuous(breaks = seq(-2.5, 2.5, 1)) +
  scale_y_continuous(breaks = seq(-6, 6, 1)) +
  theme_classic() +
  ggtitle("SQNL") +
  labs(x = "", y = "") +
  theme (
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#E8E8E8"),
    text = element_text(size = 35, family = "Cambria"),
    plot.title = element_text(family = "Cambria", hjust = 0.5, size = 45, margin = margin(0, 0, 25, 0))
  )
square_nonlinearity_plot
