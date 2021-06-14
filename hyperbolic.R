
# hyperbolic tangent coefficients ------------------------------------------

library(readxl)
library(tidyverse)

tim_cvn <-
  read_excel(
    "MasterDB-SQL-2020-09-24.xlsx",
    sheet = "Charpy"
  ) %>% janitor::clean_names() %>%
  filter(full_transition_curve=="TRUE") %>%
  select(feature,
         temperature_f,
         absorbed_energy_ft_lbs) %>%
  rename(temp = temperature_f,
         cvn = absorbed_energy_ft_lbs,
         id = feature)

ids <- unique(tim_cvn$id)

`%notin%` <- Negate(`%in%`)

# hyperbolic tangent function
func <- function(t, A, B, C, D) {
  A + B * tanh((t - D) / C)
}

## played around with A,B,C,D so that the function was
## close for first few data points
## then used this as starting points for nls
Ai <- 18
Bi <- 13
Ci <- 30
Di <- 89


func(t, Ai, Bi, Ci, Di)
badtemp <- c(120)

idx <- 2
data <- tim_cvn %>% filter(id== ids[idx], temp %notin% badtemp)
t <- data$temp

## plot the data
data %>%
  rename(t = temp) %>%
  ggplot(aes(t, cvn)) +
  geom_point() +
  stat_function(fun = func,
                args = list(Ai, Bi, Ci, Di)) +
  theme_minimal() +
  annotate(
    "text",
    x = min(t + 5),
    y = 20,
    label = paste("A =", Ai,
                  ",B =", Bi,
                  ",C =", Ci,
                  ",D =", Di),
    hjust = 0
  ) +
  labs(title = paste("ID =", ids[idx]))
# not great fit but *close enough* for initial guess

# create a fit with initial guess from above
fit <-
  nls(
    cvn ~ func(t, A, B, C, D),
    data = data ,
    start = list(
      A = Ai,
      B = Bi,
      C = Ci,
      D = Di
    ),algorithm = "port",
    trace = F
  )


# extract coefficients from model
coefs <- coef(fit)

# specify new A,B,C,D from model
A <- coefs[1]
B <- coefs[2]
C <- coefs[3]
D <- coefs[4]


## plot with optimized coefficients
## excellent fit now
data %>%
  rename(t = temp) %>%
  ggplot(aes(t, cvn)) +
  geom_point() +
  stat_function(fun = func, args = list(A, B, C, D))+
  theme_minimal() +
  annotate(
    "text",
    x = min(t)+5,
    y = max(data$cvn)- 5,
    label = paste("A =", round(A,1),
                  ",B =", round(B,1),
                  ",C =", round(C,1),
                  ",D =", round(D,1)),
    hjust = 0
  )+
  labs(title = paste("ID =",ids[idx]))



make_plot <- function(id) {
  tim_cvn %>%
    filter(.data$id == .env$id) %>%
    rename(t = temp) %>%
    ggplot(aes(t, cvn)) +
    geom_point() +
    stat_function(fun = func,
                  args = list(Ai, Bi, Ci, Di)) +
    theme_minimal() +
    annotate(
      "text",
      x = min(t + 5),
      y = 20,
      label = paste("A =", Ai,
                    ",B =", Bi,
                    ",C =", Ci,
                    ",D =", Di),
      hjust = 0
    ) +
    labs(title = paste("ID =", ids[idx]))
}

p <- make_plot(ids[6])
data <- tim_cvn %>% filter(id== ids[6]) %>% rename(t = temp)


# Try different way with self starter -------------------------------------

fit <-
  nls(
    cvn ~ SSlogis(input = t,Asym ,xmid,scal),
    data = data ,
    trace = F
  )

pred <- predict(fit, newdata = data.frame(x = seq(20, 140, length.out = 200)))

data = bind_cols(data, pred = pred[1:10])

p + geom_line(aes(t, pred),col='red')
