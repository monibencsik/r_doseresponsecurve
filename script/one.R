library(drc)
library(tidyverse)
library(broom)
theme_set(theme_bw())

drc <- read.delim("drc.txt")
drc

drc_tidy <- drc %>% 
  pivot_longer(
    cols = -dose,
    names_to = "sample",
    values_to = "response"
  ) %>% 
  filter(
    !is.na(response)
  ) %>% 
  mutate(
    condition = str_sub(sample, 1,1),
    replicate = as.numeric(str_sub(sample, 2,2))
  )

drc_tidy %>% 
  ggplot(aes(x = dose, y = response, group = sample, colour = condition)) +
  geom_line() +
  geom_point() +
  scale_x_log10()


max_responses <- drc_tidy %>% 
  group_by(dose, condition) %>% 
  summarise(max_resp = mean(response)) %>% 
  ungroup() %>% 
  arrange(desc(max_resp)) %>% 
  group_by(condition) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-dose)

drc_norm <- drc_tidy %>% 
  left_join(max_responses) %>% 
  mutate(
    norm_response = 100*response / max_resp
  ) %>% 
  select(dose, sample, condition, replicate, norm_response)

drc_per_condition <- drc_norm %>% 
  group_by(dose, condition) %>% 
  summarise(
    sem = sd(norm_response) / sqrt(n()),
    response = mean(norm_response)
  )


model <- drm(
  data = drc_per_condition,
  formula = response ~ dose,
  curveid = condition,
  fct = LL.4(names = c("hill slope", "Min", "Max", "EC50"))
)


tidy(model)
compParm(
  model,
  "EC50",
  "-"
)