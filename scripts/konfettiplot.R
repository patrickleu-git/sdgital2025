

# load libraries ----------------------------------------------------------



library(tidyverse)
library(readxl)
library(janitor)
library(ggbeeswarm)
library(patchwork)
library(ggtext)
library(glue)
library(viridis)
library(ggview)




# load data ---------------------------------------------------------------



bund = read_xlsx("data/export-interne-final.xlsx", sheet = "Targets") |> 
  clean_names()

externe = read_xlsx("data/export-externe-final.xlsx", sheet = "TargetEvaluations") |> 
  clean_names()

externe_actor = read_xlsx("data/export-externe-final.xlsx", sheet = "ActorProfiles") |> 
  clean_names()



# data wrangling ----------------------------------------------------------



# SNE focus topic's targets
st1 = c("12.8", "12.c", "12.2", "8.4", "8.2", "12.4", "2.1", "12.3", "12.2", "2.4", "12.6")
st1_themen = c(rep("Nachhaltige Konsummuster fördern und ermöglichen", 2),
               rep("Wohlstand und Wohlergehen unter Schonung der natürlichen Ressourcen sichern", 4),
               rep("Transformation hin zu nachhaltigen Ernährungssystemen vorantreiben", 4),
               "Unternehmensverantwortung stärken")
names(st1) = st1_themen

st2 = c("13.2", "13.1", "11.b", "13.3", "7.3", "7.1", "7.2", "15.5", "15.8", "15.a", "15.1", "6.6", "15.3")
st2_themen = c(rep("Treibhausgasemissionen reduzieren und klimabedingte Auswirkungen bewältigen", 4),
               rep("Energieverbrauch senken, Energie effizienter nutzen und erneuerbare Energien ausbauen", 3),
               rep("Biologische Vielfalt erhalten, nachhaltig nutzen, fördern und wiederherstellen", 6))
names(st2) = st2_themen

st3 = c("1.2", "3.8", "11.1", "4.3", "10.3", "8.5", "10.7", "10.2", "11.a", "1.3", "5.1", "5.4", "8.5", "5.5", "5.2")
st3_themen = c(rep("Selbstbestimmung jeder und jedes Einzelnen fördern", 4),
               rep("Sozialen Zusammenhalt sicherstellen", 6),
               rep("Tatsächliche Gleichstellung von Frau und Mann gewährleisten", 5))
names(st3) = st3_themen

targets = c(st1, st2, st3)


# trend levels, internal ones in English, externe auf Deutsch...
# recode trend levels
bund$trend_evaluation_level <- recode(
  bund$trend_evaluation_level,
  "VeryUnrealistic"    = "sehr unrealistisch",
  "RatherUnrealistic"  = "eher unrealistisch",
  "Realistic"          = "realistisch",
  "RatherRealistic"    = "eher realistisch",
  "VeryRealistic"      = "sehr realistisch"
)

trend_levels = c("sehr unrealistisch", "eher unrealistisch", "realistisch", "eher realistisch", "sehr realistisch")

# factor trend evaluation, filter for SNE targets, select relevant cols
bund = bund |> 
  filter(trend_evaluation_level != "NotSpecified") |> 
  mutate(
    trend = factor(trend_evaluation_level, levels = trend_levels, ordered = T),
    actor = "Bund"
  ) |> 
  filter(target_id %in% targets) |> 
  select(target_id, actor, trend)

# join to pull actor category, factor trend evaluation, filter, select relevant cols
externe = externe |> 
  filter(trend_assessment != "keine Angaben") |> 
  left_join(
    externe_actor, 
    by = c("akteur" = "akteur_id")
  ) |> 
  mutate(
    trend = factor(trend_assessment, levels = trend_levels, ordered = T),
    actor = if_else(category == "Öffentlich-rechtliche Körperschaften", subcategory, category),
    target_id = target
  ) |> 
  filter(
    target_id %in% targets,    # SNE target
    !is.na(trend),             # filter NAs 
    completed.x == "True",     # completed trend eval (avoid potential 2022 inputs)
    completed.y == "True"      # completed actor tasks (avoid potential 2022 inputs)
  ) |>
  select(target_id, actor, trend)

# factor actors
fct_actor = c(
  unique(externe$actor)[5], 
  unique(externe$actor)[6], 
  unique(externe$actor)[2], 
  unique(externe$actor)[3], 
  unique(externe$actor)[4], 
  unique(externe$actor)[1], 
  "Bund")

# give colours; similar ones for public entities
names(fct_actor) = c("#82EEFD", "#0492C2", "#281E5D", "darkred", "darkgreen", "#E69F00", "black")

# bind rows for final df, add subtopic and arrange by actor
df = bind_rows(externe, bund) |> 
  mutate(
    subtopic = names(targets)[match(target_id, targets)],
    actor = fct(actor, levels = fct_actor)) |> 
  arrange(desc(actor))



# plotting all sub topics -------------------------------------------------



for (i in unique(names(targets))){
  
  # targets in subtopic
  targets_subtopic = targets[names(targets == i)]
    
  # beeswarm layer; change target ordering for "Sozialer Zuasmmenhalt" s.t. remove overlapping
  # as there are several targets and many data points
    beeswarm_layer = if (i == "Sozialen Zusammenhalt sicherstellen") {
    geom_beeswarm(
      data = filter(df, actor != "Bund" & target_id %in% targets_subtopic), 
      aes(x = factor(target_id, c("10.2", "1.3", "10.3", "10.7", "11.a", "8.5")), 
          y = trend, 
          colour = actor), 
      size = 3, 
      cex = 1, 
      alpha = 0.7,
      method = "center"
    )
  } else {
    geom_beeswarm(
      data = filter(df, actor != "Bund" & target_id %in% targets_subtopic), 
      aes(x = target_id, 
          y = trend, 
          colour = actor), 
      size = 3, 
      cex = if_else(length(targets_subtopic) > 2, 1, 1.2), 
      alpha = 0.7,
      method = "center"
    )
  }
  
  # plot
  ggplot()+
    beeswarm_layer +
    geom_point(
      data = filter(df, actor == "Bund" & target_id %in% targets_subtopic), 
      aes(x = target_id, y = as.numeric(trend)+0.07, colour = actor),
      shape = 18,
      size = 5,
      alpha = 0.8
    )+
    scale_colour_manual(
      values = names(fct_actor),
      labels = str_wrap(fct_actor, 50),
      drop = FALSE
    )+
    scale_y_discrete(
      labels = c("sehr unrealistisch", "", "realistisch", "", "sehr realistisch"),
      drop = FALSE
    )+
    coord_cartesian(
      xlim = c(1, length(targets_subtopic)),
      clip = "off"
    )+
    annotate(
      "text", x = length(targets_subtopic) + 0.7, y = 1.2, 
      label = "Ein Punkt entspricht der Einschätzung \neines Akteurs, d.h. einer Organisation \noder einer Köperschaft", 
      size = 3.5, hjust = 0, vjust = -0.75
    )+
    labs(
      title = "Einschätzung der Zielerreichung nach Target und Akteur",
      subtitle = glue("Unterthema: {i}"),
      x = NULL, y = NULL,
      color = "Akteursgruppe",
      caption = "Quelle: Bestandesaufnahme 2025"
    )+
    theme_minimal()+
    theme(
      plot.title = element_text(face = "bold", hjust = 0, size = 16),
      plot.subtitle = element_text(hjust = 0, size = 13),
      plot.margin = margin(1, 0, 1, 0.5, "cm"),
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0, vjust = -1),
      axis.text = element_text(face = "bold"),
      legend.title = element_text(face = "bold")
    )+
    canvas(16, 9, scale = if_else(length(targets_subtopic) > 2, 0.95, 0.9)) # adjust scale to length of x axis
  
  # save the plot
  save_ggplot(last_plot(), glue("outputs/Konfettiplots/{i}.png"))
  
}












# Abstellgleis ------------------------------------------------------------



# test how the plot looks with several targets (broad x axis)
test = df |> filter(subtopic == "Sozialen Zusammenhalt sicherstellen") |> select(target_id) |> unique() |> as_vector()

ggplot()+
  geom_beeswarm(
    data = filter(df, actor != "Bund" & target_id %in% test), 
    aes(x = factor(target_id, c("10.2", "1.3", "10.3", "10.7", "11.a", "8.5")), y = trend, colour = actor), 
    size = 3, 
    cex = 1, 
    alpha = 0.7,
    method = "centre",
  )+
  geom_point(
    data = filter(df, actor == "Bund" & target_id %in% test), 
    aes(x = factor(target_id, c("10.2", "1.3", "10.3", "10.7", "11.a", "8.5")), y = as.numeric(trend) + 0.07, colour = actor),
    shape = 18,
    size = 6,
    alpha = 0.7,
  )+
  scale_colour_manual(
    values = names(fct_actor),
    labels = str_wrap(fct_actor, 50)
  )+
  coord_cartesian(
    xlim = c(1, length(test)),
    clip = "off"
  )+
  annotate(
    "text", x = length(test) + 0.7, y = 1.2, 
    label = "Ein Punkt entspricht der Einschätzung \neines Akteurs, d.h. einer Organisation \noder einer Köperschaft", 
    size = 3.5, hjust = 0, vjust = -0.75
  )+
  labs(
    title = "Trendeinschätzung",
    subtitle = "Unterthema: abd",
    x = NULL, y = NULL,
    color = "Akteursgruppe",
    caption = "Quelle: SDGital2030.ch"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", hjust = 0, size = 16),
    plot.subtitle = element_text(hjust = 0, size = 14),
    plot.margin = margin(1, 0, 1, 0.5, "cm"),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, vjust = -1),
    axis.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )+
  canvas(16, 9, scale = 1)






# shift individual groups' data points on different levels
# not really visually appealing as soon as one group is missing...
ggplot()+
  geom_beeswarm(
    data = filter(df, actor == unique(actor)[1] & target_id %in% test), 
    aes(x = factor(target_id, c("10.2", "1.3", "10.3", "10.7", "11.a", "8.5")), y = trend, colour = str_wrap(actor, 50)), 
    size = 3, 
    alpha = 0.7,
    method = "centre",
  )+
  geom_beeswarm(
    data = filter(df, actor == unique(actor)[2] & target_id %in% test), 
    aes(x = factor(target_id, c("10.2", "1.3", "10.3", "10.7", "11.a", "8.5")), y = as.numeric(trend)+0.07, colour = str_wrap(actor, 50)), 
    size = 3, 
    alpha = 0.7,
    method = "centre",
  )+
  geom_beeswarm(
    data = filter(df, actor == unique(actor)[3] & target_id %in% test), 
    aes(x = factor(target_id, c("10.2", "1.3", "10.3", "10.7", "11.a", "8.5")), y = as.numeric(trend)-0.07, colour = str_wrap(actor, 50)), 
    size = 3, 
    alpha = 0.7,
    method = "centre",
  )+
  geom_beeswarm(
    data = filter(df, actor == unique(actor)[4] & target_id %in% test), 
    aes(x = factor(target_id, c("10.2", "1.3", "10.3", "10.7", "11.a", "8.5")), y = as.numeric(trend)+0.14, colour = str_wrap(actor, 50)), 
    size = 3, 
    alpha = 0.7,
    method = "centre",
  )+
  geom_beeswarm(
    data = filter(df, actor == unique(actor)[5] & target_id %in% test), 
    aes(x = factor(target_id, c("10.2", "1.3", "10.3", "10.7", "11.a", "8.5")), y = as.numeric(trend)-0.14, colour = str_wrap(actor, 50)), 
    size = 3, 
    alpha = 0.7,
    method = "centre",
  )+
  geom_beeswarm(
    data = filter(df, actor == unique(actor)[7] & target_id %in% test), 
    aes(x = factor(target_id, c("10.2", "1.3", "10.3", "10.7", "11.a", "8.5")), y = as.numeric(trend)-0.21, colour = str_wrap(actor, 50)), 
    size = 3, 
    alpha = 0.7,
    method = "centre",
  )+
  geom_point(
    data = filter(df, actor == "Bund" & target_id %in% test), 
    aes(x = factor(target_id, c("10.2", "1.3", "10.3", "10.7", "11.a", "8.5")), y = as.numeric(trend) + 0.21, colour = actor),
    shape = 18,
    size = 4,
    alpha = 0.8,
  )+
  scale_colour_viridis(
    discrete = TRUE
  )+
  coord_cartesian(
    xlim = c(1, length(test)),
    clip = "off"
  )+
  annotate(
    "text", x = length(test) + 0.7, y = 1.2, 
    label = "Ein Punkt entspricht der Einschätzung \neines Akteurs, d.h. einer Organisation \noder einer Köperschaft", 
    size = 3.5, hjust = 0, vjust = -0.75
  )+
  labs(
    title = "Trendeinschätzung",
    subtitle = "Unterthema: abd",
    x = NULL, y = NULL,
    color = "Akteursgruppe",
    caption = "Quelle: SDGital2030.ch"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", hjust = 0, size = 16),
    plot.subtitle = element_text(hjust = 0, size = 14),
    plot.margin = margin(1, 0, 1, 0.5, "cm"),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, vjust = -1),
    axis.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )+
  canvas(16, 9, scale = 1)




