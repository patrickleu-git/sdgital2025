

# load libraries ----------------------------------------------------------


library(tidyverse)
library(readxl)
library(writexl)
library(janitor)


# load data ---------------------------------------------------------------


externe_actor = read_xlsx("data/cleaned/export-externe-final.xlsx", sheet = "ActorProfiles") |> 
  clean_names()


# data wrangling ----------------------------------------------------------



# data frame that counts the mentions per SDG for the actor categories
df = externe_actor |> 
  separate_longer_delim(selected_goals, ",") |> 
  mutate(
    actor = if_else(category == "Öffentlich-rechtliche Körperschaften", subcategory, category),
    selected_goals = factor(selected_goals, levels = 1:17)
    ) |> 
  filter(!is.na(selected_goals)) |> 
  distinct(akteur_id, actor, selected_goals) |> 
  count(actor, selected_goals) |> 
  pivot_wider(
    names_from = selected_goals,
    names_prefix = "SDG",
    values_from = n,
    names_sort = TRUE
    )


# data frame that counts the number of distinct actors per category
df2 = externe_actor |> 
  mutate(actor = if_else(category == "Öffentlich-rechtliche Körperschaften", subcategory, category)) |> 
  summarise(.by = actor, total = n_distinct(akteur_id)) |> 
  arrange(actor)


# combine 1 and 2
summary_table = df |> 
  mutate(Total = df2$total)


# add the total number of mentions and the total number of actors
summary_table = bind_rows(summary_table, colSums(summary_table[2:19])) |> 
  replace_na(list(actor = "Total"))
  

# write xl
write_xlsx(summary_table, path = "outputs/summary_table.xlsx")
