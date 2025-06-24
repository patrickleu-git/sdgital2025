
# load libraries ----------------------------------------------------------


library(tidyverse)
library(readxl)
library(writexl)
library(reticulate)
library(janitor)
library(ellmer)
library(ollamar)
library(glue)



# load data (and define factor cols) --------------------------------------


# define list containing respective focus topic's targets (in order of appearence)
focus_targets = list(
  st1 = c("12.8", "12.c", "12.2", "8.4", "8.2", "12.4", "2.1", "12.3", "2.4", "12.6"),
  st2 = c("13.2", "13.1", "11.b", "13.3", "7.3", "7.1", "7.2", "15.5", "15.8", "15.a", "15.1", "6.6", "15.3"),
  st3 = c("1.2", "3.8", "11.1", "4.3", "10.3", "8.5", "10.7", "10.2", "11.a", "1.3", "5.1", "5.4", "5.5", "5.2")
)

# read data
targets_I = read_xlsx("./data/export-interne-final.xlsx", sheet = "Targets") |> clean_names() |> filter(target_id %in% unlist(focus_targets))
action_fields_I = read_xlsx("./data/export-interne-final.xlsx", sheet = "ActionFields") |> clean_names() |> filter(target_id %in% unlist(focus_targets))
action_fields_E = read_xlsx("./data/export-externe-final.xlsx", sheet = "ActionFields") |> clean_names() |> filter(target %in% unlist(focus_targets))
action_field_contexts_E = read_xlsx("./data/export-externe-final.xlsx", sheet = "ActionFieldContexts") |> clean_names() |> filter(target %in% unlist(focus_targets))

# map(focus_targets, \(x) filter(targets_I, target_id %in% x))




# Summary : Function ------------------------------------------------------



# summarising function
summarise_gemini = function(
    text, 
    column_to_summarise = c(
      "ActionFieldGeneralEstimation", 
      "DescriptionRaw-interne", 
      "Description-externe", 
      "GeneralEstimation", 
      "RemainingActionFieldsDescription",
      "Unterthema"),
    n_words = 30,
    rate_limit = TRUE,
    model = "gemini-2.0-flash",
    temperature = 0.6,
    seed = 2030,
    echo = "output") {
  
  # instruct the model
  sys_prompt = "
  Du bist ein hilfsbereiter Assistent und bist Experte im Bereich der nachhaltigen Entwicklung.
  Du **formatierst niemals Text**, ausser du wirst explizit dazu aufgefordert.
  Du gibst **nur** die verlangte Antwort, ohne einleitende Worte oder Anschlussfragen."
  
  # match column_to_summarise
  col =  match.arg(column_to_summarise)
  
  # prompt pre- and suffix
  prompt_pre = "
  Du erhältst nachfolgend einen auf Deutsch oder Französisch formulierten Text.\n"
  prompt_suff = glue("
  Bitte fasse diesen Text in **höchstens {n_words} in der Originalsprache** zusammen. \n\n")
  
  # set the prompt
  if (col %in% c("DescriptionRaw-interne",  "ActionFieldGeneralEstimation")){
    
    prompt = paste0(prompt_pre, "
    Der Text beschreibt ein prioritäres Handlungsfeld eines Targets der Agenda 2030 der Vereinten Nationen und die Herausforderungen bei der Zielerreichung.
    Der Text ist aus der Sicht der Schweizer Bundesverwaltung verfasst.\n" , prompt_suff)
    
    } else if (col %in% c("Description-externe", "GeneralEstimation")){
    
    prompt = paste0(prompt_pre, "
    Der Text beschreibt ein prioritäres Handlungsfeld eines Targets der Agenda 2030 der Vereinten Nationen und die Herausforderungen bei der Zeilerreichung.
    Der Text ist aus der Sicht eines schweizerischen Stakeholders verfasst (z. B. NGO, Unternehmen, Partei).\n" , prompt_suff)
    
    } else if (col == "RemainingActionFieldsDescription"){
    
    prompt = paste0(prompt_pre, "
    Der Text beschreibt Herausforderungen, welche bei der Umsetzung eines Targets der Agenda 2030 bestehen und welche Massnahmen die Schweiz auf Bundesebene ergreifen soll, um die Herauforderungen anzupacken.
    Der Text ist aus der Sicht eines schweizerischen Stakeholders verfasst (z. B. NGO, Unternehmen, Partei).\n", prompt_suff)
    
    } else  {
    
    prompt = paste0(prompt_pre, prompt_suff)
    
    }
  
  
  # set up agent
  chat = chat_google_gemini(
    system_prompt = sys_prompt,
    model = model,
    params = params(
      temperature = temperature,
      seed = seed),
    echo = echo
  )
  
  # 4 second interval due to 15RPM restriction
  if (rate_limit) Sys.sleep(4)
  
  # create summaries
  result = chat$chat(paste(prompt, text))
  
  # return
  return(result)
  
}



# Summary: Application ----------------------------------------------------

### targets_I
targets_I_Sum = targets_I |>
  mutate(
    action_field_general_estimation_summary = map_chr(
      str_remove_all(action_field_general_estimation_raw, "\\n"),
      \(x) summarise_gemini(
        x, 
        column_to_summarise = "ActionFieldGeneralEstimation", 
        temperature = 0) # no creativity
    )
  ) |> 
  select(target_id, action_field_general_estimation_raw, action_field_general_estimation_summary, everything()) 

# targets_I_Sum |> write_xlsx(path = "outputs/excel/targets_I_Sum.xlsx", col_names = TRUE)



### action_fields_I
action_fields_I_Sum = action_fields_I |>
  mutate(
    description_raw_summary = map_chr(
      str_remove_all(description_raw, "\\n"),
      \(x) summarise_gemini(
        x, 
        column_to_summarise = "DescriptionRaw-interne", 
        temperature = 0) # no creativity
    )
  ) |> 
  select(target_id, description_raw, description_raw_summary, everything())

action_fields_I |> write_xlsx(path = "outputs/excel/action_fields_I.xlsx", col_names = TRUE)





### action_fields_E
action_fields_E_Sum = action_fields_E |>
  mutate(
    description_summary = map_chr(
      str_remove_all(description, "\\n"),
      \(x) summarise_gemini(
        x, 
        column_to_summarise = "Description-externe", 
        temperature = 0) # no creativity
    )
  ) |> 
  select(target, description, description_summary, everything())

# action_fields_E_Sum |> write_xlsx(path = "outputs/excel/action_fields_E_Sum.xlsx", col_names = TRUE)



### action_field_contexts_E
action_field_contexts_E_Sum = action_field_contexts_E |>
  mutate(
    general_estimation_summary = map_chr(
      str_remove_all(general_estimation, "\\n"),
      \(x) {
        if (is.na(x) || x == "") return(NA_character_) # many NAs, might filter out
        summarise_gemini(
          x, 
          column_to_summarise = "GeneralEstimation", 
          temperature = 0 # no creativity
        )
      }
    ),
    remaining_action_fields_description_summary = map_chr(
      str_remove_all(remaining_action_fields_description, "\\n"),
      \(x) {
        if (is.na(x) || x == "") return (NA_character_) # many NAs, might filter out
        summarise_gemini(
          x,
          column_to_summarise = "RemainingActionFieldsDescription",
          temperature = 0 # no creativity
        ) 
      }  
    )
  ) |> 
  select(
    target, 
    general_estimation, general_estimation_summary, 
    remaining_action_fields_description, remaining_action_fields_description_summary, 
    everything()) 

# action_field_contexts_E_Sum |> write_xlsx(path = "outputs/excel/action_field_contexts_E_Sum.xlsx", col_names = TRUE)



# Clustering : Function ---------------------------------------------------




# helper to get clean output
parse_output = function(response) {
  
  # clean output
  x = str_split(response, "\n")[[1]] |> 
    str_remove_all("\\d+\\.") |> 
    trimws()
  
  # remove last element if only line break
  if (x[length(x)] == ""){
    x = x[-length(x)]
  } else x = x 
  
  # return result 
  return(x)
  
}



# clustering function, with predefined labels or zero-shot classification
cluster_gemini = function(
    texts,
    mode = c("predefined", "zero-shot"),
    predefined_labels,
    k_clusters = 10,
    rate_limit = TRUE,
    model = "gemini-2.0-flash",
    temperature = 0.6,
    seed = 2030,
    echo = "output"){
  
  # instruct the model
  sys_prompt = "
  Du bist ein hilfsbereiter Assistent und bist Experte im Bereich der nachhaltigen Entwicklung.
  Du formatierst niemals Text, ausser du wirst explizit dazu aufgefordert.
  Du gibst NUR die verlangte Antwort, keine einleitenden Worte oder Anschlussfragen."
  
  # match cluster mode
  mode = match.arg(mode)
  
  # mode
  if (mode == "predefined"){
    
    # get labels
    labels = paste(predefined_labels, collapse = "; ")
    
    # prompt 
    prompt = glue("
    Unten findest du eine Liste von {length(text)} kurzen Texten, die Herausforderungen bei der Umsetzung der Agenda 2030 beschreiben.
    Die Texte können auf Deutsch oder Französisch formuliert sein, dies spielt für die folgende Aufgabe keine Rolle. 
    Ordne jedem Text exakt ein passendes Clusterlabel aus folgender Liste zu (ohne diese umzuschreiben):
         
    {labels}
    
    Gib als Antwort eine nummerierte Liste mit dem zugewiesenen Label pro Text, in exakt dieser Form:
    1. Clusterlabel
    2. Clusterlabel
    etc.

    Texte:
    {paste(sprintf('%d. %s', seq_along(texts), texts), collapse = '\n')}")
    
  } else {
   
    prompt = glue("
    Du erhältst eine Liste von {length(text)} kurzen Texten. Die Texte sind auf Deutsch oder Französisch verfasst.
    Jeder Text beschreibt die Herausforderungen bei der Umsetzung eines Targets der Agenda 2030 in der Schweiz.
    
    Deine Aufgabe ist es, diese Texte in **höchstens {k_clusters} thematisch sinnvolle Cluster** zu gruppieren.
    **Jedes Cluster sollte mehrere Texte enthalten** – vermeide Cluster mit nur einem Text.
    Jedes Cluster soll einen kurzen, prägnanten Titel auf Deutsch erhalten, der den Inhalt gut beschreibt.
    
    Wichtig:
    - Analysiere den inhaltlichen Fokus jedes Textes (z. B. Ressourcen, rechtliche Rahmenbedingungen, Datenmangel etc.).
    - Es spielt keine Rolle, in welcher Sprache der Text geschrieben ist.
    - Maximal {k_clusters} verschiedene Clusterlabels über alle Texte hinweg.
    - Gib als Antwort **nur** eine mit dem Index des jeweiligen Textes nummerierte Liste mit dem jeweils zugewiesenen Clusterlabel für jeden der {length(texts)} Texte, in exakt dieser Form, ohne die Clusterlabel separat widerzugeben:
    
    1. Clusterlabel
    2. Clusterlabel
    etc.
    
    Texte:
    {paste(sprintf('%d. %s', seq_along(texts), texts), collapse = '\n')}")
     
  } 
  
  # initiate agent
  chat = chat_google_gemini(
    system_prompt = sys_prompt,
    model = model,
    params = params(
      temperature = temperature,
      seed = seed),
    echo = echo
  )
  
  # create summaries
  result = chat$chat(prompt)
  
  # return
  return = parse_output(result)
  
}




# Clustering : Application ------------------------------------------------



# predefined labels for clustering
predefined_labels = c(
  "finanzielle Ressourcen", 
  "personelle Ressourcen", 
  "rechtliche Rahmenbedingungen", 
  "politisches Umfeld",
  "Zielkonflikte",
  "Wissens- und Datenlücken",
  "fehlendes Bewusstsein",
  "schwierige Messbarkeit", 
  "fehlende finanzielle Anreize bzw. Fehlanreize", 
  "Marktversagen, Internalisierung externer Kosten", 
  "fehlendes Angebot",
  "fehlende Instrumente",
  "erschwerter Marktzugang", 
  "fehlende Politikkohärenz", 
  "unklare Zuständigkeiten",
  "langsame Prozesse",
  "Bürokratie",
  "Anderes")


# clusters for action fields: internal description
action_fields_I_Sum$clusters = cluster_gemini(action_fields_I$description_raw, mode = "predefined", predefined_labels = predefined_labels)


# clusters for action fields: external description
action_fields_E_Sum$clusters = cluster_gemini(action_fields_E$description, mode = "predefined", predefined_labels = predefined_labels)


# cluster_zs = cluster_gemini(df$st1$description_raw, mode = "zero-shot", k_clusters = 8)


