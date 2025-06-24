# sdgital2025

## Context
Switzerland plans to report on the state of the implementation of the UN 2030 Agenda for the third time for the 2026 High-Level Political Forum (HLPL). In this context, the federal administration conducts a stocktaking survey, where experts from the federal administration, as well as other public institutions (cantons, communes) and private stakeholders (businesses, associations, NGOs, and the sciences) can assess the implementation status of each of the 17 SDGs and their 169 targets. The country report will be based on the results of this comprehensive review — more info can be found [here ](https://www.agenda-2030.eda.admin.ch/en/country-report-and-stocktaking-survey).

[The Federal Office for Spatial Development ARE, where I conducted my civil service, is responsible for coordinating the implementation of the 2030 Agenda at the national level, and drafted Switzerland's 2030 Sustainable Development Strategy](https://www.are.admin.ch/are/en/home/sustainable-development/strategy/sds.html). I could provide some quantitative and qualitative analyses as well as data visualisations for their reporting, which I uploaded to this repo. Only the publicly available assessments and evaluations are available here, i.e., the contents that will be published on [SDGital2030](https://www.sdgital2030.ch/).

## Contents
- summary_table.R: summarises the number of external stakeholders that consider the respective SDGs in their activities, grouped by actor category (e.g., canton, businesses).
- konfettiplot.R: beeswarm plot for the individual actors' assessments regarding target attainment (i.e., is Switzerland on track to reach the target by 2030). Grouped by actor category.
- qualitative-analyse.R: summary of various assessment reasonings, context windows, etc. and pre-defined label clustering via generative AI. I used the [ellmer package and its built-in Gemini chat function](https://ellmer.tidyverse.org/reference/chat_google_gemini.html) to create a summarising and clustering function.
- input-doc-quarto.qmd: quarto file as the basis for an input document to be used in workshops.
