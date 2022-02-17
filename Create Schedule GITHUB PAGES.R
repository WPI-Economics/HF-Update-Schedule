#'Publication table pre-procesing
#'This produces the RDS data file saved in seperate github pages enabled repo `HF_Update_Schedule`

## ---- charting publication dates ----

## Packages
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(googledrive)
library(highcharter)
library(reactable)
library(htmltools)

today <- "2022-02-17" #TYPE REQUIRED DATE HERE
df <- readRDS(file = paste0("../HF-Update-Schedule/THF_Update_Schedule_",today,".RDS"))

#The table 
table <- df %>% rename("Sprint end date" = `sprint_end_var`) %>% 
  filter(!`Publication frequency` == "RETIRED") %>% 
  select(-`New data available`, 
                       -Topic_Subtopic, 
                       -`Last publication date2`, 
                       -`Last publication date` ) %>% 
  rename("Chart title" = `chart title 2 published`, "Data source" = `Data source (clean)`) %>% 
  
  reactable(groupBy = "Initial sprint (adjusted)" ,
            
            defaultPageSize = 19,
            highlight = T,
            striped = T,
            theme = reactableTheme(
              stripedColor = "#e2dfd8",
              highlightColor = "#c8c3be",
              cellPadding = "6px 10px",
              style = list(fontFamily = "Arial", fontSize = "12px"),
              #searchInputStyle = list(width = "100%", fontWeight = "400"),
              headerStyle = list(color = "white",background = "#932116",
                                 "&:hover[aria-sort]" = list(background = "#ee9b90 "),
                                 "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "#ee9b90"),
                                 borderColor = "#ee9b90"
              )
            ), filterable = T, sortable = T) 

table

withtitle <- htmlwidgets::prependContent(table, 
                                         h2(class = "title", style = "font-family: Arial; color: #e84245",
                                            ("THF Evidence Hub Publication Timetable"), 
                                         
                                            p(style = "font-family: Arial;font-weight: normal;font-size: 12px;color: #000000",
                                               paste0("Last updated ", Sys.Date() )
                                            ) 
))

browsable(withtitle)

