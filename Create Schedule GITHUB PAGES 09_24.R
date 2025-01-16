#'Publication table pre-processing
#'This produces the RDS data file saved in separate github pages enabled repo `HF_Update_Schedule`
#' USE VERSION 4.4.2 as downloadthis is not working in previous versions 

## ---- charting publication dates ----

## Packages
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(googledrive)
library(highcharter)
library(reactable)
library(htmltools)
library(downloadthis)

today <- "2024-11-13" #TYPE REQUIRED DATE HERE
df <- read_csv(file = "../HF-Evidence-Hub/Schedule and planning/THF Schedule data Sept 2024.csv")


sprintdates <- tibble(
  Sprint = c("Sprint11",
             "ad-hoc-Oct",
             "ad-hoc-Nov", 
             "Sprint12", 
             "Sprint13", 
             "Sprint14", 
             "Sprint15", 
             "Sprint16", 
             "Sprint17", 
             "Sprint18"),
  
  `Delivery date` = as.Date( c("2024-09-30",
                               "2024-10-18",
                               "2024-11-22", 
                               "2025-01-06", 
                               "2025-02-03", 
                               "2025-03-03", 
                               "2025-04-07", 
                               "2025-05-05", 
                               "2025-06-02", 
                               "2025-07-31"))
)


df <- left_join(df, sprintdates, by = "Sprint")

#slugs from the MASTER list
slugs <- googlesheets4::read_sheet(ss = "1sRo1wyOvrnG5JwY6D7LbOmn9UnPK8ABNaZ0Il4_WDtk", sheet = "INDICATORS" ) %>% select(Index, url, `Current/Retired`) %>% 
  filter(is.na(`Current/Retired`)) %>% 
  select(-`Current/Retired`) %>% 
  unique()

#merge
df <- df %>% left_join(slugs, by = c("index" = "Index"))


#remove retired, on hold etc.
#slugs
notupdating <- googlesheets4::read_sheet(ss = "1sRo1wyOvrnG5JwY6D7LbOmn9UnPK8ABNaZ0Il4_WDtk", sheet = "INDICATORS" ) %>% 
  select(Index, url, `Current/Retired`) %>% 
  filter(!is.na(`Current/Retired`)) %>% 
  select(-`Current/Retired`) %>% 
  unique()


df <- df %>% filter(!index %in% notupdating$Index)

#pare dataset down
df <- df %>% select(
 index,
 subtitle,
 Topic,
 `Sub-topic`,
 url,
 Sprint,
 `Delivery date`,
  `Expected next`,
"Data source code" =  data.source.code.full,
"Data source" = data.source.product,
"Data source producer" =  data.source.producer
 
)

#There are a few chart titles missing because they have never been updated by us as the data are too infrequent, i.e. no fl or plot charts
master.gdoc.titles <- read_sheet("https://docs.google.com/spreadsheets/d/1sRo1wyOvrnG5JwY6D7LbOmn9UnPK8ABNaZ0Il4_WDtk",
                           sheet = "INDICATORS") %>% #filter(is.na(`Current/Retired`)) %>% 
  select(Index, contains("Chart title 2")) %>% unique() %>% rename("subtitle" = "Chart title 2 published")
missing.title <- df %>% filter(is.na(subtitle))
master.gdoc.titles <- master.gdoc.titles %>% filter(Index %in% missing.title$index)

#replace missing subtitle fileds from master.gdoc.titles
df <- df %>% left_join(master.gdoc.titles, by = c("index" = "Index")) %>% 
  mutate(subtitle = coalesce(subtitle.x, subtitle.y)) %>% 
  select(-subtitle.x, -subtitle.y)

#rogue THFV0116 is a wealth and assets indicator not done by WPI, but I can't find a record of it to know which one it is!


# #sprint allocation revised
# #Sprint 11 set for September 2024
# #Sprint 11.5 set for November 2024
# #Sprint 12 set for January 2025
# #Sprint 13 set for February 2025
# 
# df <- df %>% 
#   mutate(`Sprints revised 2024/25` = case_when(
#     `Expected next` < "2024-07-13" ~ "Sprint 10",
#     `Expected next` < "2024-08-31" & `Expected next` > "2024-07-13"  ~ "Sprint 11",
#     `Expected next` < "2024-11-05" & `Expected next` > "2024-08-31" ~ "Sprint 11.5",
#     `Expected next` < "2025-01-30" & `Expected next` > "2024-11-05" ~ "Sprint 12", #NEED TO SPLIT Sp12 into Sp13 as TOO MANY
#     `Expected next` > "2025-01-30" & `Expected next` < "2025-04-01" ~ "Sprint 13",
#     TRUE ~ "2025/26"
#   ))
# 
# #allocate Friends family community to Sp13 if currently Sprints revised is Sprint 12
# df <- df %>% 
#   mutate(`Sprints revised 2024/25` = case_when(
#     Topic == "Friends, family and community" & `Sprints revised 2024/25` == "Sprint 12" ~ "Sprint 13",
#     Topic == "Housing" & `Sprints revised 2024/25` == "Sprint 12" ~ "Sprint 13",
#     TRUE ~ `Sprints revised 2024/25`
#   )
#   )

df <- df %>% arrange(`Expected next`) %>% 
  #move subtitle column to second column
  select(index, subtitle,`Delivery date`, everything())

#The table 
table <- df %>% 


  rename("Chart title" = subtitle,
         "Data release date (approx)" = `Expected next`) %>% 
  arrange(`Data release date (approx)`) %>% 
  
  reactable(groupBy = "Sprint", 
            
            columns = list(
              index = colDef(aggregate = "unique"),
              `Topic`=  colDef(aggregate = "frequency"),
              `Delivery date` = colDef(aggregate = "unique"),
              "Chart title" = colDef(
                width = 300,
                html = T,
                cell = function(value, index){
                  label <- df$subtitle[index]
                  sprintf('<a href="%s" target="_blank">%s</a>',df$url[index], label)
                }),
              
              "Data source" = colDef(width = 300),
              url = colDef(show = F)
              
            ),
            
            
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
                                            ("THF Evidence Hub Publication Timetable 2024/25"), 
                                         
                                            p(style = "font-family: Arial;font-weight: normal;font-size: 12px;color: #000000",
                                               paste0("Last updated ", Sys.Date() )
                                            ) 
),download_this(df, 
                output_name = "Sprint timetable",
                output_extension = ".xlsx", 
                button_label = "Download data"), p())

browsable(withtitle)




#plot
#remove millenium cohort and WAS indicators
df <- df %>% filter(!is.na(subtitle)) %>% arrange(`Expected next`)

#This one plots colours by topic
plot_topic <-  highchart() %>% 
  
  #these are the dots for each indicator
  hc_add_series(data = df[!is.na(df$`Expected next`),],
                type = "scatter",
                hcaes(x = `Expected next`, #`Update month.sortonme`, 
                      y = row_number(),
                      gw1 = `subtitle`, 
                      gw2 = index,
                      gw3 = `Topic`,
                      gw4 = `Sub-topic`,
                      #color = `Topic colours`,
                      group = Topic),
                showInLegend = T,
                
                marker = list(symbol = "circle",
                              radius = 4
                              # lineColor = colours.obj2[[1]]
                )
  )  %>%
  
  hc_yAxis(visible = T) %>% 
  hc_xAxis(type = "datetime") %>% 
  
  hc_tooltip(pointFormat = "Date: {point.x:%Y-%m-%d} <br> Title: {point.gw1} <br> 
             Index: {point.gw2} <br> Topic: {point.gw3} <br> 
             Sub-topic: {point.gw4} <br>", 
             shared = TRUE
  ) %>% 
  
  #hc_colors(c(colours.obj2[2:3],colours.obj[c(6,4)],colours.obj2[6],colours.obj[3]  )) %>% 
  
  hc_title(text = "Indicator release dates by Sprint",
           align = "left",
           style = list(fontSize ="18px",color = "#000000",
                        fontFamily = "Arial", fontWeight = "400" ))
plot_topic


#This one plots colours by topic
plot_sprint <-  highchart() %>% 
  
  #these are the dots for each indicator
  hc_add_series(data = df[!is.na(df$`Expected next`),],
                type = "scatter",
                hcaes(x = `Expected next`, #`Update month.sortonme`, 
                      y = row_number(),
                      gw1 = `subtitle`, 
                      gw2 = index,
                      gw3 = `Topic`,
                      gw4 = `Sub-topic`,
                      #color = `Topic colours`,
                      group = `Sprint`),
                showInLegend = T,
                
                marker = list(symbol = "circle",
                              radius = 4
                              # lineColor = colours.obj2[[1]]
                )
  )  %>%
  
  hc_yAxis(visible = T) %>% 
  hc_xAxis(type = "datetime") %>% 
  
  hc_tooltip(pointFormat = "Date: {point.x:%Y-%m-%d} <br> Title: {point.gw1} <br> 
             Index: {point.gw2} <br> Topic: {point.gw3} <br> 
             Sub-topic: {point.gw4} <br>", 
             shared = TRUE
  ) %>% 
  
  #hc_colors(c(colours.obj2[2:3],colours.obj[c(6,4)],colours.obj2[6],colours.obj[3]  )) %>% 
  
  hc_title(text = "Indicator release dates by Topic",
           align = "left",
           style = list(fontSize ="18px",color = "#000000",
                        fontFamily = "Arial", fontWeight = "400" ))
plot_sprint




save_html(withtitle, "index.html") 
