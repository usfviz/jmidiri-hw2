library(ggvis)
library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plyr)

setwd("/data/")

# Data
le.metadata <- read.csv("Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", header = TRUE, check.names = FALSE)
le.metadata <- le.metadata[, names(le.metadata) %in% c("Country Code", "Region")]

fertility <- read.csv("API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv", header = TRUE, check.names = FALSE, skip = 4)
fertility <- fertility[, !names(fertility) %in% c("Indicator Name", "Indicator Code", "2015", "2016", "")]
fertility <- gather(fertility, year, fert_rate, which(names(fertility) %in% c(1960:2014)))

lifeexpectancy <- read.csv("API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", header = TRUE, check.names = FALSE, skip = 4)
lifeexpectancy <- lifeexpectancy[, !names(lifeexpectancy) %in% c("Indicator Name", "Indicator Code", "2015", "2016", "")]
lifeexpectancy <- gather(lifeexpectancy, year, life_exp, which(names(lifeexpectancy) %in% c(1960:2014)))

population <- read.csv("API_SP.POP.TOTL_DS2_en_csv_v2.csv", header = TRUE, check.names = FALSE)
population <- population[, !names(population) %in% c("Indicator Name", "Indicator Code", "2015", "2016", "")]
population <- gather(population, year, pop, which(names(population) %in% c(1960:2014)))

# Join tables to get all necessary information into one
# Life Expectancy, Fertility Rate, Region, etc.
fert.life.join <- join(fertility, lifeexpectancy, by = c("Country Name", "Country Code", "year"), type = "left")
add.metadata <- join(fert.life.join, le.metadata, by = c("Country Code"), type = "left")
complete.data <- join(add.metadata, population, by = c("Country Name", "Country Code", "year"), type = "left")

# Remove rows where Region is "" or NA
# Corresponds to those records that aren't countries
complete.data <- subset(complete.data, Region != "" & !is.na(Region))
complete.data <- complete.data[complete.cases(complete.data), ]
complete.data$id <- 1:nrow(complete.data)

ui <- fluidPage(
  fluidRow(
    column(3, wellPanel(
      sliderInput("year.of.interest", label = "Year", min = 1960, max = 2014, value = 1960, animate = TRUE, sep = ""),
      selectInput("select", label = "Region", choices = append(list("All"), as.vector(sort(unique(complete.data$Region)))), selected = "All"))), #  multiple = TRUE
    column(9, uiOutput("ggvis_ui"), ggvisOutput("ggvis"))
  )
)

server <- function(input, output) {
  d <- reactive({
    if (input$select == "All"){
      complete.data %>% filter(year == input$year.of.interest)
    } else {
      complete.data %>% filter(year == input$year.of.interest & Region == input$select)
    }
    })
  get.country.name <- function(x) {
    if(is.null(x)) return(NULL)
    row <- d()[d()$id == x$id, ]$`Country Name`
    paste0(format(row), collapse = "<br />")
  }
  ggvis.plot <- reactive({
    d() %>%
      ggvis(x = ~life_exp, y = ~fert_rate, size = ~pop, fill = ~factor(Region), key := ~id) %>%
      add_tooltip(get.country.name, c("hover", "click")) %>%
      layer_points() %>%
      add_legend("fill", title = "Region") %>% 
      hide_legend("size") %>%
      scale_numeric("x", domain = c(10,90), nice = FALSE) %>%
      scale_numeric("y", domain = c(0.5, 9), nice = FALSE) %>%
      add_axis("x", title = "Life Expectancy", values = seq(10, 90, 5)) %>%
      add_axis("x", orient = "top", ticks = 0, title = ifelse(length(unique(d()$Region)) > 1, "Countries", paste("Countries, ", d()$Region[1], sep = "")), properties = axis_props(axis = list(stroke = "white"), labels = list(fontSize = 0))) %>%
      add_axis("y", title = "Fertility Rate")
    })
  ggvis.plot %>% bind_shiny("ggvis", "ggvis_ui")
}

shinyApp(ui = ui, server = server)
