pti <- c("readxl","dplyr","tidyverse", "ggplot2", "lubridate", "tidyr", "stringi", "hrbrthemes", "viridis", "scales", "knitr","shinyWidgets","DT")
pti <- pti[!(pti %in% installed.packages())]
if(length(pti)>0){
  install.packages(pti)
    }


library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringi)
library(hrbrthemes)
library(viridis)
library(scales)
library(knitr)
library(shiny)
library(shinyWidgets)
library(DT)


labour_force_status_by_reg <- readRDS("Labour_force_status_by_reg.rds")
head(labour_force_status_by_reg)

a_year <- 
  labour_force_status_by_reg %>% 
  distinct(year) %>% 
  unlist(.)

a_region <- 
  labour_force_status_by_reg %>% 
  distinct(region) %>% 
  unlist(.)

a_region_nt <- 
  labour_force_status_by_reg %>% 
  filter(region != "Total") %>%
  distinct(region) %>% 
  unlist(.)

a_gender <- 
  labour_force_status_by_reg %>% 
  distinct(gender) %>% 
  unlist(.) 

################################################
  
employed_rate_by_marital_status <- readRDS("Employed_rate_by_marital_status.rds")
head(employed_rate_by_marital_status)

b_year <- 
  employed_rate_by_marital_status %>% 
  distinct(year) %>% 
  unlist(.) 

b_gender <- 
  employed_rate_by_marital_status %>% 
  distinct(gender) %>% 
  unlist(.)

b_marital_status <- 
  employed_rate_by_marital_status %>% 
  distinct(marital_status) %>% 
  unlist(.)

###############################################
  
reasons_of_not_being_in_lab_for <- readRDS("Reasons_of_not_being_in_lab_for.rds")
head(reasons_of_not_being_in_lab_for)  

c_year <- 
  reasons_of_not_being_in_lab_for %>% 
  distinct(year) %>% 
  unlist(.)

c_gender <- 
  reasons_of_not_being_in_lab_for %>% 
  distinct(gender) %>% 
  unlist(.)

c_reason <- 
  reasons_of_not_being_in_lab_for %>% 
  distinct(reason) %>% 
  unlist(.)

c_reason_nt <- 
  reasons_of_not_being_in_lab_for %>% 
  filter(reason != "Total") %>%
  distinct(reason) %>% 
  unlist(.)

###############################################

crude_divorce_rate_by_provinces <- readRDS("Crude_divorce_rate_by_provinces.rds")  
head(crude_divorce_rate_by_provinces)

d_year <- 
  crude_divorce_rate_by_provinces %>% 
  distinct(year) %>% 
  unlist(.)

d_province <- 
  crude_divorce_rate_by_provinces %>% 
  distinct(province) %>% 
  unlist(.)



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Women Participation in Turkish Labour Force"),
  
  # Sidebar with a slider input for number of bins 
    
    # Show a plot of the generated distribution
    mainPanel(
    tabsetPanel(
        tabPanel("Labour Force Participation & Employment Rate by Gender", 
                 sliderInput("a_year","Years:", min = min(labour_force_status_by_reg$year), max = max(labour_force_status_by_reg$year), value = c(min(labour_force_status_by_reg$year),max(labour_force_status_by_reg$year)),sep = ""),
                 pickerInput("a_gender","Gender:", choices=unique(a_gender), selected=a_gender, options = list(`actions-box` = TRUE),  multiple=TRUE),
                 plotOutput("a_lineplot"),
                 p(" "),
                 p(" "),
                 plotOutput("plot")),
        tabPanel("Labour Force Participation Ratio vs. Employment Ratio", 
                 sliderInput("b_year","Years:", min = min(employed_rate_by_marital_status$year), max = max(employed_rate_by_marital_status$year), value = c(min(employed_rate_by_marital_status$year),max(employed_rate_by_marital_status$year)),sep = ""),
                 pickerInput("b_marital_status","Marital Status:", choices=unique(b_marital_status), selected=b_marital_status, options = list(`actions-box` = TRUE),  multiple=TRUE),
                 p("Employment Ratio explains the ratio of Men Employment Rate to Women Employment Rate. It's a KPI for understanding men dominance over women in working life"),
                 plotOutput("maritalplot"),
                 p(" "),
                 p(" "),
                 tableOutput("maritaltable")),
        tabPanel("Variability in Women Unemployment by Region", 
                 sliderInput("a2_year","Years:", min = min(labour_force_status_by_reg$year), max = max(labour_force_status_by_reg$year), value = c(min(labour_force_status_by_reg$year),max(labour_force_status_by_reg$year)),sep = ""),
                 pickerInput("a2_gender","Gender:", choices=unique(a_gender), selected=a_gender, options = list(`actions-box` = TRUE),  multiple=TRUE),
                 pickerInput("a2_region","Region:", choices=unique(a_region_nt), selected=a_region_nt, options = list(`actions-box` = TRUE),  multiple=TRUE),
                 plotOutput("regionalunemployment"),
                 p(" "),
                 p(" "),
                 plotOutput("boxplotunemployment")),
        tabPanel("Reasons of not Being in Labour Force", 
                 sliderInput("c_year","Years:", min = min(reasons_of_not_being_in_lab_for$year), max = max(reasons_of_not_being_in_lab_for$year), value = c(min(reasons_of_not_being_in_lab_for$year),max(reasons_of_not_being_in_lab_for$year)),sep = ""),
                 pickerInput("c_gender","Gender:", choices=unique(c_gender), selected=c_gender, options = list(`actions-box` = TRUE),  multiple=TRUE),
                 pickerInput("c_reason","Reason:", choices=unique(c_reason_nt), selected=c_reason_nt, options = list(`actions-box` = TRUE),  multiple=TRUE),
                 plotOutput("reasonpieplot"),
                 p(" "),
                 p(" "),
                 tableOutput("reasontable")),
        tabPanel("Average Divorce Rate by Provinces", 
                 sliderInput("d_year","Years:", min = min(reasons_of_not_being_in_lab_for$year), max = max(reasons_of_not_being_in_lab_for$year), value = c(min(reasons_of_not_being_in_lab_for$year),max(reasons_of_not_being_in_lab_for$year)),sep = ""),
                 pickerInput("d_province","Province:", choices=unique(d_province), selected=d_province, options = list(`actions-box` = TRUE),  multiple=TRUE),
                 plotOutput("divorce"))
        
        
      )
    )
  )



server <- function(input, output) {
  
  output$a_lineplot <- renderPlot({
    df1 <- labour_force_status_by_reg %>%
      filter(year >= input$a_year[1], year <= input$a_year[2], gender %in% c(input$a_gender), region == "Total") %>%
      select(year, gender, labour_force_participation_rate, unemployment_rate, employment_rate)
      

    df2 <- employed_rate_by_marital_status %>%
      filter(marital_status == "Divorced") %>%
      select(year, gender, divorced_emp_rate = employed_rate)
    
    df3 <- df1 %>% 
      inner_join(df2, by = c("year", "gender"))
    
    df3 %>%
    ggplot() + 
      geom_line(aes(x = year, y = labour_force_participation_rate, colour = "red")) + 
      geom_line(aes(x = year, y = employment_rate, colour = "blue")) + 
      scale_color_identity(name = '',
                           breaks = c('red', 'blue'),
                           labels = c("LF Participation Rate", "Employment Rate"),
                           guide = 'legend') +
      theme(legend.position = "top") +
      scale_x_continuous(breaks = c(min(df3$year):max(df3$year))) + 
      theme(axis.text.x = element_text(angle=90,vjust=2,hjust=2)) + 
      facet_wrap( ~ gender) +
      ylab("")
  })
  
  output$plot <- renderPlot({
    df1 <- labour_force_status_by_reg %>%
      filter(year >= input$a_year[1], year <= input$a_year[2], region == "Total") %>%
      select(year, gender, labour_force_participation_rate, unemployment_rate, employment_rate)
    
    df2 <- employed_rate_by_marital_status %>%
      filter(marital_status == "Divorced") %>%
      select(year, gender, divorced_emp_rate = employed_rate)
    
    df3 <- df1 %>% 
      inner_join(df2, by = c("year", "gender"))
    
    df4 <- df3 %>% 
      select(year, gender, labour_force_participation_rate) %>%
      pivot_wider(names_from = gender, values_from =  labour_force_participation_rate) %>%
      mutate(lfp_ratio = Male / Female) %>%
      select(year, lfp_ratio)
    
    df5 <- df3 %>% 
      select(year, gender, employment_rate) %>%
      pivot_wider(names_from = gender, values_from =  employment_rate) %>%
      mutate(emp_ratio = Male / Female) %>%
      select(year, emp_ratio)
    
    df4 %>% 
      inner_join(df5, by = "year") %>%
      ggplot() +
      geom_line(aes(x = year, y = lfp_ratio, colour = "green")) +
      geom_line(aes(x = year, y = emp_ratio, colour = "blue")) + 
      scale_color_identity(name = '',
                           breaks = c('green', 'blue'),
                           labels = c("LF Participation Ratio", "Employment Ratio"),
                           guide = 'legend') +
      theme(legend.position = "top") +
      scale_x_continuous(breaks = c(min(df5$year):max(df5$year))) +
      ylab("")
  })
  
  output$maritaltable <- renderTable({
    female<-employed_rate_by_marital_status%>%filter(gender=="Female")
    male<-employed_rate_by_marital_status%>%filter(gender=="Male")
    
    diff<-female%>%mutate(ratio=male$employed_rate/female$employed_rate) %>%select(-gender)%>%filter(marital_status %in% c(input$b_marital_status), year >= input$b_year[1], year <= input$b_year[2])
    
    diff2 <- diff %>% select(year,marital_status, ratio, female_emp_rate = employed_rate) %>% pivot_wider(names_from = marital_status, values_from = c(female_emp_rate,ratio))
    
    diff2
    
  })
  
  output$maritalplot <- renderPlot({
    female<-employed_rate_by_marital_status%>%filter(gender=="Female")
    male<-employed_rate_by_marital_status%>%filter(gender=="Male")
    
    diff<-female%>%mutate(ratio=male$employed_rate/female$employed_rate) %>%select(-gender)%>%filter(marital_status %in% c(input$b_marital_status), year >= input$b_year[1], year <= input$b_year[2])
    ggplot(diff, aes(x = year, y = ratio, color = marital_status)) + geom_line() +
      scale_x_continuous(breaks = c(min(female$year):max(female$year))) +
      labs(title = "Employment Ratio")
  })
  
  output$regionalunemployment <- renderPlot({
    labour_force_status_by_reg %>%
      filter(region != "Total") %>%
      filter(year >= input$a2_year[1], year <= input$a2_year[2], gender %in% c(input$a2_gender), region %in% c(input$a2_region)) %>%
      
      ggplot() + 
      geom_line(aes(x = year, y = unemployment_rate, color = gender)) + 
      facet_wrap( ~ region) +
      scale_x_continuous(breaks = c(min(labour_force_status_by_reg$year):max(labour_force_status_by_reg$year))) + 
      theme(axis.text.x = element_text(angle=90,vjust=1,hjust=1)) +
      labs(title = "Unemployment Rate by Region") +
      ylab("unemployment rate")
  })
  
  output$boxplotunemployment <- renderPlot({ 
    labour_force_status_by_reg %>%
      filter(region != "Total") %>%
      filter(year >= input$a2_year[1], year <= input$a2_year[2], gender %in% c(input$a2_gender), region %in% c(input$a2_region)) %>%
      ggplot(aes(y=unemployment_rate, group = gender)) + 
      geom_boxplot(fill="steelblue") +
      facet_grid(gender ~ region) + 
      theme(axis.text.x = element_text(angle=90,vjust=1,hjust=1)) +
      theme(strip.text.x = element_text(size = 3)) +
      labs(title = "Unemployment Distribution by Region") +
      ylab("unemployment rate")
    })
  
  
  output$reasonpieplot <- renderPlot({
    df6 <- reasons_of_not_being_in_lab_for %>%
      filter(year >= input$c_year[1], year <= input$c_year[2], gender %in% c(input$c_gender)) %>%
      group_by(gender, reason) %>%
      summarise(pop_not_in_lab_force = sum(pop_not_in_lab_force), .groups = "drop")
    
    df7 <- df6 %>% 
      filter(reason == "Total") %>%
      select(gender, total_pop_not_in_lab_force = pop_not_in_lab_force)
    
    df8 <- df6 %>% 
      filter(reason != "Total")
    
    df9 <- df7 %>%
      inner_join(df8, by = "gender") %>%
      mutate(pop_not_in_lab_force_rate = pop_not_in_lab_force / total_pop_not_in_lab_force) %>%
      select(gender, reason, pop_not_in_lab_force_rate)
    
    df9 %>%
      ggplot(aes(x = "", y = pop_not_in_lab_force_rate, fill = reason )) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) +
      geom_text(aes(label = scales::percent(round(pop_not_in_lab_force_rate,2))),
                position = position_stack(vjust = 0.5), size = 5, angle = -80) +
      facet_wrap( ~ gender) +
      xlab("") +
      ylab("") +
      theme_void() +
      theme(legend.text=element_text(size=11), legend.position = "bottom", legend.title=element_blank())
    
  })
  
  output$reasontable <- renderTable({
    df6 <- reasons_of_not_being_in_lab_for %>%
      filter(year >= input$c_year[1], year <= input$c_year[2], gender %in% c(input$c_gender), reason %in% c(input$c_reason)) %>%
      group_by(gender, reason) %>%
      summarise(pop_not_in_lab_force = sum(pop_not_in_lab_force), .groups = "drop")
    
    df7 <- df6 %>% 
      filter(reason == "Total") %>%
      select(gender, total_pop_not_in_lab_force = pop_not_in_lab_force)
    
    df8 <- df6 %>% 
      filter(reason != "Total")
    
    df8%>% 
      pivot_wider(names_from = gender, values_from = pop_not_in_lab_force)})
  
  
  output$divorce <- renderPlot({
    plot_data1<-crude_divorce_rate_by_provinces %>% 
      filter(year >= input$d_year[1], year <= input$d_year[2], province %in% c(input$d_province)) %>%
      group_by(province) %>% 
      summarise(avg_rate = mean(crude_divorce_rate))%>% 
      arrange(desc(avg_rate)) %>%
      top_n(10)
    
    ggplot(plot_data1,aes(x=reorder(province, -avg_rate), y=avg_rate, fill= province)) + 
      geom_bar(stat = "identity") +
      labs(title = "Top 10 provinces for the average divorce rate") +
      xlab("province") +
      ylab("average divorce rate")
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
