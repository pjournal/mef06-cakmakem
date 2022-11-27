pti <- c("shiny","tidyverse", "readxl","openxlsx","shinyWidgets","DT")
pti <- pti[!(pti %in% installed.packages())]
if(length(pti)>0){
  install.packages(pti)
}

library(shiny)
library(tidyverse)
library(readxl)
library(openxlsx)
library(shinyWidgets)
library(DT)

#path <- "C:/Users/EMRE.CAKMAK/Documents/GitHub/mef06-cakmakem/foreign_students_by_nationality_2021_2022.xlsx"

# getting data from sheets
#sheets <- openxlsx::getSheetNames(path)
#data_frame <- lapply(sheets, openxlsx::read.xlsx, xlsxFile=path)

#data_frame

# assigning names to data frame
#names(data_frame) <- 'YOK_Dataset'

# printing the data
#print (data_frame)

#list2env(data_frame, envir = .GlobalEnv)

# Save an object to a file
#saveRDS(YOK_Dataset, file = "YOK_Dataset.rds")
# Restore the object
new_data_frame <- readRDS(file = "YOK_Dataset.rds")

colnames(new_data_frame) <- c("Uni_Name", "Uni_Type", "City_Name", "Student_Nationality", "Male_Count","Female_Count","Total_Count")


new_data_frame<- new_data_frame %>% mutate(Female_Count = as.numeric(Female_Count))
new_data_frame<- new_data_frame %>% mutate(Male_Count = as.numeric(Male_Count))
new_data_frame<- new_data_frame %>% mutate(Total_Count = as.numeric(Total_Count))

# Get genre list
City_Name <- 
  new_data_frame %>% 
  distinct(City_Name) %>% 
  unlist(.)

names(City_Name) <- NULL

Uni_Name <- 
  new_data_frame %>% 
  distinct(Uni_Name) %>% 
  unlist(.)

names(Uni_Name) <- NULL

Uni_Type <- 
  new_data_frame %>% 
  distinct(Uni_Type) %>% 
  unlist(.)

names(Uni_Type) <- NULL

Student_Nationality <- 
  new_data_frame %>% 
  distinct(Student_Nationality) %>% 
  unlist(.)

names(Student_Nationality) <- NULL





# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("YOK Dataset for foreign students"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      pickerInput("City_Name","City", choices=unique(City_Name), selected=City_Name, options = list(`actions-box` = TRUE),  multiple=TRUE),
      pickerInput("Uni_Name","University", choices=unique(Uni_Name), selected=Uni_Name, options = list(`actions-box` = TRUE),  multiple=TRUE),
      pickerInput("Uni_Type","University Type", choices=unique(Uni_Type), selected=Uni_Type,options = list(`actions-box` = TRUE),  multiple=TRUE),
      pickerInput("Student_Nationality","Student Nationality", choices=unique(Student_Nationality), selected=Student_Nationality, options = list(`actions-box` = TRUE), multiple=TRUE),
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("colplot"), plotOutput("plot")),
        tabPanel("Table", tableOutput("table"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$colplot <- renderPlot({
    filtered_dataset <- new_data_frame %>%
      filter(City_Name %in% c(input$City_Name), Uni_Name %in% c(input$Uni_Name)
             , Uni_Type %in% c(input$Uni_Type), Student_Nationality %in% c(input$Student_Nationality))
    
    ggplot(filtered_dataset,aes(x=Uni_Type, y=Total_Count))+
      geom_col()
  })
  
  output$plot <- renderPlot({
    filtered_dataset <- new_data_frame %>%
      filter(City_Name %in% c(input$City_Name), Uni_Name %in% c(input$Uni_Name)
             , Uni_Type %in% c(input$Uni_Type), Student_Nationality %in% c(input$Student_Nationality)) 
    
    ggplot(filtered_dataset,aes(x=City_Name, y=Total_Count, fill=Uni_Type))+
      geom_col() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$table <- renderTable({ filtered_dataset <- new_data_frame %>%
    filter(City_Name %in% c(input$City_Name), Uni_Name %in% c(input$Uni_Name)
           , Uni_Type %in% c(input$Uni_Type), Student_Nationality %in% c(input$Student_Nationality))
    filtered_dataset
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
