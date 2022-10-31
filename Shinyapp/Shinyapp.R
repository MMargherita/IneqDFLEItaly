# Libraries -----
library(tidyverse)
library(dplyr)
library(shiny)
library(shinythemes)
library(bslib)
library(markdown)
library(readxl)



# set the names of the figures to be shown
figures <- as.factor(c(
  "DFLE",
  "DFLE with CIs",
  "LE",
  "DFLE/LE %",
  "Map gender gap",
  "Decomp. gender gap",
  "Map educational gap",
  "Decomp. educational gap"))

# read the data with figures' captions
captions <- read_excel("./fig_for_shiny/captions.xlsx") %>%
  select(`Caption`)

# name and order captions
capt <- captions$`Caption`
names(capt) <- c(
  "DFLE",
  "DFLE with CIs",
  "LE",
  "DFLE/LE %",
  "Map gender gap",
  "Decomp. gender gap",
  "Map educational gap",
  "Decomp. educational gap")




# UI and Server -----------------------------------------------------------

# set-up for light/dark mode
light <- bs_theme(bootswatch = "flatly")
dark <- bs_theme(bootswatch = "darkly")


# ui
ui <- navbarPage(
  title = "Inequalities in DFLE",
  
  theme = light, 
  checkboxInput("dark_mode", "Dark mode"),
  
  tabPanel(
    "Main page",
    withMathJax((includeMarkdown("Cover_Page.Rmd")))
  ), 
  
  
  tabPanel(
    "Data and methods", 
    withMathJax((includeMarkdown("Data_Methods.Rmd")))
  ),
 
  tabPanel(
    "Results",
    withMathJax((includeMarkdown("Figures.Rmd"))),
    sidebarLayout(
      sidebarPanel(width = 3,
                   selectizeInput( # select figure
                     "figures",
                     label     = "Chose the figure",
                     choices   = figures, 
                     multiple = FALSE,
                     selected  = "DFLE",
                     options = list(create = TRUE, maxItems = 1)
                   ),
      ),
      mainPanel(
        plotOutput("plot", height = "500px", width = "100%"), # Show the image 
        textOutput("caption") # display the caption under the figures
        
      )
    )
  )
)



# server 
server <- function(input, output, session) {
  
  observe(session$setCurrentTheme(
    if (isTRUE(input$dark_mode)) dark else light
  ))
  
  output$plot <- renderImage(
    
    if(input$figures == "DFLE"){            
      list(src = "./fig_for_shiny/regions_DFLE.svg",
           height = 500, width = 850,bg = "white")
    }
    else if(input$figures == "DFLE with CIs"){      
      list(src = "./fig_for_shiny/regions_DFLE_CIs.svg",
           height = 500, width = 850)
    }
    else if(input$figures == "LE"){      
      list(src = "./fig_for_shiny/regions_LE.svg",
           height = 500, width = 850)
    }
    else if(input$figures == "DFLE/LE %"){      
      list(src = "./fig_for_shiny/regions_DFLE-LE.svg",
           height = 500, width = 850)
    }
    else if(input$figures == "Map gender gap"){      
      list(src = "./fig_for_shiny/map_gender_gap.svg",
           height = 400, width = 950)
    }
    else if(input$figures == "Decomp. gender gap"){            
      list(src = "./fig_for_shiny/contr_g_gap_A-ME-BA.svg",
           height = 500, width = 950)
    }
    else if(input$figures == "Map educational gap"){            
      list(src = "./fig_for_shiny/map_edu_gap.svg",
           height = 450, width = 950)
    }
    else if(input$figures == "Decomp. educational gap"){            
      list(src = "./fig_for_shiny/contr_edu_gap_F-M.svg",
           height = 500, width = 750)
    },
    deleteFile = FALSE
  )
  
  output$caption <- renderText({
    capt[input$figures] # captions
  })
  
  
}  


# Run the app ------------------------------------------------------------------
shinyApp(ui = ui, server = server)

