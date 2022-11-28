#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Carbon tracking"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("ssp_file", label="Select SSP:",
                         choices = list("SSP 1-1.9"="input/hector_ssp119.ini",
                                        "SSP 1-2.6"="input/hector_ssp126.ini",
                                        "SSP 2-4.5"="input/hector_ssp245.ini",
                                        "SSP 3-7.0"="input/hector_ssp370.ini",
                                        "SSP 4-3.4"="input/hector_ssp434.ini",
                                        "SSP 4-6.0"="input/hector_ssp460.ini",
                                        "SSP 5-3.4OS"="input/hector_ssp534-over.ini",
                                        "SSP 5-8.5"="input/hector_ssp585.ini"),
                        selected = "input/hector_ssp245.ini"),
            sliderInput("start", label="Select year to begin tracking:",
                        min = 1750, max = 2050, value = 1900, sep=""),
            selectInput("pool", label="Select pool to view:",
                        choices = list("High latitude ocean"="HL Ocean",
                                       "Low latitude ocean"="LL Ocean",
                                       "Intermediate ocean"="Intermediate Ocean",
                                       "Deep ocean"="Deep Ocean",
                                       "Atmosphere"="Atmosphere",
                                       "Vegetation"="Vegetation",
                                       "Detritus"="Detritus",
                                       "Soil"="Soil"),
                        selected="Atmosphere"),
            radioButtons("view", label="View:",
                         choices = list("Carbon Amount"=1,
                                        "Carbon Fraction"=2),
                         selected = 1),
            radioButtons("ff", label="Toggle fossil fuels:",
                         choices = list("On"=1,"Off"=2)),
            radioButtons("plotSelect", label="Select plot to view:",
                         choices = list("Area Plot"=1,
                                        "Bar Plot (Amount Only)"=2),
                         selected=1),
            actionButton("generate","Generate Animation"),
            downloadButton("downloadGif","Download Animation"),
        ),

        # Show animation
        mainPanel(
          #plotOutput("areaStill"),
          withSpinner(imageOutput("animation"), type=7),
          #imageOutput("barGif"),
          #imageOutput("pieGif"),
          #withSpinner(imageOutput("barGif"), type=7),
          #withSpinner(imageOutput("pieGif"), type=7),
          #withSpinner(imageOutput("movingBar"), type=7)
          )
        )
    )
)
