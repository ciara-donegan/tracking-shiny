#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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
                        min = 1750, max = 2200, value = 1900, sep=""),
            selectInput("pool", label="Select pool to view:",
                        choices = list("High latitude ocean"="HL ocean",
                                       "Low latitude ocean"="LL ocean",
                                       "Intermediate ocean"="Intermediate ocean",
                                       "Deep ocean"="Deep ocean",
                                       "Atmosphere"="Atmosphere",
                                       "Vegetation"="Vegetation",
                                       "Detritus"="Detritus",
                                       "Soil"="Soil"),
                        selected="Atmosphere"),
            radioButtons("view", label="View:",
                         choices = list("Carbon Amount"=1,
                                        "Carbon Fraction"=2),
                         selected = 1),
            #radioButtons("plot", label="View:",
            #             choices = list("Area Gif"=1,
            #                            "Barplot Gif"=2),
            #             selected = 1)#,
            #actionButton("generate_plots","Generate plots (may take time to load)")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("areaStill"),
          imageOutput("areaGif"),
          imageOutput("barGif")
        )
    )
))
