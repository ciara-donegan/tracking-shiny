#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(hector)
library(gganimate)
library(ggplot2)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$gif <- renderImage({
      
      # run hector, get tracking data
      ssp_file <- input$ssp_file
      ini_file <- system.file(ssp_file, package = "hector")
      core <- newcore(ini_file)
      tunits <- getunits(TRACKING_DATE())
      setvar(core, NA, TRACKING_DATE(), input$start, tunits)
      reset(core, core$reset_date)
      run(core, runtodate = 2300)
      df <- get_tracking_data(core)
      
      # clean up pool names
      df[df=="atmos_co2"] <- "Atmosphere"
      df[df=="deep"] <- "Deep ocean"
      df[df=="detritus_c"] <- "Detritus"
      df[df=="earth_c"] <- "Fossil fuels"
      df[df=="HL"] <- "High latitude ocean"
      df[df=="intermediate"] <- "Intermediate ocean"
      df[df=="LL"] <- "Low latitude ocean"
      df[df=="soil_c"] <- "Soil"
      df[df=="veg_c"] <- "Vegetation"
      
      ## get vars
      selectedPool <- input$pool
      df <- filter(df,pool_name==selectedPool)
      source_frac <- df$source_fraction
      source_amt <- source_frac*df$pool_value
      Source <- df$source_name
      year <- df$year
      
      ## Plot!
      if (input$view == 1) {
        p = ggplot(df, aes(fill=Source, y=source_frac, x=year, width=1)) +
              geom_bar(position="stack",stat="identity") +
              ggtitle(paste0("Fraction of Carbon in ",selectedPool," by Source")) +
              xlab("Year") + ylab("Carbon Pool (Fraction)") +
              # gganimate
              transition_time(year) +
              shadow_mark() +
              ease_aes('linear')
      } else {
        p = ggplot(df, aes(fill=Source, y=source_amt, x=year, width=1)) +
              geom_bar(position="stack",stat="identity") +
              ggtitle(paste0("Amount of Carbon in ",selectedPool," by Source")) +
              xlab("Year") + ylab("Carbon Pool (Pg C)") +
              # gganimate
              transition_time(year) +
              shadow_mark() +
              ease_aes('linear')
      }
      
      anim_save("outfile.gif",animate(p)) # save gif
      
      # return list with filename
      list(src = "outfile.gif",
           contentType = 'image/gif'
           # width = 300
           # height = 300
           # alt = "alt text, edit later!!!"
           )}, deleteFile = TRUE)

    output$still <- renderPlot({
      
      # run hector, get tracking data
      ssp_file <- input$ssp_file
      ini_file <- system.file(ssp_file, package = "hector")
      core <- newcore(ini_file)
      tunits <- getunits(TRACKING_DATE())
      setvar(core, NA, TRACKING_DATE(), input$start, tunits)
      reset(core, core$reset_date)
      run(core, runtodate = 2300)
      df <- get_tracking_data(core)
      
      # clean up pool names
      df[df=="atmos_co2"] <- "Atmosphere"
      df[df=="deep"] <- "Deep ocean"
      df[df=="detritus_c"] <- "Detritus"
      df[df=="earth_c"] <- "Fossil fuels"
      df[df=="HL"] <- "High latitude ocean"
      df[df=="intermediate"] <- "Intermediate ocean"
      df[df=="LL"] <- "Low latitude ocean"
      df[df=="soil_c"] <- "Soil"
      df[df=="veg_c"] <- "Vegetation"
      
      ## get vars
      selectedPool <- input$pool
      df <- filter(df,pool_name==selectedPool)
      source_frac <- df$source_fraction
      source_amt <- source_frac*df$pool_value
      Source <- df$source_name
      year <- df$year
      
      ## Plot!
      if (input$view == 1) {
        ggplot(df, aes(fill=Source, y=source_frac, x=year, width=1)) +
          geom_bar(position="stack",stat="identity") +
          ggtitle(paste0("Fraction of Carbon in ",selectedPool," by Source")) +
          xlab("Year") + ylab("Carbon Pool (Fraction)")
      } else {
        ggplot(df, aes(fill=Source, y=source_amt, x=year, width=1)) +
          geom_bar(position="stack",stat="identity") +
          ggtitle(paste0("Amount of Carbon in ",selectedPool," by Source")) +
          xlab("Year") + ylab("Carbon Pool (Pg C)")
      }
    })

    })