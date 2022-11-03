library(shiny)
library(hector)
library(gganimate)
library(ggplot2)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$areaGif <- renderImage({
      
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
      
      barwidth <- (2300-input$start)/100
      
      ## Plot!
      tempdir()
      outfile <- tempfile(tmpdir=tempdir(), fileext='.gif')
      
      if (input$view == 2) {
        p = ggplot(df, aes(fill=Source, y=source_frac, x=year, width=barwidth)) +
              geom_bar(position="stack",stat="identity") +
              ggtitle(paste0("Fraction of Carbon in ",selectedPool," by Source")) +
              xlab("Year") + ylab("Carbon Pool (Fraction)") +
              # gganimate
              transition_time(year) +
              shadow_mark() +
              ease_aes('linear')
      } else {
        p = ggplot(df, aes(fill=Source, y=source_amt, x=year, width=barwidth)) +
              geom_bar(position="stack",stat="identity") +
              ggtitle(paste0("Amount of Carbon in ",selectedPool," by Source")) +
              xlab("Year") + ylab("Carbon Pool (Pg C)") +
              # gganimate
              transition_time(year) +
              shadow_mark() +
              ease_aes('linear')
      }
      
      animate(p, height = 400, width = 800)
      anim_save("outfile.gif") # save gif
      
      # return list with filename
      list(src = "outfile.gif",
           contentType = 'image/gif'
           # width = 300
           # height = 300
           # alt = "alt text, edit later!!!"
           )}, deleteFile = TRUE)

    output$areaStill <- renderPlot({
      
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
      if (input$view == 2) {
        ggplot(df, aes(fill=Source, y=source_frac, x=year, width=1)) +
          geom_area(stat="identity") +
          ggtitle(paste0("Fraction of Carbon in ",selectedPool," by Source")) +
          xlab("Year") + ylab("Carbon Pool (Fraction)")
      } else {
        ggplot(df, aes(fill=Source, y=source_amt, x=year, width=1)) +
          geom_area(stat="identity") +
          ggtitle(paste0("Amount of Carbon in ",selectedPool," by Source")) +
          xlab("Year") + ylab("Carbon Pool (Pg C)")
      }
    })

    output$barGif <- renderImage({
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
      tempdir()
      outfile <- tempfile(tmpdir=tempdir(), fileext='.gif')
      
      if (input$view == 2) {
        p <- ggplot(df,aes(fill=Source,x=Source,y=source_frac)) +
          geom_bar(stat="identity") +
          ylim(0,max(source_frac)) +
          # gganimate
          labs(title="Year: {frame_time}",y="Carbon Pool (Fraction)") +
          transition_time(year) +
          ease_aes('linear')
      } else {
        p <- ggplot(df,aes(fill=Source,x=Source,y=source_amt)) +
          geom_bar(stat="identity") +
          ylim(0,max(source_amt)) +
          # gganimate
          labs(title="Year: {frame_time}",y="Carbon Pool (Pg)") +
          transition_time(year) +
          ease_aes('linear')
      }
      
      animate(p, height = 400, width = 800)
      anim_save("outfile.gif") # save gif
      
      # return list with filename
      list(src = "outfile.gif",
           contentType = 'image/gif'
           # width = 1000
           # height = 300
           # alt = "alt text, edit later!!!"
      )}, deleteFile = TRUE)
    })