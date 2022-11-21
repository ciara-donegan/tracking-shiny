library(shiny)
library(hector)
library(gganimate)
library(ggplot2)
library(tidyverse)
library(gifski)

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
      df[df=="deep"] <- "Deep Ocean"
      df[df=="detritus_c"] <- "Detritus"
      df[df=="earth_c"] <- "Fossil Fuels"
      df[df=="HL"] <- "HL Ocean"
      df[df=="intermediate"] <- "Intermediate Ocean"
      df[df=="LL"] <- "LL Ocean"
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
      
      if (input$view == 1) {
        p = ggplot(df, aes(fill=Source, y=source_amt, x=year, width=barwidth)) +
          geom_bar(position="stack",stat="identity") +
          ggtitle(paste0("Amount of Carbon in ",selectedPool," by Source")) +
          xlab("Year") + ylab("Carbon Pool (Pg C)") +
          # gganimate
          transition_time(year) +
          shadow_mark() +
          ease_aes('linear')
      } else {
        p = ggplot(df, aes(fill=Source, y=source_frac, x=year, width=barwidth)) +
          geom_bar(position="stack",stat="identity") +
          ggtitle(paste0("Fraction of Carbon in ",selectedPool," by Source")) +
          xlab("Year") + ylab("Carbon Pool (Fraction)") +
          # gganimate
          transition_time(year) +
          shadow_mark() +
          ease_aes('linear')
      }
      
      #animate(p, height = 400, width = 800)
      animate(p, height=400, width=800, renderer = gifski_renderer())
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
      df[df=="deep"] <- "Deep Ocean"
      df[df=="detritus_c"] <- "Detritus"
      df[df=="earth_c"] <- "Fossil Fuels"
      df[df=="HL"] <- "HL Ocean"
      df[df=="intermediate"] <- "Intermediate Ocean"
      df[df=="LL"] <- "LL Ocean"
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
      df[df=="deep"] <- "Deep Ocean"
      df[df=="detritus_c"] <- "Detritus"
      df[df=="earth_c"] <- "Fossil Fuels"
      df[df=="HL"] <- "HL Ocean"
      df[df=="intermediate"] <- "Intermediate Ocean"
      df[df=="LL"] <- "LL Ocean"
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
      
      animate(p, height=400, width=800, renderer = gifski_renderer())
      anim_save("outfile.gif") # save gif
      
      # return list with filename
      list(src = "outfile.gif",
           contentType = 'image/gif'
           # width = 300
           # height = 300
           # alt = "alt text, edit later!!!"
      )}, deleteFile = TRUE)
    
    output$pieGif <- renderImage({
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
      df[df=="deep"] <- "Deep Ocean"
      df[df=="detritus_c"] <- "Detritus"
      df[df=="earth_c"] <- "Fossil Fuels"
      df[df=="HL"] <- "HL Ocean"
      df[df=="intermediate"] <- "Intermediate Ocean"
      df[df=="LL"] <- "LL Ocean"
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
        p <- ggplot(df,aes(x="",y=source_frac,fill=Source)) +
          geom_bar(stat="identity",width=1) +
          coord_polar("y",start=0) +
          # gganimate
          labs(title="Year: {frame_time}") +
          transition_time(year) +
          ease_aes('linear')}
      
      animate(p, renderer = gifski_renderer())
      anim_save("outfile.gif") # save gif
      
      # return list with filename
      list(src = "outfile.gif",
           contentType = 'image/gif'
           # width = 300
           # height = 300
           # alt = "alt text, edit later!!!"
      )}, deleteFile = TRUE)
    
    output$movingBar <- renderImage({
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
      df[df=="deep"] <- "Deep Ocean"
      df[df=="detritus_c"] <- "Detritus"
      df[df=="earth_c"] <- "Fossil Fuels"
      df[df=="HL"] <- "HL Ocean"
      df[df=="intermediate"] <- "Intermediate Ocean"
      df[df=="LL"] <- "LL Ocean"
      df[df=="soil_c"] <- "Soil"
      df[df=="veg_c"] <- "Vegetation"
      
      ## filter df to just selected pool
      selectedPool <- input$pool
      df <- filter(df,pool_name==selectedPool)
      if (input$ff == 2) {
        df <- subset(df, source_name!="Fossil Fuels")
      }
      
      # add rank column
      df <- df %>%
        group_by(year) %>%
        mutate(rank = rank(-source_fraction),
               frac_rel = source_fraction/source_fraction[rank==1],
               source_amt = source_fraction*pool_value,
               amt_lbl = paste0(format(round(source_amt,2), nsmall=2), " Pg C"),
               frac_lbl = paste0(format(round(source_fraction,2),nsmall=2))) %>%
        group_by(source_name) %>%
        ungroup()
      
      #
      #source_frac <- df$source_fraction
      #source_amt <- source_frac*df$pool_value
      #Source <- df$source_name
      #year <- df$year
      
      ## Plot!
      tempdir()
      outfile <- tempfile(tmpdir=tempdir(), fileext='.gif')
      
      # Fixed bar order
      if (input$view == 1) {
       p <- ggplot(df,aes(fill=source_name,color=source_name,
                          x=reorder(source_name,source_amt),
                          y=source_amt)) +
         geom_bar(stat="identity") +
         geom_text(aes(y=0, label = paste(source_name, " ")),
                   vjust = 0.2, hjust = 1, size = 6) +
         geom_text(aes(y = source_amt, label = paste(" ",amt_lbl), hjust=0), size = 6) +
         #ylim(0,max(df$source_amt)+100) +
         coord_flip(clip = "off", expand = FALSE) +
         theme_void() +
         theme(plot.title = element_text(size=20,face="bold"),
               plot.subtitle = element_text(size=18),
               legend.position="none",
               panel.grid.major.x = element_line(size=.1,color="snow2"),
               panel.grid.minor.x = element_line(size=.1,color="snow2"),
               plot.margin = margin(1,6,1,6,"cm")) +
         ylab("Carbon (Pg)") +
         xlab("") +
         
         # gganimate
         transition_time(year) +
         ease_aes('linear')
      } else {
        p <- ggplot(df,aes(fill=source_name,color=source_name,
                           x=reorder(source_name,source_fraction),
                           y=source_fraction)) +
          geom_bar(stat="identity") +
          geom_text(aes(y = source_fraction, label = paste(" ",frac_lbl), hjust=0), size = 6) +
          #ylim(0,1) +
          coord_flip(clip = "off", expand = FALSE) +
          theme_void() +
          theme(plot.title = element_text(size=20,face="bold"),
                plot.subtitle = element_text(size=18),
                legend.position="none",
                panel.grid.major.x = element_line(size=.1,color="snow2"),
                panel.grid.minor.x = element_line(size=.1,color="snow2"),
                plot.margin = margin(1,6,1,6,"cm")) +
          ylab("Carbon Fraction") +
          xlab("") +
          
          # gganimate
          transition_time(year) +
          ease_aes('linear')
      }
      
      # # Moving bar order
      # if (input$view == 1) {
      #   p <- ggplot(df, aes(-rank, group = source_name,
      #                       fill = as.factor(source_name),
      #                       color = as.factor(source_name))) +
      #     geom_tile(aes(y = source_amt/2,
      #                   height = source_amt,
      #                   width = 0.9), alpha = 0.8, color = NA) +
      #     geom_text(aes(y=0, label = paste(source_name, " ")),
      #               vjust = 0.2, hjust = 1, size = 6) +
      #     geom_text(aes(y = source_amt, label = paste(" ",amt_lbl), hjust=0), size = 6) +
      #     ylim(0,max(df$source_amt)+100) +
      #     coord_flip(clip = "off", expand = FALSE) +
      #     theme(axis.text.y=element_blank(),
      #           axis.ticks.y=element_blank(),
      #           legend.position="none",
      #           plot.margin = margin(4,4,4,6,"cm")) +
      #     ylab("Carbon (Pg)") +
      #     xlab("") +
      #     # gganimate
      #     transition_time(year) +
      #     ease_aes('linear')
      # } else {
      #   p <- ggplot(df, aes(-rank, group = source_name,
      #                       fill = as.factor(source_name),
      #                       color = as.factor(source_name))) +
      #     geom_tile(aes(y = source_fraction/2,
      #                   height = source_fraction,
      #                   width = 0.9), alpha = 0.8, color = NA) +
      #     geom_text(aes(y=0, label = paste(source_name, " ")),
      #               vjust = 0.2, hjust = 1, size = 6) +
      #     geom_text(aes(y = source_fraction, label = paste(" ",frac_lbl), hjust=0),
      #               size = 6) +
      #     ylim(0,1) +
      #     coord_flip(clip = "off", expand = FALSE) +
      #     theme(axis.text.y=element_blank(),
      #           axis.ticks.y=element_blank(),
      #           legend.position="none",
      #           plot.margin = margin(4,4,4,6,"cm")) +
      #     ylab("Carbon Fraction") +
      #     xlab("") +
      #   # gganimate
      #   transition_time(year) +
      #     ease_aes('linear')
      # }
      
      # animation
      anim = p + transition_states(year,transition_length=4, state_length=2,wrap=FALSE) +
        view_follow(fixed_x = TRUE) +
        labs(title=paste0(selectedPool," Carbon Sources"),
             subtitle="Year: {closest_state}")
      
      # render
      animate(anim, width = 900, height = 600, renderer = gifski_renderer(),end_pause=30)
      anim_save("outfile.gif") # save gif
      
      # file info
      list(src = "outfile.gif",
           contentType = 'image/gif'
           # width = 300
           # height = 300
           # alt = "alt text, edit later!!!"
      )}, deleteFile = FALSE)
    
    output$downloadgif <- downloadHandler(
      filename = "outfile.gif",
      contentType = "image/gif",
      content = function(file) {
        # copy file from image location to download location
        file.copy("outfile.gif",file)
      }
    )
    })