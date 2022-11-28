library(shiny)
library(hector)
library(gganimate)
library(ggplot2)
library(tidyverse)
library(gifski)

# Define server logic
shinyServer(function(input, output) {
    
    runAnimation <- eventReactive(input$generate, {
      
      # run hector, get tracking data
      print("Loading SSP...")
      ssp_file <- input$ssp_file
      ini_file <- system.file(ssp_file, package = "hector")
      core <- newcore(ini_file)
      tunits <- getunits(TRACKING_DATE())
      setvar(core, NA, TRACKING_DATE(), input$start, tunits)
      reset(core, core$reset_date)
      print("Running Hector...")
      run(core, runtodate = 2100)
      print("Gathering data...")
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
      # filter out fossil fuels option
      if (input$ff == 2) {
        df <- subset(df, source_name!="Fossil Fuels")
      }
      
      # bar width for area plot
      barwidth <- (2100-input$start)/100
      
      # rank column for moving bar plot
      df <- df %>%
        group_by(year) %>%
        mutate(rank = rank(-source_fraction),
               frac_rel = source_fraction/source_fraction[rank==1],
               source_amt = source_fraction*pool_value,
               amt_lbl = paste0(format(round(source_amt,2), nsmall=2), " Pg C"),
               frac_lbl = paste0(format(round(source_fraction,2),nsmall=2))) %>%
        group_by(source_name) %>%
        ungroup()
      
      ## Plot!
      tempdir()
      outfile <- tempfile(tmpdir=tempdir(), fileext='.gif')
      
      print("Plotting...")
      # area plot
      if (input$plotSelect==1) {
        if (input$view == 1) {
          p = ggplot(df, aes(fill=source_name, y=source_amt, x=year, width=barwidth)) +
            geom_bar(position="stack",stat="identity") +
            ggtitle(paste0("Amount of Carbon in ",selectedPool," by Source")) +
            xlab("Year") + ylab("Carbon Pool (Pg C)") +
            # gganimate
            transition_time(year) +
            shadow_mark() +
            ease_aes('linear')
        } else {
          p = ggplot(df, aes(fill=source_name, y=source_frac, x=year, width=barwidth)) +
            geom_bar(position="stack",stat="identity") +
            ggtitle(paste0("Fraction of Carbon in ",selectedPool," by Source")) +
            xlab("Year") + ylab("Carbon Pool (Fraction)") +
            # gganimate
            transition_time(year) +
            shadow_mark() +
            ease_aes('linear')
        }
        anim = p
      }
      # bar plot
      if (input$plotSelect==2) {
        p <- ggplot(df,aes(fill=source_name,color=source_name,
                           x=reorder(source_name,source_amt),
                           y=source_amt)) +
          geom_bar(stat="identity") +
          geom_text(aes(y=0, label = paste(source_name, " ")),
                    vjust = 0.2, hjust = 1, size = 6) +
          geom_text(aes(y = source_amt, label = paste(" ",amt_lbl), hjust=0), size = 6) +
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
          
        # animate
        anim = p + transition_states(year,transition_length=4, state_length=2,wrap=FALSE) +
          view_follow(fixed_x = TRUE) +
          labs(title=paste0(selectedPool," Carbon Sources"),
               subtitle="Year: {closest_state}")
      }
      
      # render animation
      print("Rendering animation...")
      animate(anim, width = 900, height = 600, renderer = gifski_renderer(),end_pause=30)
      anim_save("outfile.gif") # save gif
      
      # file info
      list(src = "outfile.gif",
           contentType = 'image/gif'
           # width = 300
           # height = 300
           # alt = "alt text, edit later!!!"
      )
    })

    output$animation <- renderImage({
        runAnimation()
    }, deleteFile = TRUE)

    output$downloadgif <- downloadHandler(
      filename = "outfile.gif",
      contentType = "image/gif",
      content = function(file) {
        # copy file from image location to download location
        file.copy("outfile.gif",file)})

})
