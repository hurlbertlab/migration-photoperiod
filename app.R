#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    # App title ----
    titlePanel("Photoperiods Experienced by Migratory Birds"),
    
    # Create tabs
    # 1 Between simulation variable plots
    # 2 Between simulation PCA plots
    # 3 Within simulation variable plots
    
    tabsetPanel(
        
        # TAB 1: Migration not spanning Jan 1
        tabPanel("Migration Not Spanning Jan 1",
                 
                 # Sidebar layout with input and output definitions ----
                 sidebarLayout(
                     
                     # Sidebar panel for inputs ----
                     sidebarPanel(
                         
                        selectInput(inputId = "daylightDef",
                                     label = "Daylight Definition",
                                     choices = c("Sun Center at Horizon",
                                                 "Sun Top at Horizon",
                                                 "Sun Top Apparent at Horizon",
                                                 "Civil Twilight Included",
                                                 "Nautical Twilight Included",
                                                 "Astronomical Twilight Included")),
                         
                         sliderInput(inputId = "earlyLateYearLatitude",
                                     label = "Early/Late-Year Latitude",
                                     min = -90,
                                     max = 90,
                                     value = -70),
                         
                         sliderInput(inputId = "midYearLatitude",
                                     label = "Mid-Year Latitude",
                                     min = -90,
                                     max = 90,
                                     value = 70),
                         
                         sliderInput(inputId = "earlyMigBegins",
                                     label = "Early-Year Migration Begins",
                                     min = 1,
                                     max = 358,
                                     value = 93),
                         
                         uiOutput("earlyMigEndsSlider"),
                        
                         uiOutput("lateMigBeginsSlider"),
                        
                         uiOutput("lateMigEndsSlider"), 
                        
                        width = 2
                        
                     ),
                     
                     # Main panel for displaying outputs ----
                     mainPanel(
                         
                         plotOutput("latPlot", height = 250),
                         
                         plotOutput("photoperiodPlot", height = 500),
                         
                         width = 6
                         
                     )
                 ) #end sidebarLayout
        ), #end tab 1
        
        # TAB 2
        tabPanel("Migration Spanning Jan 1",
                 
                 # Sidebar layout with input and output definitions ----
                 sidebarLayout(
                     
                     # Sidebar panel for inputs ----
                     sidebarPanel(
                         
                         helpText("Daylight Definition: Sun Center at Horizon"),
                         
                         sliderInput(inputId = "earlyYearLatitude",
                                     label = "Early-Year Latitude",
                                     min = -90,
                                     max = 90,
                                     value = -70),
                         
                         sliderInput(inputId = "lateYearLatitude",
                                     label = "Late-Year Latitude",
                                     min = -90,
                                     max = 90,
                                     value = 70),
                         
                         sliderInput(inputId = "overYearMigEnds",
                                     label = "Over-Year Migration Ends",
                                     min = 1,
                                     max = 358,
                                     value = 50),
                         
                         uiOutput("midYearMigBeginsSlider"),
                         
                         uiOutput("midYearMigEndsSlider"),
                         
                         uiOutput("overYearMigBeginsSlider"), 
                         
                         width = 2
                         
                     ),
                     
                     # Main panel for displaying outputs ----
                     mainPanel(
                         
                         plotOutput("latPlot2", height = 250),
                         
                         plotOutput("photoperiodPlot2", height = 500),
                         
                         width = 6
                         
                     )
                 ) #end sidebarLayout
        ) #end tab 2

    ) #end tabSetPanel
) #end UI

# Define server logic required to draw a histogram
server <- function(input, output) {

    ## Dynamic sliders
    # Tab 1
    output$earlyMigEndsSlider <- renderUI({
        sliderInput(inputId = "earlyMigEnds", 
                    label = "Early-Year Migration Ends", 
                    min = input$earlyMigBegins + 1, 
                    max = 360, value = 160)
    })
    
    output$lateMigBeginsSlider = renderUI({
        sliderInput(inputId = "lateMigBegins",
                    label = "Late-Year Migration Begins",
                    min = input$earlyMigEnds + 1,
                    max = 362,
                    value = 229)    
    })
    
    output$lateMigEndsSlider = renderUI({
        sliderInput(inputId = "lateMigEnds",
                    label = "Late-Year Migration Ends",
                    min = input$lateMigBegins + 1,
                    max = 364,
                    value = 309)
    })
    
    # Tab 2
    output$midYearMigBeginsSlider <- renderUI({
        sliderInput(inputId = "midYearMigBegins",
                    label = "Mid-Year Migration Begins",
                    min = input$overYearMigEnds + 1,
                    max = 360,
                    value = 190)
    })
    
    output$midYearMigEndsSlider = renderUI({
        sliderInput(inputId = "midYearMigEnds",
                    label = "Mid-Year Migration Ends",
                    min = input$midYearMigBegins + 1,
                    max = 362,
                    value = 250)    
    })
    
    output$overYearMigBeginsSlider = renderUI({
        sliderInput(inputId = "overYearMigBegins",
                    label = "Over-Year Migration Begins",
                    min = input$midYearMigEnds + 1,
                    max = 364,
                    value = 345)
    })
    

    # Plot latitude as a function of julianday based on timing and latitudes of migration
    output$latPlot <- renderPlot({
        
        tickVals = c(79, 172, 265, 355)
        dateLabels = c("Mar 20", "Jun 21", "Sep 22", "Dec 21")
        par(mar = c(3, 3, 3, 1), cex.lab = 1.75)
        plot(c(0,365), c(-90, 90), type = 'n', xaxt = 'n', yaxt = 'n', bty = 'n', 
             xlab = '', ylab = '', xaxs = 'i', main = "Degrees Latitude", cex.main = 1.5)
        axis(2, labels = seq(-90, 90, by = 30), at = seq(-90, 90, by = 30), las = 1, line = 0)
        segments(-10, 0, 370, 0)
        segments(tickVals, rep(0, 4), tickVals, rep(3, 4))
        text(tickVals, rep(-10, 4), dateLabels)
        
        # Early latitude
        segments(0, input$earlyLateYearLatitude, input$earlyMigBegins, input$earlyLateYearLatitude, lwd = 4)
        segments(input$earlyMigBegins, input$earlyLateYearLatitude, input$earlyMigEnds, input$midYearLatitude, 
                 lwd = 2, lty = 'dotted')
        # Breeding latitude
        segments(input$earlyMigEnds, input$midYearLatitude, input$lateMigBegins, input$midYearLatitude, lwd = 4)
        segments(input$lateMigBegins, input$midYearLatitude, input$lateMigEnds, input$earlyLateYearLatitude, 
                 lwd = 2, lty = 'dotted')
        # Late latitude
        segments(input$lateMigEnds, input$earlyLateYearLatitude, 366, input$earlyLateYearLatitude, lwd = 4)
        
    })
    
    # Plot photoperiod as a function of julianday based on timing and latitude of migration
    output$photoperiodPlot <- renderPlot({
        
        daylightDef <- switch(input$daylightDef, 
                              "Sun Center at Horizon" = 0,
                              "Sun Top at Horizon" = 0.26667,
                              "Sun Top Apparent at Horizon" = 0.8333,
                              "Civil Twilight Included" = 6,
                              "Nautical Twilight Included" = 12,
                              "Astronomical Twilight Included" = 18)
        
        
        tickVals = c(79, 172, 265, 355)
        dateLabels = c("Mar 20", "Jun 21", "Sep 22", "Dec 21")
        par(mar = c(8, 3, 1, 1), cex.lab = 1.75)
        plot(c(0,365), c(0, 24), type = 'n', xaxt = 'n', yaxt = 'n', bty = 'n', 
             xlab = '', ylab = '', xaxs = 'i', main = "Hours of Daylight", cex.main = 1.5)
        axis(2, labels = seq(0, 24, by = 2), at = seq(0, 24, by = 2), las = 1, line = 0)
        segments(-10, 12, 370, 12)
        segments(tickVals, rep(12, 4), tickVals, rep(12.4, 4))
        text(tickVals, rep(11.2, 4), dateLabels)
        
        # Photoperiod plotting
        Clip = function(x) { y = x; y[y < -1] = -1; y[y > 1] = 1; return (y)}
        
        jd = 1:365
        P = asin(0.39795*cos(0.2163108+2*atan(0.9671396*tan(0.0086*(jd - 186)))))
        AEL = (sin(daylightDef*pi/180) + sin(input$earlyLateYearLatitude*pi/180)*sin(P))/(cos(input$earlyLateYearLatitude*pi/180)*cos(P))
        CAEL = Clip(AEL)
        AML = (sin(daylightDef*pi/180) + sin(input$midYearLatitude*pi/180)*sin(P))/(cos(input$midYearLatitude*pi/180)*cos(P))
        CAML = Clip(AML)
        AMEL = (sin(daylightDef*pi/180) + sin(((((input$earlyLateYearLatitude*jd) - (input$earlyLateYearLatitude*input$lateMigBegins) - (input$midYearLatitude*jd) + (input$midYearLatitude*input$lateMigBegins))/(input$lateMigEnds - input$lateMigBegins)) + input$midYearLatitude)*pi/180)*sin(P))/(cos(((((input$earlyLateYearLatitude*jd) - (input$earlyLateYearLatitude*input$lateMigBegins) - (input$midYearLatitude*jd) + (input$midYearLatitude*input$lateMigBegins))/(input$lateMigEnds - input$lateMigBegins)) + input$midYearLatitude)*pi/180)*cos(P))
        CAMEL = Clip(AMEL)
        AMML = (sin(daylightDef*pi/180) + sin(((((input$midYearLatitude*jd) - (input$midYearLatitude*input$earlyMigBegins) - (input$earlyLateYearLatitude*jd) + (input$earlyLateYearLatitude*input$earlyMigBegins))/(input$earlyMigEnds - input$earlyMigBegins)) + input$earlyLateYearLatitude)*pi/180)*sin(P))/(cos(((((input$midYearLatitude*jd) - (input$midYearLatitude*input$earlyMigBegins) - (input$earlyLateYearLatitude*jd) + (input$earlyLateYearLatitude*input$earlyMigBegins))/(input$earlyMigEnds - input$earlyMigBegins)) + input$earlyLateYearLatitude)*pi/180)*cos(P))
        CAMML = Clip(AMML)
        
        # Photoperiods at the breeding and non-breeding latitudes
        points(jd, 24-(24/pi)*acos(CAEL), type = 'l')
        points(jd, 24-(24/pi)*acos(CAML), type = 'l')
        
        # Photoperiods during migratory segments
        points(1:input$earlyMigBegins, 24-(24/pi)*acos(CAEL[1:input$earlyMigBegins]), type = 'l', lwd = 4)
        points(input$earlyMigBegins:input$earlyMigEnds, 24-(24/pi)*acos(CAMML[input$earlyMigBegins:input$earlyMigEnds]), type = 'l', lty = 'dotted', lwd = 2)
        points(input$earlyMigEnds:input$lateMigBegins, 24-(24/pi)*acos(CAML[input$earlyMigEnds:input$lateMigBegins]), type = 'l', lwd = 4)
        points(input$lateMigBegins:input$lateMigEnds, 24-(24/pi)*acos(CAMEL[input$lateMigBegins:input$lateMigEnds]), type = 'l', lty = 'dotted', lwd = 2)
        points(input$lateMigEnds:365, 24-(24/pi)*acos(CAEL[input$lateMigEnds:365]), type = 'l', lwd = 4)      
        
        propYearDaylight = (sum(24-(24/pi)*acos(CAEL[1:input$earlyMigBegins])) +
            sum(24-(24/pi)*acos(CAMML[(input$earlyMigBegins+1):input$earlyMigEnds])) +
            sum(24-(24/pi)*acos(CAML[(input$earlyMigEnds+1):input$lateMigBegins])) +
            sum(24-(24/pi)*acos(CAMEL[(input$lateMigBegins+1):input$lateMigEnds])) +
            sum(24-(24/pi)*acos(CAEL[(input$lateMigEnds+1):365])))/8760
        
        hoursDaylightEarlyLat = sum(24-(24/pi)*acos(CAEL[1:input$earlyMigBegins])) +
            sum(24-(24/pi)*acos(CAEL[(input$lateMigEnds+1):365]))
        
        hoursDaylightMidLat = sum(24-(24/pi)*acos(CAML[(input$earlyMigEnds+1):input$lateMigBegins]))
        
        mtext(paste("Proportion of Year in Daylight:", round(propYearDaylight, 2)), 1, line = 1, cex = 1.4)
        mtext(paste("Hours Daylight at Early/Late-Year Latitude:", round(hoursDaylightEarlyLat)), 1, line = 2.5, cex = 1.4)
        mtext(paste("Hours Daylight at Mid-Year Latitude:", round(hoursDaylightMidLat)), 1, line = 4, cex = 1.4)
    })
   
    #####################################################################################
    # PLOTTING FOR MIGRATION SPANNING JANUARY 1
    # Plot latitude as a function of julianday based on timing and latitudes of migration
    output$latPlot2 <- renderPlot({
        
        tickVals = c(79, 172, 265, 355)
        dateLabels = c("Mar 20", "Jun 21", "Sep 22", "Dec 21")
        par(mar = c(3, 3, 3, 1), cex.lab = 1.75)
        plot(c(0,365), c(-90, 90), type = 'n', xaxt = 'n', yaxt = 'n', bty = 'n', 
             xlab = '', ylab = '', xaxs = 'i', main = "Degrees Latitude", cex.main = 1.5)
        axis(2, labels = seq(-90, 90, by = 30), at = seq(-90, 90, by = 30), las = 1, line = 0)
        segments(-10, 0, 370, 0)
        segments(tickVals, rep(0, 4), tickVals, rep(3, 4))
        text(tickVals, rep(-10, 4), dateLabels)
        
        # y-intercept
        yint = input$lateYearLatitude + ((input$earlyYearLatitude - input$lateYearLatitude)/(input$overYearMigEnds - (input$overYearMigBegins - 365)))*(365 - input$overYearMigBegins)
                                         
        segments(0, yint, input$overYearMigEnds, input$earlyYearLatitude, lwd = 2, lty = 'dotted')
        segments(input$overYearMigEnds, input$earlyYearLatitude, input$midYearMigBegins, input$earlyYearLatitude, lwd = 4)
        segments(input$midYearMigBegins, input$earlyYearLatitude, input$midYearMigEnds, input$lateYearLatitude, lwd = 2, lty = 'dotted')
        segments(input$midYearMigEnds, input$lateYearLatitude, input$overYearMigBegins, input$lateYearLatitude, lwd = 4)
        segments(input$overYearMigBegins, input$lateYearLatitude, 365, yint, lwd = 2, lty = 'dotted')
        
    })
    
    # Plot photoperiod as a function of julianday based on timing and latitude of migration
    output$photoperiodPlot2 <- renderPlot({
        
        daylightDef <- switch(input$daylightDef, 
                              "Sun Center at Horizon" = 0,
                              "Sun Top at Horizon" = 0.26667,
                              "Sun Top Apparent at Horizon" = 0.8333,
                              "Civil Twilight Included" = 6,
                              "Nautical Twilight Included" = 12,
                              "Astronomical Twilight Included" = 18)
        
        
        tickVals = c(79, 172, 265, 355)
        dateLabels = c("Mar 20", "Jun 21", "Sep 22", "Dec 21")
        par(mar = c(8, 3, 1, 1), cex.lab = 1.75)
        plot(c(0,365), c(0, 24), type = 'n', xaxt = 'n', yaxt = 'n', bty = 'n', 
             xlab = '', ylab = '', xaxs = 'i', main = "Hours of Daylight", cex.main = 1.5)
        axis(2, labels = seq(0, 24, by = 2), at = seq(0, 24, by = 2), las = 1, line = 0)
        segments(-10, 12, 370, 12)
        segments(tickVals, rep(12, 4), tickVals, rep(12.4, 4))
        text(tickVals, rep(11.2, 4), dateLabels)
        
        # Photoperiod plotting
        Clip = function(x) { y = x; y[y < -1] = -1; y[y > 1] = 1; return (y)}
        
        jd = 1:365
        P = asin(0.39795*cos(0.2163108+2*atan(0.9671396*tan(0.0086*(jd - 186)))))
        AEL = (sin(daylightDef*pi/180) + sin(input$earlyYearLatitude*pi/180)*sin(P))/(cos(input$earlyYearLatitude*pi/180)*cos(P))
        CAEL = Clip(AEL)
        AML = (sin(daylightDef*pi/180) + sin(input$lateYearLatitude*pi/180)*sin(P))/(cos(input$lateYearLatitude*pi/180)*cos(P))
        CAML = Clip(AML)
        AMEL = (sin(daylightDef*pi/180) + sin(((((input$lateYearLatitude - input$earlyYearLatitude)*(jd - (input$overYearMigBegins - 365)))/(input$overYearMigEnds - (input$overYearMigBegins - 365))) + input$earlyYearLatitude)*pi/180)*sin(P))/(cos(((((input$lateYearLatitude - input$earlyYearLatitude)*(jd - (input$overYearMigBegins - 365)))/(input$overYearMigEnds - (input$overYearMigBegins - 365))) + input$earlyYearLatitude)*pi/180)*cos(P))
        CAMEL = Clip(AMEL)
        AMML = (sin(daylightDef*pi/180) + sin(((((input$earlyYearLatitude*jd) - (input$earlyYearLatitude*input$midYearMigBegins) - (input$lateYearLatitude*jd) + (input$lateYearLatitude*input$midYearMigBegins))/(input$midYearMigEnds - input$midYearMigBegins)) + input$lateYearLatitude)*pi/180)*sin(P))/(cos(((((input$earlyYearLatitude*jd) - (input$earlyYearLatitude*input$midYearMigBegins) - (input$lateYearLatitude*jd) + (input$lateYearLatitude*input$midYearMigBegins))/(input$midYearMigEnds - input$midYearMigBegins)) + input$lateYearLatitude)*pi/180)*cos(P))
        CAMML = Clip(AMML)
        AMELTWO = (sin(daylightDef*pi/180) + sin(((((input$earlyYearLatitude - input$lateYearLatitude)*(jd - input$overYearMigBegins))/(input$overYearMigBegins - (365 + input$overYearMigEnds))) + input$earlyYearLatitude)*pi/180)*sin(P))/(cos(((((input$earlyYearLatitude - input$lateYearLatitude)*(jd - input$overYearMigBegins))/(input$overYearMigBegins - (365 + input$overYearMigEnds))) + input$earlyYearLatitude)*pi/180)*cos(P))
        CAMELTWO = Clip(AMELTWO)
        
        
        # Photoperiods at the breeding and non-breeding latitudes
        points(jd, 24-(24/pi)*acos(CAEL), type = 'l')
        points(jd, 24-(24/pi)*acos(CAML), type = 'l')
        
        # Photoperiods during migratory segments
        points(1:input$overYearMigEnds, 24-(24/pi)*acos(CAMEL[1:input$overYearMigEnds]), type = 'l', lty = 'dotted', lwd = 2)
        points((input$overYearMigEnds+1):input$midYearMigBegins, 24-(24/pi)*acos(CAML[(input$overYearMigEnds+1):input$midYearMigBegins]), type = 'l', lwd = 4)
        points((input$midYearMigBegins+1):input$midYearMigEnds, 24-(24/pi)*acos(CAMML[(input$midYearMigBegins+1):input$midYearMigEnds]), type = 'l', lty = 'dotted', lwd = 2)
        points((input$midYearMigEnds+1):input$overYearMigBegins, 24-(24/pi)*acos(CAEL[(input$midYearMigEnds+1):input$overYearMigBegins]), type = 'l', lwd = 4)
        points((input$overYearMigBegins+1):365, 24-(24/pi)*acos(CAMELTWO[(input$overYearMigBegins+1):365]), type = 'l', lwd = 2, lty = 'dotted')      
        
        propYearDaylight = (sum(24-(24/pi)*acos(CAMEL[1:input$overYearMigEnds])) +
                                sum(24-(24/pi)*acos(CAML[(input$overYearMigEnds+1):input$midYearMigBegins])) +
                                sum(24-(24/pi)*acos(CAMML[(input$midYearMigBegins+1):input$midYearMigEnds])) +
                                sum(24-(24/pi)*acos(CAEL[(input$midYearMigEnds+1):input$overYearMigBegins])) +
                                sum(24-(24/pi)*acos(CAMELTWO[(input$overYearMigBegins+1):365])))/8760
        
        hoursDaylightEarlyLat = sum(24-(24/pi)*acos(CAML[(input$overYearMigEnds+1):input$midYearMigBegins]))
        
        hoursDaylightLateLat = sum(24-(24/pi)*acos(CAEL[(input$midYearMigEnds+1):input$overYearMigBegins]))
        
        mtext(paste("Proportion of Year in Daylight:", round(propYearDaylight, 2)), 1, line = 1, cex = 1.4)
        mtext(paste("Hours Daylight at Early-Year Latitude:", round(hoursDaylightEarlyLat)), 1, line = 2.5, cex = 1.4)
        mtext(paste("Hours Daylight at Late-Year Latitude:", round(hoursDaylightLateLat)), 1, line = 4, cex = 1.4)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
