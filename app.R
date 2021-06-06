library(shiny)
library(datasets)
library(tidyverse)
library(gridExtra)

ui <- shinyUI(fluidPage(
    titlePanel("Vibration Plot"),
    tabsetPanel(
        tabPanel("Upload File",
                 titlePanel("Uploading Files"),
                 sidebarLayout(
                     sidebarPanel(
                         fileInput('file1', 'Choose CSV File',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         
                         # added interface for uploading data from
                         # http://shiny.rstudio.com/gallery/file-upload.html
                         tags$br(),
                         checkboxInput('header', 'Header', TRUE),
                         radioButtons('sep', 'Separator',
                                      c(Comma=',',
                                        Semicolon=';',
                                        Tab='\t'),
                                      ','),
                         radioButtons('quote', 'Quote',
                                      c(None='',
                                        'Double Quote'='"',
                                        'Single Quote'="'"),
                                      '"')
                         
                     ),
                     mainPanel(
                         tableOutput('contents')
                     )
                 )
        ),
        tabPanel("Plots",
                 pageWithSidebar(
                     headerPanel('Vibration Analysis Plots'),
                     sidebarPanel(
                         
                         # "Empty inputs" - they will be updated after the data is uploaded
                         selectInput('xcol', 'X Variable', ""),
                         #selectInput('ycol', 'Y Variable', "", selected = "")
                         
                     ),
                     mainPanel(
                         plotOutput('MyPlot')
                     )
                 )
        )
        
    ),
    "If Harmonic Freq==0 or # of Harmonic to search==0,
    Plots don't create any vertical lines."
    ,
    numericInput("x", label = "Sampling Rate (Hz)", min = 1, value = 20000),
    numericInput("y", label= "Harmonic Frequency Selection",min=1,value=300),
    numericInput("z", label= "# of Harmonic to search",min=1,value=3),
    numericInput("t", label= "# of Freq for Spec.ar",min=1,value=1000),
    numericInput("w", label= "AR Order for Spec.ar",min=1,value=100)
    )
)

server <- shinyServer(function(input, output, session) {
    # added "session" because updateSelectInput requires it
    
    
    data <- reactive({ 
        req(input$file1) ## ?req #  require that the input is available
        
        inFile <- input$file1 
        
        # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
        # and                              write.csv(iris, "iris.csv")
        df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                       quote = input$quote)
        
        
        # Update inputs (you could create an observer with both updateSel...)
        # You can also constraint your choices. If you wanted select only numeric
        # variables you could set "choices = sapply(df, is.numeric)"
        # It depends on what do you want to do later on.
        
        updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                          choices = names(df), selected = names(df))

        return(df)
    })
    
    output$contents <- renderTable({
        data()
    })
    
    output$MyPlot <- renderPlot({
        # for a histogram: remove the second variable (it has to be numeric as well):
        # x    <- data()[, c(input$xcol, input$ycol)]
        # bins <- nrow(data())
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
        # Correct way:
        # x    <- data()[, input$xcol]
        # bins <- nrow(data())
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
        
        # I Since you have two inputs I decided to make a scatterplot
        #x <- data()[, c(input$xcol, input$ycol)]
        #plot(x)
        x    <- data()[, input$xcol]
        #x[is.na(x)] = round(mean(x,na.rm=T))
        x<- x-mean(x)
        apply_FFT <- function(x, sampling_rate = input$x)
        {
            # Center the signal first
            x <- x - mean(x)
            # Get the amplitudes
            amp <- fft(x)[1:(length(x) / 2)]%>% # Drop the second half - it's juast a mirror of the first half 
                Mod # Calculate the amplitude of the complex output of the previous line
            
            # Make a vector containing the frequencies
            freq <- seq(0, sampling_rate / 2, length.out = length(x) / 2)
            
            # and make a dataframe out of them (remove the dc term)
            fft_out <- data.frame("amp" = amp[-1] / length(amp), "freq" = freq[-1])
            
            return(fft_out)
        }
        #apply_FFT(x) %>%
        #    ggplot(aes(x = freq, y = amp)) + 
        #    geom_line() + 
        #    ggtitle("Spectral density with our FFT function: test on a vibration signal")+
        #    theme_minimal()
        
        spec <- spec.ar(x = x, 
                        n.freq = input$t,
                        order = input$w, # Use an AR(100) model - might be an overkill
                        plot = F) # Do not plot
        spec <- data.frame("freq" = seq(0, input$x / 2, length.out = length(spec$spec)),
                           "amp" = spec$spec)
        
        #ggplot(spec, aes(x = freq, y = amp)) + 
        #    geom_line() + 
        #    ggtitle("Spectral density using an AR(100) model: test on a vibration signal")
        
        if(input$y == 0 | input$z == 0 ){
            grid.arrange(
                apply_FFT(x) %>%
                    ggplot(aes(x = freq, y = amp)) + 
                    geom_line() + 
                    ggtitle("Spectral density with FFT function")+
                    theme_minimal(),
                ggplot(spec, aes(x = freq, y = amp)) + 
                    geom_line() + 
                    ggtitle(paste("Spectral density using an AR(",as.character(input$w),") model"))+
                    theme_minimal(),
                ncol=1,nrow=2
                
            )
            
        }
        
        else{
            grid.arrange(
                apply_FFT(x) %>%
                    ggplot(aes(x = freq, y = amp)) + 
                    geom_line() + 
                    ggtitle("Spectral density with FFT function")+
                    theme_minimal()+
                    geom_vline(xintercept = c((1:input$z)*input$y),colour="red"),
                ggplot(spec, aes(x = freq, y = amp)) + 
                    geom_line() + 
                    ggtitle(paste("Spectral density using an AR(",as.character(input$w),") model"))+
                    theme_minimal()+
                    geom_vline(xintercept = c((1:input$z)*input$y),colour="red"),
                ncol=1,nrow=2
                
            )
        }
        
    })
})

shinyApp(ui, server)