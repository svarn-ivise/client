library(shiny)
library(RMySQL)
library(httr)
library(dplyr)
library(jsonlite)
library(taucharts)
library(highcharter)

#######CLIENT VIEW
total <- data.frame(day = seq(Sys.Date(),Sys.Date() + 365,by = 1))

x <- seq(0,2*pi,length.out=nrow(total))
total$Quantity <- (cos(x)* 50+50) * runif(nrow(total),.65,1.35)
options(shiny.trace = TRUE, shiny.sanitize.errors = FALSE)

total$type <- "Demand"
df <- rbind(total, data.frame(Quantity = if_else(total$Quantity/1.15 < 35, 35, total$Quantity/1.15),
                           day = total$day,
                           type = "Price"))

forecast_demand <- function(trip.dt){
  start.dt <- Sys.Date() 
  #trip.dt <- Sys.Date() + 20
  n.days <- trip.dt - start.dt
  
  T<-50
  N0<-runif(1,0,50)
  K<-runif(1,130,170)
  rmax.mean<-0.2
  rmax.sd<-0.2
  rmax<- runif(T,0,.2)#rnorm(T,rmax.mean,rmax.sd)
  t<-N<-array(dim=T+1)
  
  #first element is initial value
  N[1]<-N0
  t[1]<-0
  for (i in 1:T)
  {
    N[i+1]<-N[i]+rmax[i]*N[i]*(1-N[i]/K)
    t[i+1]=t[i]+1
  }
  #make it pretty
  t <- seq(trip.dt - 50,trip.dt,"1 day")
  df <- data.frame(Date=t,Quantity=N)
  
  return(df)
  
}

toggle <- "<input type='checkbox' id='inventory_type' checked data-toggle='toggle' data-on='On' 
                                   data-off='Off' data-onstyle='success' data-offstyle='danger' data-width='120'>"

settings <- c("Service has sold out", 
              "Service is 80% Capacity", 
              "Pricing Model has been updated", 
              "Demand forecasts updated",
              "Forecasted Demand & Price")

toggle <- data.frame(Option = settings, Toggle = toggle, Settings = NA)
toggle <- capture.output(print(xtable::xtable(toggle),type="html", 
                               html.table.attributes="class=settings-table",
                               include.rownames = FALSE, 
                               sanitize.text.function = function(x){x}))

ui <- fluidPage(
  tags$head(
    HTML('<link href="https://fonts.googleapis.com/css?family=Kodchasan" rel="stylesheet">
          <link href=\"https://gitcdn.github.io/bootstrap-toggle/2.2.2/css/bootstrap-toggle.min.css\" rel=\"stylesheet\">
          <script src=\"https://gitcdn.github.io/bootstrap-toggle/2.2.2/js/bootstrap-toggle.min.js\"></script>
         '),
    
    tags$link(rel = "stylesheet", type = "text/css", href = "table.css")
  ),
  div(class='header',
      HTML('<div id="content">
           <img src="https://static1.squarespace.com/static/5b19e1c23917ee2defbc2e1a/t/5b1a0436575d1f6a61b137cd/1539567080718/?format=1500w" class="ribbon"/>
           </div>'),
      HTML("<div style='padding-top: 4px, padding-bottom:1px;'>
           <h2><b>Client Dashboard</b></h2>
           </div>")),
  tabsetPanel(id = "activeTab",
              tabPanel("Overview", value="manage",
                       fluidRow(
                         column(width=1),
                         column(width=5,
                         HTML("<b>Select Date:</b><br>"),
                         div(style="display: inline-block;vertical-align:top;",
                            actionButton("leftDt", "",icon = icon('caret-left'))
                         ),
                         div(style="display: inline-block;vertical-align:top;  width: 120px;",
                            dateInput("forecastDate",label=NULL, value=Sys.Date(), min=Sys.Date())
                         ),
                         div(style="display: inline-block;vertical-align:top;",
                            actionButton("rightDt", "",icon = icon('caret-right'))
                         ))
                       ),
                       fluidRow(
                          column(width=1),
                          column(width=11,
                            tableOutput("forecastTable")
                          )
                       ),
                       fluidRow(
                          column(width=1),
                          column(width=5,
                            HTML("<div id='graph-container'>"),
                            highchartOutput("forecastDay", height="487px"),
                            HTML("</div>")),
                          column(width=5,
                            HTML("<div id='graph-container2'>"),
                            htmlOutput("forecastBreakdown"),
                            HTML("</div>")),
                          column(width=1)
                       ),br(),
                       fluidRow(
                          column(width=1),
                          column(width=10,
                          HTML("<div id='graph-container'>"), 
                          tauchartsOutput("lineGraph"),
                          HTML("</div>"))),
                          column(width=1)),
           #   tabPanel("Simulate", value="explorer"),
              tabPanel("Download", value="explorer",
                       column(width=1),
                       column(width=10,
                              div(style="display: inline-block;vertical-align:top;",
                              sliderInput("dlRange",
                                          "Select Date Range:",
                                          min = Sys.Date() + 1,
                                          max = Sys.Date() + 100,
                                          value=c(Sys.Date(),Sys.Date() + 14),
                                          timeFormat="%Y-%m-%d")),
                              div(style="display: inline-block;vertical-align:top;",
                                  br(style="line-height:20px;"),HTML("&nbsp&nbsp&nbsp"),downloadButton("dlData")),
                              tableOutput("dlTable")
                       )),
              tabPanel("Settings", value="simulator",
                        #  uiOutput('settingsTable'),
                          fluidRow(
                            div(style="display: inline-block;vertical-align:top;",
                            selectInput("emailAccount", "Select Email:", 
                                        c("dave@ferriesusa.com", "operations@ferriesusa.com"))
                            ),
                            div(style="display: inline-block;vertical-align:top;",
                                br(),actionButton("saveSettings", "Save")
                            ),
                            div(style="display: inline-block;vertical-align:top;",
                                br(),actionButton("resetSettings", "Reset")
                            )
                          ),
                          h2("Email Settings"),
                          HTML(toggle)
                       ),
           tabPanel("Test Model", value="simulator")
           ),
              HTML("<div class='footer'>
                   <img src='http://mellopipelines.com/public/image/footer-hills.png'>
                   </img></div>")
)

server <- function(input,output,session){
  
  observeEvent(input$leftDt,{
    if(!(input$forecastDate - 1 < Sys.Date())){
      updateDateInput(session, "forecastDate", value = input$forecastDate - 1)
    }
  })
  
  observeEvent(input$rightDt,{
    updateDateInput(session, "forecastDate", value = input$forecastDate + 1)
  })
   
  output$forecastTable <- renderTable({
    
    forDate <- input$forecastDate
    
    seats <- round(runif(1,100,170))
    sold <- round(seats*runif(1,.5,1))
      
    df <- as.data.frame(round(abs(replicate(10, runif(5,.85,1.15)))* 100,2))
    df[] <- lapply(df, function(x){
      seats <- round(runif(1,100,170))
      sold <- round(seats*runif(1,.5,1))
      paste0("$",x,"<br> ",sold,"/",seats)
      })
    names(df) <- seq(from=forDate+1, to=forDate + 10, by="1 day")
    row.names(df) <- paste("Service", 1:5)
    df }, rownames=T, sanitize.text.function = function(x) x)
  
  output$forecastDay <- renderHighchart({
    
    forecastDate <- input$forecastDate
    
    df <- forecast_demand(as.Date(forecastDate)) %>%
      mutate(Type = if_else(Date < Sys.Date(), "Realized", "Forecast")) %>%
      bind_rows(.[.$Date == Sys.Date()-1,] %>% mutate(Type="Forecast")) %>%
      arrange(Date)
    
    
    highchart() %>%
      hc_add_series(df[df$Type=="Realized",], type="line", hcaes(x=Date, y=Quantity),
                    name="Actual") %>%
      hc_add_series(df[df$Type=="Forecast",], type="line", hcaes(x=Date, y=Quantity), 
                    dashStyle="shortdot", name="Forecast") %>%
      hc_plotOptions(line = list(marker = list(enabled= F))) %>%
      hc_xAxis(type='datetime',labels=list(rotation=90, format="{value:%b-%d}")) %>%
      hc_title(text=HTML(paste0("<div><b>Forecasted Demand</b></div>"))) %>%
      hc_subtitle(text=HTML(paste0("<div>",input$forecastDate,"</div>"))) %>%
      hc_tooltip(valueDecimals= 2)
      # hc_yAxis(
      #   plotLines = list(
      #     value= max(df$Quantity),
      #     color= 'red',
      #     dashStyle= 'shortdash',
      #     width= 2,
      #     label= list(
      #       text=paste0('Forecasted Demand: ',round(max(df$Quantity),4))
      #     )
      #   )
      # )
  })
  
  output$forecastBreakdown <- renderUI({
    
    ###WEATHER
    weather <- data.frame(
      Factor = c("Temperature", "Percipitation", "Humidity", "Cloud Coverage"),
      Relative = c(round(runif(4, 0, 1),4) * 100),
      Effect = round(runif(4,0,10),2),
      Grouping = c("Weather")
    )
    
    ###SEASONALITY
    seasonality <- data.frame(
      Factor = c("Month", "Week", "Weekend", "Leadtime"),
      Relative = c(round(runif(4, 0, 1),4) * 100),
      Effect = round(runif(4,0,10),2),
      Grouping = c("Seasonality")
    )
    
    ###EXTERNAL FACTORS
    external <- data.frame(
      Factor = c("Car Rentals", "Accomadations", "Airport", "Traffic"),
      Relative = c(round(runif(4, 0, 1),4) * 100),
      Effect = round(runif(4,0,10),2),
      Grouping = c("External")
    )
    
    df <- suppressWarnings(bind_rows(weather,seasonality,external)) %>% data.table::melt(id=c(1,4))
    
    unique(df$Grouping)
    
   hc <- hw_grid(
      lapply(unique(df$Grouping),function(Group){
        hchart(df[df$Grouping == Group,], "bar", hcaes(x = Factor, y = value, group = variable)) %>% 
          hc_legend(enabled=F) %>% hc_xAxis(title=list(text=NULL)) %>% hc_yAxis(title=list(text="")) %>%
          hc_title(text=HTML(paste0("<b>",Group,"</b>")))
      }),ncol=3
    )
    
   return(hc)
   
  })
  
  output$lineGraph <- renderTaucharts({
    
    tauchart(df) %>% 
      tau_line("day", "Quantity", "type") %>% 
      tau_guide_x(label="Date", tick_format="%Y-%m") %>%
      tau_guide_y(label="Quantity/Price") %>%
      tau_legend() %>%
      tau_tooltip()
    
  })
  
  output$dlTable <- renderTable({
    
    start <- input$dlRange[1]
    end <- input$dlRange[2]
    
    # start <- Sys.Date()
    # end <- Sys.Date() + 10
    
    paste("Service", 1:5)
    
    df <- as.data.frame(replicate(5, runif(as.numeric(end - start)+1,85,115))) %>%
      setNames(paste("Service", 1:5))
    
    row.names(df) <- paste0("<b>",seq(start, end, "1 day"),"</b>")
    
    return(df)
    
    
    
  }, rownames=T, sanitize.text.function = function(x) x)
  
  output$settingsTable <- renderTable({
    
    
    toggle <- "<input type='checkbox' id='inventory_type' checked data-toggle='toggle' data-on='On' 
                                   data-off='Off' data-onstyle='success' data-offstyle='danger' data-width='120'>"
    
    data.frame(Setting = paste0("Setting ",1:10), Toggle = toggle)
    
  },sanitize.text.function = function(x) x)
}

shinyApp(ui,server)