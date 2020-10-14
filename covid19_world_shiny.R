## install packages if not already
list.of.packages <- c("shiny","shinythemes","ggplot2","lubridate", "tidyverse","magrittr","dplyr","maps","ggmap","mapdata","gridExtra","viridis","ggforce","kableExtra", "gganimate","ggthemes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
eval(parse(text=paste("library(",list.of.packages,")")))####load the needed packages

## Two CSV file from github
filenames <- c('time_series_covid19_confirmed_global.csv',
               'time_series_covid19_deaths_global.csv',
               'time_series_covid19_recovered_global.csv'
               )
url.path <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series'

## download files to local
download <- function(filename) {
    url <- file.path(url.path, filename)
    dest <- file.path('./data', filename)
    download.file(url, dest)
}
bin <- lapply(filenames, download)

data.deaths.world<-read.csv('/home/tan/proj/r/covid19/data/time_series_covid19_deaths_global.csv')
data.recovered.world<-read.csv('/home/tan/proj/r/covid19/data/time_series_covid19_recovered_global.csv')
data.confirmed.world <- read.csv('/home/tan/proj/r/covid19/data/time_series_covid19_confirmed_global.csv')
data.confirmed.world_orig <- data.confirmed.world

n.col <- ncol(data.confirmed.world)
## get dates from column names
dates <- names(data.confirmed.world_orig)[5:n.col] %>% substr(2,8) %>% mdy()
max.date <- max(dates)
min.date <- min(dates)

                                        # data preparation
data_world <- data.confirmed.world   %>%  
    select(-c('Province.State','Country.Region')) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>%
    filter(!is.na(Long) & !is.na(Lat)) %>%
    gather(key=Date, value=count, -Long, -Lat) %>%
    mutate(Date = Date %>% substr(2,8) %>% mdy()) %>%
    group_by(Long, Lat, Date) %>% 
    summarise(Confirmed = sum(count)) %>%
    filter(Confirmed>0)  %>% ungroup()


world_map <- map_data("world")


## data for death (with "Population")
dt_death <- data.deaths.world   %>%  
    select(-c("Province.State","Country.Region","Long","Lat")) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>%
    gather(key=Date, value=count) %>%
    mutate(Date = Date %>% substr(2,8) %>% mdy()) %>%
    filter(count>0) %>% # remove the 0 from plotting
    group_by(Date) %>% 
    summarise(Death = sum(count)) %>% ungroup()

## Shiny App
server <- shinyServer(function(input, output) {
    output$distPlot <- renderPlot({
        start_date<-input$dateSlider
        dt <- data_world %>% filter(Date <= start_date)
        
        ## USA state map overlay with bubble plot
        p1 <- ggplot() + geom_polygon(data=world_map, aes(x=long, y=lat, group = group),color="white",alpha=0.1) +
            geom_point(data=dt, aes(x=Long, y=Lat, size =Confirmed, color=Confirmed,alpha=Confirmed )) +
            scale_size(limits = c(1,5000),range = c(.1,10),guide_legend(title=""))+
            scale_alpha_continuous(range=c(0.1,0.4),guide = FALSE) + # turn off legend
            scale_color_viridis(limits = c(1,5000)) +
            guides(size=guide_legend("Counts")) +
            coord_fixed(1.3) +
            theme_void() +
            ## labs(title=paste0('全球新冠病毒分布图(更新至', max.date, ')')) +
            guides(fill=FALSE) + # do this to leave off the color legend
            theme(
                text = element_text(color = "#22211d"),
                plot.title = element_text(size= 16, hjust=0.5, color = "#4e4d47", margin = margin(b = 0.1, t = 0.4, l = 2, unit = "cm")),
                )
        
        ddt <- dt_death %>% filter(Date <= start_date)
        ## summarise confirmed data    
        dt_c <- dt %>%
            select(c("Date","Confirmed")) %>%
            group_by(Date) %>% 
            summarise(Confirmed = sum(Confirmed))  %>%
            ungroup()
        
        ## Bar and line plot in time series with 2 y-axes        
        p2 <- ggplot() +
            geom_col(data=dt_c, aes(x = Date, y = Confirmed)) +
            geom_line(data=ddt, aes(x=Date, y=Death*40), colour="red", group=1)+
            scale_y_continuous(name = "Confirmed", expand = c(0,0), # limits = c(0,1800000),
                               sec.axis = sec_axis(~./40, name = "Death")) +
            scale_fill_manual(values = c("#b2d1e0","gold")) +
            scale_x_date(limits = c(as.Date(min.date), as.Date(max.date+1)), expand = c(0,0)) +
            labs(x = "Date", y = "Confirmed") +
            theme(legend.position = "none",
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_rect(fill = "grey99", colour = "grey80"),
                  plot.title = element_text(hjust = 0.5),
                  axis.title.y.right = element_text(color = "red"))
        grid.arrange(p1, p2, ncol=1)
    }, height=600, width=600,bg = "transparent")
})

ui <- shinyUI(fixedPage(
    ## skin and themes
    ## theme = shinytheme("simplex"),
    tags$head(
             tags$script(src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_CHTML"),
             tags$style(HTML("
                              @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                              h1 {font-family: 'Lobster', cursive;}
                              h1,h2 {font-weight: 500; line-height: 1.1; color: #d9230f; text-shadow: 3px 4px 4.9px rgba(3, 3, 3, 0.5);}
                              h2 {color:green;}
                              #distPlot{background:none !important;  height:600px !important;}
                               .irs {max-width: 800px;}
                             "))
         ),
    
    fixedRow(align="center",h1("Covid-19 Data - World")),
    
    fixedRow(align="center",br(),br()),
    
    fixedRow(align="center",
             sliderInput("dateSlider",
                         "Dates:",
                         min = min.date,
                         max = max.date,
                         value=as.Date("2020-03-01"),
                         timeFormat="%Y-%m-%d",
                         width = "80%", 
                         animate = TRUE),
             
             ),
    
    fixedRow(align="center", plotOutput("distPlot")),
    
    fixedRow(align="center",br(),br())
))

shiny::shinyApp(ui=ui,server=server)
