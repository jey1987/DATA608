library(maps)
library(dplyr)
library(ggplot2)
library(Amelia)
library(kableExtra)
library(shiny)
library(shinythemes)
library(shinyWidgets)
data<- read.csv("globalterrorismdb_csv.csv", header=TRUE, check.names = FALSE)
colnames(data)

df <- data %>%
  select(iyear,imonth,iday,country,country_txt,region,region_txt,provstate,city,attacktype1,attacktype1_txt,attacktype2,attacktype2_txt,attacktype3,attacktype3_txt)

df %>% head() %>% kable()
df_sample <- df %>% head(50)

summary(df)

colSums(is.na(df))
missmap(df, main="Missing Values")

df_final <- df %>% select(iyear,imonth,iday,country,country_txt,region,region_txt,provstate,city,attacktype1,attacktype1_txt)
colSums(is.na(df_final))
df_final$event_date  <- as.Date(paste(df_final$iyear,"-",df_final$imonth,"-",df_final$iday,sep=""))
df_final <- na.omit(df_final)
ggplot(df_sample, aes(x = country_txt)) + geom_dotplot(method="histodot", binwidth = 0.2,stackratio=0.8,stroke=2,fill="red") + new_retro()

ggplot(df_sample, aes(x = region_txt)) + geom_dotplot(method="histodot", binwidth = 0.2,stackratio=0.8,stroke=2,fill="green")

ggplot(df_sample, aes(x = attacktype1_txt)) + geom_dotplot(method="histodot", binwidth = 0.2,stackratio=0.8,stroke=2,fill="blue")

df_country_cnt <- df_final %>%
  group_by(country_txt) %>%
  summarise(event_cnt=n())

df_year_cnt <- df_final %>%
  group_by(iyear) %>%
  summarise(event_cnt=n())

ggplot(df_year_cnt, aes(x=iyear, y=event_cnt)) +  geom_point(aes(size=event_cnt),shape=18,color="red")  +   geom_smooth(method=lm,  linetype="dashed",  color="darkgreen", fill="gray") + xlab("Year") + ylab("Water Resource Scale")   + ggtitle("Yearly trend of Water Resource impact over Child Mortality")

df_country_2010 <- df_final %>%
  filter(iyear > 2010) %>%
  group_by(country_txt) %>%
  summarise(event_cnt=n()) %>%
  arrange(desc(event_cnt)) %>%
  head(10)

ggplot(df_country_2010, aes(x="", y=event_cnt, fill=country_txt)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_brewer(palette="Spectral")


df_country_cnt$region <- df_country_cnt$country_txt

df_country_cnt <- df_country_cnt %>%
  mutate(region = ifelse(region == "United States", "USA", region)) %>%
  mutate(region = ifelse(region == "Russian Federation", "Russia", region)) %>%
  mutate(region = ifelse(region == "Iran (Islamic Republic of)", "Iran", region)) %>%
  mutate(region = ifelse(region == "Kazakstan", "Kazakhstan", region))

world_map <- map_data("world")

terror_map <- left_join(world_map, df_country_cnt, by = "region")

ggplot(terror_map, aes(map_id = region, fill = event_cnt))+
  geom_map(map = terror_map,  color = "white")+
  expand_limits(x = terror_map$long, y = terror_map$lat)+
  scale_fill_gradient(low='#edaf98', high='#fa0511')


# Shiny App 1

ui <- fluidPage(
  setBackgroundColor(  color = c("#F7FBFF", "#2171B5"), gradient = c("linear", "radial"),  direction = c("bottom", "top", "right", "left"),shinydashboard = FALSE),
  titlePanel(HTML("<h1><center><font size=14> Top Countries with most terrorist Attacks  </font></center></h1>")),
  sidebarLayout(
    sidebarPanel(
      
      tags$div(img(src = "https://ibc-static.broad.msu.edu/sites/globaledge/blog/54529.jpg",height="75%", width="100%")),
    ),
    mainPanel(
      dateRangeInput("daterange4", "Pick Date Range ",start = df$event_date,end = df$event_date),
      actionButton("do", "Fetch Report", class = "btn-warning"),
      htmlOutput("text"),
      plotOutput("country_by_date")))
)


server <- function(input, output) {
  output$text <- renderText({
    paste0("<br>", " ", "</br>","<br>", " ", "</br>","<br>", " ", "</br>")
  })
  output$text1 <- renderText({
    paste0("<br>", " ", "</br>")
  })
  observeEvent(input$do, {
    output$country_by_date <- renderPlot({
      df_final %>%
        filter(event_date >= input$daterange4[1] & event_date < input$daterange4[2]) %>%
        select(country_txt) %>%
        group_by(country_txt) %>%
        summarise(event_cnt=n()) %>%
        arrange(desc(event_cnt)) %>%
        head(10) %>%
        ggplot(aes(country_txt,event_cnt,size=event_cnt,color=factor(event_cnt))) + geom_point() +   coord_flip() + theme_bw()      
    })
  })
}

shinyApp(ui, server)

# Shiny App 2


ui <- fluidPage(
  setBackgroundColor(  color = c("#F7FBFF", "#2171B5"), gradient = c("linear", "radial"),  direction = c("bottom", "top", "right", "left"),shinydashboard = FALSE),
  titlePanel(HTML("<h1><center><font size=14> State/Provinces with Most Terrorist Attacks  </font></center></h1>")),
  sidebarLayout(
    sidebarPanel(
      
      tags$div(img(src = "https://ibc-static.broad.msu.edu/sites/globaledge/blog/54529.jpg",height="75%", width="100%")),
    ),
    mainPanel(
      selectInput("variables", "Variable:", choices=c("All",unique(df_country_list$country_txt))),
      dateRangeInput("daterange4", "Pick Date Range ",start = df$event_date,end = df$event_date),
      actionButton("do", "Fetch Report", class = "btn-warning"),
      htmlOutput("text"),
      plotOutput("country_by_date")))
)

server <- function(input, output) {
  output$text <- renderText({
    paste0("<br>", " ", "</br>","<br>", " ", "</br>","<br>", " ", "</br>")
  })
  output$text1 <- renderText({
    paste0("<br>", " ", "</br>")
  })
  observeEvent(input$do, {
    output$country_by_date <- renderPlot({
      df_final %>%
        filter(event_date >= input$daterange4[1] & event_date < input$daterange4[2] & country_txt==input$variables) %>%
        select(provstate) %>%
        group_by(provstate) %>%
        summarise(event_cnt=n()) %>%
        arrange(desc(event_cnt)) %>%
        head(10) %>%
        ggplot(aes(provstate,event_cnt,size=event_cnt,color=factor(event_cnt))) + geom_col() +   coord_flip() + theme_bw()
      
    })
  })
}

shinyApp(ui, server)

# Shiny App 3


ui <- fluidPage(
  setBackgroundColor(  color = c("#F7FBFF", "#2171B5"), gradient = c("linear", "radial"),  direction = c("bottom", "top", "right", "left"),shinydashboard = FALSE),
  titlePanel(HTML("<h1><center><font size=14> Regions with Most Terrorism Impacts  </font></center></h1>")),
  sidebarLayout(
    sidebarPanel(
      
      tags$div(img(src = "https://ibc-static.broad.msu.edu/sites/globaledge/blog/54529.jpg",height="75%", width="100%")),
    ),
    mainPanel(
      dateRangeInput("daterange4", "Pick Date Range ",start = df$event_date,end = df$event_date),
      actionButton("do", "Fetch Report", class = "btn-warning"),
      htmlOutput("text"),
      plotOutput("country_by_date")))
  )

server <- function(input, output) {
  output$text <- renderText({
    paste0("<br>", " ", "</br>","<br>", " ", "</br>","<br>", " ", "</br>")
  })
  output$text1 <- renderText({
    paste0("<br>", " ", "</br>")
  })
  observeEvent(input$do, {
    output$country_by_date <- renderPlot({
      df_final %>%
        filter(event_date >= input$daterange4[1] & event_date < input$daterange4[2]) %>%
        select(region_txt) %>%
        group_by(region_txt) %>%
        summarise(event_cnt=n()) %>%
        arrange(desc(event_cnt)) %>%
        head(10) %>%
        ggplot(aes(region_txt,event_cnt)) + geom_col(color="red",fill="gray",size=1)+ theme_bw()
      
    })
  })
}

shinyApp(ui, server)

# Shiny App 4

ui <- fluidPage(
 setBackgroundColor(  color = c("#F7FBFF", "#2171B5"), gradient = c("linear", "radial"),  direction = c("bottom", "top", "right", "left"),shinydashboard = FALSE),
 titlePanel(HTML("<h1><center><font size=14> Type of Terrorist Attack On Countries over Years  </font></center></h1>")),
 sidebarLayout(
  sidebarPanel(
    
tags$div(img(src = "https://ibc-static.broad.msu.edu/sites/globaledge/blog/54529.jpg",height="75%", width="100%")),
  ),
mainPanel(
selectInput("variables", "Attack Type:", choices=c("All",unique(df_final$attacktype1_txt)),multiple=TRUE),
dateRangeInput("daterange4", "Pick Date Range ",start = df$event_date,end = df$event_date),
actionButton("do", "Fetch Report", class = "btn-warning"),
htmlOutput("text"),
plotOutput("country_by_date"))
))


server <- function(input, output) {
output$text <- renderText({
  paste0("<br>", " ", "</br>","<br>", " ", "</br>")
})

observeEvent(input$do, {
  output$country_by_date <- renderPlot({
    df_final %>%
      filter(event_date >= input$daterange4[1] & event_date < input$daterange4[2] & attacktype1_txt==input$variables) %>%
      select(country_txt) %>%
      mutate(region=country_txt) %>%
      group_by(region) %>%
      summarise(event_cnt=n()) %>%
      arrange(desc(event_cnt)) %>%
      head(10) %>%
      right_join(world_map, by = "region") %>%
      ggplot(aes(map_id = region, fill = event_cnt))+  geom_map(map = terror_map,  color = "white")+  expand_limits(x = terror_map$long, y = terror_map$lat)+  scale_fill_gradient(low='#edaf98', high='#fa0511')+ theme_bw()
  })
})
}

shinyApp(ui, server)