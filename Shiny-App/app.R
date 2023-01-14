# Load Packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(plotly)
library(tidyquant)
library(stringr)
library(quantmod)
library(gganimate)
library(lubridate)
library(sqldf)
library(RCurl)
library(zoo)
library(dplyr)
library(data.table)
library(forecast)
library(tsbox)
library(memoise)

# Function to create vertical line from https://stackoverflow.com/questions/34093169/horizontal-vertical-line-in-plotly, by Carson.
vline <- function(x = 0, color = "red") {
    list(
        type = "line", 
        y0 = 0, 
        y1 = 1, 
        yref = "paper",
        x0 = x, 
        x1 = x, 
        line = list(color = color)
    )
}

# Import Covid Data
covid_data <- read.csv("covid_reduced.csv",stringsAsFactors = FALSE,check.names =  FALSE)

# Convert to correct data data type.
covid_data$date = as.Date(covid_data$date, format =  "%Y-%m-%d")

######################################################
## Process Data for use in the cases plot
## i.e. the plot where students observe the trends with COVID cases in Australia and the world.
######################################################

# Australian subset of data
AUS_subset <- covid_data %>%
    filter(iso_code == "AUS") %>%
    select(iso_code, total_cases, date) %>%
    rename(location = iso_code)

# Standardise cases to be between 0 and 1.
AUS_subset$total_cases_scaled = scales::rescale(AUS_subset$total_cases)

# Whole world subset of data.
whole_world <- covid_data 

# Remove rows with a NA value in the total_cases column
whole_world <- whole_world[!is.na(whole_world$total_cases),]

whole_world <- whole_world %>%
    group_by(date) %>%
    summarise(total_cases = sum(total_cases))
whole_world$location <- "WORLD"

# Standardise cases to be between 0 and 1.
whole_world$total_cases_scaled = scales::rescale(whole_world$total_cases)

cases = rbind(AUS_subset, whole_world)

######################################################
## Process data in the industries plot
######################################################

total_data = readRDS(file = 'dow_jones_index.Rda')
total_data = na.omit(total_data)
column_name = c('date', 'index', 'industry')
colnames(total_data) = column_name

asx_prices_percent = readRDS(file = 'asx_price_percentage.Rda')
covid_discovered = as.numeric(as.Date("2019-12-31"))
covid_start = as.numeric(as.Date("2020-01-30"))
pandemic = as.numeric(as.Date("2020-03-11"))

total_data$country = "America"
asx_prices_percent$country = "Australia"

dj_asx = rbind(total_data,asx_prices_percent)

compare_industry <- function(industry_selection) {
    au_industry = dj_asx[dj_asx$industry == industry_selection[1],]
    usa_industry = dj_asx[dj_asx$industry == industry_selection[2],]
    
    select_industry = rbind(au_industry, usa_industry)
    p = ggplot() + geom_line(select_industry,mapping = aes(x = date, y = index, group = industry, color = country)) + xlab("Date") + ylab("Percentage") + ggtitle(industry_selection[2]) + facet_grid(select_industry$country ~ .)
    return(p)
}

dj_asx = rbind(total_data,asx_prices_percent)

#Unify the industry labels
dj_asx$industry = str_replace(dj_asx$industry,'health care service', 'Health Care Equipment & Services')
dj_asx$industry = str_replace(dj_asx$industry,'insurance', 'Insurance')
dj_asx$industry = str_replace(dj_asx$industry,'pharmaceuticals', 'Pharmaceuticals')
dj_asx$industry = str_replace(dj_asx$industry,'Pharmaceuticals, Biotechnology & Life Sciences', 'Pharmaceuticals')
dj_asx$industry = str_replace(dj_asx$industry,'semiconductor', 'Semiconductors & Semiconductor Equipment')
dj_asx$industry = str_replace(dj_asx$industry,'software and service', 'Software & Services')
dj_asx$industry = str_replace(dj_asx$industry,'technology harware', 'Technology Hardware & Equipment')
dj_asx$industry = str_replace(dj_asx$industry,'telecom', 'Telecommunication Services')
dj_asx$industry = str_replace(dj_asx$industry,'transportation', 'Transportation')

health_care = 'Health Care Equipment & Services'
insurance = 'Insurance'
pharmaceuticals = 'Pharmaceuticals'
semiconductor = 'Semiconductors & Semiconductor Equipment'
software = 'Software & Services'
technology_hardware = 'Technology Hardware & Equipment'
telecom = 'Telecommunication Services'
transportation = 'Transportation'

compare_industry <- function(dj_asx = dj_asx, industry_selection) {
    select_industry = c()
    # vertical lines for key events
    covid_discovered = as.numeric(as.Date("2019-12-31"))
    covid_start = as.numeric(as.Date("2020-01-30"))
    pandemic = as.numeric(as.Date("2020-03-11"))
    vacc_50_aus = as.numeric(as.Date("2021-10-10"))
    vacc_75_aus = as.numeric(as.Date("2021-12-14"))
    vacc_50_us = as.numeric(as.Date("2021-07-16"))
    pfizer = as.numeric(as.Date("2020-12-11"))
    for (i in (industry_selection)) {
        select_industry = rbind(select_industry,dj_asx[dj_asx$industry == i,])
    }
    p = ggplot() + 
        geom_line(select_industry,mapping = aes(x = date, y = index, group = industry, color = industry)) + 
        xlab("Date") + ylab("Percentage Change") + ggtitle("Percentage Change of Industry's stock prices from 1 August 2019") + facet_grid(select_industry$country ~ .)  + 
        geom_vline(data = select_industry, aes(xintercept = covid_discovered, color = "Covid Discovered")) + 
        geom_vline(data = select_industry,aes(xintercept = covid_start, color = "Covid Starts")) + 
        geom_vline(data = select_industry,aes(xintercept = pandemic, color = "Pandemic Declared")) +
        geom_vline(data = select_industry,aes(xintercept = vacc_50_aus, color = "AUS 50% vaccinated")) + 
        geom_vline(data = select_industry,aes(xintercept = vacc_50_us, color = "US 50% vaccniated")) +
        geom_vline(data = select_industry,aes(xintercept = vacc_75_aus, color = "AUS 75% vaccniated")) +
        geom_vline(data = select_industry,aes(xintercept = pfizer, color = "WHO approves Pfizer")) +
        scale_color_discrete(name = "Key")
    return(p)
}

######################################################
## BHP vs COVID Cases
######################################################

#x <- getURL("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
covid_orig <- read.csv("covid_reduced.csv")
covid_bhp <- covid_orig
covid_bhp$date <- as.Date(covid_bhp$date) 

# Locate a more appropriate date range for Australia
start_date = min(covid_bhp$date)
covid_aus <- subset(covid_bhp, date > as.Date(start_date))
covid_aus <- covid_aus %>% filter(covid_aus$location == "Australia")

# Get BHP prices
bhp_prices  <- tq_get("BHP.AX", get = "stock.prices", from = start_date)

# Join with covid aus data by date
joined_data <- inner_join(covid_aus, bhp_prices, by ="date")

covid_aus_max = max(joined_data$new_cases_smoothed, na.rm = TRUE)
bhp_max = max(joined_data$close, na.rm = TRUE)

coeff =  covid_aus_max/bhp_max

######################################################
## Choose your own One Stock-Function
######################################################

stock_vs_covid_variable = function(stock, covid_variable){
    start_date = min(covid_bhp$date)
    covid_world <- subset(covid_bhp, date > as.Date(start_date))
    covid_world <- covid_world %>% filter(covid_world$location == "World")

    # stock.warning = FALSE
    stock_prices  <- tryCatch({
        tq_get(stock, get = "stock.prices", from = start_date)
    }, warning = function(e) {
        print(paste(stock, " symbol is not available, reset to default (AAPL)"))
        stock <<- "APPL"
        return(tq_get("AAPL", get = "stock.prices", from = start_date))
    })
    
    # Join with covid aus data by date
    joined_data <- inner_join(covid_world, stock_prices, by ="date")
    
    
    Legend = paste(stock, " Stock")
    axis_title = paste(stock, " close price ($)")
    axis2_title = gsub("_", " ", covid_variable)
  
    str1 = paste(stock, "stock vs")
    
    graph_title = paste(str1, axis2_title)
    
    coeff = max(joined_data[,covid_variable], na.rm = TRUE) / max(joined_data$close, na.rm = TRUE)
    
    
    p <- ggplot(joined_data, aes(x = date)) +
        geom_line(aes(y = close, color = Legend)) +
        geom_ma(aes(y = close, color = "50 Day Moving Average"), ma_fun = EMA, n = 50, wilder = TRUE, linetype = 5) +
        geom_line(aes(y = joined_data[,covid_variable]/coeff, color = "COVID Statistic"), size = 1.05) +
        labs(title = graph_title,
             x = "Date") +
        scale_y_continuous(
            
            # Features of the first axis
            name = axis_title,
            # Add a second axis and specify its features
            sec.axis = sec_axis(~.*coeff, name=axis2_title, labels = scales::comma)
        )
    return(p)
}

######################################################
## Portfolio Builder
######################################################

covid_porfolio <- covid_bhp
min(covid_porfolio$date)

portfolio_builder = function(stocks, weights){
    # Retrieve date range
    start_date = min(covid_porfolio$date)
    end_date = "2022-04-01"
    # Length variable used 
    len = length(weights) / length(stocks)
    # Checking if weights table is correct length
    if (length(weights) %% length(stocks) != 0){
        stop("Weights is not correct dimensions")
    }
    
    # Error checking to see if weights add to 1
    sum = 0
    for(i in 1:length(weights)){
        sum = sum + weights[[i]]
        if (i %% length(stocks) == 0){
            if (sum != 1){
                stop("Weights do not add to 1")
            }
            sum = 0
        }
    }
    
    # Convert the weights to a vector
    weights <- c(weights)
    
    # Create a string, with the stocks for graphs
    stock_string = paste(stocks, collapse=', ' )
    
    # Binding the weights with their stocks
    weights_table <- tibble(stocks) %>%
        tq_repeat_df(n = len) %>%
        bind_cols(tibble(weights)) %>%
        group_by(portfolio)
    
    # Creates a table for monthly stock prices for each stock, with tryCatch
    monthly_stock_returns <- tryCatch({stocks %>% 
            tq_get(get = "stock.prices",
                   from = start_date,
                   to = end_date) %>% 
            group_by(symbol) %>% 
            tq_transmute(select =  adjusted, 
                         mutate_fun = periodReturn,
                         period = "monthly",
                         col_rename = "Ra")
    }, warning = function(w){
        stop("Symbol not in database")
    }
    )
    
    # Repeats for each portfolio
    multi_monthly_stock_returns <- monthly_stock_returns %>%
        tq_repeat_df(n = len)
    
    # Create table with returns for a start $10000 investment in the stock
    monthly_portfolio_growth <- multi_monthly_stock_returns %>%
        tq_portfolio(assets_col   = symbol, 
                     returns_col  = Ra, 
                     weights      = weights_table, 
                     col_rename   = "investment.growth",
                     wealth.index = TRUE) %>%
        mutate(investment.growth = investment.growth * 10000)
    
    # Find the best performing portfolio at end of period
    indices = which(monthly_portfolio_growth$date == max(monthly_portfolio_growth$date), arr.ind = TRUE)
    i = 1
    portfolio_no = 1
    while(i <= length(indices) - 1){
        if(monthly_portfolio_growth$investment.growth[indices[i]] > monthly_portfolio_growth$investment.growth[indices[i+1]]){
            portfolio_no = monthly_portfolio_growth$portfolio[indices[i]]
        }
        i = i + 1
    }
    
    # Create caption string which highlights the best performing portfolio
    str1 = "Portfolio "
    str2 = " is a standout!"
    cap = paste(str1, portfolio_no)
    cap = paste(cap, str2)
    
    # Create a label for each protfolio which is the stocks and their corresponding weights
    str = ""
    label <- list()
    c = 1
    for (row in 1:nrow(weights_table)) {
        
        stock = weights_table[row, "stocks"]
        weight = weights_table[row, "weights"] * 100
        stock_str = paste(stock, ":")
        weight_str = paste(weight, "%\n ")
        str2  <- paste(stock_str, weight_str)
        str = paste(str, str2)
        if (row %% length(stocks) == 0){
            numb_str = paste(as.character(c), "\n")
            port_str = paste("Portfolio ", numb_str)
            str = paste(port_str, str)
            label <- append(label, str)
            str = ""
            c = c + 1
        }
    }
    
    # Changes the name of each portfolio to it's label
    i = 1
    while(i <= len){
        monthly_portfolio_growth$portfolio[monthly_portfolio_growth$portfolio == i ] <- label[[i]]
        i = i + 1
    }
    
    # Convert the monthly portfolio growth into a ggplot 
    p <- monthly_portfolio_growth %>%
        ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) +
        geom_line(size = 2) +
        labs(title = "Portfolio Growth",
             x = "", y = "Portfolio Value",
             color = "Portfolio") +
        geom_smooth(method = "loess") + # local regression
        theme_tq() +
        scale_color_tq() +
        scale_y_continuous(labels = scales::dollar)
    
    # Convert the ggplot into a plotly
    ggplotly(p) %>%
        layout(title = list(text = paste0('Portfolio Growth',
                                          '<br>',
                                          '<sup>',
                                          stock_string,
                                          '</sup>')),
               legend=list(text=list(text=label)),
               annotations = 
                   list(x = 1, y = -0.1, text = "", 
                        showarrow = F, xref='paper', yref='paper', 
                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                        font=list(size=12, color="black"))) %>% 
        animation_opts(1000, easing = 'elastic', redraw = FALSE)
}

######################################################
## Extension
######################################################

# Input file filtered by industry and covid variables
# Returns VAR model
full_model <- function (Ys,var){
  library(vars)
  columns = c("date",var,"two_week_return")
  Ys = Ys[columns]
  #separate the data into training data set
  full_train = Ys[Ys$date < as.Date("2022-03-01"),]
  full_test = Ys[Ys$date > as.Date("2022-02-28"),]
  
  new_ys_train = full_train[,-1]
  #train model
  lagselect <- VARselect(new_ys_train,lag.max = 200,type = "const")
  lag = lagselect$selection[2]
  model = VAR(new_ys_train,p = lag, type = "const",season = NULL, exog = NULL)
  return (model)
}

# Input file filtered by industry, covid variables and model
# returns ggplot graph of prediction vs actual from 1 March ~ 10 March 2022
prediction_result <-  function(Ys,variable,model){
  # predict for next 10 days
  forecast = predict(model,n.ahead = 10,ci = 0.95)
  
  prediction = as.data.frame(forecast$fcst["two_week_return"])
  prediction["date"] = seq(as.Date("2022-03-01"),length.out = length(prediction$two_week_return.fcst),by = "day")
  # Get actual data for the predicted dates
  end_date = tail(prediction$date,1)  
  model_col = c("date",variable,"two_week_return")
  vacc_price = Ys[model_col]
  # Merge  predicted data and actual into one table
  vacc_price_test = vacc_price[vacc_price$date > as.Date("2022-02-28"),]
  vacc_price_test = vacc_price_test[vacc_price_test$date < end_date +1,]
  prediction_merge = merge(prediction,vacc_price_test,by = "date",all.x = TRUE)
  # get line graph
  graph = ggplot() +
   geom_line(data = prediction_merge,aes(x= date,  y = two_week_return.fcst, color = "Prediction")) +
   geom_line(data = prediction_merge,aes(x= date,  y = two_week_return, color = "Actual")) +
   labs(x = "Date",
        y = "10 day return rate")
 return(graph)
}

# Input industry name and covid variable
# Runs the VAR model and draws prediction vs actual graph
extension_func <- function(industry, var){
  filename = paste(industry,".csv", sep = "")
  print(filename)
  Ys = read.csv(filename)
  Ys$date = as.Date(Ys$date)
  model_multi = full_model(Ys,var)
  prediction_result(Ys,var,model_multi)
}

######################################################
## Vaccination Rate
######################################################
temp = read.csv("covid_reduced.csv")
temp$date = as.Date(temp$date, "%Y-%m-%d")
temp[, "month"] =  format(as.Date(temp[,"date"]), "%Y-%m") ## reformatting the date column to be able to group by months
temp = drop_na(temp, continent, location, people_fully_vaccinated_per_hundred)
valid_cont = c('Oceania', 'Asia', 'North America', 'South America', 'Africa', 'Europe') #only including results with recorded continents
covid_clean_SS = temp[temp$continent %in% valid_cont, ]
vaccine_df = sqldf("select month,population,location, continent, SUM(new_deaths) new_deaths, MAX(people_fully_vaccinated_per_hundred) people_fully_vaccinated_per_hundred from covid_clean_SS GROUP BY location, month") ## grouping by months so the animation will be more userfriendly 
vaccine_df = data.frame(vaccine_df)
vaccine_df[is.na(vaccine_df)] = 0 #in early results vaccines weren't recorded as there were none - therefore changing them to zero


######################################################

width_dashboard = 150

ui <- dashboardPage(
    
    skin = "green",
    
    dashboardHeader(titleWidth = width_dashboard),
    
    dashboardSidebar(
        width = width_dashboard,
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Let's Explore", tabName = "explore", icon = icon("book")),
            menuItem("Extension", tabName = "extension", icon = icon("signal")),
            menuItem("References", tabName = "references", icon = icon("bookmark"))
        )
    ),
    
    dashboardBody(
        
        tags$head(
            tags$style(HTML("
            .box_style_1 {
                height: 300px;
                background-color: HoneyDew;
                border-style: groove;
            }
            .box_style_2 {
                height: 300px;
                background-color: Ivory;
                border-style: groove;
            }
            "))
        ),
        
        tabItems(
            
            # Home
            tabItem(tabName = "home",
                    
                    fluidRow(
                        
                        column(width = 7,
                               br(),
                               titlePanel(
                                   h2("COVID-19 & Economic Trends: Exploring With Data")
                               )
                        ),
                        column(width = 5,
                               tags$a(
                                   href = "https://www.surveymonkey.com/r/WG5FX6L", 
                                   target = "_blank",
                                   valueBox("Feedback", "Please click to report errors or provide feedback!",
                                            icon = icon("comment"), color = "light-blue", width = NULL)
                               )
                        )
                        
                    ),
                    
                    box(
                        title = "Welcome",
                        height = 300,
                        width = NULL,
                        p("Welcome to the COVID-19 & Economic Trends interactive dashboard!"),
                        p("\n"),
                        p("In early 2020, the unseen occurred - a new variant of the SARS-CoV-2 virus, named COVID-19 entered the world stage. 
                          Over a month, the world as we knew it started to close down. International borders started to close, people were forced to isolate
                          in their homes and companies were shut down. This rolled on to widespread unemployment. The international equity markets
                          took a huge hit, many assets plummeted in value."),
                        p("\n"),
                        p("By engaging with this dashboard, you will learn how to observe trends related to COVID statistics (such as case numbers, numbers of deaths, 
                          vacination rates) and learn the impact that external economic factor can have on investment decisions."),
                        p("\n"),
                        p("To get started, press the \"Let's Explore\" tab on the left!")
                    ),
                    
                    fluidRow(
                        
                        column(width = 7,
                               infoBox("Let's Get Started", "Click on the \"Let's Explore\" tab to begin!",
                                        icon = icon("chevron-left"), color = "purple", width = NULL, fill = TRUE)
                        ),
                        
                        column(width = 5,
                            box(
                                solidHeader = TRUE,
                                title = "Authors",
                                p("Toby Cullen"),
                                p("\n"),
                                a("Link Ding", href="https://www.linkedin.com/in/link-ding-286975238/"),
                                p("\n"),
                                a("Thomas Elton", href="https://www.linkedin.com/in/thomas-elton-a86aaa215/"),
                                p("\n"),
                                a("Kosta Konstant", href="https://www.linkedin.com/in/kosta-konstant-7508691aa/"),
                                p("\n"),
                                a("Josh Sung", href="https://www.linkedin.com/in/josh-sung-8584a719b/"),
                                p("\n"),
                                a("Sarah Sweeting", href="https://www.linkedin.com/in/sarah-sweeting/"),
                                width = NULL,
                                status = "primary"
                            )
                        )
                            
                    )

                    

                    

            ),
            
            # Explore
            tabItem(tabName = "explore",
                    
                    # Lesson objectives. 
                    fluidRow(
                        column(11, align="right", offset=1,
                               br(),
                               actionButton("objectives", "Lesson Objectives", width = "150px", style="color: #000000; background-color: #d9d1d0; border-color: #2e6da4")
                        )
                    ),
                    
                    br(),
                    
                    # Introduction text box
                    box(
                        width = 12,
                        title = "Getting Started",
                        id = "mybox",
                        p("Today we will be using real data and working through a number of exercises, in order to learn about how an event like COVID-19 can affect industries, businesses and individuals investment decisions."),
                        p("We will be employing two types of data:"),
                        p("1) COVID-19 Data"),
                        p("2) Equity API Data"),
                        p(HTML("<b>Click on the boxes below to learn more information about the data sources!</b>"))
                    ),
                    
                    # Text boxes for info on the different data sources.
                    fluidRow(
                        column(
                            width = 6,
                            uiOutput("active_side"), 
                            flipBox(
                                id = "myflipbox", 
                                width = 12,
                                front = div(
                                    class = "text-center",
                                    div(
                                        class = "box_style_1",
                                        width = 12,
                                        h1("COVID-19 DATA"),
                                        br(),
                                        br(),
                                        img(src = "virus.png", height = 150, width = 150)
                                    ),

                                ),
                                back = div(
                                    class = "text-center",
                                    div(
                                        class = "box_style_1",
                                        width = 12,
                                        br(),
                                        p(HTML('The covid data set is a collation of 68 different variables that report certain COVID related statistics for each country over time. The measurements taken include; Tests and positivity, Vaccinations, Confirmed Cases and Deaths, Hospital and ICUS, and other variables of interests. COVID-19 data updates daily, ensuring the presented insights remain contemporary and relevant.'), style="font-size: 15px"),
                                        br(),
                                        p(HTML('<b>Note:</b> The visualisations in this module omit dates for countries with missing data.'), style="font-size: 15px")

                                        
                                    )
                                )
                            )
                        ),
                        
                        column(
                            width = 6,
                            uiOutput("active_side_2"),
                            flipBox(
                                id = "myflipbox2",
                                width = 12,
                                front = div(
                                    class = "text-center",
                                    div(
                                        class = "box_style_2",
                                        width = 12,
                                        h1("Equity API Data"),
                                        br(),
                                        br(),
                                        img(src = "stock.png", height = 150, width = 150)
                                        
                                        
                                    )
                                ),
                                back = div(
                                    class = "text-center",
                                    div(
                                        class = "box_style_2",
                                        width = 12,
                                        br(),
                                        br(),
                                        p(HTML("An <b>API (Application Programming Interface)</b> allows users to communicate with other websites. In this interactive module, we utilize APIs to obtain live share prices and industry data."), style="font-size: 15px"),
                                        br(),
                                        p(HTML('If you want to find out more, check out this youtube video <a href="https://www.youtube.com/watch?v=OVvTv9Hy91Q">here</a>'), style="font-size: 15px")
                                    )
                                )
                            )
                        ),
                        
                        # Continue button.
                        fluidRow(
                            column(6, align="center", offset=3,
                                   br(),
                                   actionButton("display_section_2", "Continue", width = "200px", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                   )
                        ),

                        # COVID cases Trends Section
                        column(width = 12,
                               conditionalPanel(
                                   condition = "typeof display_section_2 == \"undefined\"",
                                   br(),
                                   br(),
                                   br(),
                                       box(
                                           width = 4,
                                           title = "COVID Cases Trends",
                                           p(HTML("Before diving into how the prices of equities were affected by the pandemic, it is importat that we begin with what is called <b>initial data analysis</b>, or <b>IDA</b> for short.")),
                                           p("But what exactly is IDA? Well, IDA is a high level overview of what the data looks like."),
                                           p("One factor that is particuarly important is the number of cases. Both Australian and World data will be very useful! Here's a quick question to get you thinking."),
                                           p(HTML("<b><i>What trends do you observere in the graph to the right?</i></b>"), style = "color:blue"),

                                           p("Now, let's take this a bit further! Use the buttons below to show some key COVID milestones on top of the plot, this helps to provide more context around our data."),
                                           radioButtons("milestones", "Add Milestones to Graph",
                                                        c("No Milestones" = "none",
                                                          "Milestones 1" = "mil1",
                                                          "Milestones 2" = "mil2")),
                                           p(HTML("<b><i>Is there any obvious association between COVID cases and different key milestones which you can add to the graph?</i></b>"), style = "color:blue")
                                       ),
                                       conditionalPanel(
                                         condition = "input.milestones == 'none'",
                                         box(
                                           width = 8,
                                           title = "Cumulative COVID Cases in Australia and the World",
                                           plotly::plotlyOutput("covid_cases_no_milestones")
                                         )
                                       ),
                                       conditionalPanel(
                                         condition = "input.milestones == 'mil1'",
                                         box(
                                           width = 8,
                                           title = "Cumulative COVID Cases in Australia and the World",
                                           plotly::plotlyOutput("covid_cases_mil1")
                                         )
                                       ),
                                       conditionalPanel(
                                         condition = "input.milestones == 'mil2'",
                                         box(
                                           width = 8,
                                           title = "Cumulative COVID Cases in Australia and the World",
                                           plotly::plotlyOutput("covid_cases_mil2")
                                         )
                                       )
                               )
                        ),

                        # Continue button.
                        fluidRow(
                            conditionalPanel(
                                condition = "typeof display_section_2 == \"undefined\"",
                                column(6, align="center", offset=3,
                                       br(),
                                       actionButton("display_section_3", "Continue", width = "200px", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                )
                            )
                        ),
                        
                        # Section on COVID Vaccinations and Death Trends
                        column(width = 12,
                               conditionalPanel(
                                   condition = "typeof display_section_3 == \"undefined\"",
                                   br(),
                                   br(),
                                   br(),
                                   box(
                                       width = 4,
                                       title = "COVID Trends",
                                       
                                       p("Within datasets, different variables can have different types of relationships with each other. These are important at predicting and analysing different trends. One type of relationship is inversely proportional, as one goes up the other goes down. You would be familiar with this relationship from your studies of supply and demand."), 
                                       
                                       p("Take a look at this graph and look at the relationship between vaccination rate and new deaths."),
                                       
                                       p("As you can see over time as the vaccinations increased, the number of new deaths had a downward trend."),
                                       
                                       p(HTML("<b><i>What other plots do you think will demonstrate this inverse relationship?</i></b>"), style = "color:blue")
                                   ),
                                   box(
                                       width = 8,
                                       title = "Vaccinations vs Deaths (Make Take a Few Seconds To Load)",
                                       plotly::plotlyOutput("vaccinations")
                                   )
                               )
                        ),

                        # Section on testing COVID trends
                        column(width = 12,
                               conditionalPanel(
                                   condition = "typeof display_section_3 == \"undefined\"",
                                   box(
                                       width = 12,
                                       title = "Questions",
                                       p(HTML("<b><i>What other plots do you think will demonstrate this inverse relationship?</i></b>"), style = "color:blue"),
                                       radioButtons("trends_multi", "Answer",
                                                    c("New Cases vs New Deaths" = "new_cases",
                                                      "Positive Rate vs New Cases" = "positive_rate",
                                                      "Hospital Patients vs Time" = "hosp_patients"),
                                                      selected = character(0)),
                                       conditionalPanel(
                                           condition = "input.trends_multi == 'new_cases'",
                                           br(),
                                           plotOutput("trend_1"),
                                           p(HTML("<b><i>No, the relationship with new cases and new deaths are linear.</i></b>"), style = "color:red")
                                       ),
                                       conditionalPanel(
                                           condition = "input.trends_multi == 'positive_rate'",
                                           br(),
                                           plotOutput("trend_2"),
                                           p(HTML("<b><i>No, the relationship with new cases and positive rate are parabolic</i></b>"), style = "color:red")
                                       ),
                                       conditionalPanel(
                                           condition = "input.trends_multi == 'hosp_patients'",
                                           br(),
                                           plotOutput("trend_3"),
                                           p(HTML("<b><i>Yes! The relationship between hospital patients and time is inversely proportional. As time increased the number of hospital patients decreased. Understanding these trends are important for governing bodies, financial markets and businesses to plan for the future. Brainstorm different policies that governments might change/create,  effects this would have on financial markets or decisions businesses might make in the future based on these trends.</i></b>"), style = "color:green")
                                       )
                                   )
                               )
                        ),
                        
                        # Continue button.
                        fluidRow(
                            conditionalPanel(
                                condition = "typeof display_section_3 == \"undefined\"",
                                column(6, align="center", offset=3,
                                       br(),
                                       actionButton("display_section_4", "Continue", width = "200px", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                )
                            )
                        ),
                    
                        # Information box on economics, and why there could be an effect with COVID and equity prices.
                        column(
                            width = 12,
                            conditionalPanel(
                                condition = "typeof display_section_4 == \"undefined\"",
                                br(),
                                br(),
                                br(),
                                box(
                                    width = 12,
                                    title = "COVID-19's Impact on Stock Prices",
                                    p("The stock market is a place where people can buy and sell ownership of a company. Having ownership (called shares) within a company entitles you to a portion of all future earnings and dividends generated by that company corresponding to how much of the company you own. Share prices are constantly changing, so a few points must be considered before investing:"),
                                    p("1. How will the company perform in the future?"),
                                    p("2. What is the potential return on my investment?"),
                                    p("3. How risky is it to invest in the company?"),
                                    p("4. What is the overall risk/reward?"),
                                    p("The outbreak of Covid-19 significantly changed the way people answered the first 2 questions. It destroyed profit-making opportunities for some companies (e.g. Qantas, Flight Centre), causing their share prices to fall, and created new opportunities for others (e.g. Zoom, Microsoft), which saw their share prices rise.")
                                )
                            )
                        ),
                        
                        # Continue button.
                        fluidRow(
                            conditionalPanel(
                                condition = "typeof display_section_4 == \"undefined\"",
                                column(6, align="center", offset=3,
                                       br(),
                                       actionButton("display_section_5", "Continue", width = "200px", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                )
                            )
                        ),
                    
                        # BHP Share Price vs COVID Cases
                        column(width = 12,
                               conditionalPanel(
                                   condition = "typeof display_section_5 == \"undefined\"",
                                   br(),
                                   br(),
                                   br(),
                                   box(
                                       width = 4,
                                       title = "How COVID Cases impacted BHP's stock price",
                                       p("The stock price of a business reflects its value and can be used to evaluate its past performance, and whether a business has fulfilled its role in an economy. BHP Billiton, a company listed in the ASX top 50, is one of the world’s largest multinational mining companies, with headquarters located in Melbourne, Australia. The graph to the right illustrates the BHP stock price and the new COVID cases in Australia. "),
                                       p(HTML("<b><i>Have a look at the graph below, which illustrates the BHP stock price and the COVID cases in Australia. What are the ways COVID could impat a mining company?</i></b>"), style = "color:blue")
                                   ),
                                   box(
                                       width = 8,
                                       title = "BHP vs New COVID Cases in Australia",
                                       plotOutput("bhp")
                                   )
                                   
                               )
                        ),
                        
                        # Continue button.
                        fluidRow(
                            conditionalPanel(
                                condition = "typeof display_section_5 == \"undefined\"",
                                column(6, align="center", offset=3,
                                       br(),
                                       actionButton("display_section_6", "Continue", width = "200px", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                )
                            )
                        ),
                        
                        # Choose your own stock vs COVID statistic
                        column(width = 12,
                               conditionalPanel(
                                   condition = "typeof display_section_6 == \"undefined\"",
                                   br(),
                                   br(),
                                   br(),
                                   box(
                                       width = 4,
                                       title = "Your Own Stock vs Your Own COVID Statistic",
                                       p("Now it’s time to explore some businesses you’re interested and compare their stock performance to different world COVID statistics."),
                                       selectInput(
                                           inputId = "covid_stat_choice",
                                           label = "Choose your COVID statistic:",
                                           choices = c(
                                               "Total Cases" = "total_cases",
                                               "New Cases Smoothed" = "new_cases_smoothed",
                                               "Total Deaths" = "total_deaths",
                                               "New Deaths Smoothed" = "new_deaths_smoothed",
                                               "Reproduction Rate" = "reproduction_rate"
                                           )
                                       ),
                                       textInput(inputId = "stock_choice", label = "Stock Code (USE .AX for ASX Stocks)", value = "AAPL"),
                                       p(HTML("<b><i>If the Stock Code is invalid (i.e. it doesn't exist), then \"AAPL\" is used by default.</i></b>"), style = "color:red"),
                                       p(HTML("<b><i>Can you see any correlation between the share price and your chosen COVID statistic?</i></b>"), style = "color:blue"),
                                       p(HTML("<b><i>Has your business fared well or poorly during COVID?</i></b>"), style = "color:blue"),
                                       p(HTML("<b><i>Specifically, how do you think COVID has impacted the business and it’s stock price? If there seems to be no significant change in the share price, explain why that might be.</i></b>"), style = "color:blue"),
                                       p(HTML("<b><i>Would you invest in this stock? What is the risk/reward?</i></b>"), style = "color:blue")
                                       
                                   ),
                                   box(
                                       width = 8,
                                       title = "Stock vs COVID",
                                       plotOutput("own_stock_and_covid_statistic")
                                   )
                               )
                        ),
                        
                        # Continue button.
                        fluidRow(
                            conditionalPanel(
                                condition = "typeof display_section_6 == \"undefined\"",
                                column(6, align="center", offset=3,
                                       br(),
                                       actionButton("display_section_7", "Continue", width = "200px", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                )
                            )
                        ),
 
                        # Different industries information box.
                        column(
                            width = 12,
                            conditionalPanel(
                                condition = "typeof display_section_7 == \"undefined\"",
                                br(),
                                br(),
                                br(),
                                box(
                                    width = 12,
                                    title = "Discussion Questions",
                                    p(HTML("<b><i>What are the roles of a business in the economy?</i></b>"), style = "color:blue"),
                                    p(HTML("<b><i>Why is the share price important for businesses and investors?</i></b>"), style = "color:blue"),
                                    p(HTML("<b><i>Has BHP been impacted by COVID in Australia? Why/why not?</i></b>"), style = "color:blue"),
                                    p(HTML("<b><i>What are the risks an investor should know if they were investing in BHP now?</i></b>"), style = "color:blue")
                                )
                            )
                        ),
                        
                        # Continue button.
                        fluidRow(
                            conditionalPanel(
                                condition = "typeof display_section_7 == \"undefined\"",
                                column(6, align="center", offset=3,
                                       br(),
                                       actionButton("display_section_8", "Continue", width = "200px", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                )
                            )
                        ),
                        
                        # Graph on comparing industries in the asx.
                        column(width = 12,
                               conditionalPanel(
                                   condition = "typeof display_section_8 == \"undefined\"",
                                   br(),
                                   br(),
                                   br(),
                                   box(
                                       width = 12,
                                       title = "Comparing Different ASX Industries",
                                       p("It is undeniable that COVID has affected the lives of all. This is also reflected in the stock market. The graph below is the average stock price for each industry in the Australian stock market (ASX)."),
                                       plotly::plotlyOutput("asx_industries"),
                                       br(),
                                       p(HTML("<b>Tip: Too many lines on the graph? In the legend on the right, you can de-select some industries! Also, try zooming in on the graph by dragging and selecting the area of interest!</b>"), style = "color:black"),
                                       br()
                                    )
                               )
                        ),
                        
                        column(width = 12,
                               conditionalPanel(
                                   condition = "typeof display_section_8 == \"undefined\"",
                                   box(
                                       width = 12,
                                       title = "Question",
                                       p(HTML("<b><i>The pandemic was declared on 11th of March 2020 (see the black line on the plot) and is continuing until today.</i></b>"), style = "color:blue"),
                                       p(HTML("<b><i>From sight, which industry’s price seems to have benefitted the most during the COVID pandemic?</i></b>"), style = "color:blue"),
                                       radioButtons("asx_multi", "Answer",
                                                    c("Automobile & Components" = "automobile",
                                                      "Energy" = "energy",
                                                      "Retailing" = "retailing",
                                                      "Technology Hardware & Equipment" = "technology",
                                                      "Utilities" = "utilities"), selected = character(0)),
                                       conditionalPanel(
                                           condition = "input.asx_multi == 'automobile' || input.asx_multi == 'energy' || input.asx_multi == 'retailing' || input.asx_multi == 'utilities'",
                                           br(),
                                           p(HTML("<b><i>Not quite... Have another look at the graph, and try again!</i></b>"), style = "color:red")
                                       ),
                                       conditionalPanel(
                                           condition = "input.asx_multi == 'technology'",
                                           br(),
                                           p(HTML("<b><i>Correct! Great job! </i></b>"), style = "color:green")
                                       )
                                   )
                               )
                        ),
                        
                        
                        
                        
                        # Continue button.
                        fluidRow(
                            conditionalPanel(
                                condition = "typeof display_section_8 == \"undefined\"",
                                column(6, align="center", offset=3,
                                       br(),
                                       actionButton("display_section_9", "Continue", width = "200px", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                )
                            )
                        ),
                        
                        # Graph on comparing industries performance.
                        column(width = 12,
                               conditionalPanel(
                                   condition = "typeof display_section_9 == \"undefined\"",
                                   br(),
                                   br(),
                                   br(),
                                   box(
                                       width = 4,
                                       title = "Industry Performance Overseas",
                                       p("The impact of the pandemic was not only felt in Australia, but also globally. The graph to the right explores the affect that COVID had on industries in Australia and in the U.S., by comparing overall stock prices of the different industries listed on the ASX and DOW JONES Index."),
                                       p("This time, you get to choose which industries you plot by ticking the checkboxes below (if no box is selected, \"Health Care\" is shown):"),
                                       checkboxGroupInput("global_industries", "Select industires", 
                                                          c('Health Care Equipment & Services' = 'health_care',
                                                            'Insurance' = 'insurance',
                                                            'Pharmaceuticals' = 'pharmaceuticals',
                                                            'Semiconductors & Semiconductor Equipment' = 'semiconductor',
                                                            'Software & Services' = 'software',
                                                            'Technology Hardware & Equipment' = 'technology_hardware',
                                                            'Telecommunication Services' = 'telecom',
                                                            'Transportation' = 'transportation')),
                                       p(HTML("<b><i>Compare the industry you are the most interested in and discuss their similarities with the person next to you</i></b>"), style = "color:blue"),
                                       p(HTML("<b><i>For your chosen industry, how has the pandemic affected its stock price?</i></b>"), style = "color:blue"),
                                       p(HTML("<b><i>For your chosen industry, does the vaccination seem to affect it?</i></b>"), style = "color:blue"),
                                       p(HTML("<b><i>Which industries were the hit the hardest during COVID? What are the likely changes that these industries may have made during this pandemic?</i></b>"), style = "color:blue")
                                   ),
                                   box(
                                       width = 8,
                                       title = "ASX + Dow Jones Graph",
                                       plotly::plotlyOutput("comparing_industries", height = "550px")
                                   )
                               )
                        ),
                        
                        # Continue button.
                        fluidRow(
                            conditionalPanel(
                                condition = "typeof display_section_9 == \"undefined\"",
                                column(6, align="center", offset=3,
                                       br(),
                                       actionButton("display_section_10", "Continue", width = "200px", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                )
                            )
                        ),
                        
                        # Diversification info box
                        column(
                            width = 12,
                            conditionalPanel(
                                condition = "typeof display_section_10 == \"undefined\"",
                                br(),
                                br(),
                                br(),
                                box(
                                    width = 12,
                                    title = "Diversification",
                                    p("So far we’ve covered the impacts COVID has had on different industries, individual businesses and even how they influence individual’s decision’s to invest in them. Investing in business stocks can be a great way to make money if you use the right information. However, there are still risks many risks associated with this and no single investment will always be the best. One way to mitigate this risk is to diversify your investments, this involves spreading your money across several different investments. An investment portfolio is a tool where investors will own a number of different stocks so they are not subjected to changes in just one businesses stock. For instance, if you bought Apple shares and Qantas shares just before COVID, your Qantas shares may have sharply fallen, but Apple still performed reasonably well. So by owning both you have reduced the risk of the fall in the value of investments, as opposed to if you just had all your money in Qantas.")
                                )
                            )
                        ),
                        
                        # Continue button.
                        fluidRow(
                            conditionalPanel(
                                condition = "typeof display_section_10 == \"undefined\"",
                                column(6, align="center", offset=3,
                                       br(),
                                       actionButton("display_section_11", "Continue", width = "200px", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                )
                            )
                        ),
                        
                        # Portfolio picker introduction
                        column(
                            width = 12,
                            conditionalPanel(
                                condition = "typeof display_section_11 == \"undefined\"",
                                br(),
                                br(),
                                br(),
                                box(
                                    width = 12,
                                    title = "Portfolio Picker Introduction",
                                    p("Now it’s time to apply your knowledge! You are tasked with building your own portfolio of 4 stocks to invest in, during COVID. Here a few things to consider:"),
                                    p("- How do you evaluate a portfolio’s success?"),
                                    p("- What are the advantages of investing in a portfolio structure?"),
                                    p("- What are the factors (personal and economic circumstances) that influence investment decisions?"),
                                    p("- What are the risk’s associated with the stocks you are choosing?"),
                                    br(),
                                    p("Once you have created your portfolio, put them through the portfolio builder tool and to see how an initial $10,000 investment would perform during COVID. This tool allows you to pick stocks and the weights they have in the portfolio. You will be able to pick 3 different variations of the weights and see how these changes impact the portfolio’s returns. Evaluate your portfolio’s success and the factors that contributed to their outcome. Then consider the reasons some portfolio's perform better than others."),
                                    p(HTML("<b>NOTE: Please wait some time, when entering new symbols, make sure the symbols are correct and that your weights add to 1.</b>"))
                                )
                            )
                        ),
                        
                        column(width = 12,
                               conditionalPanel(
                                 condition = "typeof display_section_11 == \"undefined\"",
                                 box(width = 3,
                                     title = "Choose your stocks",
                                     textInput(inputId = "stock_1", label = "Stock 1 Code", value = "AAPL"),
                                     textInput(inputId = "stock_2", label = "Stock 2 Code", value = "ANZ.AX"),
                                     textInput(inputId = "stock_3", label = "Stock 3 Code", value = "BHP.AX"),
                                     textInput(inputId = "stock_4", label = "Stock 4 Code", value = "TSLA")
                                   
                                 ),
                                 box(width = 3,
                                     title = "Porfolio 1 Weights",
                                     numericInput(inputId = "p1_w1", label = "Stock Weight 1", value = "0.25"),
                                     numericInput(inputId = "p1_w2", label = "Stock Weight 1", value = "0.25"),
                                     numericInput(inputId = "p1_w3", label = "Stock Weight 1", value = "0.25"),
                                     numericInput(inputId = "p1_w4", label = "Stock Weight 1", value = "0.25")
                                     
                                 ),
                                 box(width = 3,
                                     title = "Porfolio 2 Weights",
                                     numericInput(inputId = "p2_w1", label = "Stock Weight 2", value = "0.25"),
                                     numericInput(inputId = "p2_w2", label = "Stock Weight 2", value = "0.25"),
                                     numericInput(inputId = "p2_w3", label = "Stock Weight 2", value = "0.25"),
                                     numericInput(inputId = "p2_w4", label = "Stock Weight 2", value = "0.25")
                                   
                                 ),
                                 box(width = 3,
                                     title = "Porfolio 3 Weights",
                                     numericInput(inputId = "p3_w1", label = "Stock Weight 3", value = "0.25"),
                                     numericInput(inputId = "p3_w2", label = "Stock Weight 3", value = "0.25"),
                                     numericInput(inputId = "p3_w3", label = "Stock Weight 3", value = "0.25"),
                                     numericInput(inputId = "p3_w4", label = "Stock Weight 3", value = "0.25")
                                     
                                   
                                 )
                                 
                               )
                        ),
                        
                        # Portfolio Picker
                        column(width = 12,
                               conditionalPanel(
                                   condition = "typeof display_section_11 == \"undefined\"",
                                   box(
                                       width = 12,
                                       title = "Portfolio Builder",
                                       plotly::plotlyOutput("portfolio_picker")
                                   )
                               )
                        ),
                        
                        # Continue button.
                        fluidRow(
                            conditionalPanel(
                                condition = "typeof display_section_11 == \"undefined\"",
                                column(6, align="center", offset=3,
                                       br(),
                                       actionButton("display_section_12", "Continue", width = "200px", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                )
                            )
                        ),
                        
                        
                        # Conclusion
                        column(
                            width = 12,
                            conditionalPanel(
                                condition = "typeof display_section_12 == \"undefined\"",
                                br(),
                                br(),
                                br(),
                                box(
                                    width = 12,
                                    title = "Conclusion",
                                    p("Great job on getting through this lesson!"),
                                    p("Please review the lesson objectives at the top of this page, and ensure that you are comfortable with all of the learning objectives."),
                                    p("Thank you to our student and teacher testers! Please fill out the survey that has been provided elsewhere!")
                                )
                            )
                        ),
                        
                        # Extension Information
                        column(
                            width = 7,
                            conditionalPanel(
                                condition = "typeof display_section_12 == \"undefined\"",
                                infoBox("Extension", "If you finished early, click on the \"Extension\" tab to engage with some extension exercises!",
                                        icon = icon("chevron-left"), color = "purple", width = NULL, fill = TRUE)
                            )
                        )
                    )
                    
            ),
            
            tabItem(tabName = "extension",
                    
                    column(
                        width = 12,
                        conditionalPanel(
                            condition = "typeof display_section_12 != \"undefined\"",
                            infoBox("LOCKED", "Please finish the lesson in \"Let's Explore\" before accessing the extension content!",
                                    icon = icon("lock"), color = "red", width = NULL, fill = TRUE)
                        )
                    ),
                    
                    
                    column(
                        width = 12,
                        conditionalPanel(
                            condition = "typeof display_section_12 == \"undefined\"",
                            box(
                                width = 12,
                                title = "Extension",
                                p("In the lesson we have explored the impact that COVID has had on businesses and stock performance. In this extension we will be using a data model to further our understanding of the relationship between certain COVID factors and how they might impact stock price. This will be achieved using a Vector Auto Regressive (VAR) model, which will predict the 10 day return of average industry stock price given certain COVID variables."),
                                br(),
                                p(HTML("<b><i>What is a VAR model?</i></b>"), style = "color:blue"),
                                p("It is a regression model used to predict variables that are historically related to each other. For example, the price of a stock today is related to the price it was yesterday."),
                                br(),
                                p(HTML("<b><i>How is 10 day return of average industry price calculated?</i></b>"), style = "color:blue"),
                                p("We’ve gathered around 2000 ASX listed companies’ stock prices and grouped them by their industries. Then for each day, we calculated the average stock prices for each of the industries. Then we compared today’s price and price 10 days prior like this formula: (today’s price – 10 days before price) / (10 days before price)"),
                                br(),
                                p(HTML("<b><i>What do each of the COVID variables means?</i></b>"), style = "color:blue"),
                                p(HTML("<b>New cases:</b> This is the number of new covid cases arising each day.")),
                                p(HTML("<b>Reproduction rate:</b> This is the rate of how much a single case of covid spreads to other people.")),
                                p(HTML("<b>Hospital patients per million:</b> Number of hospital patients per million people.")),
                                p(HTML("<b>New tests:</b> Number of covid tests conducted each day.")),
                                p(HTML("<b>Positive rate:</b> The portion of people that test positive for covid.")),
                                p(HTML("<b>Stringency index:</b> This is a metric used to measure the “strictness” of the response by a country to Covid. It is a score based on number of variables. For example, a stay-at-home requirement would change this score.")),
                                br(),
                                p(HTML("<b><i>Why only these variables?</i></b>"), style = "color:blue"),
                                p("In order to the VAR model to produce any valid result, number of assumptions must be met for each variable. These were the only ones that met the criteria for our dataset."),
                                br(),
                                p(HTML("<b><i>What is happening in the model?</i></b>"), style = "color:blue"),
                                p("The model is being trained on data from April of 2020 to February of 2022. We are predicting 10 days over the March 2022 period."),
                                br(),   
                                p("For your chosen industry, choose a variable to predict the 10 day return rate for the next 10 days.")
                            ),
                            box(
                                width = 12,
                                title = "Predicting 10 day return of average industry price with VAR model.",
                                plotOutput("extension")
                            ),
                            box(
                              width = 6,
                              title = "Choose your COVID variable",
                              
                              checkboxGroupInput("ext_variable", "COVID variables:",
                                                 c("New cases" = "new_cases",
                                                   "Reproduction rate" = "reproduction_rate",
                                                   "Hospital patients per million" = "hosp_patients_per_million",
                                                   "New tests" = "new_tests",
                                                   "Positive rate" = "positive_rate",
                                                   "Stringency index" = "stringency_index"), selected = c("new_cases"))
                              
                            ),
                            box(
                              width = 6,
                              title = "Industry variable",
                              
                              selectInput(
                                inputId = "industry_choice",
                                label = "Choose your Industry:",
                                choices = c(
                                  "Automobiles & Components" = "Automobiles & Components",
                                  "Banks" = "Banks",
                                  "Capital Goods" = "Capital Goods",
                                  "Commercial & Professional Services" = "Commercial & Professional Services",
                                  "Consumer Durables & Apparel" = "Consumer Durables & Apparel",
                                  "Consumer Services" = "Consumer Services",
                                  "Diversified Financials" = "Diversified Financials",
                                  "Energy" = "Energy",
                                  "Food & Staples Retailing" = "Food & Staples Retailing",
                                  "Food, Beverage & Tobacco" = "Food, Beverage & Tobacco",
                                  "Health Care Equipment & Services" = "Health Care Equipment & Services",
                                  "Household & Personal Products" = "Household & Personal Products",
                                  "Insurance" = "Insurance",
                                  "Materials" = "Materials",
                                  "Media & Entertainment" = "Media & Entertainment",
                                  "Pharmaceuticals, Biotechnology & Life Sciences" = "Pharmaceuticals, Biotechnology & Life Sciences",
                                  "Real Estate" = "Real Estate",
                                  "Retailing" = "Retailing",
                                  "Semiconductors & Semiconductor Equipment" = "Semiconductors & Semiconductor Equipment",
                                  "Software & Services" = "Software & Services",
                                  "Technology Hardware & Equipment" = "Technology Hardware & Equipment",
                                  "Telecommunication Services" = "Telecommunication Services",
                                  "Transportation" = "Transportation",
                                  "Utilities" = "Utilities"
                                )
                              )
                              
                              
                            )
                            
                        )
                    ),
                    column(
                      width = 12,
                      conditionalPanel(
                        condition = "typeof display_section_12 == \"undefined\"",
                        box(
                          width = 12,
                          title = "Discussion questions",
                          p(HTML("<b><i>Does the model predict accurately? Discuss the result</i></b>"), style = "color:blue"),
                          p(HTML("<b><i>Try choosing another variable or multiple of them. Does the prediction improve?</i></b>"), style = "color:blue"),
                          p(HTML("<b><i>If prediction improved, why do you think it improved?</i></b>"), style = "color:blue"),
                          p(HTML("<b><i>If the predictions did not improve, why do you think the predictions are not accurate?</i></b>"), style = "color:blue")
                        ),
                        column(11, align="center", offset=1,
                               br(),
                               actionButton("ext_answer", "Suggested Answer", width = "150px", style="color: #000000; background-color: #d9d1d0; border-color: #2e6da4"),
                               br(),
                               br(),
                               br()
                        )
                        
                     
                        
                      )
                    )
                    
                    
                    
                    
                    
                    
            ),
            
            tabItem(tabName = "references",
            
                    titlePanel(
                        h2("R-Package References")
                    ),
                    br(),
                    box(
                      width = 12,
                      title = "data.table",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsed = TRUE,
                      p("data.table: Extension of `data.frame`"),
                      p("Matt Dowle and Arun Srinivasan (2021)"),
                      p("R package version 1.14.2"),
                      p("https://CRAN.R-project.org/package=data.table")
                    ),
                    box(
                      width = 12,
                      title = "forecast",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsed = TRUE,
                      p("forecast: Forecasting functions for time series and linear models"),
                      p("Hyndman R, Athanasopoulos G, Bergmeir C, Caceres G, Chhay L, O'Hara-Wild M, Petropoulos F, Razbash S, Wang E, Yasmeen F (2022)"),
                      p("R package version 8.16"),
                      p("https://pkg.robjhyndman.com/forecast/")
                    ),
                    box(
                      width = 12,
                      title = "gganimate",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsed = TRUE,
                      p("gganimate: A Grammar of Animated Graphics"),
                      p("Thomas Lin Pedersen, David Robinson (2020)"),
                      p("https://CRAN.R-project.org/package=gganimate")
                    ),
                    box(
                      width = 12,
                      title = "lubridate",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsed = TRUE,
                      p("Dates and Times Made Easy with lubridate"),
                      p("Garrett Grolemund, Hadley Wickham (2011)"),
                      p("Volume: 40(3)"),
                      p("Pages: 1-25"),
                      p("https://www.jstatsoft.org/v40/i03/")
                    ),
                    box(
                      width = 12,
                      title = "memoise",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsed = TRUE,
                      p("memoise: 'Memoisation' of Functions"),
                      p("Christoph Sax (2021)"),
                      p("R package"),
                      p("https://www.tsbox.help")
                    ),
                    box(
                        width = 12,
                        title = "plotly",
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        status = "primary",
                        collapsed = TRUE,
                        p("Interactive Web-Based Data Visualization with R, plotly, and shiny"),
                        p("Carson Sievert (2020)"),
                        p("Published: Chapman and Hall/CRC"),
                        p("ISBN: 9781138331457"),
                        p("https://plotly-r.com")
                    ),
                    box(
                      width = 12,
                      title = "purr",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsed = TRUE,
                      p("purrr: Functional Programming Tools"),
                      p("Lionel Henry, Hadley Wickham (2020)"),
                      p("R package version 0.3.4"),
                      p("https://CRAN.R-project.org/package=purrr")
                    ),
                    box(
                      width = 12,
                      title = "quantmod",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsed = TRUE,
                      p("quantmod: Quantitative Financial Modelling Framework"),
                      p("Jeffrey A. Ryan, Joshua M. Ulrich (2022)"),
                      p("http://www.quantmod.com https://github.com/joshuaulrich/quantmod")
                    ),
                    box(
                      width = 12,
                      title = "RCurl",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsed = TRUE,
                      p("purrr: Functional Programming Tools"),
                      p("Duncan Temple Lang (2022)"),
                      p("R package version 1.98-1.6"),
                      p("https://CRAN.R-project.org/package=RCurl")
                    ),
                    box(
                      width = 12,
                      title = "shiny",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsed = TRUE,
                      p("shiny: Web Application Framework for R"),
                      p("Winston Chang, Joe Cheng, JJ Allaire, Carson Sievert, Barret Schloerke, Yihui Xie, Jeff Allen, Jonathan McPherson, Alan Dipert, Barbara Borges (2021)"),
                      p("R package version 1.7.1"),
                      p("https://CRAN.R-project.org/package=shiny")
                    ),
                    box(
                      width = 12,
                      title = "shinydashboard",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsed = TRUE,
                      p("shinydashboard: Create Dashboards with 'Shiny'"),
                      p("Winston Chang, Barbara Borges Ribeiro (2021)"),
                      p("R package version 0.7.2"),
                      p("https://CRAN.R-project.org/package=shinydashboard")
                    ),
                    box(
                      width = 12,
                      title = "shinydashboardPlus",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsed = TRUE,
                      p("shinydashboardPlus: Add More 'AdminLTE2' Components to 'shinydashboard'"),
                      p("David Granjon (2021)"),
                      p("R package version 2.0.3"),
                      p("https://CRAN.R-project.org/package=shinydashboardPlus")
                    ),
                    box(
                        width = 12,
                        title = "stringr",
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        status = "primary",
                        collapsed = TRUE,
                        p("stringr: Simple, Consistent Wrappers for Common String Operations"),
                        p("Hadley Wickham (2019)"),
                        p("R package version 1.4.0"),
                        p("https://CRAN.R-project.org/package=stringr")
                    ),
                    box(
                      width = 12,
                      title = "sqldf",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsed = TRUE,
                      p("sqldf: Manipulate R Data Frames Using SQL"),
                      p("G. Grothendieck (2017)"),
                      p("R package version 0.4-11"),
                      p("https://CRAN.R-project.org/package=sqldf")
                    ),
                    box(
                      width = 12,
                      title = "tidyverse",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsed = TRUE,
                      p("Welcome to the tidyverse"),
                      p("Hadley Wickham, Mara Averick, Jennifer Bryan, Winston Chang, Lucy D'Agostino McGowan, Romain François, Garrett Grolemund, Alex Hayes, Lionel Henry, Jim Hester, Max Kuhn, Thomas Lin Pedersen, Evan Miller, Stephan Milton Bache, Kirill Müller, Jeroen Ooms, David Robinson, Dana Paige Seidel, Vitalie Spinu, Kohske Takahashi, Davis Vaughan, Claus Wilke, Kara Woo, Hiroaki Yutani (2019)"),
                      p("Journal of Open Source Software, volume 4, number 43"),
                      p("DOI: 10.21105/joss.01686")
                    ),
                    box(
                      width = 12,
                      title = "tsbox",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsed = TRUE,
                      p("tsbox: Class-Agnostic Time Series in in {R}"),
                      p("Christoph Sax (2021)"),
                      p("R package"),
                      p("https://www.tsbox.help")
                    ),
                    box(
                        width = 12,
                        title = "zoo",
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        status = "primary",
                        collapsed = TRUE,
                        p("zoo: S3 Infrastructure for Regular and Irregular Time Series"),
                        p("Achim Zeileis, Gabor Grothendieck (2005)"),
                        p("Journal of Statistical Software"),
                        p("Volume: 14(6)"),
                        p("Pages: 1-27"),
                        p("10.18637/jss.v014.i06")
                    ),
                    box(
                      width = 12,
                      title = "vars",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsed = TRUE,
                      p("VAR, SVAR and SVEC Models: Implementation Within R Package vars"),
                      p("Bernhard Pfaff (2008)"),
                      p("Journal of Statistical Software 27(4)"),
                      p("https://www.jstatsoft.org/v27/i04/")
                    ),
                    br(),
                    titlePanel(
                        h2("API References")
                    ),
                    br(),
                    box(
                        width = 12,
                        title = "ASX company directory",
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        status = "success",
                        collapsed = TRUE,
                        p("ASX company directory"),
                        p("https://asx.api.markitdigital.com/asx-research/1.0/companies/directory/file?access_token=83ff96335c2d45a094df02a206a39ff4")
                    ),
                    box(
                        width = 12,
                        title = "Yahoo Finance API",
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        status = "success",
                        collapsed = TRUE,
                        p("YH Finance API Specification"),
                        p("https://www.yahoofinanceapi.com/")
                    ),
                    br(),
                    titlePanel(
                        h2("Other References")
                    ),
                    br(),
                    box(
                      width = 12,
                      title = "APIs YouTube Video",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      status = "info",
                      collapsed = TRUE,
                      p("Simply Explained (2019, November 13)"),
                      p("What Are APIs? - Simply Explained"),
                      p("https://www.youtube.com/watch?v=OVvTv9Hy91Q")
                    ),
                    box(
                      width = 12,
                      title = "Australia COVID-19 Timeline",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      status = "info",
                      collapsed = TRUE,
                      p("Cassidy Knowlton (2022, April 28)"),
                      p("A timeline of Covid-19 in Australia, two years on"),
                      p("https://www.timeout.com/melbourne/things-to-do/a-timeline-of-covid-19-in-australia-two-years-on")
                    ),
                    box(
                        width = 12,
                        title = "Immunize.org COVID-19 Timeline",
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        status = "info",
                        collapsed = TRUE,
                        p("Immunize.org"),
                        p("Vaccine Timeline. (2020, April 28)"),
                        p("https://www.immunize.org/timeline/?fbclid=IwAR2LcuaoWM0VPwv0vVCkgo9vRjmdoNsQva_9a97N008B4kUy0CpTReWL3xY")
                    ),
                    box(
                        width = 12,
                        title = "Immunize.org COVID-19 Timeline",
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        status = "info",
                        collapsed = TRUE,
                        p("Kate Whiting, Johnny Wood (2021, December 8)"),
                        p("Two years of COVID-19: Key milestones in the pandemic"),
                        p("https://www.weforum.org/agenda/2021/12/covid19-coronavirus-pandemic-two-years-milestones/")
                    ),
                    box(
                        width = 12,
                        title = "Vaccine Milestone Australia",
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        status = "info",
                        collapsed = TRUE,
                        p("Melissa Clarke (2021, November 6)"),
                        p("Scott Morrison hails vaccine milestone as double-dose vaccination rate hits 80 per cent for Australians over 16"),
                        p("https://www.abc.net.au/news/2021-11-06/pm-hails-80pc-vaccine-milestone/100600132")
                    ),
                    box(
                      width = 12,
                      title = "WHO COVID-19 Timeline",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      status = "info",
                      collapsed = TRUE,
                      p("WHO"),
                      p("Archived: WHO Timeline - COVID-19. (2022, April 1)"),
                      p("https://www.who.int/news/item/27-04-2020-who-timeline---covid-19")
                    )
                    
                    
                    
                    
                    
            
            )
            )
        
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Feedback button box
    output$feedback_button <- renderValueBox({
        valueBox(
            "Feedback", "Please click to report errors or provide feedback!", icon = icon("comment"),
            color = "light-blue",
        )
    })
    
    ####################################################
    ################# Lesson Objectives ################
    ####################################################
    
    observeEvent(input$objectives, {
        showModal(
            modalDialog(
                title = "Lesson Objectives",
                HTML("
                
                    - Investigate the concepts of financial markets and how:<br>
                    + Australian markets are affected by global interactions<br>
                    + Markets are affected by global social and economic events<br>
                    + Different markets are interconnected<br>
                    + Markets in similar industries may follow similar trends<br>

                    - The cyclical nature of social and economic events – cause and effect<br>
                    - Predict changes in economic markets based on global and local events<br>
                    - Understand the relationship between economic and social data<br>
                    - Stock markets as indicators of social confidence in the future economy<br>
                    - The effect international trade has on the demand of industries and their products<br>
                    - Construct a hypothetical portfolio and watch its changes in value<br>
                     ")
            )
        )
    })
    
    ####################################################
    ################# CONTINUE BUTTONS ################# 
    ####################################################
    
    # To delete first continue button on press.
    observeEvent(input$display_section_2, {
        removeUI(selector='#display_section_2', immediate=TRUE)
    }, autoDestroy=TRUE)
    
    # To delete second continue button on press.
    observeEvent(input$display_section_3, {
        removeUI(selector='#display_section_3', immediate=TRUE)
    }, autoDestroy=TRUE)
    
    # To delete third continue button on press.
    observeEvent(input$display_section_4, {
        removeUI(selector='#display_section_4', immediate=TRUE)
    }, autoDestroy=TRUE)
    
    # To delete fourth continue button on press.
    observeEvent(input$display_section_5, {
        removeUI(selector='#display_section_5', immediate=TRUE)
    }, autoDestroy=TRUE)
    
    # To delete fifth continue button on press.
    observeEvent(input$display_section_6, {
        removeUI(selector='#display_section_6', immediate=TRUE)
    }, autoDestroy=TRUE)
    
    # To delete sixth continue button on press.
    observeEvent(input$display_section_7, {
        removeUI(selector='#display_section_7', immediate=TRUE)
    }, autoDestroy=TRUE)
    
    # To delete seventh continue button on press.
    observeEvent(input$display_section_8, {
        removeUI(selector='#display_section_8', immediate=TRUE)
    }, autoDestroy=TRUE)
    
    # To delete eighth continue button on press.
    observeEvent(input$display_section_9, {
        removeUI(selector='#display_section_9', immediate=TRUE)
    }, autoDestroy=TRUE)
    
    # To delete ninth continue button on press.
    observeEvent(input$display_section_10, {
        removeUI(selector='#display_section_10', immediate=TRUE)
    }, autoDestroy=TRUE)
    
    # To delete tenth continue button on press.
    observeEvent(input$display_section_11, {
        removeUI(selector='#display_section_11', immediate=TRUE)
    }, autoDestroy=TRUE)
    
    # To delete eleventh continue button on press.
    observeEvent(input$display_section_12, {
        removeUI(selector='#display_section_12', immediate=TRUE)
    }, autoDestroy=TRUE)
    
    #####################################################
    ################# COVID CASES PLOTS ################# 
    #####################################################
    
    # Plotly graph containing only the cumulative covid cases (in standardised units) for the World and Australia
    # N.B. No milestones on this plot.
    output$covid_cases_no_milestones = plotly::renderPlotly({
        fig <- plot_ly(cases, x = ~date, y = ~total_cases_scaled, color = ~location,  hoverinfo = "text", hovertext = paste("Category :", cases$location, "<br>Date :", cases$date, "<br>Cases :", cases$total_cases)) %>%
            add_lines() %>%
            layout(yaxis = list(title = "Cumulative COVID Cases", zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE)) %>%
            layout(xaxis = list(title = "Date"))
        return(fig)
    })
    
    # Covid milestones from:
    # https://www.weforum.org/agenda/2021/12/covid19-coronavirus-pandemic-two-years-milestones/
    # https://www.timeout.com/melbourne/things-to-do/a-timeline-of-covid-19-in-australia-two-years-on
    # https://www.abc.net.au/news/2021-11-06/pm-hails-80pc-vaccine-milestone/100600132
    # https://www.washingtonpost.com/business/2020/04/02/jobless-march-coronavirus/
    # https://www.nih.gov/news-events/nih-research-matters/experimental-coronavirus-vaccine-safe-produces-immune-response
    # https://www.devex.com/news/world-hits-1m-covid-19-deaths-how-did-we-get-here-and-will-it-get-worse-98163
    # https://www.who.int/activities/tracking-SARS-CoV-2-variants
    
    # Plotly graph containing cumulative covid cases with some milestones (part 1).
    output$covid_cases_mil1 = plotly::renderPlotly({
        fig <- plot_ly(cases, x = ~date, y = ~total_cases_scaled, color = ~location,  hoverinfo = "text", hovertext = paste("Category :", cases$location, "<br>Date :", cases$date, "<br>Cases :", cases$total_cases)) %>%
            add_lines() %>%
            layout(yaxis = list(title = "Cumulative COVID Cases", zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE)) %>%
            layout(xaxis = list(title = "Date")) %>%
            layout(shapes = list(vline(x = "2020-03-11", color = "black"), vline(x = "2020-12-02", color = "black"), vline(x = "2021-02-22", color = "black"),  vline(x = "2021-06-25", color = "black"), vline(x = "2021-11-06", color = "black"))) %>%
            layout(annotations = list(x = "2020-03-14", y = 0.9, text = "(11-03-20) WHO declares \nCOVID-19 a pandemic", font = list(color = "black"), showarrow = FALSE, xanchor = "left" )) %>%
            layout(annotations = list(x = "2020-11-30", y = 0.7, text = "(02-12-20) UK becomes\n first country to \napprove Pfizer vaccine", font = list(color = "black"), showarrow = FALSE, xanchor = "right" )) %>%
            layout(annotations = list(x = "2021-02-25", y = 0.8, text = "(22-02-21)\nFirst\nvaccines\nadministered\nin Australia.", font = list(color = "black"), showarrow = FALSE, xanchor = "left" )) %>%
            layout(annotations = list(x = "2021-06-28", y = 0.8, text = "(25-06-21)\nSydney\nentering\nwidespread\nlockdown", font = list(color = "black"), showarrow = FALSE, xanchor = "left" )) %>%
            layout(annotations = list(x = "2021-06-28", y = 0.8, text = "(25-06-21)\nSydney\nentering\nwidespread\nlockdown", font = list(color = "black"), showarrow = FALSE, xanchor = "left" )) %>%
            layout(annotations = list(x = "2021-11-02", y = 0.3, text = "(06-11-21)\n80% of\nAustralians\nover 16 are\ndouble\nvacinated", font = list(color = "black"), showarrow = FALSE, xanchor = "right" ))
        return(fig)
    })
    
    # Plotly graph containing cumulative covid cases with some milestones (part 2).
    output$covid_cases_mil2 = plotly::renderPlotly({
        fig <- plot_ly(cases, x = ~date, y = ~total_cases_scaled, color = ~location,  hoverinfo = "text", hovertext = paste("Category :", cases$location, "<br>Date :", cases$date, "<br>Cases :", cases$total_cases)) %>%
            add_lines() %>%
            layout(yaxis = list(title = "Cumulative COVID Cases", zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE)) %>%
            layout(xaxis = list(title = "Date")) %>%
            layout(shapes = list(vline(x = "2020-03-28", color = "black"), vline(x = "2020-07-21", color = "black"), vline(x = "2020-09-28", color = "black"), vline(x = "2021-05-11", color = "black"))) %>%
            layout(annotations = list(x = "2020-03-31", y = 0.8, text = "(28-03-20)\nOver 10\nmillion\nAmericans\napplied\nfor\nunempl-\noyment\nbenefits", font = list(color = "black"), showarrow = FALSE, xanchor = "left" )) %>%
            layout(annotations = list(x = "2020-07-21", y = 0.3, text = "(21-07-20)\nFirst\nhuman\ntested\nvaccine\n(Moderna)\nshows\npromising\nsigns", font = list(color = "black"), showarrow = FALSE, xanchor = "right" )) %>%
            layout(annotations = list(x = "2020-10-01", y = 0.8, text = "(28-09-20) Global COVID\ndeaths reach\1 million", font = list(color = "black"), showarrow = FALSE, xanchor = "left" )) %>%
            layout(annotations = list(x = "2021-05-14", y = 0.8, text = "(11-05-21) Delta strain\nis declared a\nvariant of concern\nby WHO", font = list(color = "black"), showarrow = FALSE, xanchor = "left" ))
        return(fig)
    })
    
    #####################################################
    ################# VACCINATIONS PLOT ################# 
    #####################################################
    output$vaccinations = plotly::renderPlotly({
        p <- ggplot(vaccine_df, aes(people_fully_vaccinated_per_hundred, log10(new_deaths), color = continent , frame = month, ids = location)) +
            geom_point(aes(size = population)) 
        fig <- ggplotly(p)
        fig <- fig %>% 
            animation_opts(
                1000, easing = "elastic", redraw = TRUE
            )
        fig <- fig %>%
            animation_button(
                x = 1, xanchor = "right", y = 0, yanchor = "bottom"
            )
        return(fig)
    })
    
    #####################################################
    ################ Trend Question 1 Plot ############## 
    #####################################################
    output$trend_1 = renderCachedPlot({
        newcases_newdeaths = ggplot(covid_clean_SS, aes(new_deaths, new_cases_smoothed)) + geom_smooth() + labs(x = "New Deaths", y = "New Cases")
        return(newcases_newdeaths)
    },cacheKeyExpr = {covid_clean_SS})
    
    #####################################################
    ################ Trend Question 2 Plot ############## 
    #####################################################
    output$trend_2 = renderPlot({
        pos_newcases = ggplot(covid_clean_SS, aes(new_cases_per_million, positive_rate)) + geom_smooth() + labs(x = "New Cases Per Million", y = "Positive Rate")
        return(pos_newcases)
    })
    
    #####################################################
    ################ Trend Question 3 Plot ############## 
    #####################################################
    output$trend_3 = renderPlot({
        hosp_patients = ggplot(covid_clean_SS, aes(date, hosp_patients)) + geom_smooth() + labs(x = "Date", y = "Hospital Patients")
        return(hosp_patients)
    })
    
    #####################################################
    ############## BHP vs COVID Cases PLOT ############## 
    #####################################################
    output$bhp = renderPlot({
        Legend = "BHP Stock"
        p <- ggplot(data = subset(joined_data, !is.na(new_cases_smoothed)), aes(x = date)) +
            geom_line(aes(y = close, color = Legend)) +
            geom_ma(aes(y = close, color = "50 Day Moving Average"), ma_fun = EMA, n = 50, wilder = TRUE, linetype = 5, size = 1.25) +
            geom_line(aes(y = new_cases_smoothed/coeff, color = "Covid Cases", na.rm = TRUE)) +
            labs(x = "Date") +
            scale_y_continuous(
                
                # Features of the first axis
                name = "BHP close price ($)",
                # Add a second axis and specify its features
                sec.axis = sec_axis(~.*coeff, name="Case numbers", labels = scales::comma, breaks = round(seq(0, 120000, by = 20000),1)),
                breaks = round(seq(0, 50, by = 10),1))
        return(p)
    })
    
    #####################################################
    ###### Own Stock vs Own COVID statistic PLOT ########
    #####################################################
    output$own_stock_and_covid_statistic = renderPlot({
        print(input$stock_choice)
        print(input$covid_stat_choice)
        return(stock_vs_covid_variable(input$stock_choice, input$covid_stat_choice))
    })
    
    #####################################################
    ############### ASX Industries PLOT #################
    #####################################################
    output$asx_industries = plotly::renderPlotly({
        pandemic = as.numeric(as.Date("2020-03-11"))
        industry = ggplot() + geom_line(asx_prices_percent,mapping = aes(x = date, y = index, color = industry)) + xlab("Date") + ylab("Percentage")+  geom_vline(xintercept = pandemic)
        return(ggplotly(industry))
    })
    
    #####################################################
    ############ Comparing Industries PLOT ##############
    #####################################################
    output$comparing_industries = plotly::renderPlotly({
        
        industries = c()
        for (i in input$global_industries) {
            if (i == "health_care") {
                industries = append(industries, health_care)
            }
            if (i == "insurance") {
                industries = append(industries, insurance)
            }
            if (i == "pharmaceuticals") {
                industries = append(industries, pharmaceuticals)
            }
            if (i == "semiconductor") {
                industries = append(industries, semiconductor)
            }
            if (i == "software") {
                industries = append(industries, software)
            }
            if (i == "technology_hardware") {
                industries = append(industries, technology_hardware)
            }
            if (i == "telecom") {
                industries = append(industries, telecom)
            }
            if (i == "transportation") {
                industries = append(industries, transportation)
            }
        }
        
        # Default to health_care if nothing else is clicked
        if (length(industries) == 0) {
            industries = append(industries, health_care)
        }
        
        result = compare_industry(dj_asx,industries)
        return(ggplotly(result))
    })
    
    #####################################################
    ############## Portfolio Picker Plot ################
    #####################################################
    output$portfolio_picker = plotly::renderPlotly({
        
        weights = c(input$p1_w1, input$p1_w2, input$p1_w3, input$p1_w4, input$p2_w1, input$p2_w2, input$p2_w3, input$p2_w4, input$p3_w1, input$p3_w2, input$p3_w3, input$p3_w4)
        print(weights)
        stocks = c(input$stock_1, input$stock_2, input$stock_3, input$stock_4)
        print(stocks)
        fig <- portfolio_builder(stocks, weights)
        return(fig)
    })
    
    #####################################################
    ################# Extension Plot ####################
    #####################################################
    # output$extension = plotly::renderPlotly({
    #     
    #     print(input$ext_variable)
    #     var = input$ext_variable
    #     industry = input$industry_choice
    #     if (is.null(var)){
    #       df <- data.frame()
    #       ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 10) + labs(title = "Pick some variables and an industry")
    #     }
    #     else{
    #       
    #       fig <- extension_func(covid, asx_prices, asx, industry, var) 
    #      
    #       return(ggplotly(fig))
    #     } 
    #     
    # })
    output$extension <- renderCachedPlot({
           print(input$ext_variable)
           print(input$industry_choice)
           fig <- extension_func(input$industry_choice, input$ext_variable)
     
           return(fig)
           
       
     }, cacheKeyExpr = {list(input$ext_variable, input$industry_choice)})
    
    #####################################################
    ########### Extension Suggested Answer ##############
    #####################################################
    
    observeEvent(input$ext_answer, {
      showModal(
        modalDialog(
          title = "Suggested Answer",
          HTML("There are numerous factors impacting the stock prices and predicting with just COVID related health data limits the prediction heavily. Also, the prediction model we have chosen may not be optimal in predicting the returns. There are so many models that are already out there and are currently being developed as we speak to predict the movement of the market. However, we should be aware that there are no perfect models, and we should only use them as one of many things to consider in predicting a market’s movement.")
          
        )
      )
    })
    

        
}



# Run the application 
shinyApp(ui = ui, server = server)
