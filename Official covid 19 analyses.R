
library(rvest)
library(tidyverse)
library(dplyr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(shiny)
library(plotly)
library(scales)
url<- 'https://www.worldometers.info/coronavirus/?utm_campaign=homeAdvegas1?'
h<- read_html(url)
tab<- h%>%html_nodes('table')
tab<- tab%>%html_table()
dataset<- tab[[1]]

'%ni%'<- Negate('%in%')

cases<- ifelse(dataset$NewCases == ''|is.na(dataset$NewCases), 0, dataset$NewCases)
death_cases<- ifelse(dataset$NewDeaths == '', 0, dataset$NewCases)
extra_cases<- parse_number(cases)
covid_cases_data<- dataset
total_cases<- parse_number(covid_cases_data$TotalCases)
full_deaths<- total_cases+extra_cases
full_deaths_abb<- ifelse(nchar(full_deaths)>=7, paste(as.character(round(full_deaths/1000000,1)), 'M'), full_deaths)
full_deaths_abb<- ifelse(str_detect(full_deaths_abb, 'M')== FALSE& nchar(full_deaths)>4, paste(as.character(round(full_deaths/1000,1)), 'K'), full_deaths_abb)
covid_cases_data<- covid_cases_data%>%mutate(fulldeaths = full_deaths, fulldeathsabb = full_deaths_abb)

cases<- covid_cases_data%>%filter(`Country,Other`%ni% c('North America', 'South America', 'Asia', 'Europe', 'Africa', 'Oceania', 'World', '', 'Total:'))

paste('World covid 19 cases:  ' , paste(as.character(round(sum(cases$fulldeaths)/1000000,2)),'M'))

top_cases<- covid_cases_data%>%filter(`Country,Other`%ni% c('North America', 'South America', 'Asia', 'Europe', 'Africa', 'Oceania', 'World', ''))%>%head(20)%>%ggplot(aes(x = reorder(`Country,Other`, -(fulldeaths)), y = (fulldeaths)/1000000 ))+geom_bar(stat = 'identity', fill = 'steelblue')+geom_text(aes(label = paste(as.character(round((fulldeaths)/1000000,2)),'M')), vjust=1, size=3.5,col = 'black',hjust = 0, angle = 40)+ylab('total covid cases in millions')+xlab('countries')+theme(axis.text.x = element_text(angle = 60, hjust = 1))+ggtitle('Covid cases for top countries')

continet_cases<-covid_cases_data%>%filter(`Country,Other`%in% c('North America', 'South America', 'Asia', 'Europe', 'Africa', 'Oceania', 'World'))%>%plot_ly(x = ~reorder(`Country,Other`, -(fulldeaths)), y = ~fulldeaths/1000, type = 'bar',text = ~paste(`Country,Other`,':',as.character(fulldeathsabb),'cases'), hoverinfo = 'text',label = ~fulldeaths,texttemplate = ~fulldeaths)%>%config(displayModeBar = F)%>%layout(xaxis = list(title = 'Continents'), yaxis = list(title = 'confirmed cases'), title = 'Covid 19 cases')
continet_cases


deaths<- ifelse(is.na(dataset$NewDeaths)|dataset$NewDeaths == '', '0', dataset$NewDeaths)
extra_deaths<- as.numeric(parse_number(deaths))
total_deaths<- parse_number(dataset$TotalDeaths)
full_deaths<- extra_deaths+total_deaths
full_deaths_abb<- ifelse(nchar(full_deaths)>4, paste(as.character(round(full_deaths/1000,1)), 'K'), full_deaths)
covid_19_deaths<- dataset%>%mutate(fulldeaths = full_deaths, fulldeathsabb = full_deaths_abb)
covid_19_deaths<- covid_19_deaths%>%filter(`Country,Other`%ni% c('North America', 'South America', 'Asia', 'Europe', 'Africa', 'Oceania', 'World', ''))
top_deaths<- covid_19_deaths%>%head(10)%>%ggplot(aes(x = reorder(`Country,Other`, -(fulldeaths)), y = fulldeaths/1000))+geom_bar(stat = 'identity', fill = 'green')+geom_text(aes(label = paste(as.character(round(fulldeaths/1000,1)),'K')), vjust=1, size=3.5,col = 'black',hjust = 0, angle = 40)+ylab('covid deaths in thousands')+xlab('countries')+theme(axis.text.x = element_text(angle = 60, hjust = 1))+ggtitle('countries with top deaths')

death_cases<- covid_19_deaths%>%filter(`Country,Other`%ni% c('North America', 'South America', 'Asia', 'Europe', 'Africa', 'Oceania', 'World', '', 'Total:'))


paste('World covid 19 deaths:  ', paste(as.character(round(sum(death_cases$fulldeaths, na.rm=TRUE)/1000,1)),'K'))




percent_per_pop<-ifelse(covid_cases_data$Population == ''|is.na(covid_cases_data$Population) , 0,round(covid_cases_data$fulldeaths/ (parse_number(covid_cases_data$Population)),3))
covid_cases_data<- covid_cases_data%>%mutate(cases_per_pop =percent_per_pop)

covid_rates<- covid_cases_data%>%arrange(cases_per_pop)%>%tail(20)%>%ggplot(aes(x = reorder(`Country,Other`, -cases_per_pop), y = cases_per_pop))+geom_bar(stat = 'identity', fill = 'orange')+geom_text(aes(label = paste(as.character(cases_per_pop*100), '%')), vjust=1, color="black", size=3.5,hjust = 0, angle = 40)+ylab('covid cases per whole population')+xlab('countries')+theme(axis.text.x = element_text(angle = 60, hjust = 1))+ggtitle('countries with top covid rates')
cases_per_popu<- covid_cases_data%>%filter(`Country,Other`%ni% c('North America', 'South America', 'Asia', 'Europe', 'Africa', 'Oceania', 'World', '', 'Total:'))

paste('Covid 19 cases per world population:   ',  paste(as.character(round((sum(cases_per_popu$fulldeaths,na.rm = TRUE)/sum(parse_number(cases_per_popu$Population),na.rm = TRUE)*100),2)),'%'))
covid_cases_data<-covid_cases_data%>%mutate(dead = full_deaths)
covid_cases_data<- covid_cases_data%>%mutate(death_per_cases= dead/fulldeaths)

json_url<- 'https://covid.ourworldindata.org/data/owid-covid-data.json'

cases<- fromJSON(json_url)
first_data<- cases[[1]][['data']]
first_data<- first_data%>%mutate(date = as.Date(date))
first_data<- first_data%>%mutate(loc = cases[[1]]['location']%>%unlist())%>%mutate(population = cases[[1]]['population']%>%unlist())
union_data<- first_data%>%select(date, total_cases, new_cases, total_deaths, new_deaths,total_cases_per_million, new_cases_per_million,total_deaths_per_million, new_deaths_per_million, loc, population)
for(i in 2:length(cases)){
  country_data<- cases[[i]][['data']]
  country_data<- country_data%>%mutate(date = as.Date(date))
  country_data<- country_data%>%mutate(loc = cases[[i]]['location']%>%unlist())%>%mutate(population = cases[[i]]['population']%>%unlist())
  if(length(names(country_data))>=11){
    union_data<-union(union_data,country_data%>%select(date, total_cases, new_cases, total_deaths, new_deaths,total_cases_per_million, new_cases_per_million,total_deaths_per_million, new_deaths_per_million, loc,population)) 
  }
}
union_data<- union_data%>%mutate(all_cases = total_cases+new_cases)
desc<- 'Covid 19 has caused curfew across the world, it has killed many people and countries are in lockdown. 
        Furthermore it has crashed the economy and closed down many companies,this virus eats up the cells 
        
        
        in our body to reproduce,
        and it affects people with respiratory problems and people with weak immunity, so
        please stay home and stay safe
        '

ui<- fluidPage(h1('Covid 19 data'),p('updates at 10:40 a.m (GMT)'), h4(desc), selectInput('Countries', 'Countries' ,choices = unique(union_data$loc),selected = 'World'),textOutput('cases'),textOutput('dead'),textOutput('per_pop'),textOutput('per_dead'),tags$style('#per_dead{ font-size:20px}'),tags$style('#per_pop{ font-size:20px}'),tags$style('h4{color:blue; font-size:25px}'),textOutput('rank'),h2(), h2(), h2(),plotlyOutput('cases_throughout_months'),tags$style('#dead{ font-size:20px}'),tags$style('#cases{ font-size:20px}'),tags$style('#rank{ font-size:20px}'),tags$style('p{ color:grey}'),h2(), h2(), h2(),h2(), h2(), h2(),selectInput('Continent', 'Continent' ,choices = unique(covid_cases_data$Continent),selected = 'All'),h2(), h2(), h2(),sliderInput('values', 'top x values',  min = 15, max = 50, value = 1),tabsetPanel(type = 'tab',tabPanel('Total cases',h2(), h2(), h2(),plotlyOutput('total_cases')) ,tabPanel('cases across continents',h2(), h2(), h2(),plotlyOutput('continents')),tabPanel('Total deaths',h2(), h2(), h2(),plotlyOutput('total_deaths')) ,tabPanel('Cases per country population',h2(), h2(), h2(),plotlyOutput('cases_per_pop')),tabPanel('deaths per confirmed cases',h2(), h2(), h2(), plotlyOutput('death_per_cases'))))
server<- function(input, output){
  output$cases_per_pop<- renderPlotly({
    if (input$Continent == 'All'){
      covid_cases_data%>%arrange(cases_per_pop)%>%filter(`Country,Other`!='Total:', is.na(fulldeaths)==FALSE)%>%tail(input$values)%>%plot_ly(x = ~reorder(`Country,Other`, -(cases_per_pop)), y = ~cases_per_pop, type = 'bar',text = ~paste(`Country,Other`,':',as.character(cases_per_pop*100),'% cases per whole population'), hoverinfo = 'text',label = 'text',color = ~Continent)%>%config(displayModeBar = F)%>%layout(xaxis = list(title = 'countries',showgrid = FALSE), yaxis = list(title = 'total covid 19 deaths in thousands',showgrid = FALSE),title = 'covid 19 cases per whole population')
    }else{
      covid_cases_data%>%arrange(cases_per_pop)%>%filter(Continent == input$Continent, `Country,Other`!='Total:', is.na(fulldeaths)==FALSE)%>%tail(input$values)%>%plot_ly(x = ~reorder(`Country,Other`, -(cases_per_pop)), y = ~cases_per_pop, type = 'bar',text = ~paste(`Country,Other`,':',as.character(cases_per_pop*100),'% cases per whole population'), hoverinfo = 'text',label = ~cases_per_pop,color = 'pink')%>%config(displayModeBar = F)%>%layout(xaxis = list(title = 'countries',showgrid = FALSE), yaxis = list(title = 'cases per population',showgrid = FALSE),title = 'covid 19 cases per whole population')
    }
    
  })
  output$total_deaths<- renderPlotly({
    if (input$Continent == 'All'){
      covid_19_deaths%>%arrange(fulldeaths)%>%filter(`Country,Other`!='Total:', is.na(fulldeaths)==FALSE)%>%tail(input$values)%>%plot_ly(x = ~reorder(`Country,Other`, -(fulldeaths)), y = ~fulldeaths/1000, type = 'bar',text = ~paste(`Country,Other`,':',as.character(fulldeathsabb),'deaths'), hoverinfo = 'text',label = 'text',color = ~Continent)%>%config(displayModeBar = F)%>%layout(xaxis = list(title = 'countries',showgrid = FALSE), yaxis = list(title = 'total covid 19 deaths in thousands',showgrid = FALSE),title = 'covid 19 deaths')
    }else{
      covid_19_deaths%>%arrange(fulldeaths)%>%filter(Continent == input$Continent, `Country,Other`!='Total:', is.na(fulldeaths)==FALSE)%>%tail(input$values)%>%plot_ly(x = ~reorder(`Country,Other`, -(fulldeaths)), y = ~fulldeaths/1000, type = 'bar',text = ~paste(`Country,Other`,':',as.character(fulldeathsabb),'deaths'), hoverinfo = 'text',label = ~fulldeaths,color = 'red')%>%config(displayModeBar = F)%>%layout(xaxis = list(title = 'countries',showgrid = FALSE), yaxis = list(title = 'total covid 19 deaths in thousands',showgrid = FALSE),title = 'covid 19 deaths')
    }
    
  })
  output$total_cases<- renderPlotly({
    if(input$Continent == 'All'){
      covid_cases_data%>%filter(`Country,Other`%ni% c('North America', 'South America', 'Asia', 'Europe', 'Africa', 'Oceania', 'World', '','Total:'),is.na(fulldeaths)==FALSE)%>%head(input$values)%>%plot_ly(x = ~reorder(`Country,Other`, -(fulldeaths)), y = ~fulldeaths/1000, type = 'bar',text = ~paste(`Country,Other`,':',as.character(fulldeathsabb),'cases'), hoverinfo = 'text',label = ~fulldeaths,texttemplate = ~fulldeaths,color = ~Continent)%>%config(displayModeBar = F)%>%layout(xaxis = list(title = 'Countries',showgrid = FALSE), yaxis = list(title = 'confirmed cases in thousands',showgrid = FALSE), title = 'Covid 19 cases')
      
    }else{
      covid_cases_data%>%filter(`Country,Other`%ni% c('North America', 'South America', 'Asia', 'Europe', 'Africa', 'Oceania', 'World', '','Total:'),is.na(fulldeaths)==FALSE,Continent == input$Continent)%>%head(input$values)%>%plot_ly(x = ~reorder(`Country,Other`, -(fulldeaths)), y = ~fulldeaths/1000, type = 'bar',text = ~paste(`Country,Other`,':',as.character(fulldeathsabb),'cases'), hoverinfo = 'text',label = ~fulldeaths,texttemplate = ~fulldeaths,color = 'pink')%>%config(displayModeBar = F)%>%layout(xaxis = list(title = 'Countries',showgrid = FALSE), yaxis = list(title = 'confirmed cases in thousands',showgrid = FALSE), title = 'Covid 19 cases')
    }
    
  })
  output$continents<-renderPlotly({covid_cases_data%>%filter(`Country,Other`%in% c('North America', 'South America', 'Asia', 'Europe', 'Africa', 'Oceania'))%>%plot_ly(x = ~reorder(`Country,Other`, -(fulldeaths)), y = ~fulldeaths/1000, type = 'bar',text = ~paste(`Country,Other`,':',as.character(fulldeathsabb),'cases'), hoverinfo = 'text',label = ~fulldeaths,texttemplate = ~fulldeaths,color = 'pink')%>%config(displayModeBar = F)%>%layout(xaxis = list(title = 'Continents',showgrid = FALSE), yaxis = list(title = 'confirmed cases in thousands',showgrid = FALSE), title = 'Covid 19 cases across continents')})
  output$death_per_cases<- renderPlotly({
    if (input$Continent == 'All'){
      covid_cases_data%>%arrange(death_per_cases)%>%filter(`Country,Other`!='Total:', is.na(death_per_cases)==FALSE)%>%tail(input$values)%>%plot_ly(x = ~reorder(`Country,Other`, -(death_per_cases)), y = ~death_per_cases, type = 'bar',text = ~paste(`Country,Other`,':',as.character(round(death_per_cases*100,2)),'% deaths per confirmed cases'), hoverinfo = 'text',label = 'text',color = ~Continent)%>%config(displayModeBar = F)%>%layout(xaxis = list(title = 'countries',showgrid = FALSE), yaxis = list(title = 'deaths per cases',showgrid = FALSE),title = 'covid 19 deaths per confirmed cases')
    }else{
      covid_cases_data%>%arrange(death_per_cases)%>%filter(Continent == input$Continent, `Country,Other`!='Total:', is.na(death_per_cases)==FALSE)%>%tail(input$values)%>%plot_ly(x = ~reorder(`Country,Other`, -(death_per_cases)), y = ~death_per_cases, type = 'bar',text = ~paste(`Country,Other`,':',as.character(round(death_per_cases*100,2)),'% deaths per confirmed case'), hoverinfo = 'text',label = ~death_per_cases,color = 'green')%>%config(displayModeBar = F)%>%layout(xaxis = list(title = 'countries',showgrid = FALSE), yaxis = list(title = 'deaths per cases',showgrid = FALSE),title = 'covid 19 deaths per confirmed cases')
    }
    
  })
  
  output$cases_throughout_months<- renderPlotly({
    union_data%>%filter(loc == input$Countries)%>%plot_ly( x = ~date, y = ~all_cases, type = 'scatter', mode = 'lines',text = ~paste('Date:', date, 'Case: ' , comma(all_cases)), hoverinfo = 'text')%>%config(displayModeBar = F)%>%layout(title = paste('Covid 19 cases throughout time in', input$Countries), yaxis = list(title = 'Countries'), xaxis = list(title = 'total cases'))
    
    
  })
  output$cases<- renderText({
    filtered<- union_data%>%filter(loc == input$Countries&date == max(date))
    print(paste(comma(filtered$all_cases), 'total cases'))
  })
  output$dead<- renderText({
    filtered<- union_data%>%filter(loc == input$Countries&date == max(date))
    print(paste(comma(filtered$total_deaths+filtered$new_deaths), 'total deaths'))
  })
  output$per_pop<- renderText({
    filtered<- union_data%>%filter(loc == input$Countries&date == max(date))
    print(paste(as.character(round((filtered$all_cases/filtered$population)*100,2)), '% cases per whole population'))
  })
  output$per_dead<- renderText({
    filtered<- union_data%>%filter(loc == input$Countries&date == max(date))
    print(paste(as.character(round(((filtered$total_deaths+filtered$new_deaths)/filtered$all_cases)*100,2)), '% Total deaths per cases'))
  })
  output$rank<- renderText({
    filtered<- union_data%>%filter(date == max(date)&loc!='World')%>%mutate(rank = (length(all_cases)-rank(all_cases))+1)
    if(input$Countries!='World'){
      rank_value<- filtered%>%filter(loc == input$Countries)%>%.$rank
      paste('Rank: ',as.character(rank_value), ' number of cases across', as.character(length(filtered$all_cases)), ' countries and territories')
    }
    
  })
  
}
shinyApp(ui,server)