# This is the Blursday database server source code.
# The live application can be accessed at 
# https://dnacombo.shinyapps.io/Blursday/
# This code is publicly available at
# https://github.com/dnacombo/TSDshiny

#     Copyright (C) 2021  Maximilien Chaumon
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.

library(shiny)
library(shinyjs)
library(tidyverse)
library(skimr)
library(ggridges)
library(ggrepel)
library(patchwork)
library(flextable)
# library(ggthemes)
datadir = 'data'
source('gimmedata.R')
theme_set(theme_minimal())

countryMapping <- c(FR = 'France',
                    DE = 'Germany',
                    IT = 'Italy',
                    TR = 'Turkey',
                    AR = 'Argentina',
                    UK = 'United Kingdom',
                    CA = 'Canada',
                    CO = 'Colombia',
                    GR = 'Greece', 
                    IN = 'India',
                    JP = 'Japan'
)
mappingCountry <- names(countryMapping)
names(mappingCountry) = countryMapping

fs <- list.files('data','TSD_.*.RData', full.names = F) %>%
    tibble(fname = .) %>%
    rowwise() %>%
    mutate(S = str_match_all(basename(fname),'TSD_([^_]*)_(S[^_]*)_([^\\.]*).RData'),
           Country = S[2],
           Session = S[3],
           Unique_Name = S[4]) %>%
    select(-S)


ui <- fluidPage(
    useShinyjs(),
    titlePanel("Blursday data server"),
    
    sidebarLayout(
        sidebarPanel(selectInput(inputId='Country',
                                 label = 'Country',
                                 choices = sort(recode(unique(fs$Country),!!!countryMapping)),
                                 multiple = TRUE,
                                 selected = 'France'),
                     selectInput(inputId='UniqueName',
                                 label = 'Task/Questionnaire Name',
                                 choices = sort(unique(fs$Unique_Name)),
                                 multiple = TRUE,
                                 selected = 'ConfinementTrack'),
                     selectInput(inputId='Session',
                                 label = 'Session number',
                                 choices = sort(unique(fs$Session)),
                                 multiple = TRUE,
                                 selected = unique(fs$Session)),
                     checkboxGroupInput(inputId = 'ToAdd',
                                        label = 'Add covariates...',
                                        choices = c('Demographics','Stringency Index','Mobility Indices', 'Subjective Confinement Indices', 'Subjective Confinement Duration'),
                                        selected = c('Demographics','Stringency Index')),
                     actionButton(inputId = "readData", label = "Update", icon = icon('redo')),
                     br(),br(),
                     strong('Download'),br(),
                     disabled(
                         downloadButton(outputId = 'downloadcsvData',label = '.csv'),
                         downloadButton(outputId = 'downloadRData',label = '.RData')
                     )
                     
        ),
        
        mainPanel(
            # dataTableOutput(outputId = 'skim'),
            # dataTableOutput(outputId = 'table')
            h3('Demographics'),
            uiOutput(outputId = 'table0'),
            # plotOutput(outputId = 'plot0'),
            # h2('Subjects count'),
            # plotOutput(outputId = 'plot2'),
            # h3('Participation dates and lockdown stringency'),
            # plotOutput(outputId = 'plot1'),
        )
        
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observeEvent(c(input$Country, input$UniqueName, input$Session, input$ToAdd),{
        disable('downloadcsvData')
        disable('downloadRData')
    })
    
    currentData <- eventReactive(input$readData,{
        d <- gimmeRdata('data',UniqueName = input$UniqueName, Country = recode(input$Country,!!!mappingCountry), Session = input$Session)
        if (!'Trial_Number' %in% names(d)) d$Trial_Number = NA
        if (!'Question_Key' %in% names(d)) d$Question_Key = NA
        
        if (nrow(d) == 0) {return(d)}
        if (any(str_detect(input$ToAdd, 'Stringency Index'))) {
            d <- add_StringencyIndex(d)
        }
        if (any(str_detect(input$ToAdd, 'Demographics'))) {
            d <- add_Demographics(d)
        }
        if (any(str_detect(input$ToAdd, 'Mobility Indices'))) {
            d <- add_Mobility(d)
        }
        if (any(str_detect(input$ToAdd, 'Subjective Confinement Indices'))) {
            d <- add_SubjectiveConfinementIndices(d)
        }
        if (any(str_detect(input$ToAdd, 'Subjective Confinement Duration'))) {
            d <- add_SubjectiveConfinementDuration(d)
        }
        enable('downloadcsvData')
        enable('downloadRData')
        
        d
    }, ignoreNULL = FALSE)
    
    # output$table <- renderDataTable(currentData())
    # output$skim <- renderDataTable(skim(currentData()))
    output$table0 <- renderUI({
        t <- currentData() %>%
            unite(col = 'filt', Trial_Number, Question_Key, na.rm = T) %>%
            filter(filt %in% c('END TASK','END QUESTIONNAIRE')) %>%
            group_by(Country_Name,Session, Unique_Name) %>%
            summarize(N = n(),
                      meanAge = mean(Age, na.rm = T),
                      Age = list(Age[!is.na(Age)]),
                      propMale = sum(Sex == 'M', na.rm = T) / n(),
                      propRightHand = sum(Handedness == 'right-handed', na.rm = T)/n())
        flextable(t) %>%
            compose(j = "Age", value = as_paragraph(
                plot_chunk(value = Age, type = "box", col = "red",
                           width = 1, height = .4, free_scale = FALSE)
            )) %>%
            colformat_double(big.mark = " ", digits = 1) %>% 
            set_header_labels(box = "composite content", density = "density") %>%
            theme_vanilla() %>%
            htmltools_value()
        
    })
    
    
    output$plot1 <- renderPlot({
        
        toplot <- currentData() %>% group_by(Country_Name,PID,Unique_Name,Run,Session)%>%
            unite(col = 'filt', Trial_Number, Question_Key, na.rm = T) %>%
            filter(filt %in% c('END TASK','END QUESTIONNAIRE')) %>%
            arrange(Local_Date) %>%
            mutate(Day = lubridate::date(Local_Date))
        
        firstday <- toplot %>% ungroup() %>% arrange(Day) %>%
            slice(1) %>% .$Day
        lastday <- toplot %>% ungroup() %>% arrange(Day) %>%
            slice(n()) %>% .$Day
        
        stringencyIndex <- read_csv('data/covid-stringency-index.csv',
                                    col_types = cols(
                                        Entity = col_character(),
                                        Code = col_character(),
                                        Day = col_date(format = ""),
                                        stringency_index = col_double())) %>%
            rename(Country_Name = Entity) %>%
            filter(Country_Name %in% unique(toplot$Country_Name)) %>%
            filter(between(Day,firstday, lastday)) %>%
            group_by(Country_Name,Day) %>%
            slice(1)
        
        sessionDates <- toplot %>%
            group_by(Session, Country_Name) %>%
            summarize(Start = first(Local_Date),
                      Stop = last(Local_Date)) %>%
            select(Session, Country_Name, Start,Stop)
        
        # confinement <- tibble(Date = lubridate::ymd_hms(c('2020/03/17 12:00:00','2020/05/11 23:59:59','2020/10/30 00:00:00', '2020/12/15 23:59:59'),tz = 'CET'), Session = c(1,1,2,2), Begend = c('Start','Stop','Start','Stop')) %>%
        #   pivot_wider(values_from=Date,names_from=Begend)
        
        coef <- toplot %>%
            group_by(Country_Name) %>%
            transmute(coef = length(unique(PID)))
        
        stringencyIndex <- stringencyIndex %>% 
            left_join(coef)
        
        ggplot(toplot,aes(x=Day, y=PID, col=Unique_Name)) +
            geom_point(aes(shape = Session),alpha = .5) +
            # geom_tile(aes(y = coef/2, height = coef, fill = stringency_index), col = NA, data = stringencyIndex, alpha = .5) +
            geom_point(alpha = .5) +
            geom_line(aes(y = stringency_index * coef / max(stringency_index), shape = NA), col='black', data = stringencyIndex, size = .1) +
            # geom_ribbon(aes(xmin = Start, xmax = Stop, fill = Session, x = NA, y = NA, col = NA, shape = NA), data=sessionDates) +
            # geom_ribbon(aes(xmin = Start, xmax = Stop,fill=Session, x = NA, y = NA, col = NA, shape = NA), data=confinement) +
            # geom_vline(aes(xintercept = Start),linetype = 2, data=sessionDates) +
            # geom_vline(aes(xintercept = Stop),linetype = 3, data=sessionDates) +
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  panel.grid.major.y = element_blank()) +
            ylab('Participants') + xlab('Day task completed') +
            facet_wrap(~Country_Name, scales = 'free_y') +
            scale_fill_gradient(low = 'azure', high = 'azure3')
    })
    
    output$downloadcsvData <- downloadHandler(
        filename = function() {
            paste0("data-", paste0(input$UniqueName, collapse = '-'),'_', paste0(input$Country, collapse = '-'), '_', Sys.Date(), ".csv")
        },
        content = function(file) {
            write.csv(currentData(), file)
        }
    )
    
    output$downloadRData <- downloadHandler(
        filename = function() {
            paste0("data-", paste0(input$UniqueName, collapse = '-'),'_', paste0(input$Country, collapse = '-'), '_', Sys.Date(), ".RData")
        },
        content = function(file) {
            TSDdata <- currentData()
            save(TSDdata, file = file)
        }
    )
    
    
}


# output$plot0 <- renderPlot({
#     Demo <- currentData() %>% group_by(PID,Session) %>%
#         slice(1)
#     
#     p1 <- Demo %>%
#         ggplot(aes(y= Country, x= Age,  fill= Country)) +
#         geom_density_ridges(alpha=0.6) +
#         theme_ridges() +
#         theme(legend.position="none",
#               strip.text.x = element_text(size = 8),
#               axis.text.x = element_text(angle = 60)) +
#         ylab("Country") + xlab("Age") + ggtitle("Age distributions of participants") +
#         theme(plot.title = element_text(face = "bold", "hjust" = 0.5)) +
#         scale_x_continuous(breaks = seq(from = 0, to = 80, by = 10),
#                            minor_breaks = seq(from = 0, to = 80, by = 5))
#     
#     p2 <- Demo %>% group_by(Country) %>%
#         mutate(n=n()) %>%
#         group_by(Country,Handedness) %>%
#         summarize(H = n()/n) %>% distinct() %>%
#         ggplot(aes(x=Country,y=H, label = round(H, d = 2), fill=Handedness)) +
#         geom_col()  +# geom_label_repel(position = 'stack', direction = 'x', segment.size = NA) + 
#         xlab("Country") + ylab("") + 
#         ggtitle('Handedness proportions across countries')
#     
#     p3 <- Demo %>% group_by(Country) %>%
#         mutate(n=n()) %>%
#         group_by(Country,Sex) %>%
#         summarize(S = n()/n) %>% distinct() %>%
#         ggplot(aes(x=Country,y=S,fill=Sex)) +
#         geom_col() +
#         xlab("Country") + ylab("") + 
#         ggtitle('Sex proportions across countries')
#     
#     return(p1+p2/p3)
#   })   
# output$plot2 <- renderPlot({
#     count <- currentData() %>%
#         unite(col = 'filt', Trial_Number, Question_Key, na.rm = T) %>%
#         filter(filt %in% c('END TASK','END QUESTIONNAIRE')) %>%
#         group_by(PID,Country, Session, Unique_Name) %>%
#         slice(1) %>%
#         group_by(Country, Session, Unique_Name) %>%
#         summarize(N = n())
#     
#     ggplot(count,aes(x=Unique_Name,y=N,fill = Session)) +
#         facet_wrap(~ Country, scales = 'free_y') +
#         geom_col(position = position_dodge(width = 1)) +
#         geom_label(aes(label=N), position = position_dodge(width = 1), show.legend = F) +
#         theme(axis.text.x = element_text(angle = 30,hjust = 1)) +
#         labs(x='',y='Count')
#     
#     
#     
# })
# 
# Run the application 
shinyApp(ui = ui,
         server = server)
