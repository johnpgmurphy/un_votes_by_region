
#Source code:

source("source_code.R")
country_mil_issue1 <- read_rds("./country.rds")
un_mil_issue <- read_rds("./region.rds")

# Define UI for application that draws a plot with two lines with overlaid points

ui <- navbarPage(
    "UNGA Regional Voting Patterns",
    tabPanel("Region Comparison",
             fluidPage(theme = shinytheme("cerulean"),
                       titlePanel("Vote Proportions in the UN General Assembly
                                  by Year"),
                       sidebarLayout(
                           sidebarPanel(
                               selectInput(
                                   "plot_type",
                                   "Region 1",
                                   c("Middle East and North Africa" = "a", 
                                     "Sub Saharan Africa" = "b",
                                     "North America" = "d",
                                     "South America" = "e",
                                     "Central America" = "f", 
                                     "Central Asia" = "g", 
                                     "South Asia" = "h",
                                     "Southeast Asia" = "i",
                                     "East Asia" = "j",
                                     "Oceania" = "k",
                                     "Central Europe" = "l",
                                     "Western Europe" = "m",
                                     "Eastern Europe" = "n",
                                     "World" = "o")
                               ),
                               selectInput(
                                   "plot_type_2",
                                   "Region 2",
                                   c("World" = "o",
                                     "Middle East and North Africa" = "a", 
                                     "Sub Saharan Africa" = "b",
                                     "North America" = "d",
                                     "South America" = "e",
                                     "Central America" = "f", 
                                     "Central Asia" = "g", 
                                     "South Asia" = "h",
                                     "Southeast Asia" = "i",
                                     "East Asia" = "j",
                                     "Oceania" = "k",
                                     "Central Europe" = "l",
                                     "Western Europe" = "m",
                                     "Eastern Europe" = "n"
                                     )
                               ),
                               selectInput(
                                   "vote_type_r",
                                   "Vote Type",
                                   c("Yes Votes" = "a",
                                     "No Votes" = "b",
                                     "Abstentions" = "c",
                                     "Absences" = "d")
                               ),
                               selectInput(
                                   "issue_r",
                                   "Issue",
                                   c("All Resolutions" = "g",
                                     "Israel/Palestine" = "a",
                                     "Nuclear Weapons" = "b",
                                     "Arms Control" = "c",
                                     "Colonialism" = "d",
                                     "Human Rights" = "e",
                                     "Economic Development" = "f")
                               )
                               
                               #decided against having an input for military 
                               #expenditure as mean or median as it seemed
                               #superfluous, stuck with mean_mil_ex to take
                               #into account outlier years as outliers are relevant
                               
                               ),
                           mainPanel(plotOutput("line_plot")))
             )),
    tabPanel("Country Comparison",
             fluidPage(theme = shinytheme("cerulean"),
                       titlePanel("Comparison of Country-level Voting Patterns"),
                       sidebarLayout(
                           sidebarPanel(
                               selectInput("region_c",
                                           "Region",
                                           c("Middle East and North Africa" = "a", 
                                             "Sub Saharan Africa" = "b",
                                             "North America" = "d",
                                             "South America" = "e",
                                             "Central America" = "f", 
                                             "Central Asia" = "g", 
                                             "South Asia" = "h",
                                             "Southeast Asia" = "i",
                                             "East Asia" = "j",
                                             "Oceania" = "k",
                                             "Central Europe" = "l",
                                             "Western Europe" = "m",
                                             "Eastern Europe" = "n"
                                           )),
                               uiOutput("countrylist_1"),
                               uiOutput("countrylist_2"),
                               selectInput(
                                   "vote_type_c",
                                   "Vote Type",
                                   c("Yes Votes" = "a",
                                     "No Votes" = "b",
                                     "Abstentions" = "c",
                                     "Absences" = "d")
                               ),
                               selectInput(
                                   "issue_c",
                                   "Issue",
                                   c("All Resolutions" = "g",
                                     "Israel/Palestine" = "a",
                                     "Nuclear Weapons" = "b",
                                     "Arms Control" = "c",
                                     "Colonialism" = "d",
                                     "Human Rights" = "e",
                                     "Economic Development" = "f"))),
                               mainPanel(plotOutput("country_plot")))
                           )),
    tabPanel("Discussion",
             titlePanel("Implications of the Models"),
             h3("About the Model"),
             p("The first model, of the frequency of yes and no votes 
               represents a first attempt at visualising
               the data present in the dataset. The presented graphs 
               show the distribution of
               voting patterns for 'yes' votes and 'no' votes respectively
               in the UN General Assembly.\n
               The information on the 'no' votes is interesting to me because
               it shows how frequently UNGA resolutions are passed
               without any opposition. This makes the 'yes' histogram
               even more interesting as it seems that unanimous resolutions
               are the least common result, with most resolutions achieving
               somewhere in the 100-150 'yes' votes range.\n
               This indicates that on most resolutions, there are at least
               40 countries that abstain from voting."),
             h3("About the General Vote Proportion"),
             p("This graphic depicts the proportion of yes votes in the UN General
               Assembly from 1949 to 2019 by geographic region. It takes into
               account the number of resolutions in a given year and the varying
               number of member states per region in calculating the proportion.
               It it useful to see in which years entire geographic regions
               voted in tandem, shown by proportions of yes votes above 70% and
               below 30%. In addition, it's useful to see when geographic regions
               were internally divided in their voting decisions, shown by 
               proportions of yes votes between 30-70% percent."),
             p("There were a few hurdles that had to be overcome to present
               this data in this way. First, the UN voting dataset did not code
               member states by their region. To do so, I used the region lists
               present in the SIPRI military expenditure database, with the 
               exception of combining the Middle East and the North Africa regions
               into one. Second, the SIPRI military database, although comprehensive,
               lacks data for many countries especially before 1980. This leads
               to the situation where in 1956 Morocco is the only country in the
               MENA region with reported military spending. Therefore, the reliability 
               of the military expenditure data was calculated by determining what 
               proportion of countries for each region reported their defense 
               spending in a given year, and plotted as point opacity.
               It warrants mentioning as well that users will notice a gap
               in the data in 1964: this is due to historical events that caused 
               instability within the UN and as a result no votes were held.")
             ,
             p("I decided to add the Issue Voting option to allow the user
             to see how the data compares on specific issues. The 'all resolutions' 
               model is valuable for the insights it gives us on broad trends among
               the regions. The issues provide value in the more specific pictures
               of regional voting patterns on those issues, to see
               which issues have regional consensus (or are divisive). 
               For example, consider the voting trends of either 
               Oceania or Western Europe on Human Rights resolutions in the 
               past 40 years.")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("This project aims to use the UN General Assembly Voting Data to identify
               patterns in the voting habits of particular world regions.  \n
               I wanted to conduct this project to learn more about the UN
               and how/whether regional politics manifest themselves in
               UNGA voting patterns. I used the SIPRI Military Expenditure 
               database in addition to the UN data to see what conclusions
               about the relationship between voting habits in the UNGA
               and military expenditure could be drawn at the regional
               level."),
             h3("About Me"),
             p("My name is Jack Murphy and I am a second-year Master's student
             in Harvard's Regional Studies - Middle East program. I'm interested
             in the role (both actual and potential) of multilateral 
             organisations in international relations, and medieval history.
             You can reach me at john_murphy@fas.harvard.edu.")))

# Define server logic required to draw a histogram

server <- function(input, output) {
    
    # filter the data set to the two selected regions of plot_type 
    # and plot_type_2
    
    output$line_plot <- renderPlot({
        
        # use custom two_region_filter function to filter data set 
        # to two specified regions using input$plot_type and input$plot_type_2
        
              y <- two_region_filter(data = un_mil_issue, 
                                     input1 = input$plot_type_2, 
                                     input2 = input$plot_type)
              
              # filter further to specific issues as determined by input$issue
              # using custom issue_filter function 
              
              x <- reg_issue_filter(data = y, input = input$issue_r)
              
            # plot the filtered dataset in ggplot, allow the selected input$vote_type
            # input to dictate which column is used for the y-axis
 
            ggplot(x, aes(x = year, 
                          y = case_when(
                              input$vote_type_r == "a" ~ percent_yes,
                              input$vote_type_r == "b" ~ percent_no,
                              input$vote_type_r == "c" ~ percent_abstain,
                              input$vote_type_r == "d" ~ percent_absent,
                              TRUE ~ percent_yes),
                          color = region)) +
                geom_line() +
            
            # plot the military expenditures overlaid on the line graph of 
            # voting patterns, varying point size positively correlated to % GDP
            # and varying point shape according to amount of data
                
                geom_point(aes(size = avg_mil_ex, 
                               alpha = prop_has_data)) +
                labs(x = "Year", y = "Vote Proportion", 
                     caption = "Sources: SIPRI Military Expenditure Database,\n 
                      Voeten et al., 2009") +
                scale_y_continuous(name = "Vote Proportion", 
                                   breaks = c(seq(0, 1, 0.1))) +
                scale_x_continuous(breaks = c(seq(1950, 2020, 10))) +
                scale_size_continuous(name = "Defense\nExpenditures\nas % GDP", 
                                    breaks = c(seq(0, 12, 2))) +
                scale_alpha(name = "Expenditure\nReliability", 
                            breaks = c(seq(0.25, 1, 0.25))) +
                scale_color_discrete(name = "Regions") +
                theme_bw()
    })
    
    # outputs a reactive country toggle for the country comparison
    # depending on what region is selected
    
    output$countrylist_1 <- renderUI({
        y <- one_region_filter(data = country_mil_issue1, input1 = input$region_c)
        x <- unique(y$country) %>% sort()
        selectInput("country_1",
                    "Country 1",
                    c(x))
    })
    
    # outputs a reactive country toggle for the country comparison
    # depending on what region is selected, sorted reverse alphabetically
    # so that it doesn't default to the same country as the previous one
    
    output$countrylist_2 <- renderUI({
        y <- one_region_filter(data = country_mil_issue1, input1 = input$region_c)
        x <- unique(y$country) %>% sort(., decreasing = TRUE)
        selectInput("country_2",
                    "Country 2",
                    c(x))
    })
    
    # filters the data to the selected region, and then to the selected countries
    # and then to the selected issue
    
    output$country_plot <- renderPlot({
        y <- one_region_filter(data = country_mil_issue1, input1 = input$region_c)
        z <- y %>% filter(country %in% c(input$country_1, input$country_2))
        x <- c_issue_filter(data = z, input$issue_c)
        
        # modify y axis according to selected vote type
        
        ggplot(x, aes(x = year, 
                      y = case_when(
                          input$vote_type_c == "a" ~ percent_yes,
                          input$vote_type_c == "b" ~ percent_no,
                          input$vote_type_c == "c" ~ percent_abstain,
                          input$vote_type_c == "d" ~ percent_absent),
                      color = country)) +
            geom_line() +
            
            # plot the military expenditures overlaid on the line graph of 
            # voting patterns, varying point size positively correlated to % GDP
            # and varying point shape according to amount of data
            
            geom_point(aes(size = mil_ex_gdp), alpha = 0.7) +
            labs(x = "Year", y = "Vote Proportion", 
                 caption = "Sources: SIPRI Military Expenditure Database,\n 
                      Voeten et al., 2009") +
            scale_y_continuous(name = "Vote Proportion", 
                               breaks = c(seq(0, 1, 0.1))) +
            scale_x_continuous(breaks = c(seq(1950, 2020, 10))) +
            scale_size_continuous(name = "Defense\nExpenditures\nas % GDP", 
                                  breaks = c(seq(0, 12, 2))) +
            scale_color_discrete(name = "Countries") +
            theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
