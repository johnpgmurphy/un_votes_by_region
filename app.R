
# Source code:

source("source_code.R")
country_mil_issue1 <- read_rds("./country.rds")
un_mil_issue <- read_rds("./region.rds")

# Define UI for application that is interactive and creates two plots:
# one that lets the user compare regions on UNGA voting trends with a choice
# of vote type and issue topic (and overlays military expenditures), and one
# which does the same on a country level. It also presents a table of results from
# a predictive statistical model for regional voting habits.

ui <- navbarPage(
    "UNGA Regional Voting Patterns",
    tabPanel("Region Comparison",
             fluidPage(theme = shinytheme("cerulean"),
                       titlePanel("Vote Proportions in the UN General Assembly
                                  by Year"),
                       sidebarLayout(
                           sidebarPanel(
                               selectInput(
                                   "region_1",
                                   "Region 1",
                                   c("Middle East and North Africa" 
                                     = "MENA", 
                                     "Sub Saharan Africa" = "SubSah",
                                     "North America" = "NorAm",
                                     "South America" = "SouthAm",
                                     "Central America" = "CentAm", 
                                     "Central Asia" = "CentAs", 
                                     "South Asia" = "SouthAs",
                                     "Southeast Asia" = "SEAs",
                                     "East Asia" = "EastAs",
                                     "Oceania" = "Oceania",
                                     "Central Europe" = "CentEur",
                                     "Western Europe" = "WestEur",
                                     "Eastern Europe" = "EastEur",
                                     "World" = "World")
                               ),
                               selectInput(
                                   "region_2",
                                   "Region 2",
                                   c("World" = "World",
                                     "Middle East and North Africa" 
                                     = "MENA", 
                                     "Sub Saharan Africa" = "SubSah",
                                     "North America" = "NorAm",
                                     "South America" = "SouthAm",
                                     "Central America" = "CentAm", 
                                     "Central Asia" = "CentAs", 
                                     "South Asia" = "SouthAs",
                                     "Southeast Asia" = "SEAs",
                                     "East Asia" = "EastAs",
                                     "Oceania" = "Oceania",
                                     "Central Europe" = "CentEur",
                                     "Western Europe" = "WestEur",
                                     "Eastern Europe" = "EastEur")
                               ),
                               selectInput(
                                   "vote_type_r",
                                   "Vote Type",
                                   c("Yes Votes" = "yes",
                                     "No Votes" = "no",
                                     "Abstentions" = "abstain",
                                     "Absences" = "absent")
                               ),
                               selectInput(
                                   "issue_r",
                                   "Issue",
                                   c("All Resolutions" = "all",
                                     "Israel/Palestine" = "isrpal",
                                     "Nuclear Weapons" = "nukes",
                                     "Arms Control" = "arms",
                                     "Colonialism" = "colony",
                                     "Human Rights" = "rights",
                                     "Economic Development" = "dev")
                               )
                               
                               # decided against having an input for military 
                               # expenditure as mean or median as it seemed
                               # superfluous, stuck with mean_mil_ex to take
                               # into account outlier years as outliers are relevant
                               
                               ),
                           mainPanel(plotOutput("region_comp"))),
                       p("This chart lets you compare the voting habits of two
                         geographic regions in the UN General Assembly, as well
                         as compare their military expenditures. The data for
                         government spending on military budgets isn't 100% 
                         complete due to varying degrees of transparency,
                         so be aware that the opacity of the points in the graph
                         are tied to how much military data we have for that 
                         region in that year."),
                       p("See how much consensus there is within regions by
                         checking whether their votes are coordinated or not 
                         (i.e. if >75% (or <25%) of votes cast by a whole region in a year
                         are yes votes, then that indicates some consensus, but if
                         only 50% of votes cast were yes votes, then there were 
                         clearly differing views on the topics being voted on).
                         Try comparing Oceania to Southeast Asia to see an 
                         example (especially for 1970 onwards).")
             )),
    tabPanel("Country Comparison",
             fluidPage(theme = shinytheme("cerulean"),
                       titlePanel("Comparison of Country-level Voting Patterns"),
                       sidebarLayout(
                           sidebarPanel(
                               selectInput("region_c",
                                           "Region",
                                           c("Middle East and North Africa" 
                                             = "MENA", 
                                             "Sub Saharan Africa" = "SubSah",
                                             "North America" = "NorAm",
                                             "South America" = "SouthAm",
                                             "Central America" = "CentAm", 
                                             "Central Asia" = "CentAs", 
                                             "South Asia" = "SouthAs",
                                             "Southeast Asia" = "SEAs",
                                             "East Asia" = "EastAs",
                                             "Oceania" = "Oceania",
                                             "Central Europe" = "CentEur",
                                             "Western Europe" = "WestEur",
                                             "Eastern Europe" = "EastEur")),
                               uiOutput("countrylist_1"),
                               uiOutput("countrylist_2"),
                               selectInput(
                                   "vote_type_c",
                                   "Vote Type",
                                   c("Yes Votes" = "yes",
                                    "No Votes" = "no",
                                    "Abstentions" = "abstain",
                                    "Absences" = "absent")
                               ),
                               selectInput(
                                   "issue_c",
                                   "Issue",
                                   c("All Resolutions" = "all",
                                     "Israel/Palestine" = "isrpal",
                                     "Nuclear Weapons" = "nukes",
                                     "Arms Control" = "arms",
                                     "Colonialism" = "colony",
                                     "Human Rights" = "rights",
                                     "Economic Development" = "dev"))),
                               mainPanel(plotOutput("country_plot"))),
                       p("This chart lets you compare two countries from the
                         same geographic region on their voting habits over
                         time in the UN General Assembly, as well as on
                         their military expenditures (where data is available).
                         Play with the toggles to see which countries vote
                         yes or no more frequently (or abstain or are absent 
                         from votes) in the UNGA, and how their voting records
                         compare on specific issues.")
                           )),
    tabPanel("Statistical Model",
            fluidPage(theme = shinytheme("cerulean"),
                      titlePanel("Statistical Analysis of the Impact of
                                 Increased Military Expenditure on Voting
                                 Habits"),
                      sidebarLayout(
                          sidebarPanel(
                              selectInput("region_model",
                                          "Region",
                                          c("Middle East and North Africa" 
                                            = "MENA", 
                                            "Sub Saharan Africa" = "SubSah",
                                            "North America" = "NorAm",
                                            "South America" = "SouthAm",
                                            "Central America" = "CentAm", 
                                            "Central Asia" = "CentAs", 
                                            "South Asia" = "SouthAs",
                                            "Southeast Asia" = "SEAs",
                                            "East Asia" = "EastAs",
                                            "Oceania" = "Oceania",
                                            "Central Europe" = "CentEur",
                                            "Western Europe" = "WestEur",
                                            "Eastern Europe" = "EastEur")),
                              selectInput(
                                  "vote_type_model",
                                  "Vote Type",
                                  c("Yes Votes" = "yes",
                                    "No Votes" = "no",
                                    "Abstentions" = "abstain",
                                    "Absences" = "absent")),
                              selectInput(
                                  "issue_model",
                                  "Issue",
                                  c("All Resolutions" = "all",
                                    "Israel/Palestine" = "isrpal",
                                    "Nuclear Weapons" = "nukes",
                                    "Arms Control" = "arms",
                                    "Colonialism" = "colony",
                                    "Human Rights" = "rights",
                                    "Economic Development" = "dev"))),
                          mainPanel(gt_output("model_table")
                                    )),
                      h3("Interpreting the Model"),
                      p("This table displays the variation of regional 
                        voting trends from the global average. As a disclaimer, 
                        the values in the Beta column are estimates based on 
                        thousands of various samplings of the 73 years of data.
                        They represent the most likely value for the vote proportion,
                        whether global or regional. The values
                        indicate the following, for your selected region, 
                        vote type, and issue topic."),
                      p("a. The value in the Beta column for the (Intercept) is the
                        global average voting proportion in the UNGA when 
                        military expenditures are 0% of GDP."),
                      p("b. The value in the Beta column for your selected region
                        indicates how far, on average, that region is from 
                        the global UNGA average voting proportion."),
                      p("c. The value in the Beta column for avg_mil_ex indicates
                        how the UNGA average vote proportion changes for every 
                        1% increase in global military expenditures."),
                      p("d. The value in the Beta column for the interaction term,
                        i.e. (your region)*avg_mil_ex, indicates how a 1%
                        increase on military expenditures in the selected region
                        affects that region's voting trends (starting from
                        the value of a + b)."),
                      p("e. The 95% CI column gives us
                        a sense of the range of values that the Beta is 95%
                        likely to fall between (even if the given Beta value is
                        the likeliest.)"),
                      )),
    tabPanel("Discussion",
             titlePanel("Implications of the Charts"),
             h3("About the Region Comparison"),
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
               member states by their region. To add this feature, I used the region lists
               present in the SIPRI military expenditure database, with the 
               exception of combining the Middle East and the North Africa regions
               into one. Second, the SIPRI military database, although comprehensive,
               lacks data for many countries especially before 1980. This leads
               to the situation where in 1956 Morocco is the only country in the
               MENA region with reported military spending. Therefore, the reliability 
               of the military expenditure data was calculated by determining what 
               proportion of countries for each region reported their defense 
               spending in a given year, and plotted as point opacity."),
             p("It warrants mentioning as well that users will notice a gap
               in the data in 1964: this is due to historical events that caused 
               instability within the UN and as a result no votes were held. In 
               addition, the UN voting data for 2019 was inconsistently recorded
               in the Voeten et al dataset compared to all other years. To 
               include this data, the Correlates of War country-code data set was
               downloaded and incorporated so that each country in the UN data was
               identified by code rather than by the provided country name."),
             p("I decided to add the issue option to allow the user
               to see how the data compares on specific issues. The 'all resolutions' 
               model is valuable for the insights it gives us on broad trends among
               the regions. The issues provide value in the more specific pictures
               of regional voting patterns on those issues, to see
               which issues have regional consensus (or are divisive). 
               For example, consider the voting trends of either 
               Oceania or Western Europe on Human Rights resolutions in the 
               past 40 years. Some issues that I faced in adding this feature
               included having some resolutions be coded for multiple issues, which
               I addressed by using a weighted average to combine those groups 
               while weighting for the number of resolutions each one was for as
               jointly coded resolutions were rarer than single-coded ones. 
               Unfortunately, as there was no voting data in 1964, filtering
               to specific issues causes the graphs to omit that year. As such,
               the gap in the data is only visible when 'all resolutions' are
               displayed."),
             h3("About the Country Comparison"),
             p("This graphic allows the user to choose a region and compare
               the voting patterns of two countries within that region, and if
               military expenditure data is available for a country, it is overlaid
               on top of the vote plot. Like above, it takes into account
               the number of resolutions per year."),
             p("Amending the data to make this comparison was slightly more
               complicated because some countries joined the UN partway through
               a UNGA session, and therefore did not vote on all of the resolutions
               in that year. This was solved by giving the vote proportion as 
               the proportion of votes the country cast (or resolutions they were
               absent from) in that year, rather
               than the number of resolutions voted on in that year. The original 
               UN data also was inconsistent in its labeling of country names, so
               the Correlates of War dataset was incorporated to use the country
               codes instead. Some changes had to be made to the SIPRI data as well
               so that country names were recorded the same in both cases.Adding the 
               interactive chart which reconfigures the country inputs based on
               the selected region was also a good learning experience with Shiny,
               as it required me to explore its renderUI and uiOutput features.")),
    tabPanel("About", 
             titlePanel("About the Project"),
             h3("Project Background and Motivations"),
             p("This project aims to use the UN General Assembly Voting Data to identify
               patterns in the voting habits of particular world regions.  \n
               I wanted to conduct this project to learn more about the UN
               and how/whether regional politics manifest themselves in
               UNGA voting patterns. I drew the UNGA voting data from the Voeten
               et al., study initially published in 2007 (and since updated), and
               added to it the country names from the Correlates of War country
               coding project, as well as the SIPRI Military Expenditure database.
               I used the SIPRI Military Expenditure database in particular 
               out of a curiosity to see what conclusions
               about the relationship between voting habits in the UNGA
               and military expenditure could be drawn, whether at the regional
               or national level."),
             p("A link to the GitHub repo to consult the code:",
               tags$a(href= "https://github.com/johnpgmurphy/un_votes_by_region",
                      "click here.")),
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
    
    output$region_comp <- renderPlot({
        
        # use custom two_region_filter function to filter data set 
        # to two specified regions using input$plot_type and input$plot_type_2
        
              region_filtered <- two_region_filter(data = un_mil_issue, 
                                     input1 = input$region_2, 
                                     input2 = input$region_1)
              
              # filter further to specific issues as determined by input$issue
              # using custom issue_filter function 
              
              filtered_r_i <- reg_issue_filter(data = region_filtered, input = input$issue_r)
              
            # plot the filtered dataset in ggplot, allow the selected input$vote_type
            # input to dictate which column is used for the y-axis
 
            ggplot(filtered_r_i, aes(x = year, 
                          y = case_when(
                              input$vote_type_r == "yes" ~ percent_yes,
                              input$vote_type_r == "no" ~ percent_no,
                              input$vote_type_r == "abstain" ~ percent_abstain,
                              input$vote_type_r == "absent" ~ percent_absent,
                              TRUE ~ percent_yes),
                          color = region)) +
                geom_line() +
                geom_hline(yintercept = 0.5, linetype = "dotted") +
            
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
                                    breaks = c(seq(0, 12, 2)),
                                    limits = c(0, 10)) +
                scale_alpha(name = "Expenditure\nReliability", 
                            breaks = c(seq(0.25, 1, 0.25)),
                            limits = c(0, 1)) +
                scale_color_discrete(name = "Regions") +
                theme_bw()
    })
    
    # outputs a reactive country toggle for the country comparison
    # depending on what region is selected
    
    output$countrylist_1 <- renderUI({
        filtered_reg1 <- one_region_filter(data = country_mil_issue1, input1 = input$region_c)
        c_list_1 <- unique(filtered_reg1$country) %>% sort()
        selectInput("country_1",
                    "Country 1",
                    c(c_list_1))
    })
    
    # outputs a reactive country toggle for the country comparison
    # depending on what region is selected, sorted reverse alphabetically
    # so that it doesn't default to the same country as the previous one
    
    output$countrylist_2 <- renderUI({
        filtered_reg2 <- one_region_filter(data = country_mil_issue1, input1 = input$region_c)
        c_list_2 <- unique(filtered_reg2$country) %>% sort(., decreasing = TRUE)
        selectInput("country_2",
                    "Country 2",
                    c(c_list_2))
    })
    
    # filters the data to the selected region, and then to the selected countries
    # and then to the selected issue
    
    output$country_plot <- renderPlot({
        filtered_r_c <- one_region_filter(data = country_mil_issue1, input1 = input$region_c)
        filtered_r_c2 <- filtered_r_c %>% filter(country %in% c(input$country_1, input$country_2))
        x <- c_issue_filter(data = filtered_r_c2, input$issue_c)
        
        # modify y axis according to selected vote type
        
        ggplot(x, aes(x = year, 
                      y = case_when(
                          input$vote_type_c == "yes" ~ percent_yes,
                          input$vote_type_c == "no" ~ percent_no,
                          input$vote_type_c == "abstain" ~ percent_abstain,
                          input$vote_type_c == "absent" ~ percent_absent,
                          TRUE ~ percent_yes),
                      color = country)) +
            geom_line() +
            geom_hline(yintercept = 0.5, linetype = "dotted") +
            
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
                                  breaks = c(seq(0, 12, 2)),
                                  limits = c(0, 12)) +
            scale_color_discrete(name = "Countries") +
            theme_bw()
    })
    
    output$model_table <- render_gt({
        model_filtered_r <- two_region_filter(data = un_mil_issue, 
                               input1 = input$region_model,
                               input2 = "o")
        model_filtered_i <- reg_issue_filter(data = model_filtered_r, input = input$issue_model)
        
        x <- model_filtered_i %>%
            mutate(region = as.factor(region)) %>%
            mutate(region = fct_relevel(region, "World"))
        
        model <- stan_glm(case_when(
            input$vote_type_model == "yes" ~ percent_yes,
            input$vote_type_model == "no" ~ percent_no,
            input$vote_type_model == "abstain" ~ percent_abstain,
            input$vote_type_model == "absent" ~ percent_absent,
            TRUE ~ percent_yes) ~ 
                              region + avg_mil_ex + region*avg_mil_ex,
                           refresh = 0,
                           data = x)
        
        tbl_regression(model, intercept = TRUE) %>%
            as_gt() %>%
            tab_header(title = "Regression of UN Voting Patterns",
                       subtitle = "The Effect of Military Expenditure on 
                       UNGA Voting Habits") %>%
            tab_source_note("Sources: Voeten et al, 2009, \n 
                            SIPRI Military Expenditure Database, \n
                            COW Country Codes")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
