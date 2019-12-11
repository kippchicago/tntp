#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(silounloadr)
library(tidyverse)
library(tidytext)
library(janitor)
library(kippcolors)
library(ggforce)
library(shinycssloaders)


theme_set(theme_kipp_light())

# Data -----
bq_auth_file <-Sys.getenv("BQ_AUTH_FILE")
bq_project <- Sys.getenv("BQ_PROJECT")

cat(sprintf("BQ_AUTH_FILE is %s", bq_auth_file))

bigrquery::bq_auth(path = bq_auth_file)

items <- get_tntp('items') %>% collect

items_bm <- get_tntp('items_benchmarks') %>% collect() %>%
    mutate(clean_question = str_trim(str_remove_all(clean_question, "\\*+")),
           domain_group = if_else(domain_group == "Academic Expectations",
                                  "Academic Opportunity",
                                  domain_group))

items <- items %>% mutate(clean_question = str_trim(str_remove_all(clean_question, "\\*+")),
                          domain_group = if_else(domain_group == "Academic Expectations",
                                                 "Academic Opportunity",
                                                 domain_group))


domains <- get_tntp("domains") %>% collect
domains_bm <- get_tntp("domains_benchmarks") %>% collect

ici <- get_tntp("ici") %>% collect
ici_bm <- get_tntp("ici_benchmarks") %>% collect



comments <- get_tntp("comments_teachers") %>% collect()
## load library functions


## Domains tile data prep ----
ici_ici <- ici %>%
    filter(measure == "Instructional Culture Index") %>%
    rename(domain_group = measure)

domains_ici <- domains %>%
    bind_rows(ici_ici) %>%
    mutate(season = str_extract(year, "Fall|Spring"),
           sy = str_extract(year, regex("\\d+")),
           domain_group = if_else(domain_group == "Academic Expectations",
                                  "Academic Opportunity",
                                  domain_group)) %>%
    arrange(sy, desc(season)) %>%
    mutate(year = fct_inorder(year))

domains_ici_bm <- domains_bm %>%
    bind_rows(ici_bm %>%  mutate(domain_group = "Instructional Culture Index")) %>%
    mutate(season = str_extract(year, "Fall|Spring"),
           sy = str_extract(year, regex("\\d+")),
           domain_group = if_else(domain_group == "Academic Expectations",
                                  "Academic Opportunity",
                                  domain_group)) %>%
    arrange(sy, desc(season)) %>%
    mutate(year = fct_inorder(year)) 



#ici_f18 <- ici %>% filter(year == "Fall 2018") %>%
#mutate(domain_group = "Instructional Culture Index")

#ici_bm_f18 <- ici_bm %>% filter(year == "Fall 2018") %>%

season_year <- domains_ici_bm %>% select(year, season, sy) %>% distinct() #%>% pull(year)


schools_order <- c("Region",
                   "KAP", 
                   "KACP",
                   "KBP",
                   "KOP",
                   "KAMS",
                   "KAC",
                   "KBCP",
                   "KOA")

domains_order <- c("Instructional Culture Index",
                   "Learning Environment",
                   "Instructional Planning for Student Growth",
                   "Observation & Feedback",
                   "Professional Development",
                   "Evaluation",
                   "Peer Culture",
                   "Leadership",
                   "Academic Opportunity",
                   "Career Progression",
                   "School Operations",
                   "Family and Community Engagement",
                   "Diversity, Equity, and Inclusion")

source("library.R", local = TRUE)


## JK Items ----
jk_items <- tribble(
    ~ "item_id",
    "T140",
    "T23_A",
    "T23_B",
    "T12_E",
    "T25_C",
    "T26_B",
    "T413",
    "T416",
    "T417",
    "T412",
    "T71C_Strategies",
    "T71E_Strategies",
    "T71D_Strategies",
    "T191",
    "T192",
    "T53_Adv_Oppor",
    "T113",
    "T204",
    "T838",
    "T1265",
    "T458"
)

items_jk <- items %>%
    inner_join(jk_items, by = "item_id") %>%
    mutate(season = str_extract(year, "Fall|Spring"),
           sy = str_extract(year, regex("\\d+"))) %>%
    arrange(sy, desc(season)) %>%
    mutate(year = fct_inorder(year))

items_bm_jk <- items_bm %>%
    inner_join(jk_items, by = "item_id") %>%
    mutate(season = str_extract(year, "Fall|Spring"),
           sy = str_extract(year, regex("\\d+"))) %>%
    arrange(sy, desc(season)) %>%
    mutate(year = fct_inorder(year))

jk <- bind_rows(items_jk, items_bm_jk)  %>%
    mutate(school_abbrev = abbreviate(variable, minlength = 2L, strict = FALSE)) %>%
    mutate(school_abbrev = if_else(school_abbrev == "KAPS", "KAP", school_abbrev)) %>%
    mutate(school_abbrev = if_else(school_abbrev == "KBPS", "KBP", school_abbrev)) %>%
    mutate(school_abbrev = if_else(school_abbrev == "KC", "Region", school_abbrev)) %>%
    mutate(school = factor(school_abbrev, levels = schools_order)) %>%
    mutate(prompt = str_wrap(clean_question, width = 25))

jk_schools <- jk %>% filter(!school_abbrev %in% c("KNA", "KNTQ", "Region"))
jk_bms <- jk %>% filter(school_abbrev %in% c("KNA", "KNTQ", "Region"))



# UI -----
ui <- dashboardPage(
    dashboardHeader(title = "TNTP Insight Teacher Survey"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Domain Grid",  tabName = "domain_grids", icon = icon("th")),
            menuItem("The Kanberg 21",tabName = "kanberg", icon = icon("apple")),
            menuItem("Text Analysis", tabName = "text", icon = icon("file-alt"))
        ), #end sidebarMenu
        selectInput("season_year", 
                    label = "Season/Year:",
                    choices = season_year$year,
                    selected = rev(season_year$year)[[1]])
    ), #end dasbhoardSidebar 
    dashboardBody(
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "kippcolors_orange.css")),
        tabItems(
            tabItem(tabName = "domain_grids",
                fluidRow(
                    ## Tab Box for grid_plot 
                    tabBox(width = 12,
                           id = "grids", 
                           title = "TNTP Insight Grids",
                           # Grid Plot Panel ----
                           tabPanel(title = "Domain Scores",
                                    id = "grid",
                                    plotOutput("grid_plot", 
                                               click = "grid_plot_click"
                                    ) %>%
                                        withSpinner()
                           ), #end tab panel Grid Plot
                           # YOY Grid Plot Panel ----
                           tabPanel(title = "YOY Change Domain Scores",
                                    id = "yoy",
                                    plotOutput("yoy_grid_plot") %>%
                                        withSpinner()
                                    ) #end tab Panel YOY
                            ) #end TabBox 
                        ), #end fluidRow
                #fluidRow(verbatimTextOutput(outputId = "grid_plot_click_text")),
                fluidRow(
                    box(title = textOutput("detail_title_text"),
                        width = 12,
                        height = 800,
                        plotOutput("domain_detail") %>% withSpinner()
                        )  #nd box
                    ) #end fluidRow
                ), #end  tabItem domain_grides
            ## Kanberg ----
            tabItem(tabName = "kanberg",
                    fluidRow(
                        box(width = 5, #height = 150,
                            selectInput("kanberg_bm",
                                        label = "Show Benchmark?",
                                        choices = list("Top Quartile" = "KNTQ", 
                                                       "Network Avg" = "KNA",
                                                       "None" = "None"),
                                        selected = "None")
                        ), #end BM Box
                        box(width = 7, 
                            div("Selecting a benchmark to the left will add an orange gradient ribbon, which goes from lighter to darker in the direction of the year, indicating the YOY change for the selected benchmark"))#end box
                    ), #end fluidRow
                    fluidRow(
                        box(title = "YOY Change in Kanberg's 21 Questions",
                            width = 12,
                            height = 800,
                            plotOutput("kanberg_plot") %>% withSpinner()
                            ) #end Kanberg Box
                    ),
                    fluidRow(
                        box(title = "Kanberg's 21 Questions by Domain",
                            width = 12,
                            height = 1100,
                            plotOutput("kanberg_items") %>% withSpinner()
                        ) #end Kanberg Box
                    )# end fluidRow
                ) #end tabItem kanberg
        ) # endTabItems
    ) # end dashboardBody
) # end dashboardPage 

# Server  -----
server <- function(input, output) {
    # domains_s19
    # domains_bm_s19
    domains_selected <- reactive({domains_ici %>% filter(year == input$season_year)})
    
    
    domains_bm_selected <- reactive(domains_ici_bm %>% filter(year == input$season_year))
    
    
    domains_bm_selected_kcs <- reactive(domains_bm_selected() %>% filter(variable == "KIPP Chicago"))
    
    
    
    domains_tile_data <- reactive({
        domains_selected() %>%
        bind_rows(domains_bm_selected_kcs()) %>%
        left_join(domains_bm_selected() %>%
                      filter(variable != "KIPP Chicago") %>%
                      select(domain_group, variable, value) %>%
                      spread(variable, value) %>%
                      clean_names(), 
                  by = "domain_group"
        ) %>%
        select(-`_line`, -`_file`) %>%
        mutate(score = (value>=kipp_network_average) + (value >= kipp_network_top_quartile),
               domain_group = factor(domain_group, levels = domains_order),
               school_abbrev = abbreviate(variable, minlength = 2L, strict = FALSE)) %>%
        mutate(school_abbrev = if_else(school_abbrev == "KAPS", "KAP", school_abbrev)) %>%
        mutate(school_abbrev = if_else(school_abbrev == "KBPS", "KBP", school_abbrev)) %>%
        mutate(school_abbrev = if_else(school_abbrev == "KC", "Region", school_abbrev)) %>%
        mutate(school = factor(school_abbrev, levels = schools_order)) %>%
        arrange(domain_group) %>%
        mutate(domain = forcats::fct_inorder(str_pad(str_wrap(domain_group, width = 10), 
                                                     side = "both", width=1))
               )
    })

    
    
    
    
    
    
    ## Grid Plot -----
    output$grid_plot <-renderPlot(
        domains_tile_data() %>%
            ggplot(aes(x=1, y=1, fill=as.factor(score))) +
            geom_tile() +
            geom_text(aes(label = round(value, 1)), size = 4, color = "white") +
            facet_grid(school ~ domain, switch="y") +
            theme_linedraw() +
            theme(axis.text =  element_blank(),
                  axis.ticks = element_blank(),
                  axis.title = element_blank(),
                  panel.border = element_blank(),
                  panel.grid = element_blank(),
                  panel.background = element_blank(),
                  strip.text.x = element_text(size = 10),
                  strip.text.y = element_text(angle = 180,
                                              size = 10),
                  legend.position = "bottom") +
            scale_fill_manual("KIPP Chicago score is:", values = c("#A7CFEE", "#BCD631","#439539"),
                              labels = c("< Network Avg", "≥ Network Avg", "≥ Top Quartile Avg ")) +
            labs(title= sprintf("Domain Scores | %s", input$season_year))
        
    )
    
    ## Verbatim grid click output ----
    output$grid_plot_click_text <- renderText({
        sprintf("Panel 1: %s\nPanel 2: %s\nTab selected: %s", 
                input$grid_plot_click$panelvar1, 
                input$grid_plot_click$panelvar2,
                input$grids)
        
        })
    
    grid_tab<-reactive(as.character(input$grids))
    
    output$detail_title_text <- renderText({
        if(grid_tab()=="YOY Change Domain Scores"){
            as.character("Longitudinal Domain Scores")
        } else {
            as.character("Domain Item Details")
        }
    })
    
    ## Domain Detail plot ----
    output$domain_detail <- renderPlot({
        if(grid_tab()=="YOY Change Domain Scores"){
            domains_long_data <- domains_ici %>%
                select(-`_line`, -`_file`) %>%
                mutate(
                    domain_group = factor(domain_group, levels = domains_order),
                    school_abbrev = abbreviate(variable, minlength = 2L, strict = FALSE)) %>%
                mutate(school_abbrev = if_else(school_abbrev == "KAPS", "KAP", school_abbrev)) %>%
                mutate(school_abbrev = if_else(school_abbrev == "KBPS", "KBP", school_abbrev)) %>%
                mutate(school_abbrev = if_else(school_abbrev == "KC", "Region", school_abbrev)) %>%
                mutate(school = factor(school_abbrev, levels = schools_order)) %>%
                arrange(domain_group) %>%
                mutate(domain = forcats::fct_inorder(str_pad(str_wrap(domain_group, width = 10), 
                                                             side = "both", 
                                                             width=1), 
                                                     ordered = TRUE)
                ) 
            
            domains_long_data %>%
                ggplot(aes(x=year, y=value)) +
                geom_line(aes(group = school), ) +
                # Min point highlighting
                geom_point(data = domains_long_data %>% group_by(school, domain_group) %>%
                               filter(value == min(value)), 
                           color = kipp_colors$darkorange)  +
                #Max Point highlighting 
                geom_point(data = domains_long_data %>% group_by(school, domain_group) %>%
                               filter(value == max(value)), 
                           color = kipp_colors$green) +
          
                facet_grid(domain_group ~ school , 
                           scales = "free_y", 
                           switch = "y") +
                scale_y_continuous(position = "right") +
                theme_kipp_light() +
                theme( strip.text.y = element_text(angle = 180,
                                                   size = 8),
                       axis.text.x = element_text(angle = 45,
                                                  hjust = 1)) +
                labs(x = "Term administered",
                     y = "Score")
        } else {
            validate(
                need(input$grid_plot_click, 'click above!')
            )
            
            
            domain <- input$grid_plot_click$panelvar1 %>%
                str_replace_all("\\n", " ")
            
            validate( need(domain != "Instructional Culture Index", "NO ICI!!!!!!!!"))
            
            school <- input$grid_plot_click$panelvar2
            
            p <- plot_items(domain = domain, term = input$season_year) 
            p +   labs(subtitle= sprintf("%s Item Scores | %s",domain, input$season_year))
        }
        
        
        
        
        
    }, height = 700) # End Domain detail
    
    year_end <- reactive(input$season_year)
    year <- reactive(year_end() %>% str_extract(regex("\\d+")))
    season <- reactive(year_end() %>% str_extract("Fall|Spring"))
    last_season <- reactive(if_else(season() == "Fall", "Spring", "Fall"))
    year_start <- reactive(sprintf("%s %s", season(), as.integer(year())-1))
    
    
    
    # YOY Grid plot -------
    output$yoy_grid_plot <- renderPlot({
        
        
        year_start <- year_start()
        year_end <- year_end()
        domains_s18_s19 <- domains_ici %>% filter(year %in% c(year_start(), year_end()))
        
        domains_bm_s18_s19 <- domains_ici_bm %>%
            filter(year %in% c(year_start(), year_end()))
        
        
        domains_bm_s18_s19_kcs <- domains_bm_s18_s19 %>%
            filter(variable == "KIPP Chicago")
        
        domains_tile_yoy_data <- domains_s18_s19 %>%# View
            bind_rows(domains_bm_s18_s19_kcs) %>%
            left_join(domains_bm_s18_s19 %>%
                          filter(variable != "KIPP Chicago") %>%
                          select(year, domain_group, variable, value) %>%
                          spread(variable, value) %>%
                          clean_names(), 
                      by = c("domain_group", "year")
            ) %>%
            select(-`_line`, -`_file`) %>%
            mutate(score = (value>=kipp_network_average) + (value >= kipp_network_top_quartile),
                   domain_group = factor(domain_group, levels = domains_order),
                   school_abbrev = abbreviate(variable, minlength = 2L, strict = FALSE)) %>%
            mutate(school_abbrev = if_else(school_abbrev == "KAPS", "KAP", school_abbrev)) %>%
            mutate(school_abbrev = if_else(school_abbrev == "KBPS", "KBP", school_abbrev)) %>%
            mutate(school_abbrev = if_else(school_abbrev == "KC", "Region", school_abbrev)) %>%
            mutate(school = factor(school_abbrev, levels = schools_order)) %>%
            arrange(domain_group) %>%
            mutate(domain = forcats::fct_inorder(str_pad(str_wrap(domain_group, width = 10), 
                                                         side = "both", width=1))
            ) %>%
            select(school,  domain, year, value) %>%
            spread(key = year, value=value) %>%
            clean_names() %>%
            rename_("year_1" = make_clean_names(year_start()),
                    "year_2" = make_clean_names(year_end())) %>%
            mutate(diff = year_2 - year_1,
                   perf_category = case_when(
                       diff < -0.5 ~ "Worse",
                       diff >  0.5 ~ "Better",
                       is.na(diff) ~ "",
                       TRUE ~ "Similar"
                   ))
        
        domains_tile_yoy_data %>%
            ggplot(aes(x=1, y=1, fill=perf_category)) +
            geom_tile() +
            geom_text(aes(label = round(diff, 1)), size = 4, color = "white") +
            facet_grid(school ~ domain, switch="y") +
            theme_linedraw() +
            theme(axis.text =  element_blank(),
                  axis.ticks = element_blank(),
                  axis.title = element_blank(),
                  panel.border = element_blank(),
                  panel.grid = element_blank(),
                  panel.background = element_blank(),
                  strip.text.x = element_text(size = 10),
                  strip.text.y = element_text(angle = 180,
                                              size = 10),
                  legend.position = "bottom") +
            #scale_fill_kipp(discrete = TRUE) +
            scale_fill_manual(sprintf("Change in %s to %s  is:", year_start, year_end), 
                              values = c("#8D8685", "#17345B", "#255694", "#FEDA00"),
                              labels = c("N/A", "Better", "Similar", "Worse")) +
            #scale_fill_viridis_d(labels = c("N/A", "Better", "Similar", "Worse")) +
            labs(title= sprintf("YOY Change in Domain Scores | %s to %s", year_start, year_end))
    })
    # Kanberg 21 questions plot -----
    
    jk_schools_spread <- reactive({
        jk_schools %>%
            filter(year %in% c(year_start(), year_end())) %>%
            select(domain_group, item_id,clean_question, prompt, school_abbrev, value, year) %>%
            spread(year, value) %>%
            clean_names() %>%
            rename_("year_1" = make_clean_names(year_start()), "year_2" = make_clean_names(year_end())) %>%
            mutate(pos_change = year_2 > year_1) 
    })
    
    jk_benchmarks <- reactive({
        jk_bms %>%
            select(domain_group, item_id, clean_question, prompt, school_abbrev, value, year) %>%
            filter(year %in% c(year_start(), year_end()),
                   school_abbrev == input$kanberg_bm) %>%
            spread(year, value) %>%
            clean_names() %>%
            rename_("year_1" = make_clean_names(year_start()), "year_2" = make_clean_names(year_end()))
    })
    
    output$kanberg_plot <- renderPlot({
        
       p <- jk_schools_spread() %>%
            ggplot(aes(x=year_1, xend=year_2, y=school_abbrev, yend=school_abbrev)) 
       if(input$kanberg_bm != "None"){
           p <- p +
               geom_link(data = jk_benchmarks(),
                         aes(x    =  year_1,
                             xend = year_2,
                             y    = "KBCP",
                             yend = "KBCP",
                             alpha= ..index..),
                         size = 100,
                         color = kipp_colors$darkorange,
                         show.legend = FALSE
               ) +
               
               geom_vline(data = jk_benchmarks(),
                          aes(xintercept = year_1),
                          color = kipp_colors$lightorange,
                          show.legend = FALSE,
                          size = .5
               ) +
               geom_vline(data = jk_benchmarks(),
                          aes(xintercept = year_2),
                          color = kipp_colors$darkorange,
                          show.legend = FALSE,
                          size = .5
               )
       }
       p <- p +
            geom_segment(aes(x=year_1, xend=year_2, y=school_abbrev, yend=school_abbrev, color = pos_change), arrow = arrow(length = unit(0.04, "npc"), type = "closed")) +
            geom_point(aes(x=year_1 , y=school_abbrev),  color=kipp_colors$darkgray) +
            geom_point(data = jk_schools_spread() %>% filter(is.na(year_1)), 
                       aes(x=year_2 ,
                           y=school_abbrev), color=kipp_colors$darkgray) +
            #geom_vline(data = jk_benchmarks(), aes(xintercept = value)) +
            facet_wrap(~prompt, ncol = 7) +
            scale_color_kipp(reverse = T) +
            scale_x_continuous(labels =  c(25, 50, 75), breaks= c(.25, .5, .75)) +
            scale_alpha(range=c(.25, 1), guide='none') +
            theme_kipp_light() +
            theme(strip.text = element_text( size=9, hjust = 0), 
                  strip.placement = "outside",
                  axis.text.x = element_text(angle = 45, hjust=1)) +
            labs(x = "Percent Agreeing", y="", color = sprintf("%s\n≥ %s", year_end(), year_start()))
     p    
    }, height = 700)
    
    output$kanberg_items <- renderPlot({

        p <- jk_schools_spread() %>%
            ggplot(aes(x=year_1, 
                       xend=year_2, 
                       y=prompt)) 
        if(input$kanberg_bm != "None"){
        p <- p + geom_link(data = jk_benchmarks() %>% select(-school_abbrev),
                           aes(x    =  year_1,
                               xend = year_2,
                               y    = prompt,
                               yend = prompt,
                               alpha= ..index..),
                           size = 10,
                           color = kipp_colors$darkorange,
                           show.legend = FALSE
                           ) + 
            geom_link(data = jk_benchmarks() %>% select(-school_abbrev),
                      aes(x    =  year_2,
                          xend = year_2,
                          y    = prompt,
                          yend = prompt
                          ),
                      size = 10,
                      color = kipp_colors$darkorange,
                      show.legend = FALSE
            )
        } 
             
          p <- p +  geom_segment(aes(x=year_1, 
                             xend=year_2, 
                             y=prompt, 
                             yend=prompt, 
                             color = pos_change), 
                         arrow = arrow(length = unit(0.04, "npc"), type = "closed"),
                         size = 1) +
            #geom_point(aes(x=spring_2019 , y=prompt,color = pos_change)) +
            geom_point(aes(x=year_1 , y=prompt),  color=kipp_colors$darkgray) +
            geom_point(data = jk_schools_spread() %>% filter(is.na(year_1)), 
                       aes(x=year_2 ,
                           y=prompt), color=kipp_colors$darkgray) +
            facet_grid(domain_group ~ school_abbrev, scales = "free_y", space = "free_y", switch = "y", ) +
            scale_color_kipp(reverse = T) +
            scale_x_continuous(labels =  c(25, 50, 75), breaks= c(.25, .5, .75)) +
            scale_alpha(range=c(.25, 1), guide='none') +
            theme_kipp_light() +
            theme(strip.text.y = element_text(angle=180), 
                  strip.placement = "outside",
                  axis.text.x = element_text(angle = 45, hjust=1),
                  axis.text.y = element_text(size=7)) +
            labs(x = "Percent Agreeing", y="", color = sprintf("%s\n≥ %s", year_end(), year_start()))
          
          p
    }, height = 1000
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
