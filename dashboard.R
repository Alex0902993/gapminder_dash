# Make a lifeExpectancy dashboard with `gapminder`


### TO-DO ###

#✅ 1 Make a ghghlight (whole gapminder must be used) 
#   1a Correct that when a country highlighted already exist on the plot, it appears on the top not in the middle ❌
#✅ 2 Make a second abItem with lineplot (also with highlight) - two countries to compare
#✅ 3 On the third tbaItem, make a dumbebll plot (1950-2007, 10 highest / lowest)
#   4 Put the titles of plots and context to the actual box titles, it would be nicer
#   5 Make dumbbell plot customizable - but in what way? (How many top and lowest countries to show?)
#   6 Add average numbers to value boxes first page
#####

# Packages
library(shinydashboard)
library(shinydashboardPlus)
library(fresh)
library(shiny)
library(gapminder)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(RColorBrewer)


# Constants
HIGH <- "#002240"
LOW <- "#66cccc"
HIGHLIGHT <- "#BF2C34"
GREY <- "#F5F5F5"
PLOT_TEXT_COL <- "#222d32"

NUDGE_X <- 2
NUDGE_Y <- 1

LINE_1_COL <- "#a50701"
LINE_2_COL <- "#019fa5"

# Dashboard custom theme with Fresh
## The theme that shiny uses is adminlte
app_theme <- create_theme(
  adminlte_color(
    light_blue = "#002240", # primary
    red = "#d73925", # danger
    green = "#29a13a", # success
    yellow = "#F9CB77", # warning
    #    aqua = "#002240"  # info status
  ),
  adminlte_sidebar(
    dark_bg = "#E5E4E2", # left and right sidebar col
    dark_color = "#222d32", # text col
    dark_hover_color = "#002240", # hover text col
    width = "200px"
  ),
  adminlte_global(
    content_bg = "#F5F5F5", # main body bg
    box_bg = "#F5F5F5", # box bg
    info_box_bg = "#66cccc" # info box bg
  )
)

# Plot theme
my_theme <- theme(
  legend.position = "none",
  plot.title = element_markdown(colour = PLOT_TEXT_COL),
  axis.title = element_text(face = "plain", color = PLOT_TEXT_COL),
  axis.text = element_text(color = PLOT_TEXT_COL),
  axis.text.y = element_text(
    margin = margin(t = 2, r = -20),
    colour = PLOT_TEXT_COL
  ),
  panel.grid = element_blank(),
  plot.caption = element_markdown(),
  panel.border = element_rect(colour = GREY),
  axis.ticks.y = element_blank(),
  panel.background = element_rect(fill = GREY, colour = GREY),
  plot.background = element_rect(fill = GREY, color = GREY)
)

my_colors <- colorRampPalette(brewer.pal(12, "Set3"))(20)

#### DAHSBOARD ####

## Header
header <- dashboardHeader(
  title = "Life Expectancy"
  #  ,controlbarIcon = icon("circle-info")) # only if left sidebyr exists
)
## Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Highest-Lowest", tabName = "life_expectancy", icon = icon("chart-bar")),
    menuItem("Through Years", tabName = "through_year", icon = icon("chart-line")),
    menuItem("Change", tabName = "dumbbell", icon = icon("dumbbell"))
  )
)

## Body
body <- dashboardBody(
  tags$style(HTML(
    ".box.box-solid.box-primary {
        background-color: #F5F5F5;
    }
      .main-footer {
        background: #002240;
        color: #F5F5F5;
      }
    "
  )),
  tabItems(
    tabItem(
      "life_expectancy",
      fluidRow(
        column(
          3,
          box(
            width = 12,
            solidHeader = FALSE,
            status = "primary",
            selectInput(
              inputId = "year",
              label = "Select year",
              choices = unique(gapminder$year)
            ),
            selectizeInput(
              inputId = "country",
              label = "Select country to highlight",
              choices = c("None", as.character(unique(gapminder$country))),
              selected = "None"
            )
          ),
          box(
            width = 12,
            solidHeader = TRUE,
            title = "Life Expectancy",
            status = "primary",
            p("Human life expectancy is a statistical measure of the estimate of the average remaining years of life at a given age. The most commonly used measure is life expectancy at birth (LEB, or in demographic notation e0, where ex denotes the average life remaining at age x)."),
            p("Cohort LEB is the mean length of life of a birth cohort (in this case, all individuals born in a given year) and can be computed only for cohorts born so long ago that all their members have died."),
            tags$a(href = "https://www.wikiwand.com/en/articles/Life_expectancy", "Wikipedia")
          )
        ),
        column(
          9,
          valueBoxOutput(
            "average_life_exp",
            #  width = 3
          ),
          valueBoxOutput(
            "highest_country",
            # width = 3
          ),
          valueBoxOutput(
            "lowest_country",
            #  width = 3
          ),
          box(
            width = 12,
            solidHeader = FALSE,
            color = GREY,
            status = "primary",
            plotOutput("plot_high_low", height = "550px")
          )
        )
      )
    ),
    tabItem(
      "through_year",
      fluidRow(
        column(
          3,
          box(
            width = 12,
            solidHeader = FALSE,
            status = "primary",
            selectInput(
              inputId = "country_line_1",
              label = "Highlight first country",
              choices = c("None", as.character(unique(gapminder$country)))
            ),
            selectizeInput(
              inputId = "country_line_2",
              label = "Highlight second country",
              choices = c("None", as.character(unique(gapminder$country))),
              selected = "None"
            )
          )
        ),
        column(
          9,
          box(
            width = 12,
            solidHeader = FALSE,
            color = GREY,
            status = "primary",
            plotOutput("plot_line", height = "550px")
          )
        )
      )
    ),
    tabItem(
      "dumbbell",
      fluidRow(
        column(
          9,
          box(
            width = 12,
            solidHeader = FALSE,
            color = GREY,
            status = "primary",
            plotOutput("plot_dumbbell_largest")
          ),
        box(
          width = 12, 
          silidheader = FALSE,
          color = GREY,
          status = "primary",
          plotOutput("plot_dumbbell_smallest")
        )
        )
      )
    )
  )
)

footer <- dashboardFooter(
  left = tags$b("Made with dashboardPlus")
)




## UI
ui <- dashboardPage(
  scrollToTop = TRUE,
  header = header,
  sidebar = sidebar,
  body = body,
  freshTheme = app_theme,
  #     skin = "red",
  footer = footer
)

## Server
server <- function(input, output) {
  # Set global theme
  theme_set(theme_bw(base_size = 17, base_family = "Arial"))

  # Reactive expression for the selected year
  chosen_year <- reactive({
    as.numeric(input$year) # Ensure it's numeric
  })

  # Reactive expression for chosen country
  chosen_country <- reactive({
    as.character(input$country)
  })

  # Reactive expression for top/bottom 10 countries
  gap_full <- reactive({
    gap_min <- gapminder %>%
      filter(year == chosen_year()) %>%
      slice_min(n = 10, order_by = lifeExp)

    gap_max <- gapminder %>%
      filter(year == chosen_year()) %>%
      slice_max(n = 10, order_by = lifeExp)

    df <- bind_rows(gap_max, gap_min) %>%
      mutate(
        high_low = factor(c(rep("high", 10), rep("low", 10)))
      )

    # If chosen_country() is not "None", add the filtered country data
    if (chosen_country() != "None") {
      req(gapminder) # Ensure gapminder is available

      gap_filtered <- gapminder |>
        filter(year == chosen_year()) |>
        filter(country == chosen_country()) |>
        mutate(
          high_low = factor("J") # "J" because it's in between "h"igh and "l"ow
        )

      # Combine df with the filtered data
      df <- bind_rows(df, gap_filtered)
    }

    return(df) # Return the final data for gap_full
  })

  # Reactive expression for world average life expectancy
  that_year_avg <- reactive({
    gapminder %>%
      filter(year == chosen_year()) %>%
      summarize(M = round(mean(lifeExp), 2)) %>%
      pull(M)
  })



  # Render the value box
  output$average_life_exp <- renderValueBox({
    valueBox(
      value = that_year_avg(), # Display the average
      subtitle = paste("World Avg. Life Expectancy in", chosen_year()),
      icon = icon("heartbeat"), # Add a relevant icon
      color = "blue" # Choose a color
    )
  })

  # Render highest country and lowest country value boxes
  output$highest_country <- renderValueBox({
    valueBox(
      value = gap_full() |> slice_max(order_by = lifeExp) |>
        pull(country),
      subtitle = "Highest Life Expectancy",
      color = "green",
      icon = icon("up-long")
    )
  })

  output$lowest_country <- renderValueBox({
    valueBox(
      value = gap_full() |> slice_min(order_by = lifeExp) |>
        pull(country),
      subtitle = "Lowest Life Expectancy",
      color = "orange",
      icon = icon("down-long")
    )
  })

  # Render the plot
  output$plot_high_low <- renderPlot({
    ggplot(
      data = gap_full(),
      aes(
        y = fct_rev(fct_infreq(country, w = lifeExp)),
        x = lifeExp,
        color = high_low
      )
    ) +
      geom_point(size = 3) +
      geom_segment(
        aes(
          x = min(lifeExp),
          xend = lifeExp,
          y = country,
          yend = country
        ),
        linewidth = 2,
        alpha = 0.8
      ) +
      geom_vline(
        xintercept = that_year_avg(),
        linetype = "dotted",
        color = "black",
        alpha = 0.8,
        linewidth = 1.5
      ) +
      labs(
        y = NULL,
        x = "Life Expectancy (years)",
        title = paste(
          "Top 10",
          "<span style='color:#002240'>Highest</span>",
          "and",
          "<span style='color:#83d4d4'>Lowest</span>",
          "Life Expectancies in", chosen_year()
        ),
        caption = "Source: <i>gapminder</i>."
      ) +
      my_theme +
      scale_color_manual(values = c(HIGH, LOW, HIGHLIGHT))
  })


  # Line plot
  gapminder_grouped <- gapminder |>
    group_by(country, year) |>
    summarise(
      life_exp_avg = mean(lifeExp)
    )

  # Reactive expressions for selected countries
  chosen_country_line_1 <- reactive({
    req(input$country_line_1) # Ensure input is available
    input$country_line_1
  })

  chosen_country_line_2 <- reactive({
    req(input$country_line_2)
    input$country_line_2
  })

  # Render the plot based on the selections
  output$plot_line <- renderPlot({
    # Base plot with all country lines in grey
    p2 <- ggplot(gapminder_grouped, aes(y = life_exp_avg, x = year, group = country)) +
      geom_line(alpha = 0.8, color = "grey", linewidth = 1.5) +
      geom_line(
        data = gapminder_grouped |> group_by(year) |> summarize(grand_mean = mean(life_exp_avg)),
        aes(y = grand_mean, x = year, group = 1),
        color = "black",
        linewidth = 2,
        alpha = 0.8
      ) +
      scale_x_continuous(
        breaks = scales::breaks_pretty(n = 12),
        limits = c(1950, 2010)
      ) +
      labs(
        y = "Average Life Expectancy",
        x = NULL,
        caption = "Source: <i>gapminder</i>.",
        title = "Life Expectancy by Country 1952 to 2007",
        subtitle = "Black line is the Global Average"
      ) +
      coord_cartesian(clip = "off") +
      my_theme

    # Conditional additions of country lines
    if (chosen_country_line_1() != "None") {
      p2 <- p2 +
        geom_line(
          data = gapminder_grouped %>% filter(country == chosen_country_line_1()),
          aes(y = life_exp_avg, x = year, group = country),
          color = LINE_1_COL,
          linewidth = 2
        ) +
        geom_label_repel(
          data = gapminder_grouped |> filter(country == chosen_country_line_1()) |> slice_max(year),
          aes(
            y = life_exp_avg, x = year,
            label = chosen_country_line_1()
          ),
          color = LINE_1_COL,
          size = 8
        )
    }

    if (chosen_country_line_2() != "None") {
      p2 <- p2 +
        geom_line(
          data = gapminder_grouped %>% filter(country == chosen_country_line_2()),
          aes(y = life_exp_avg, x = year, group = country),
          color = LINE_2_COL,
          linewidth = 2
        ) +
        geom_label_repel(
          data = gapminder_grouped |> filter(country == chosen_country_line_2()) |> slice_max(year),
          aes(
            y = life_exp_avg, x = year,
            label = chosen_country_line_2()
          ),
          color = LINE_2_COL,
          size = 8
        )
    }

    return(p2)
  })
  
  # Difference subset
  diff_df <- gapminder |> 
    filter(year == 1952 | year == 2007) |> 
    pivot_wider(names_from = year, 
                values_from = lifeExp,
                id_cols = country,
                names_prefix = "lifeExp_") |>
    mutate(
      diff = lifeExp_2007 - lifeExp_1952
    )

  output$plot_dumbbell_largest <- renderPlot({
    

    ## Biggest change 10
    biggest_change_countries <- diff_df |> 
      arrange(abs(diff)) |> 
      slice_max(n = 10, order_by = diff) |> 
      distinct(country, .keep_all = TRUE)
      
      biggest_change_countries |> 
        ggplot() +
        geom_segment(aes(y = reorder(country, abs(lifeExp_2007 - lifeExp_1952)), 
                         yend = reorder(country, abs(lifeExp_2007 - lifeExp_1952)),
                         x = lifeExp_1952, xend = lifeExp_2007)) +
        geom_point(aes(x = lifeExp_1952, 
                       y = reorder(country, abs(lifeExp_2007 - lifeExp_1952)),
                       fill = country),
                   color = "black",
                   size = 4,
                   shape = 21) +
        geom_point(aes(x = lifeExp_2007, 
                       y = reorder(country, abs(lifeExp_2007 - lifeExp_1952)),
                       fill = country),
                   color = "black",
                   size = 4,
                   shape = 21) +
        theme(
          legend.position = "none"
        ) +
        labs(
          y = NULL,
          x = "Life Expectancy",
          title = "Largest Differences in Life Expectancies 1952 - 2007",
          subtitle = "Top 10 countries"
        ) +
        scale_fill_manual(values = my_colors) +
        my_theme
    
    
  })
  
  # Dumbbell plots
  
  
  output$plot_dumbbell_smallest <- renderPlot({
    
    smallest_change_countries <- diff_df |> 
      arrange(abs(diff)) |> 
      slice_min(n = 10, order_by = diff) |> 
      distinct(country, .keep_all = TRUE)
    
    ## Lowest diffs
    smallest_change_countries |> 
      ggplot() +
      geom_segment(aes(y = reorder(country, abs(lifeExp_2007 - lifeExp_1952)), 
                       yend = reorder(country, abs(lifeExp_2007 - lifeExp_1952)),
                       x = lifeExp_1952, xend = lifeExp_2007)) +
      geom_point(aes(x = lifeExp_1952, y = reorder(country, abs(lifeExp_2007 - lifeExp_1952)))) +
      geom_point(aes(x = lifeExp_2007, y = reorder(country, abs(lifeExp_2007 - lifeExp_1952))))
    
    ## biggest diffs plot
    smallest_change_countries |> 
      ggplot() +
      geom_segment(aes(y = reorder(country, abs(lifeExp_2007 - lifeExp_1952)), 
                       yend = reorder(country, abs(lifeExp_2007 - lifeExp_1952)),
                       x = lifeExp_1952, xend = lifeExp_2007)) +
      geom_point(aes(x = lifeExp_1952, 
                     y = reorder(country, abs(lifeExp_2007 - lifeExp_1952)),
                     fill = country),
                 color = "black",
                 size = 4,
                 shape = 21) +
      geom_point(aes(x = lifeExp_2007, 
                     y = reorder(country, abs(lifeExp_2007 - lifeExp_1952)),
                     fill = country),
                 color = "black",
                 size = 4,
                 shape = 21) +
      theme(
        legend.position = "none"
      ) +
      labs(
        y = NULL,
        x = "Life Expectancy",
        title = "Smallest Differences in Life Expectancies 1952 - 2007",
        subtitle = "Top 10 countries"
      ) +
      scale_fill_manual(values = my_colors) +
      my_theme
    
    
  })
}


## Run
shinyApp(ui = ui, server = server)
