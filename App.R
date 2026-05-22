# =============================================================
#  Data Science Jobs in America — Salary Analytics Dashboard
#  Dataset : Glassdoor Jobs (thedevastator, Kaggle 2022)
#  Tools   : R · Shiny · bslib · ggplot2 · plotly · dplyr · tidyr
# =============================================================

library(shiny)
library(bslib)
library(bsicons)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)
library(tidyr)
library(scales)

# ─────────────────────────────────────────────────────────────
#  SECTION 1 · LOAD & CLEAN
# ─────────────────────────────────────────────────────────────
raw <- read.csv("salary_data_cleaned.csv", stringsAsFactors = FALSE)

df <- raw %>%
  mutate(
    job_state  = str_trim(job_state),
    avg_salary = as.numeric(avg_salary),
    min_salary = as.numeric(min_salary),
    max_salary = as.numeric(max_salary),
    Rating     = as.numeric(Rating),
    age        = as.numeric(age),
    Rating     = ifelse(Rating  < 0, NA, Rating),
    age        = ifelse(age     < 0, NA, age),
    Sector     = ifelse(Sector == "-1", NA, Sector),
    Size       = ifelse(Size   %in% c("-1","Unknown"), NA, Size),
    Type.of.ownership = ifelse(
      Type.of.ownership %in% c("-1","Unknown"), NA, Type.of.ownership),
    sal_spread = max_salary - min_salary,
    role = case_when(
      str_detect(tolower(Job.Title), "data scientist")          ~ "Data Scientist",
      str_detect(tolower(Job.Title), "data engineer")           ~ "Data Engineer",
      str_detect(tolower(Job.Title), "data analyst")            ~ "Data Analyst",
      str_detect(tolower(Job.Title), "machine learning|ml eng") ~ "ML Engineer",
      str_detect(tolower(Job.Title), "director|head|vp|chief")  ~ "Director/VP",
      str_detect(tolower(Job.Title), "manager")                 ~ "Manager",
      TRUE                                                       ~ "Other"
    ),
    seniority = case_when(
      str_detect(tolower(Job.Title),
                 "senior|sr\\.|lead|principal|staff") ~ "Senior",
      str_detect(tolower(Job.Title),
                 "junior|jr\\.|associate|entry")       ~ "Junior",
      str_detect(tolower(Job.Title),
                 "director|vp|head|chief")             ~ "Executive",
      TRUE                                             ~ "Mid-Level"
    ),
    Size = factor(Size, levels = c(
      "1 to 50 employees","51 to 200 employees","201 to 500 employees",
      "501 to 1000 employees","1001 to 5000 employees",
      "5001 to 10000 employees","10000+ employees")),
    sal_source = case_when(
      employer_provided == 1 ~ "Employer Confirmed",
      hourly            == 1 ~ "Hourly Rate",
      TRUE                   ~ "Glassdoor Estimate"),
    age_group = case_when(
      is.na(age)  ~ NA_character_,
      age <=  5   ~ "Startup (0-5 yrs)",
      age <= 15   ~ "Young (6-15 yrs)",
      age <= 30   ~ "Established (16-30 yrs)",
      age <= 60   ~ "Mature (31-60 yrs)",
      TRUE        ~ "Legacy (60+ yrs)"),
    age_group = factor(age_group, levels = c(
      "Startup (0-5 yrs)","Young (6-15 yrs)",
      "Established (16-30 yrs)","Mature (31-60 yrs)","Legacy (60+ yrs)"))
  ) %>%
  filter(!is.na(avg_salary), avg_salary > 10)

# ─────────────────────────────────────────────────────────────
#  SECTION 2 · CONSTANTS & COLOR SYSTEM
# ─────────────────────────────────────────────────────────────
SKILL_COLS  <- c("python_yn","R_yn","spark","aws","excel")
SKILL_NAMES <- c("Python","R","Spark","AWS","Excel")

ROLES   <- c("All", sort(unique(df$role)))
SECTORS <- c("All", sort(na.omit(unique(df$Sector))))
SENIORS <- c("All","Junior","Mid-Level","Senior","Executive")
SIZES   <- c("All", levels(df$Size))
OWNS    <- c("All", sort(na.omit(unique(df$Type.of.ownership))))
STATES  <- sort(unique(df$job_state))

# ── Light palette (Option B — LinkedIn / Glassdoor) ──────────
palette_light <- list(
  bg           = "#F7F9FC",
  bg2          = "#EBF0F8",
  bg3          = "#DDE5F0",
  primary      = "#2A5298",
  primary2     = "#3B82C4",
  primary3     = "#5A9ED6",
  accent       = "#E8623A",
  positive     = "#27A86B",
  negative     = "#D63B3B",
  text1        = "#1A2B45",
  text2        = "#3D5A7A",
  text3        = "#5A7A99",
  grid         = "#DDE5F0",
  hover_bg     = "#FFFFFF",
  hover_border = "#2A5298",
  hover_text   = "#1A2B45"
)

# ── Dark palette (Option C — Power BI / Grafana) ─────────────
palette_dark <- list(
  bg           = "#111827",
  bg2          = "#1F2E42",
  bg3          = "#243447",
  primary      = "#2E86AB",
  primary2     = "#3A9CC4",
  primary3     = "#06B6D4",
  accent       = "#F59E0B",
  positive     = "#10B981",
  negative     = "#EF4444",
  text1        = "#F0F4FF",
  text2        = "#A0B4CC",
  text3        = "#6B8FAF",
  grid         = "#1F2E42",
  hover_bg     = "#1F2E42",
  hover_border = "#2E86AB",
  hover_text   = "#F0F4FF"
)

# ── ggplot2 theme factory ────────────────────────────────────
make_theme <- function(pal) {
  theme_minimal(base_size = 13) +
    theme(
      plot.background  = element_rect(fill = pal$bg,  color = NA),
      panel.background = element_rect(fill = pal$bg,  color = NA),
      text             = element_text(color = pal$text1),
      axis.text        = element_text(color = pal$text3),
      axis.title       = element_text(color = pal$text2, size = 11),
      panel.grid.major = element_line(color = pal$grid, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 13,
                                      color = pal$text1,
                                      margin = margin(b = 3)),
      plot.subtitle    = element_text(size = 10.5, color = pal$text2,
                                      margin = margin(b = 8)),
      legend.background = element_rect(fill = pal$bg, color = NA),
      legend.text      = element_text(color = pal$text3),
      legend.title     = element_text(face = "bold", color = pal$text2),
      legend.position  = "bottom",
      strip.background = element_rect(fill = pal$bg2, color = NA),
      strip.text       = element_text(face = "bold", size = 10,
                                      color = pal$text2)
    )
}

# ── Multi-category palettes ───────────────────────────────────
COL_LIGHT <- c("#2A5298","#E8623A","#27A86B","#7BA7D4","#F4A07A",
               "#5BA3C9","#5CC49A","#D63B3B","#9B7FBF")
COL_DARK  <- c("#2E86AB","#F59E0B","#10B981","#06B6D4","#F4C06A",
               "#7EC8E3","#6EE7C0","#EF4444","#A78BFA")

# ─────────────────────────────────────────────────────────────
#  SECTION 3 · UI
# ─────────────────────────────────────────────────────────────
ui <- page_navbar(
  title  = span(bs_icon("bar-chart-line-fill"), "  DS Salary Analytics"),
  theme  = bs_theme(
    bootswatch = "flatly",
    bg         = "#F7F9FC",
    fg         = "#1A2B45",
    primary    = "#2A5298",
    success    = "#27A86B",
    warning    = "#E8623A",
    danger     = "#D63B3B",
    font_scale = 0.95
  ) %>%
    bs_add_rules("
      [data-bs-theme=dark] {
        --bs-body-bg:      #111827;
        --bs-body-color:   #F0F4FF;
        --bs-primary:      #2E86AB;
        --bs-success:      #10B981;
        --bs-warning:      #F59E0B;
        --bs-danger:       #EF4444;
        --bs-card-bg:      #1F2E42;
        --bs-border-color: #243447;
      }
    "),
  fillable = FALSE,

  nav_spacer(),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light")),

  # ── TAB 1 · DASHBOARD ─────────────────────────────────────
  nav_panel("Dashboard",
    div(
      style = "display:flex; justify-content:flex-end; margin-bottom:12px;",
      div(
        style = paste(
          "background:var(--bs-body-bg);",
          "border:1px solid var(--bs-border-color);",
          "border-radius:8px; padding:10px 16px;",
          "display:flex; gap:16px; align-items:flex-end; flex-wrap:wrap;"
        ),
        div(style = "min-width:200px;",
            sliderInput("f_sal", "Salary range (K USD)",
                        min=10, max=260, value=c(10,260),
                        step=5, ticks=FALSE)),
        div(style = "min-width:130px;",
            selectInput("f_role",   "Role",      ROLES,   "All")),
        div(style = "min-width:130px;",
            selectInput("f_sector", "Sector",    SECTORS, "All")),
        div(style = "min-width:130px;",
            selectInput("f_senior", "Seniority", SENIORS, "All")),
        div(style = "padding-bottom:6px; font-size:12px;",
            icon("circle-info"),
            " Filters apply to all tabs except Company & Profile.")
      )
    ),
    layout_columns(
      col_widths = c(3,3,3,3),
      value_box("Job Postings",         textOutput("kpi_n"),
                bs_icon("briefcase-fill"),   theme = "primary"),
      value_box("Median Salary",        textOutput("kpi_med"),
                bs_icon("currency-dollar"),  theme = "success"),
      value_box("Salary IQR",           textOutput("kpi_iqr"),
                bs_icon("arrows-expand"),    theme = "info"),
      value_box("Avg Negotiation Room", textOutput("kpi_spread"),
                bs_icon("arrow-left-right"), theme = "warning")
    ),
    layout_columns(
      col_widths = c(8,4),
      card(card_header("Salary Range by Job Role"),
           card_body(plotlyOutput("p_dumbbell", height = "390px"))),
      card(card_header("Job Postings by Role"),
           card_body(plotlyOutput("p_role_bar", height = "390px")))
    ),
    layout_columns(
      col_widths = c(5,7),
      card(card_header("Seniority Distribution and Median Salary"),
           card_body(plotlyOutput("p_seniority_lollipop", height = "300px"))),
      card(card_header("Salary Distribution by Seniority Level"),
           card_body(plotlyOutput("p_seniority_box", height = "300px")))
    )
  ),

  # ── TAB 2 · LOCATION ─────────────────────────────────────
  nav_panel("Location",
    layout_columns(
      col_widths = c(3,3,3,3),
      value_box("Top Paying State",     textOutput("geo_top"),
                bs_icon("trophy-fill"),   theme = "success"),
      value_box("Most Jobs In",         textOutput("geo_most"),
                bs_icon("building"),      theme = "primary"),
      value_box("Local Applicant Avg",  textOutput("geo_local"),
                bs_icon("house-fill"),    theme = "info"),
      value_box("Remote Applicant Avg", textOutput("geo_remote"),
                bs_icon("airplane-fill"), theme = "warning")
    ),
    layout_columns(
      col_widths = c(6,6),
      card(card_header("Top 15 States by Job Volume"),
           card_body(plotlyOutput("p_state_n",   height = "420px"))),
      card(card_header("Top 15 States by Median Salary"),
           card_body(plotlyOutput("p_state_sal", height = "420px")))
    ),
    layout_columns(
      col_widths = c(5,7),
      card(card_header("Salary Comparison — Local vs Out-of-State Applicants"),
           card_body(plotlyOutput("p_local_box",  height = "310px"))),
      card(card_header("Local vs Out-of-State Median Salary by Role"),
           card_body(plotlyOutput("p_local_role", height = "310px")))
    )
  ),

  # ── TAB 3 · SKILLS ───────────────────────────────────────
  nav_panel("Skills",
    layout_columns(
      col_widths = c(3,3,3,3),
      value_box("Most In-Demand Skill",  textOutput("sk_top_demand"),
                bs_icon("code-slash"),             theme = "primary"),
      value_box("Biggest Salary Boost",  textOutput("sk_top_premium"),
                bs_icon("lightning-charge-fill"),  theme = "success"),
      value_box("Python Demand",         textOutput("sk_python"),
                bs_icon("filetype-py"),            theme = "info"),
      value_box("Excel Demand",          textOutput("sk_excel"),
                bs_icon("file-earmark-excel-fill"),theme = "warning")
    ),
    layout_columns(
      col_widths = c(6,6),
      card(card_header("Skill Demand Across All Job Postings"),
           card_body(plotlyOutput("p_skill_demand",  height = "320px"))),
      card(card_header("Salary Premium by Technical Skill"),
           card_body(plotlyOutput("p_skill_premium", height = "320px")))
    ),
    layout_columns(
      col_widths = c(7,5),
      card(card_header("Skill Demand by Job Role"),
           card_body(plotlyOutput("p_skill_role", height = "370px"))),
      card(card_header("Skill Co-occurrence Matrix"),
           card_body(plotlyOutput("p_cooccur",    height = "370px")))
    )
  ),

  # ── TAB 4 · COMPANY ──────────────────────────────────────
  nav_panel("Company",
    layout_sidebar(
      sidebar = sidebar(
        title = "Company Filters", width = 240,
        selectInput("co_size", "Company Size",   SIZES, "All"),
        selectInput("co_own",  "Ownership Type", OWNS,  "All"),
        hr(),
        helpText("These filters are independent from the global filters.")
      ),
      layout_columns(
        col_widths = c(6,6),
        card(card_header("Median Salary by Company Size"),
             card_body(plotlyOutput("p_co_size", height = "330px"))),
        card(card_header("Median Salary by Ownership Type"),
             card_body(plotlyOutput("p_co_own",  height = "330px")))
      ),
      layout_columns(
        col_widths = c(6,6),
        card(card_header("Company Rating vs Average Salary"),
             card_body(plotlyOutput("p_rating",  height = "330px"))),
        card(card_header("Median Salary by Company Age"),
             card_body(plotlyOutput("p_age_sal", height = "330px")))
      )
    )
  ),

  # ── TAB 5 · PROFILE BUILDER ──────────────────────────────
  nav_panel("My Profile",
    layout_columns(
      col_widths = c(3,9),
      card(
        card_header(bs_icon("person-badge"), " Build Your Profile"),
        card_body(
          p("Set your details and hit", strong("Find Matches"),
            "to see your expected salary and skill gaps."),
          hr(),
          selectInput("pr_role",   "Target Role",
                      c("Any", sort(unique(df$role))), "Any"),
          selectInput("pr_state",  "Preferred State",
                      c("Any", STATES), "Any"),
          selectInput("pr_senior", "Seniority",
                      c("Any","Junior","Mid-Level","Senior","Executive"),
                      "Any"),
          checkboxGroupInput("pr_skills", "Skills I Have",
                             choiceNames  = SKILL_NAMES,
                             choiceValues = SKILL_COLS),
          hr(),
          actionButton("pr_go", "Find Matches",
                       class = "btn-primary w-100",
                       icon  = icon("magnifying-glass"))
        )
      ),
      tagList(
        layout_columns(
          col_widths = c(4,4,4),
          value_box("Matching Jobs", textOutput("pr_n"),
                    bs_icon("briefcase"),       theme = "primary"),
          value_box("Avg Salary",    textOutput("pr_avg"),
                    bs_icon("currency-dollar"), theme = "success"),
          value_box("Salary Range",  textOutput("pr_range"),
                    bs_icon("arrows-expand"),   theme = "info")
        ),
        layout_columns(
          col_widths = c(6,6),
          card(card_header("Your Expected Salary Distribution"),
               card_body(plotlyOutput("pr_dist",   height = "270px"))),
          card(card_header("Top Hiring Sectors for Your Profile"),
               card_body(plotlyOutput("pr_sector", height = "270px")))
        ),
        card(
          card_header(bs_icon("lightbulb"),
                      " Skill Gap — Estimated Salary Impact"),
          card_body(plotlyOutput("pr_gap", height = "230px"))
        )
      )
    )
  ),

  # ── TAB 6 · ABOUT ────────────────────────────────────────
  nav_panel("About",
    card(
      card_header(bs_icon("info-circle"), " About This Dashboard"),
      card_body(markdown("
## Purpose

This dashboard explores the **US data science job market** using 742 real Glassdoor
postings. It answers one central question:

> *\"What role, skills, location, and company type should I target to maximize
>    my data science salary?\"*

---

## Dashboard Tabs

| Tab | What It Shows |
|---|---|
| **Overview** | Market-wide salary landscape, role breakdown, seniority distribution |
| **Location** | Salary & job density by US state; local vs out-of-state salary advantage |
| **Skills** | Skill demand %, salary premium per skill, co-occurrence patterns |
| **Company** | How size, ownership type, rating & age affect pay |
| **My Profile** | Personalised salary estimate + skill gap analysis |

---

## Key Questions Answered

1. Which roles command the highest salaries?
2. Which US states offer the most jobs and best pay?
3. Does applying locally pay more than applying out-of-state?
4. Which technical skill gives the biggest salary boost?
5. Do large companies always pay more than startups?
6. Does a high Glassdoor rating correlate with higher pay?
7. Given my current skills, what salary range should I expect?
8. Which missing skill would boost my salary the most?

---

## Dataset

| | |
|---|---|
| **Name** | Jobs Dataset from Glassdoor |
| **Author** | thedevastator |
| **Source** | [kaggle.com/datasets/thedevastator/jobs-dataset-from-glassdoor](https://www.kaggle.com/datasets/thedevastator/jobs-dataset-from-glassdoor) |
| **Scraped from** | Glassdoor.com |
| **Period** | 2017–2018 |
| **Rows** | 742 job postings |
| **Geography** | 38 US states |
| **Sectors** | 24 industry sectors |
| **License** | Public / Open |

**Citation:**
thedevastator. (2022). *Jobs Dataset from Glassdoor* [Data set].
Kaggle. https://www.kaggle.com/datasets/thedevastator/jobs-dataset-from-glassdoor

---

## Built With

**R** · **Shiny** · **bslib** · **ggplot2** · **plotly** · **dplyr** · **tidyr** · **stringr** · **scales**

*Developed for academic purposes — Data Analytics with R course.*
      "))
    )
  )
)

# ─────────────────────────────────────────────────────────────
#  SECTION 4 · SERVER
# ─────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Mode-aware reactives ─────────────────────────────────
  PAL <- reactive({
    if (isTRUE(input$dark_mode == "dark")) palette_dark else palette_light
  })
  COL_R <- reactive({
    if (isTRUE(input$dark_mode == "dark")) COL_DARK else COL_LIGHT
  })
  TH_R <- reactive({ make_theme(PAL()) })

  # ── Plotly wrapper ───────────────────────────────────────
  to_plotly <- function(p, height = 400) {
    pal <- PAL()
    ggplotly(p, height = height, tooltip = "text") %>%
      layout(
        paper_bgcolor = pal$bg,
        plot_bgcolor  = pal$bg,
        hoverlabel = list(
          bgcolor     = pal$hover_bg,
          bordercolor = pal$hover_border,
          font        = list(family = "Arial", size = 13,
                             color  = pal$hover_text)
        ),
        legend = list(
          orientation = "h", x = 0, y = -0.15,
          font        = list(color = pal$text3)
        )
      ) %>%
      config(displayModeBar = FALSE)
  }

  # ── Global filtered data ─────────────────────────────────
  GDF <- reactive({
    d <- df %>% filter(avg_salary >= input$f_sal[1],
                       avg_salary <= input$f_sal[2])
    if (input$f_role   != "All") d <- d %>% filter(role      == input$f_role)
    if (input$f_sector != "All") d <- d %>% filter(Sector    == input$f_sector)
    if (input$f_senior != "All") d <- d %>% filter(seniority == input$f_senior)
    d
  })

  # ── Company filtered data ────────────────────────────────
  CDF <- reactive({
    d <- df
    if (input$co_size != "All") d <- d %>%
        filter(as.character(Size) == input$co_size)
    if (input$co_own  != "All") d <- d %>%
        filter(Type.of.ownership  == input$co_own)
    d
  })

  # ── Skill stats ──────────────────────────────────────────
  SKILL_STATS <- reactive({
    d <- GDF()
    tibble(
      skill   = SKILL_NAMES,
      col     = SKILL_COLS,
      demand  = sapply(SKILL_COLS,
                       \(s) mean(d[[s]], na.rm = TRUE) * 100),
      premium = sapply(SKILL_COLS, \(s)
        mean(d$avg_salary[d[[s]] == 1], na.rm = TRUE) -
        mean(d$avg_salary[d[[s]] == 0], na.rm = TRUE))
    )
  })

  # ── KPIs ─────────────────────────────────────────────────
  output$kpi_n      <- renderText(scales::comma(nrow(GDF())))
  output$kpi_med    <- renderText(
    paste0("$", round(median(GDF()$avg_salary, na.rm = TRUE), 1), "K"))
  output$kpi_iqr    <- renderText({
    d <- GDF()$avg_salary
    paste0("$", round(quantile(d, .25, na.rm = TRUE)),
           "K – $", round(quantile(d, .75, na.rm = TRUE)), "K")
  })
  output$kpi_spread <- renderText(
    paste0("$", round(mean(GDF()$sal_spread, na.rm = TRUE), 1), "K"))

  # ════════════════════════════════════════════════════════
  #  DASHBOARD
  # ════════════════════════════════════════════════════════

  output$p_dumbbell <- renderPlotly({
    pal <- PAL(); th <- TH_R()
    d <- GDF() %>%
      group_by(role) %>%
      summarise(lo  = median(min_salary, na.rm = TRUE),
                mid = median(avg_salary, na.rm = TRUE),
                hi  = median(max_salary, na.rm = TRUE),
                n   = n(), .groups = "drop") %>%
      arrange(mid) %>%
      mutate(role = factor(role, levels = role),
             tip  = paste0("<b>", role, "</b><br>",
                           "Min: $", round(lo),  "K<br>",
                           "Avg: $", round(mid), "K<br>",
                           "Max: $", round(hi),  "K<br>",
                           "Postings: ", n))
    validate(need(nrow(d) > 0, "No data for current filters."))
    top_role <- d %>% slice_max(mid, n = 1, with_ties = FALSE)
    p <- ggplot(d, aes(y = role)) +
      geom_segment(aes(x = lo, xend = hi, yend = role, text = tip),
                   color = pal$bg3, linewidth = 5, lineend = "round") +
      geom_point(aes(x = lo,  text = tip), color = pal$accent,   size = 3.5) +
      geom_point(aes(x = mid, text = tip), color = pal$primary,  size = 5.5) +
      geom_point(aes(x = hi,  text = tip), color = pal$positive, size = 3.5) +
      geom_text(aes(x = mid, label = paste0("$", round(mid), "K")),
                nudge_y = 0.38, size = 3.4, fontface = "bold",
                color = pal$primary) +
      annotate("text",
               x = top_role$hi * 1.05, y = as.numeric(top_role$role),
               label = "Highest paying role",
               hjust = 0, size = 3.2, color = pal$accent, fontface = "bold") +
      scale_x_continuous(labels = dollar_format(suffix = "K", prefix = "$"),
                         expand = expansion(mult = c(0.02, 0.22))) +
      labs(title    = "Salary Range by Job Role  (min · avg · max)",
           subtitle = "Orange dot = typical min  ·  Blue = median avg  ·  Green = typical max",
           x = "Salary (K USD)", y = NULL) +
      th
    to_plotly(p, 390)
  })

  output$p_role_bar <- renderPlotly({
    pal <- PAL(); th <- TH_R()
    d <- GDF() %>%
      count(role) %>%
      mutate(pct    = round(n / sum(n) * 100, 1),
             is_top = n == max(n),
             tip    = paste0("<b>", role, "</b><br>",
                             "Postings: ", n, "<br>",
                             "Share: ", pct, "%")) %>%
      arrange(n) %>%
      mutate(role = factor(role, levels = role))
    validate(need(nrow(d) > 0, "No data."))
    p <- ggplot(d, aes(n, role, fill = n, text = tip)) +
      geom_col(alpha = 0.88, width = 0.7, show.legend = FALSE) +
      geom_text(aes(label = paste0(n, "  (", pct, "%)")),
                hjust = -0.08, size = 3.4, fontface = "bold",
                color = pal$text1) +
      scale_fill_gradient(low = pal$primary3, high = pal$primary) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.32))) +
      labs(title    = "Job Postings by Role",
           subtitle = "Darker = more postings",
           x = "Number of Postings", y = NULL) +
      th
    to_plotly(p, 390)
  })

  output$p_seniority_lollipop <- renderPlotly({
    pal <- PAL(); th <- TH_R()
    d <- GDF() %>%
      group_by(seniority) %>%
      summarise(n   = n(),
                med = median(avg_salary, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(pct       = round(n / sum(n) * 100, 1),
             seniority = factor(seniority,
                                levels = c("Junior","Mid-Level",
                                           "Senior","Executive")),
             tip = paste0("<b>", seniority, "</b><br>",
                          "Share: ", pct, "%<br>",
                          "Median salary: $", round(med), "K<br>",
                          "Postings: ", n))
    validate(need(nrow(d) > 0, "No data."))
    p <- ggplot(d, aes(pct, seniority, text = tip)) +
      geom_segment(aes(x = 0, xend = pct, yend = seniority),
                   color = pal$bg3, linewidth = 1.5) +
      geom_point(aes(color = med), size = 8, show.legend = FALSE) +
      geom_text(aes(label = paste0(pct, "%")),
                color = "white", size = 3, fontface = "bold") +
      geom_text(aes(x = pct,
                    label = paste0("  Median: $", round(med), "K")),
                hjust = 0, size = 3.3, color = pal$text2) +
      scale_color_gradient(low = pal$primary3, high = pal$primary) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.45))) +
      labs(title    = "Seniority Distribution and Median Salary",
           subtitle = "Dot % = share of postings  ·  Label = median salary",
           x = "Share of Postings (%)", y = NULL) +
      th
    to_plotly(p, 300)
  })

  output$p_seniority_box <- renderPlotly({
    pal <- PAL(); th <- TH_R()
    d <- GDF()
    validate(need(nrow(d) > 0, "No data."))
    med_labels <- d %>%
      group_by(seniority) %>%
      summarise(med = median(avg_salary, na.rm = TRUE), .groups = "drop")
    d2 <- d %>%
      mutate(tip = paste0("<b>", seniority, "</b><br>",
                          "Salary: $", round(avg_salary), "K"))
    p <- ggplot(d2, aes(seniority, avg_salary, fill = seniority, text = tip)) +
      geom_boxplot(alpha = 0.75, outlier.alpha = 0.15,
                   outlier.size = 1, show.legend = FALSE) +
      geom_text(data = med_labels,
                aes(y = med, label = paste0("$", round(med), "K"),
                    text = NULL),
                vjust = -0.6, size = 3.4, fontface = "bold",
                color = pal$text1) +
      scale_fill_manual(values = COL_R()) +
      scale_y_continuous(labels = dollar_format(suffix = "K", prefix = "$")) +
      labs(title    = "Salary Distribution by Seniority Level",
           subtitle = "Box = middle 50% of salaries  ·  Line = median",
           x = NULL, y = "Avg Salary (K USD)") +
      th
    to_plotly(p, 300)
  })

  # ════════════════════════════════════════════════════════
  #  LOCATION
  # ════════════════════════════════════════════════════════

  output$geo_top <- renderText({
    d <- GDF() %>%
      group_by(job_state) %>%
      summarise(med = median(avg_salary, na.rm = TRUE),
                n = n(), .groups = "drop") %>%
      filter(n >= 3) %>% slice_max(med, n = 1, with_ties = FALSE)
    if (nrow(d) == 0) return("N/A")
    paste0(d$job_state, " ($", round(d$med), "K)")
  })
  output$geo_most <- renderText({
    d <- GDF() %>% count(job_state) %>%
      slice_max(n, n = 1, with_ties = FALSE)
    if (nrow(d) == 0) return("N/A")
    paste0(d$job_state, " (", d$n, " jobs)")
  })
  output$geo_local <- renderText({
    v <- mean(GDF()$avg_salary[GDF()$same_state == 1], na.rm = TRUE)
    if (is.nan(v)) return("N/A")
    paste0("$", round(v, 1), "K")
  })
  output$geo_remote <- renderText({
    v <- mean(GDF()$avg_salary[GDF()$same_state == 0], na.rm = TRUE)
    if (is.nan(v)) return("N/A")
    paste0("$", round(v, 1), "K")
  })

  output$p_state_n <- renderPlotly({
    pal <- PAL(); th <- TH_R()
    d <- GDF() %>% count(job_state) %>%
      slice_max(n, n = 15) %>% arrange(n) %>%
      mutate(job_state = factor(job_state, levels = job_state),
             is_top    = n == max(n),
             tip       = paste0("<b>", job_state, "</b><br>",
                                "Postings: ", n))
    validate(need(nrow(d) > 0, "No data."))
    p <- ggplot(d, aes(n, job_state, fill = n, text = tip)) +
      geom_col(show.legend = FALSE, alpha = 0.88, width = 0.72) +
      geom_text(aes(label = n), hjust = -0.2, size = 3.4,
                fontface = "bold", color = pal$text1) +
      geom_text(data = filter(d, is_top),
                aes(x = n / 2, label = " #1 for job volume ", text = NULL),
                hjust = 0.5, size = 3, color = "white", fontface = "bold") +
      scale_fill_gradient(low = pal$primary3, high = pal$primary) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
      labs(title    = "Top 15 States by Job Volume",
           subtitle = "Darker = more postings",
           x = "Job Postings", y = NULL) +
      th
    to_plotly(p, 420)
  })

  output$p_state_sal <- renderPlotly({
    pal <- PAL(); th <- TH_R()
    d <- GDF() %>%
      group_by(job_state) %>%
      summarise(med = median(avg_salary, na.rm = TRUE),
                n = n(), .groups = "drop") %>%
      filter(n >= 3) %>% slice_max(med, n = 15) %>%
      arrange(med) %>%
      mutate(job_state = factor(job_state, levels = job_state),
             is_top    = med == max(med),
             tip       = paste0("<b>", job_state, "</b><br>",
                                "Median salary: $", round(med), "K<br>",
                                "Postings: ", n))
    validate(need(nrow(d) > 0, "No data."))
    p <- ggplot(d, aes(med, job_state, fill = med, text = tip)) +
      geom_col(show.legend = FALSE, alpha = 0.88, width = 0.72) +
      geom_text(aes(label = paste0("$", round(med), "K")),
                hjust = -0.12, size = 3.4, fontface = "bold",
                color = pal$text1) +
      geom_text(data = filter(d, is_top),
                aes(x = med / 2, label = " Highest paying ", text = NULL),
                hjust = 0.5, size = 3, color = "white", fontface = "bold") +
      scale_fill_gradient(low = pal$primary3, high = pal$primary) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
      labs(title    = "Top 15 States by Median Salary",
           subtitle = "States with fewer than 3 postings excluded",
           x = "Median Salary (K USD)", y = NULL) +
      th
    to_plotly(p, 420)
  })

  output$p_local_box <- renderPlotly({
    pal <- PAL(); th <- TH_R()
    d <- GDF() %>%
      mutate(group = ifelse(same_state == 1, "Same State", "Different State"),
             tip   = paste0("<b>",
                            ifelse(same_state == 1,
                                   "Same State", "Different State"),
                            "</b><br>Salary: $", round(avg_salary), "K"))
    validate(need(nrow(d) > 0, "No data."))
    local_med  <- median(d$avg_salary[d$group == "Same State"],      na.rm = TRUE)
    remote_med <- median(d$avg_salary[d$group == "Different State"], na.rm = TRUE)
    gap        <- round(abs(local_med - remote_med), 1)
    winner     <- ifelse(local_med >= remote_med, "Local", "Out-of-state")
    p <- ggplot(d, aes(group, avg_salary, fill = group, text = tip)) +
      geom_boxplot(alpha = 0.78, outlier.alpha = 0.2,
                   show.legend = FALSE, outlier.size = 1.2) +
      geom_jitter(aes(color = group), width = 0.15,
                  alpha = 0.15, size = 1.2, show.legend = FALSE) +
      annotate("segment",
               x = 1, xend = 2,
               y    = max(local_med, remote_med) * 1.12,
               yend = max(local_med, remote_med) * 1.12,
               color = pal$text3, linewidth = 0.6,
               arrow = arrow(ends = "both", length = unit(0.1, "cm"))) +
      annotate("text",
               x = 1.5, y = max(local_med, remote_med) * 1.17,
               label = paste0("$", gap, "K gap"),
               size = 3.5, fontface = "bold", color = pal$text1) +
      scale_fill_manual(values  = c("Same State"      = pal$primary,
                                    "Different State" = pal$accent)) +
      scale_color_manual(values = c("Same State"      = pal$primary,
                                    "Different State" = pal$accent)) +
      scale_y_continuous(labels = dollar_format(suffix = "K", prefix = "$")) +
      labs(title    = "Salary Comparison — Local vs Out-of-State Applicants",
           subtitle = paste0(winner, " applicants earn more  ·  Median gap: $", gap, "K"),
           x = NULL, y = "Avg Salary (K USD)") +
      th
    to_plotly(p, 310)
  })

  output$p_local_role <- renderPlotly({
    pal <- PAL(); th <- TH_R()
    d <- GDF() %>%
      mutate(group = ifelse(same_state == 1, "Same State", "Different State")) %>%
      group_by(role, group) %>%
      summarise(med = median(avg_salary, na.rm = TRUE), .groups = "drop") %>%
      mutate(tip = paste0("<b>", role, " — ", group, "</b><br>",
                          "Median salary: $", round(med), "K"))
    validate(need(nrow(d) > 0, "No data."))
    p <- ggplot(d, aes(role, med, fill = group, text = tip)) +
      geom_col(position = "dodge", alpha = 0.85, width = 0.7) +
      geom_text(aes(label = paste0("$", round(med), "K")),
                position = position_dodge(width = 0.7),
                vjust = -0.4, size = 2.9, fontface = "bold",
                color = pal$text1) +
      scale_fill_manual(values = c("Same State"      = pal$primary,
                                   "Different State" = pal$accent),
                        name = NULL) +
      scale_y_continuous(labels = dollar_format(suffix = "K", prefix = "$"),
                         expand = expansion(mult = c(0, 0.15))) +
      labs(title    = "Local vs Out-of-State Median Salary by Role",
           subtitle = "Blue = same-state  ·  Orange/Amber = out-of-state",
           x = NULL, y = "Median Salary (K USD)") +
      th + theme(axis.text.x = element_text(angle = 30, hjust = 1,
                                             color = pal$text3))
    to_plotly(p, 310)
  })

  # ════════════════════════════════════════════════════════
  #  SKILLS
  # ════════════════════════════════════════════════════════

  output$sk_top_demand <- renderText({
    s <- SKILL_STATS() %>% slice_max(demand, n = 1, with_ties = FALSE)
    paste0(s$skill, " (", round(s$demand, 0), "% of jobs)")
  })
  output$sk_top_premium <- renderText({
    s <- SKILL_STATS() %>% slice_max(premium, n = 1, with_ties = FALSE)
    paste0(s$skill, " (+$", round(s$premium, 1), "K)")
  })
  output$sk_python <- renderText(
    paste0(round(mean(GDF()$python_yn, na.rm = TRUE) * 100, 1), "%"))
  output$sk_excel  <- renderText(
    paste0(round(mean(GDF()$excel,     na.rm = TRUE) * 100, 1), "%"))

  output$p_skill_demand <- renderPlotly({
    pal <- PAL(); th <- TH_R()
    d <- SKILL_STATS() %>%
      arrange(demand) %>%
      mutate(skill  = factor(skill, levels = skill),
             is_top = demand == max(demand),
             tip    = paste0("<b>", skill, "</b><br>",
                             "Required in: ", round(demand, 1), "% of postings"))
    validate(need(nrow(d) > 0, "No data."))
    p <- ggplot(d, aes(demand, skill, fill = demand, text = tip)) +
      geom_col(show.legend = FALSE, alpha = 0.85, width = 0.65) +
      geom_text(aes(label = paste0(round(demand, 1), "%")),
                hjust = -0.1, size = 4, fontface = "bold",
                color = pal$text1) +
      geom_text(data = filter(d, is_top),
                aes(x = demand / 2, label = " Most required ", text = NULL),
                hjust = 0.5, size = 3, color = "white", fontface = "bold") +
      scale_fill_gradient(low = pal$primary3, high = pal$primary) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.25)),
                         limits = c(0, 100)) +
      labs(title    = "Skill Demand Across All Job Postings",
           subtitle = "% of all job postings requiring this skill",
           x = "% of Job Postings", y = NULL) +
      th
    to_plotly(p, 320)
  })

  output$p_skill_premium <- renderPlotly({
    pal <- PAL(); th <- TH_R()
    d <- SKILL_STATS() %>%
      arrange(premium) %>%
      mutate(skill   = factor(skill, levels = skill),
             dir     = ifelse(premium >= 0, "positive", "negative"),
             is_best = premium == max(premium),
             tip     = paste0("<b>", skill, "</b><br>",
                              "Salary boost: ",
                              ifelse(premium > 0, "+", ""),
                              round(premium, 1), "K"))
    validate(need(nrow(d) > 0, "No data."))
    best <- d %>% filter(is_best)
    p <- ggplot(d, aes(premium, skill, fill = dir, text = tip)) +
      geom_col(show.legend = FALSE, alpha = 0.85, width = 0.65) +
      geom_vline(xintercept = 0, linetype = "dashed",
                 color = pal$text3) +
      geom_text(
        aes(label = paste0(ifelse(premium > 0, "+", ""),
                           round(premium, 1), "K")),
        hjust = ifelse(d$premium >= 0, -0.15, 1.15),
        size = 4, fontface = "bold", color = pal$text1) +
      annotate("text",
               x     = best$premium[1] * 1.35,
               y     = as.numeric(best$skill[1]),
               label = paste0("Best to learn\n+$",
                              round(best$premium[1], 1), "K boost"),
               hjust = 0, size = 3.2,
               color = pal$positive, fontface = "bold") +
      scale_fill_manual(values = c("positive" = pal$positive,
                                   "negative" = pal$negative)) +
      scale_x_continuous(expand = expansion(mult = c(0.2, 0.45))) +
      labs(title    = "Salary Premium by Technical Skill",
           subtitle = "Green = salary boost  ·  Red = salary penalty",
           x = "Salary Difference vs Jobs Without That Skill (K USD)",
           y = NULL) +
      th
    to_plotly(p, 320)
  })

  output$p_skill_role <- renderPlotly({
    pal <- PAL(); th <- TH_R()
    d <- GDF()
    validate(need(nrow(d) > 0, "No data."))
    plot_d <- d %>%
      group_by(role) %>%
      summarise(across(all_of(SKILL_COLS),
                       \(x) mean(x, na.rm = TRUE) * 100),
                .groups = "drop") %>%
      pivot_longer(all_of(SKILL_COLS),
                   names_to = "col", values_to = "pct") %>%
      mutate(skill = SKILL_NAMES[match(col, SKILL_COLS)],
             tip   = paste0("<b>", skill, " — ", role, "</b><br>",
                            "Required in: ", round(pct), "% of postings"))
    p <- ggplot(plot_d, aes(skill, pct, fill = pct, text = tip)) +
      geom_col(alpha = 0.88, width = 0.72, show.legend = FALSE) +
      geom_text(aes(label = paste0(round(pct), "%")),
                vjust = -0.4, size = 2.9, fontface = "bold",
                color = pal$text1) +
      facet_wrap(~role, ncol = 3) +
      scale_fill_gradient(low = pal$primary3, high = pal$primary) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.22)),
                         limits = c(0, 100)) +
      labs(title    = "Skill Demand by Job Role",
           subtitle = "% of postings in each role requiring this skill",
           x = NULL, y = "% of Postings") +
      th + theme(axis.text.x = element_text(angle = 35, hjust = 1,
                                             size = 9, color = pal$text3))
    to_plotly(p, 370)
  })

  output$p_cooccur <- renderPlotly({
    pal <- PAL(); th <- TH_R()
    d <- GDF()
    validate(need(nrow(d) >= 10, "Not enough data."))
    co <- matrix(0L, 5, 5, dimnames = list(SKILL_NAMES, SKILL_NAMES))
    for (i in 1:5) for (j in 1:5)
      co[i,j] <- round(mean(d[[SKILL_COLS[i]]] == 1 &
                              d[[SKILL_COLS[j]]] == 1, na.rm = TRUE) * 100)
    co_df <- as.data.frame(as.table(co)) %>%
      rename(S1 = Var1, S2 = Var2, pct = Freq) %>%
      mutate(is_max = pct == max(pct[S1 != S2]),
             tip    = paste0("<b>", S1, " + ", S2, "</b><br>",
                             "Co-occur in: ", pct, "% of postings"))
    p <- ggplot(co_df, aes(S1, S2, fill = pct, text = tip)) +
      geom_tile(color = pal$bg, linewidth = 1.2) +
      geom_tile(data = filter(co_df, is_max & S1 != S2),
                aes(S1, S2), fill = NA,
                color = pal$accent, linewidth = 2) +
      geom_text(aes(label = paste0(pct, "%")), size = 4.5, fontface = "bold",
                color = ifelse(co_df$pct > 50, "white", pal$text1)) +
      scale_fill_gradient(low  = pal$bg2, high = pal$primary,
                          name = "% co-occurring", limits = c(0, 100)) +
      labs(title    = "Skill Co-occurrence Matrix",
           subtitle = "Darker = more often listed together  ·  Orange/Amber border = most common pair",
           x = NULL, y = NULL) +
      th + theme(legend.position = "right",
                 axis.text = element_text(face = "bold", size = 11,
                                          color = pal$text2))
    to_plotly(p, 370)
  })

  # ════════════════════════════════════════════════════════
  #  COMPANY
  # ════════════════════════════════════════════════════════

  output$p_co_size <- renderPlotly({
    pal <- PAL(); th <- TH_R()
    d <- CDF() %>% filter(!is.na(Size)) %>%
      group_by(Size) %>%
      summarise(med = median(avg_salary, na.rm = TRUE),
                n = n(), .groups = "drop") %>%
      mutate(is_peak = med == max(med),
             tip     = paste0("<b>", Size, "</b><br>",
                              "Median salary: $", round(med), "K<br>",
                              "Postings: ", n))
    validate(need(nrow(d) > 0, "No data."))
    p <- ggplot(d, aes(Size, med, text = tip,
                       fill = ifelse(is_peak, pal$accent, pal$bg3))) +
      geom_col(show.legend = FALSE, alpha = 0.88, width = 0.72) +
      geom_text(aes(label = paste0("$", round(med), "K\n(n=", n, ")")),
                vjust = -0.3, size = 3.1, color = pal$text1) +
      geom_text(data = filter(d, is_peak),
                aes(y = med / 2, label = "Sweet spot", text = NULL),
                hjust = 0.5, size = 3.2,
                color = pal$bg, fontface = "bold") +
      scale_fill_identity() +
      scale_y_continuous(expand = expansion(mult = c(0, .22)),
                         labels = dollar_format(suffix = "K", prefix = "$")) +
      labs(title    = "Median Salary by Company Size",
           subtitle = "Highlighted bar = size group with highest median pay",
           x = NULL, y = "Median Salary (K USD)") +
      th + theme(axis.text.x = element_text(angle = 30, hjust = 1,
                                             size = 9, color = pal$text3))
    to_plotly(p, 330)
  })

  output$p_co_own <- renderPlotly({
    pal <- PAL(); th <- TH_R()
    d <- CDF() %>% filter(!is.na(Type.of.ownership)) %>%
      group_by(Type.of.ownership) %>%
      summarise(med = median(avg_salary, na.rm = TRUE),
                n = n(), .groups = "drop") %>%
      filter(n >= 5) %>% arrange(med) %>%
      mutate(Type.of.ownership = factor(Type.of.ownership,
                                        levels = Type.of.ownership),
             is_top = med == max(med),
             tip    = paste0("<b>", Type.of.ownership, "</b><br>",
                             "Median salary: $", round(med), "K<br>",
                             "Postings: ", n))
    validate(need(nrow(d) > 0, "No data."))
    p <- ggplot(d, aes(med, Type.of.ownership, fill = med, text = tip)) +
      geom_col(show.legend = FALSE, alpha = 0.88, width = 0.7) +
      geom_text(aes(label = paste0("$", round(med), "K  (n=", n, ")")),
                hjust = -0.1, size = 3.4, color = pal$text1) +
      geom_text(data = filter(d, is_top),
                aes(x = med / 2, label = "Highest paying", text = NULL),
                hjust = 0.5, size = 3,
                color = "white", fontface = "bold") +
      scale_fill_gradient(low = pal$primary3, high = pal$primary) +
      scale_x_continuous(expand = expansion(mult = c(0, .28))) +
      labs(title    = "Median Salary by Ownership Type",
           subtitle = "Only ownership types with 5+ postings shown",
           x = "Median Salary (K USD)", y = NULL) +
      th
    to_plotly(p, 330)
  })

  output$p_rating <- renderPlotly({
    pal <- PAL(); th <- TH_R()
    d <- CDF() %>% filter(!is.na(Rating)) %>%
      mutate(tip = paste0("<b>", Company.Name, "</b><br>",
                          "Rating: ", Rating, "\u2605<br>",
                          "Salary: $", round(avg_salary), "K<br>",
                          "Role: ", role))
    validate(need(nrow(d) > 5, "Not enough data."))
    avg_rating <- round(mean(d$Rating, na.rm = TRUE), 1)
    fit        <- lm(avg_salary ~ Rating, data = d)
    slope      <- round(coef(fit)[2], 1)
    slope_txt  <- paste0("Each extra \u2605 \u2248 ",
                         ifelse(slope >= 0, "+", ""), slope, "K salary")
    p <- ggplot(d, aes(Rating, avg_salary, text = tip)) +
      geom_point(color = pal$primary, alpha = 0.3, size = 2) +
      geom_smooth(method = "lm", se = TRUE,
                  color = pal$accent,
                  fill  = paste0(pal$accent, "33"),
                  linewidth = 1.1) +
      geom_vline(xintercept = avg_rating, linetype = "dashed",
                 color = pal$text3, linewidth = 0.7) +
      annotate("text", x = avg_rating, y = Inf,
               label = paste0(" Avg: ", avg_rating, "\u2605"),
               hjust = 0, vjust = 1.6, size = 3.2, color = pal$text2) +
      annotate("text", x = 4.2,
               y = min(d$avg_salary, na.rm = TRUE) * 1.05,
               label = slope_txt, size = 3.5, fontface = "bold",
               color = pal$accent, hjust = 0) +
      scale_y_continuous(labels = dollar_format(suffix = "K", prefix = "$")) +
      labs(title    = "Company Rating vs Average Salary",
           subtitle = "Trend line shows overall relationship  ·  Shaded band = 95% CI",
           x = "Glassdoor Rating (1–5 stars)", y = "Avg Salary (K USD)") +
      th
    to_plotly(p, 330)
  })

  output$p_age_sal <- renderPlotly({
    pal <- PAL(); th <- TH_R()
    d <- CDF() %>% filter(!is.na(age_group)) %>%
      group_by(age_group) %>%
      summarise(med = median(avg_salary, na.rm = TRUE),
                n = n(), .groups = "drop") %>%
      mutate(is_peak = med == max(med),
             tip     = paste0("<b>", age_group, "</b><br>",
                              "Median salary: $", round(med), "K<br>",
                              "Postings: ", n))
    validate(need(nrow(d) > 0, "No data."))
    p <- ggplot(d, aes(age_group, med, text = tip,
                       fill = ifelse(is_peak, pal$accent, pal$bg3))) +
      geom_col(show.legend = FALSE, alpha = 0.88, width = 0.7) +
      geom_text(aes(label = paste0("$", round(med), "K\n(n=", n, ")")),
                vjust = -0.3, size = 3.1, color = pal$text1) +
      geom_text(data = filter(d, is_peak),
                aes(y = med / 2, label = "Highest paying", text = NULL),
                hjust = 0.5, size = 3.1,
                color = pal$bg, fontface = "bold") +
      scale_fill_identity() +
      scale_y_continuous(expand = expansion(mult = c(0, .22)),
                         labels = dollar_format(suffix = "K", prefix = "$")) +
      labs(title    = "Median Salary by Company Age",
           subtitle = "Startups vs legacy firms",
           x = NULL, y = "Median Salary (K USD)") +
      th + theme(axis.text.x = element_text(angle = 20, hjust = 1,
                                             color = pal$text3))
    to_plotly(p, 330)
  })

  # ════════════════════════════════════════════════════════
  #  PROFILE BUILDER
  # ════════════════════════════════════════════════════════

  PD <- eventReactive(input$pr_go, {
    d <- df
    if (input$pr_role   != "Any") d <- d %>% filter(role      == input$pr_role)
    if (input$pr_state  != "Any") d <- d %>% filter(job_state == input$pr_state)
    if (input$pr_senior != "Any") d <- d %>% filter(seniority == input$pr_senior)
    for (s in input$pr_skills) d <- d %>% filter(.data[[s]] == 1)
    d
  }, ignoreNULL = FALSE)

  output$pr_n     <- renderText(scales::comma(nrow(PD())))
  output$pr_avg   <- renderText({
    v <- mean(PD()$avg_salary, na.rm = TRUE)
    if (is.nan(v)) return("N/A")
    paste0("$", round(v, 1), "K")
  })
  output$pr_range <- renderText({
    d <- PD(); if (nrow(d) == 0) return("N/A")
    paste0("$", round(median(d$min_salary, na.rm = TRUE)),
           "K – $", round(median(d$max_salary, na.rm = TRUE)), "K")
  })

  output$pr_dist <- renderPlotly({
    pal <- PAL(); th <- TH_R()
    d <- PD()
    validate(need(nrow(d) >= 5,
                  "Not enough matches — try broadening your filters."))
    avg_v <- mean(d$avg_salary, na.rm = TRUE)
    d2    <- d %>% mutate(tip = paste0("Salary: $", round(avg_salary), "K"))
    p <- ggplot(d2, aes(avg_salary, text = tip)) +
      geom_histogram(bins = 20, fill = pal$primary,
                     color = pal$bg, alpha = 0.85) +
      geom_vline(xintercept = avg_v, color = pal$accent,
                 linetype = "dashed", linewidth = 1.1) +
      annotate("text", x = avg_v, y = Inf,
               label = paste0(" Avg: $", round(avg_v, 1), "K"),
               hjust = -0.05, vjust = 1.8,
               color = pal$accent, fontface = "bold") +
      scale_x_continuous(labels = dollar_format(suffix = "K", prefix = "$")) +
      labs(title    = "Your Expected Salary Distribution",
           subtitle = paste0(nrow(d), " job postings match your profile"),
           x = "Avg Salary (K USD)", y = "Number of Jobs") +
      th
    to_plotly(p, 270)
  })

  output$pr_sector <- renderPlotly({
    pal <- PAL(); th <- TH_R()
    d <- PD() %>% filter(!is.na(Sector)) %>%
      count(Sector) %>% slice_max(n, n = 8) %>%
      arrange(n) %>%
      mutate(Sector = factor(Sector, levels = Sector),
             tip    = paste0("<b>", Sector, "</b><br>",
                             "Matching postings: ", n))
    validate(need(nrow(d) > 0, "No sector data for this profile."))
    p <- ggplot(d, aes(n, Sector, fill = n, text = tip)) +
      geom_col(show.legend = FALSE, alpha = 0.88, width = 0.7) +
      geom_text(aes(label = n), hjust = -0.2, size = 3.4,
                color = pal$text1) +
      scale_fill_gradient(low = pal$primary3, high = pal$primary) +
      scale_x_continuous(expand = expansion(mult = c(0, .2))) +
      labs(title = "Top Sectors Actively Hiring for Your Profile",
           x = "Matching Job Count", y = NULL) +
      th
    to_plotly(p, 270)
  })

  output$pr_gap <- renderPlotly({
    pal <- PAL(); th <- TH_R()
    d          <- PD()
    has        <- input$pr_skills
    miss_cols  <- SKILL_COLS[!SKILL_COLS %in% has]
    miss_names <- SKILL_NAMES[!SKILL_COLS %in% has]

    if (length(miss_cols) == 0) {
      p <- ggplot() +
        annotate("text", x = .5, y = .5, hjust = .5, vjust = .5, size = 5.5,
                 color = pal$positive, fontface = "bold",
                 label = "You have all 5 tracked skills!\nYou're in the top tier of candidates.") +
        theme_void() +
        theme(plot.background = element_rect(fill = pal$bg, color = NA))
      return(ggplotly(p, height = 230) %>% config(displayModeBar = FALSE))
    }

    my_avg <- mean(d$avg_salary, na.rm = TRUE)
    gap <- tibble(
      skill = miss_names,
      gain  = sapply(miss_cols, \(s)
        mean(df$avg_salary[df[[s]] == 1], na.rm = TRUE) - my_avg)
    ) %>%
      arrange(desc(gain)) %>%
      mutate(tip = paste0("<b>", skill, "</b><br>",
                          "Potential gain: ",
                          ifelse(gain > 0, "+", ""),
                          round(gain, 1), "K"))

    p <- ggplot(gap, aes(reorder(skill, gain), gain,
                          fill = gain > 0, text = tip)) +
      geom_col(show.legend = FALSE, alpha = 0.88, width = 0.65) +
      geom_text(aes(label = paste0(ifelse(gain > 0, "+", ""),
                                    round(gain, 1), "K")),
                hjust = ifelse(gap$gain >= 0, -0.15, 1.15),
                size = 4, fontface = "bold", color = pal$text1) +
      coord_flip() +
      geom_hline(yintercept = 0, linetype = "dashed",
                 color = pal$text3) +
      scale_fill_manual(values = c("TRUE"  = pal$positive,
                                   "FALSE" = pal$negative)) +
      labs(title    = "Skill Gap — Estimated Salary Impact",
           subtitle = "Compared to your current estimated average",
           x = NULL, y = "Potential Salary Gain (K USD)") +
      th

    to_plotly(p, 230)
  })
}

shinyApp(ui, server)