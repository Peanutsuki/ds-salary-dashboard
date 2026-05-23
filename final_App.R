# =============================================================
#  DS Salary Analytics — Glassdoor Jobs (thedevastator, Kaggle 2022)
#  R · Shiny · bslib · ggplot2 · plotly · dplyr · tidyr
#  Navigation: left sidebar drawer (collapsed by default)
# =============================================================
library(shiny); library(bslib); library(bsicons)
library(ggplot2); library(plotly); library(dplyr)
library(stringr); library(tidyr); library(scales)

# ── 1. LOAD & CLEAN ──────────────────────────────────────────
raw <- read.csv("salary_data_cleaned.csv", stringsAsFactors = FALSE)
df <- raw %>% mutate(
  job_state  = str_trim(job_state),
  across(c(avg_salary, min_salary, max_salary, Rating, age), as.numeric),
  Rating = ifelse(Rating < 0, NA, Rating),
  age    = ifelse(age    < 0, NA, age),
  Sector = ifelse(Sector == "-1", NA, Sector),
  Size   = ifelse(Size %in% c("-1","Unknown"), NA, Size),
  Type.of.ownership = ifelse(Type.of.ownership %in% c("-1","Unknown"), NA, Type.of.ownership),
  sal_spread = max_salary - min_salary,
  role = case_when(
    str_detect(tolower(Job.Title), "data scientist")          ~ "Data Scientist",
    str_detect(tolower(Job.Title), "data engineer")           ~ "Data Engineer",
    str_detect(tolower(Job.Title), "data analyst")            ~ "Data Analyst",
    str_detect(tolower(Job.Title), "machine learning|ml eng") ~ "ML Engineer",
    str_detect(tolower(Job.Title), "director|head|vp|chief")  ~ "Director/VP",
    str_detect(tolower(Job.Title), "manager")                 ~ "Manager",
    TRUE ~ "Other"),
  seniority = case_when(
    str_detect(tolower(Job.Title), "senior|sr\\.|lead|principal|staff") ~ "Senior",
    str_detect(tolower(Job.Title), "junior|jr\\.|associate|entry")       ~ "Junior",
    str_detect(tolower(Job.Title), "director|vp|head|chief")             ~ "Executive",
    TRUE ~ "Mid-Level"),
  Size = factor(Size, levels = c(
    "1 to 50 employees","51 to 200 employees","201 to 500 employees",
    "501 to 1000 employees","1001 to 5000 employees",
    "5001 to 10000 employees","10000+ employees")),
  age_group = factor(case_when(
    is.na(age) ~ NA_character_, age <= 5  ~ "Startup (0-5 yrs)",
    age <= 15  ~ "Young (6-15 yrs)",      age <= 30 ~ "Established (16-30 yrs)",
    age <= 60  ~ "Mature (31-60 yrs)",    TRUE      ~ "Legacy (60+ yrs)"),
    levels = c("Startup (0-5 yrs)","Young (6-15 yrs)","Established (16-30 yrs)",
               "Mature (31-60 yrs)","Legacy (60+ yrs)"))
) %>% filter(!is.na(avg_salary), avg_salary > 10)

# ── 2. CONSTANTS ─────────────────────────────────────────────
SC  <- c("python_yn","R_yn","spark","aws","excel")
SN  <- c("Python","R","Spark","AWS","Excel")
ROLES   <- c("All", sort(unique(df$role)))
SECTORS <- c("All", sort(na.omit(unique(df$Sector))))
SENIORS <- c("All","Junior","Mid-Level","Senior","Executive")
SIZES   <- c("All", levels(df$Size))
OWNS    <- c("All", sort(na.omit(unique(df$Type.of.ownership))))
STATES  <- sort(unique(df$job_state))

# ── Blue-Purple Palette (Light & Dark) ───────────────────────
PL <- list(
  bg      = "#F4F6FB",
  bg2     = "#E8EEFF",
  bg3     = "#D0D8F5",
  primary = "#3B4FCC",
  primary2= "#5A72E0",
  primary3= "#8B9EEE",
  accent  = "#9B5DE5",
  accent2 = "#C084FC",
  positive= "#059669",
  negative= "#DC2626",
  text1   = "#1A1F3A",
  text2   = "#3A4580",
  text3   = "#6B78C4",
  grid    = "#DDE3F5",
  hover_bg= "#FFFFFF",
  hover_border="#3B4FCC",
  hover_text="#1A1F3A",
  card_bg = "#FFFFFF",
  title_gradient="linear-gradient(135deg, #3B4FCC 0%, #9B5DE5 100%)"
)
PD <- list(
  bg      = "#0F1128",
  bg2     = "#181B3A",
  bg3     = "#252A55",
  primary = "#6B82F5",
  primary2= "#8B9EF5",
  primary3= "#4A5FD4",
  accent  = "#B87EFF",
  accent2 = "#D4AAFF",
  positive= "#34D399",
  negative= "#F87171",
  text1   = "#EEF0FF",
  text2   = "#A0AAEE",
  text3   = "#7080CC",
  grid    = "#252A55",
  hover_bg= "#181B3A",
  hover_border="#6B82F5",
  hover_text="#EEF0FF",
  card_bg = "#181B3A",
  title_gradient="linear-gradient(135deg, #6B82F5 0%, #B87EFF 100%)"
)

CL <- c("#3B4FCC","#9B5DE5","#059669","#6B82F5","#C084FC","#34D399","#F59E0B","#DC2626","#06B6D4")
CD <- c("#6B82F5","#B87EFF","#34D399","#8B9EF5","#D4AAFF","#059669","#FBBF24","#F87171","#22D3EE")

# ── Unified plot theme ────────────────────────────────────────
mth <- function(p) theme_minimal(base_size = 11) + theme(
  plot.background    = element_rect(fill = p$card_bg, color = NA),
  panel.background   = element_rect(fill = p$card_bg, color = NA),
  text               = element_text(color = p$text1, family = "Plus Jakarta Sans", size = 11),
  axis.text          = element_text(color = p$text3, size = 9, family = "Plus Jakarta Sans"),
  axis.title         = element_text(color = p$text2, size = 10, face = "bold", family = "Plus Jakarta Sans"),
  axis.text.x        = element_text(size = 9, color = p$text3, family = "Plus Jakarta Sans"),
  axis.text.y        = element_text(size = 9, color = p$text3, family = "Plus Jakarta Sans"),
  panel.grid.major   = element_line(color = p$grid, linewidth = 0.3),
  panel.grid.minor   = element_blank(),
  plot.title         = element_text(face = "bold", size = 12, color = p$primary,
                                    margin = margin(b = 3), family = "Plus Jakarta Sans",
                                    hjust = 0.5),
  plot.subtitle      = element_text(size = 9, color = p$text3, margin = margin(b = 6),
                                    family = "Plus Jakarta Sans", hjust = 0.5),
  legend.background  = element_rect(fill = p$card_bg, color = NA),
  legend.text        = element_text(color = p$text3, size = 9, family = "Plus Jakarta Sans"),
  legend.title       = element_text(face = "bold", color = p$text2, size = 10, family = "Plus Jakarta Sans"),
  legend.position    = "bottom",
  legend.key.size    = unit(0.8, "lines"),
  strip.background   = element_rect(fill = p$bg2, color = NA),
  strip.text         = element_text(face = "bold", size = 9, color = p$primary,
                                    family = "Plus Jakarta Sans"),
  plot.margin        = margin(10, 12, 10, 12)
)

# ── 3. CSS ────────────────────────────────────────────────────
extra_css <- "
@import url('https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400;500;600;700;800&family=JetBrains+Mono:wght@400;600&display=swap');

*, *::before, *::after { box-sizing: border-box; }
body, html {
  margin: 0; padding: 0; height: 100%; overflow-x: hidden;
  font-family: 'Plus Jakarta Sans', sans-serif !important;
  background: #F4F6FB;
}
[data-bs-theme=dark] body, [data-bs-theme=dark] html { background: #0F1128; }

/* ── Top Navbar ── */
.app-navbar {
  position: fixed; top: 0; left: 0; right: 0; height: 56px;
  background: linear-gradient(135deg, #1E2575 0%, #5B21B6 100%);
  z-index: 1000; display: flex; align-items: center;
  padding: 0 16px; gap: 12px;
  box-shadow: 0 2px 20px rgba(59,79,204,.45);
}
[data-bs-theme=dark] .app-navbar {
  background: linear-gradient(135deg, #0D1240 0%, #3B1A6E 100%);
}
.brand-icon-wrap {
  width: 34px; height: 34px; border-radius: 9px;
  background: rgba(255,255,255,.18); display: flex;
  align-items: center; justify-content: center;
  backdrop-filter: blur(4px);
}
.brand-icon-wrap i { font-size: 17px; color: #E0D4FF; }
.brand-title {
  font-size: 15px; font-weight: 800; color: #FFFFFF;
  letter-spacing: 0.01em; flex: 1;
  text-shadow: 0 1px 4px rgba(0,0,0,.3);
  font-family: 'Plus Jakarta Sans', sans-serif;
}
.brand-subtitle {
  font-size: 10px; color: rgba(255,255,255,.6); font-weight: 500;
  font-family: 'Plus Jakarta Sans', sans-serif;
}

/* ── Left Sidebar ── */
#left-sidebar {
  position: fixed; top: 56px; left: 0;
  width: 230px; height: calc(100vh - 56px);
  background: linear-gradient(180deg, #FFFFFF 0%, #F0F3FF 100%);
  z-index: 990; display: flex; flex-direction: column;
  transform: translateX(-230px);
  transition: transform .24s cubic-bezier(.4,0,.2,1);
  box-shadow: 4px 0 30px rgba(59,79,204,.12);
  border-right: 1px solid rgba(59,79,204,.1);
}
#left-sidebar.open { transform: translateX(0); }
[data-bs-theme=dark] #left-sidebar {
  background: linear-gradient(180deg, #181B3A 0%, #0F1128 100%);
  box-shadow: 4px 0 30px rgba(0,0,0,.4);
  border-right: 1px solid rgba(107,130,245,.15);
}

/* ── Main content area ── */
#main-content {
  margin-left: 0;
  margin-top: 56px;
  transition: margin-left .24s cubic-bezier(.4,0,.2,1);
  min-height: unset;
  height: auto;
  background: #F4F6FB;
  padding-bottom: 32px;
}
[data-bs-theme=dark] #main-content { background: #0F1128; }
#main-content.sidebar-open { margin-left: 230px; }

/* ── Sidebar internals ── */
.sidebar-brand-name {
  font-size: 13px; font-weight: 800; color: #1A1F3A;
  line-height: 1.2; font-family: 'Plus Jakarta Sans', sans-serif;
}
[data-bs-theme=dark] .sidebar-brand-name { color: #EEF0FF; }

.sidebar-brand-sub {
  font-size: 10px; color: #6B78C4; font-weight: 500;
  font-family: 'Plus Jakarta Sans', sans-serif;
}
[data-bs-theme=dark] .sidebar-brand-sub { color: #4A5580; }

.sidebar-section-label {
  font-size: 9.5px; font-weight: 700; text-transform: uppercase;
  letter-spacing: 0.13em; color: #9BA8CC;
  padding: 12px 16px 5px;
  font-family: 'Plus Jakarta Sans', sans-serif;
}
[data-bs-theme=dark] .sidebar-section-label { color: #4A5580; }

.sidebar-nav {
  flex: 1; padding: 6px 10px;
  display: flex; flex-direction: column; gap: 2px; overflow-y: auto;
}
.snav-item {
  display: flex; align-items: center; gap: 10px;
  padding: 10px 12px; border-radius: 10px; cursor: pointer;
  color: #3A4580; font-size: 13px; font-weight: 500;
  border: 1px solid transparent; white-space: nowrap;
  transition: all .15s ease; user-select: none;
  font-family: 'Plus Jakarta Sans', sans-serif;
}
[data-bs-theme=dark] .snav-item { color: #A0AAEE; }
.snav-item:hover {
  background: rgba(59,79,204,.08);
  color: #1A1F3A; border-color: rgba(59,79,204,.15);
}
[data-bs-theme=dark] .snav-item:hover {
  background: rgba(107,130,245,.12);
  color: #EEF0FF; border-color: rgba(107,130,245,.2);
}
.snav-item.active {
  background: linear-gradient(135deg, rgba(59,79,204,.12), rgba(155,93,229,.08));
  border-color: rgba(59,79,204,.25);
  color: #1A1F3A; font-weight: 700;
  box-shadow: 0 2px 12px rgba(59,79,204,.1);
}
[data-bs-theme=dark] .snav-item.active {
  background: linear-gradient(135deg, rgba(107,130,245,.18), rgba(184,126,255,.1));
  border-color: rgba(107,130,245,.3); color: #EEF0FF;
}
.snav-icon { font-size: 16px; width: 22px; text-align: center; flex-shrink: 0; color: #7B8ED0; }
[data-bs-theme=dark] .snav-icon { color: #4A5580; }
.snav-item.active .snav-icon { color: #3B4FCC; }
.snav-item:hover .snav-icon { color: #3B4FCC; }
[data-bs-theme=dark] .snav-item.active .snav-icon,
[data-bs-theme=dark] .snav-item:hover .snav-icon { color: #8B9EF5; }
.snav-divider { height: 1px; background: rgba(59,79,204,.1); margin: 6px 10px; }
[data-bs-theme=dark] .snav-divider { background: rgba(107,130,245,.15); }
.sidebar-bottom {
  padding: 8px 10px 14px;
  border-top: 1px solid rgba(59,79,204,.1);
}
[data-bs-theme=dark] .sidebar-bottom { border-top-color: rgba(107,130,245,.15); }
.sidebar-bottom .snav-item { color: #9BA8CC; }

/* ── Overlay ── */
#sidebar-overlay {
  display: none; position: fixed; inset: 0; top: 56px;
  z-index: 989; background: rgba(0,0,0,.5);
  backdrop-filter: blur(2px);
}
#sidebar-overlay.show { display: block; }

/* ── Tab toolbar (hamburger + title row) ── */
.tab-toolbar {
  display: flex; align-items: center;
  padding: 14px 20px 2px; gap: 12px;
}
#nav-hamburger {
  background: transparent;
  border: none;
  cursor: pointer; color: #3B4FCC; font-size: 22px;
  padding: 4px 6px; border-radius: 6px; line-height: 1; flex-shrink: 0;
  transition: color .15s;
}
#nav-hamburger:hover { background: transparent; color: #1A1F3A; }
[data-bs-theme=dark] #nav-hamburger { background: transparent; border: none; color: #8B9EF5; }
[data-bs-theme=dark] #nav-hamburger:hover { background: transparent; color: #EEF0FF; }
.tab-toolbar h4 {
  margin: 0; font-size: 19px; font-weight: 800; color: #1A1F3A;
  line-height: 1.2; font-family: 'Plus Jakarta Sans', sans-serif;
}
[data-bs-theme=dark] .tab-toolbar h4 { color: #EEF0FF; }
.page-subtitle {
  font-size: 12px; color: #6B78C4; margin-top: 2px; font-weight: 500;
  font-family: 'Plus Jakarta Sans', sans-serif;
}

/* ── Value boxes ── */
.bslib-value-box .value-box-title {
  font-size: 12px !important; font-weight: 700 !important;
  font-family: 'Plus Jakarta Sans', sans-serif !important;
}
.bslib-value-box .value-box-value {
  font-size: 22px !important; font-weight: 800 !important;
  font-family: 'Plus Jakarta Sans', sans-serif !important;
}
.bslib-value-box { border-radius: 14px !important; }

/* ── Cards ── */
.card {
  border: 1px solid rgba(107,130,245,.15) !important;
  border-radius: 14px !important;
  box-shadow: 0 2px 12px rgba(59,79,204,.08) !important;
  margin-bottom: 0 !important;
}
.card-header {
  font-size: 13px !important; font-weight: 700 !important;
  color: #3B4FCC !important; letter-spacing: 0.01em;
  background: linear-gradient(135deg, #F0F3FF, #F4F0FF) !important;
  border-bottom: 1px solid rgba(107,130,245,.15) !important;
  border-radius: 14px 14px 0 0 !important;
  padding: 10px 16px !important;
  display: flex; align-items: center; gap: 7px;
  font-family: 'Plus Jakarta Sans', sans-serif !important;
}
.card-body { padding: 12px 14px !important; }
[data-bs-theme=dark] .card { background: #181B3A !important; border-color: rgba(107,130,245,.2) !important; }
[data-bs-theme=dark] .card-header {
  background: linear-gradient(135deg, #1E2255, #251A45) !important;
  color: #8B9EF5 !important; border-bottom-color: rgba(107,130,245,.2) !important;
}

/* ── Fix bottom dead space ── */
.bslib-sidebar-layout {
  min-height: unset !important; height: auto !important; flex: none !important;
}
.bslib-sidebar-layout > .main {
  min-height: unset !important; height: auto !important;
  overflow-y: visible !important; flex: none !important;
}
.tab-content {
  height: auto !important; min-height: unset !important; overflow: visible !important;
}
.tab-pane {
  height: auto !important; min-height: unset !important; overflow: visible !important;
}
.bslib-navs-hidden { height: auto !important; min-height: unset !important; flex: none !important; }
.bslib-gap-spacing { height: auto !important; }
.shiny-bound-output { height: auto !important; }
.container-fluid { height: auto !important; min-height: unset !important; }
html, body { height: auto !important; min-height: unset !important; overflow-y: auto !important; }

/* ── Dark mode toggle ── */
.dark-mode-wrap { margin-left: auto; display: flex; align-items: center; }

/* ── Sidebar filter text ── */
.sidebar p, .sidebar .text-muted {
  font-size: 11.5px !important; font-family: 'Plus Jakarta Sans', sans-serif !important;
}
.sidebar .form-label {
  font-size: 12px !important; font-weight: 600 !important;
  font-family: 'Plus Jakarta Sans', sans-serif !important;
}

/* ── Plotly ── */
.js-plotly-plot .plotly { border-radius: 8px; overflow: hidden; }

/* ── Scrollbar ── */
::-webkit-scrollbar { width: 5px; height: 5px; }
::-webkit-scrollbar-track { background: transparent; }
::-webkit-scrollbar-thumb { background: rgba(107,130,245,.3); border-radius: 10px; }
::-webkit-scrollbar-thumb:hover { background: rgba(107,130,245,.6); }

/* ── About tab ── */
.card-body h2 { font-size: 16px !important; font-weight: 800; color: #3B4FCC; margin-top: 16px; font-family: 'Plus Jakarta Sans', sans-serif; }
[data-bs-theme=dark] .card-body h2 { color: #8B9EF5; }
.card-body table { font-size: 13px !important; font-family: 'Plus Jakarta Sans', sans-serif !important; }
"

# ── 4. JavaScript ─────────────────────────────────────────────
sidebar_js <- "
$(function() {
  $(document).on('click', '#nav-hamburger', function() {
    var isOpen = $('#left-sidebar').hasClass('open');
    if (isOpen) {
      $('#left-sidebar').removeClass('open');
      $('#main-content').removeClass('sidebar-open');
      $('#sidebar-overlay').removeClass('show');
    } else {
      $('#left-sidebar').addClass('open');
      $('#main-content').addClass('sidebar-open');
      if ($(window).width() < 768) {
        $('#sidebar-overlay').addClass('show');
      }
    }
  });

  $('#sidebar-overlay').on('click', function() {
    $('#left-sidebar').removeClass('open');
    $('#main-content').removeClass('sidebar-open');
    $(this).removeClass('show');
  });

  $(document).on('click', '.snav-item[data-tab]', function() {
    var tab = $(this).data('tab');
    Shiny.setInputValue('active_tab', tab, {priority: 'event'});
    $('.snav-item').removeClass('active');
    $(this).addClass('active');
    if ($(window).width() < 992) {
      $('#left-sidebar').removeClass('open');
      $('#main-content').removeClass('sidebar-open');
      $('#sidebar-overlay').removeClass('show');
    }
  });
});
"

# ── 5. UI ─────────────────────────────────────────────────────
nav_item <- function(tab, icon_class, label) {
  div(class = "snav-item", `data-tab` = tab,
    span(class = paste("snav-icon bi", icon_class)),
    label
  )
}

tab_toolbar <- function(title_text, subtitle_text) {
  div(class = "tab-toolbar",
    tags$button(id = "nav-hamburger", `aria-label` = "Toggle sidebar",
  style = "background:transparent; border:none; cursor:pointer; color:#3B4FCC; font-size:22px; padding:4px 6px; line-height:1; flex-shrink:0;",
  tags$i(class = "bi bi-list")
),
    div(
      h4(title_text),
      div(class = "page-subtitle", subtitle_text)
    )
  )
}

logo_svg <- tags$svg(
  xmlns = "http://www.w3.org/2000/svg", viewBox = "0 0 32 32",
  width = "20", height = "20", fill = "none",
  tags$path(d="M4 26 Q10 8 16 14 Q22 20 28 6",
            stroke="white", `stroke-width`="2.5",
            `stroke-linecap`="round", `stroke-linejoin`="round", fill="none"),
  tags$circle(cx="16", cy="14", r="2.5", fill="white"),
  tags$circle(cx="28", cy="6",  r="2.5", fill="white", `fill-opacity`="0.7"),
  tags$circle(cx="4",  cy="26", r="2.5", fill="white", `fill-opacity`="0.7")
)

ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "flatly",
    bg = "#F4F6FB", fg = "#1A1F3A",
    primary = "#3B4FCC", success = "#059669",
    warning = "#F59E0B", danger = "#DC2626",
    font_scale = 0.95
  ) %>%
    bs_add_rules("
      [data-bs-theme=dark]{
        --bs-body-bg:#0F1128; --bs-body-color:#EEF0FF;
        --bs-primary:#6B82F5; --bs-success:#34D399;
        --bs-warning:#FBBF24; --bs-danger:#F87171;
        --bs-card-bg:#181B3A; --bs-border-color:#252A55;
      }
    ") %>%
    bs_add_rules(extra_css),
      tags$head(
          tags$link(rel = "stylesheet",
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.css"),
      tags$script(HTML(sidebar_js))
  ),

  # ── Top Navbar ────────────────────────────────────────────
  div(class = "app-navbar",
    div(class = "brand-icon-wrap", logo_svg),
    div(style = "line-height:1.2;",
      div(class = "brand-title", "DS Salary Analytics"),
      div(class = "brand-subtitle", "Glassdoor · US Market")
    ),
    div(class = "dark-mode-wrap",
      input_dark_mode(id = "dark_mode", mode = "light")
    )
  ),

  # ── Left Sidebar ──────────────────────────────────────────
  div(id = "left-sidebar",
    div(class = "sidebar-nav",
      div(class = "sidebar-section-label", "Main"),
      nav_item("dashboard", "bi-speedometer2",     "Dashboard"),
      nav_item("location",  "bi-map-fill",          "Location"),
      nav_item("skills",    "bi-code-slash",         "Skills"),
      nav_item("company",   "bi-building-fill",      "Company"),
      div(class = "snav-divider"),
      div(class = "sidebar-section-label", "Tools"),
      nav_item("career",    "bi-person-badge-fill",  "Career Fit"),
      div(class = "snav-divider"),
      div(class = "sidebar-section-label", "Info"),
      nav_item("about",     "bi-info-circle-fill",   "About")
    )
  ),

  div(id = "sidebar-overlay"),

  # ── Main Content ─────────────────────────────────────────
  div(id = "main-content",
    navset_hidden(
      id = "main_panel",

      # ── DASHBOARD ──────────────────────────────────────
      nav_panel("dashboard",
        tab_toolbar("Dashboard", "Salary Landscape · Seniority distribution"),
        layout_sidebar(fillable = FALSE,
          sidebar = sidebar(position = "right", open = "closed", width = 260,
            title = tagList(bs_icon("sliders2-vertical"), " Filters"),
            p(class = "text-muted", style = "font-size:11px;", "Applied to Dashboard, Location & Skills."),
            sliderInput("f_sal", "Salary (K USD)", 10, 260, c(10, 260), 5, ticks = FALSE),
            selectInput("f_role",   "Role",      ROLES,   "All"),
            selectInput("f_sector", "Sector",    SECTORS, "All"),
            selectInput("f_senior", "Seniority", SENIORS, "All")),
          layout_columns(col_widths = c(3,3,3,3),
            value_box("Job Postings",        textOutput("kpi_n"),      bs_icon("briefcase-fill"),   theme = "primary"),
            value_box("Median Salary",        textOutput("kpi_med"),    bs_icon("currency-dollar"),  theme = "primary"),
            value_box("Salary IQR",           textOutput("kpi_iqr"),    bs_icon("arrows-expand"),    theme = "primary"),
            value_box("Avg Negotiation Room", textOutput("kpi_spread"), bs_icon("arrow-left-right"), theme = "primary")),
          br(),
          layout_columns(col_widths = c(8,4),
            card(card_header(tags$i(class="bi bi-bar-chart-steps"), " Salary Range by Job Role"),
                 card_body(plotlyOutput("p_dumbbell", height="370px"))),
            card(card_header(tags$i(class="bi bi-pie-chart-fill"), " Postings by Role"),
                 card_body(plotlyOutput("p_role_bar", height="370px")))),
          br(),
          layout_columns(col_widths = c(5,7),
            card(card_header(tags$i(class="bi bi-person-workspace"), " Seniority Distribution & Median Salary"),
                 card_body(plotlyOutput("p_sen_lollipop", height="290px"))),
            card(card_header(tags$i(class="bi bi-box-fill"), " Salary Distribution by Seniority"),
                 card_body(plotlyOutput("p_sen_box", height="290px"))))
        )
      ),

      # ── LOCATION ────────────────────────────────────────
      nav_panel("location",
        tab_toolbar("Location", "Job density & salary by US state · Local vs out-of-state pay"),
        layout_sidebar(fillable = FALSE,
          sidebar = sidebar(position = "right", open = "closed", width = 260,
            title = tagList(bs_icon("sliders2-vertical"), " Filters"),
            sliderInput("f_sal2", "Salary (K USD)", 10, 260, c(10, 260), 5, ticks = FALSE),
            selectInput("f_role2",   "Role",      ROLES,   "All"),
            selectInput("f_sector2", "Sector",    SECTORS, "All"),
            selectInput("f_senior2", "Seniority", SENIORS, "All")),
          layout_columns(col_widths = c(3,3,3,3),
            value_box("Top Paying State",  textOutput("geo_top"),    bs_icon("trophy-fill"),   theme = "primary"),
            value_box("Most Jobs In",       textOutput("geo_most"),   bs_icon("building"),      theme = "primary"),
            value_box("Local Avg Salary",   textOutput("geo_local"),  bs_icon("house-fill"),    theme = "primary"),
            value_box("Out-of-State Avg",   textOutput("geo_remote"), bs_icon("airplane-fill"), theme = "primary")),
          br(),
          layout_columns(col_widths = c(6,6),
            card(card_header(tags$i(class="bi bi-bar-chart-fill"), " Top 15 States by Job Volume"),
                 card_body(plotlyOutput("p_state_n", height="400px"))),
            card(card_header(tags$i(class="bi bi-graph-up-arrow"), " Top 15 States by Median Salary"),
                 card_body(plotlyOutput("p_state_sal", height="400px")))),
          br(),
          layout_columns(col_widths = c(5,7),
            card(card_header(tags$i(class="bi bi-box-fill"), " Local vs Out-of-State Salary"),
                 card_body(plotlyOutput("p_local_box", height="300px"))),
            card(card_header(tags$i(class="bi bi-bar-chart-steps"), " Local vs Out-of-State by Role"),
                 card_body(plotlyOutput("p_local_role", height="300px"))))
        )
      ),

      # ── SKILLS ──────────────────────────────────────────
      nav_panel("skills",
        tab_toolbar("Skills", "Skill demand · Salary premium · Co-occurrence patterns"),
        layout_sidebar(fillable = FALSE,
          sidebar = sidebar(position = "right", open = "closed", width = 260,
            title = tagList(bs_icon("sliders2-vertical"), " Filters"),
            sliderInput("f_sal3", "Salary (K USD)", 10, 260, c(10, 260), 5, ticks = FALSE),
            selectInput("f_role3",   "Role",      ROLES,   "All"),
            selectInput("f_sector3", "Sector",    SECTORS, "All"),
            selectInput("f_senior3", "Seniority", SENIORS, "All")),
          layout_columns(col_widths = c(3,3,3,3),
            value_box("Most In-Demand",       textOutput("sk_top_demand"),  bs_icon("code-slash"),              theme = "primary"),
            value_box("Biggest Salary Boost", textOutput("sk_top_premium"), bs_icon("lightning-charge-fill"),   theme = "primary"),
            value_box("Python Demand",         textOutput("sk_python"),      bs_icon("filetype-py"),             theme = "primary"),
            value_box("Excel Demand",          textOutput("sk_excel"),       bs_icon("file-earmark-excel-fill"), theme = "primary")),
          br(),
          layout_columns(col_widths = c(6,6),
            card(card_header(tags$i(class="bi bi-bar-chart-fill"), " Skill Demand Across All Postings"),
                 card_body(plotlyOutput("p_skill_demand", height="310px"))),
            card(card_header(tags$i(class="bi bi-lightning-charge-fill"), " Salary Premium by Technical Skill"),
                 card_body(plotlyOutput("p_skill_premium", height="310px")))),
          br(),
          layout_columns(col_widths = c(7,5),
            card(card_header(tags$i(class="bi bi-grid-3x3-gap-fill"), " Skill Demand by Job Role"),
                 card_body(plotlyOutput("p_skill_role", height="360px"))),
            card(card_header(tags$i(class="bi bi-intersect"), " Skill Co-occurrence Matrix"),
                 card_body(plotlyOutput("p_cooccur", height="360px"))))
        )
      ),

      # ── COMPANY ─────────────────────────────────────────
      nav_panel("company",
        tab_toolbar("Company", "How size, ownership, rating & age affect pay"),
        layout_sidebar(fillable = FALSE,
          sidebar = sidebar(position = "right", open = "closed", width = 260,
            title = tagList(bs_icon("building"), " Company Filters"),
            p(class = "text-muted", style = "font-size:11px;", "Independent from global filters."),
            selectInput("co_size", "Company Size",   SIZES, "All"),
            selectInput("co_own",  "Ownership Type", OWNS,  "All")),
          layout_columns(col_widths = c(6,6),
            card(card_header(tags$i(class="bi bi-building"), " Median Salary by Company Size"),
                 card_body(plotlyOutput("p_co_size", height="310px"))),
            card(card_header(tags$i(class="bi bi-diagram-3-fill"), " Median Salary by Ownership Type"),
                 card_body(plotlyOutput("p_co_own", height="310px")))),
          br(),
          layout_columns(col_widths = c(6,6),
            card(card_header(tags$i(class="bi bi-star-fill"), " Company Rating vs Avg Salary"),
                 card_body(plotlyOutput("p_rating", height="310px"))),
            card(card_header(tags$i(class="bi bi-clock-history"), " Median Salary by Company Age"),
                 card_body(plotlyOutput("p_age_sal", height="310px"))))
        )
      ),

      # ── CAREER FIT ──────────────────────────────────────
      nav_panel("career",
        tab_toolbar("Career Fit", "Personalised salary estimate + Skill gap analysis"),
        layout_sidebar(fillable = FALSE,
          sidebar = sidebar(position = "right", open = "closed", width = 270,
            title = tagList(bs_icon("person-badge"), " Your Profile"),
            p("Set your details and hit", strong("Find Matches"), " to see your estimated salary and skill gaps."),
            hr(),
            selectInput("pr_role",   "Target Role",     c("Any", sort(unique(df$role))),                  "Any"),
            selectInput("pr_state",  "Preferred State",  c("Any", STATES),                                "Any"),
            selectInput("pr_senior", "Seniority",        c("Any","Junior","Mid-Level","Senior","Executive"), "Any"),
            checkboxGroupInput("pr_skills", "Skills I Have", choiceNames = SN, choiceValues = SC),
            hr(),
            actionButton("pr_go", "Find Matches", class = "btn-primary w-100", icon = icon("magnifying-glass"))),
          layout_columns(col_widths = c(4,4,4),
            value_box("Matching Jobs", textOutput("pr_n"),     bs_icon("briefcase"),       theme = "primary"),
            value_box("Avg Salary",    textOutput("pr_avg"),   bs_icon("currency-dollar"), theme = "primary"),
            value_box("Salary Range",  textOutput("pr_range"), bs_icon("arrows-expand"),   theme = "primary")),
          br(),
          layout_columns(col_widths = c(6,6),
            card(card_header(tags$i(class="bi bi-graph-up"), " Expected Salary Distribution"),
                 card_body(plotlyOutput("pr_dist", height="260px"))),
            card(card_header(tags$i(class="bi bi-briefcase-fill"), " Top Hiring Sectors for Your Profile"),
                 card_body(plotlyOutput("pr_sector", height="260px")))),
          br(),
          card(card_header(tags$i(class="bi bi-lightbulb-fill"), " Skill Gap — Estimated Salary Impact"),
               card_body(plotlyOutput("pr_gap", height="250px")))
        )
      ),

     # ── ABOUT ───────────────────────────────────────────
nav_panel("about",
        tab_toolbar("", ""),
        card_body(
        markdown("
## About

This web dashboard serves as our learning evidence for **CS 226 Data Analytics - Statistics Using R, AY 2025–2026**.
It utilizes the **Jobs Dataset from Glassdoor** (thedevastator, Kaggle 2022), which includes 742 job postings
across 38 US states and 24 industry sectors collected between 2017–2018. The dataset contains job titles,
salary estimates, company information, required skills, and location data.

The application aims to identify patterns in the **US data science job market** and provide interactive
visualizations to answer: *What role, skills, location, and company should I target to maximize my data
science salary?* This dashboard offers valuable insights for students, job seekers, and professionals in
the data field — particularly in understanding salary drivers, skill demands, and regional hiring trends.

---

## Tabs

| Tab | What It Shows |
|---|---|
| **Dashboard** | Salary landscape, role breakdown, seniority distribution |
| **Location** | Job density & salary by US state; local vs out-of-state pay |
| **Skills** | Skill demand %, salary premium, co-occurrence patterns |
| **Company** | How size, ownership type, rating & age affect pay |
| **Career Fit** | Personalised salary estimate + skill gap analysis |
"),
        tags$a(
          href = "https://www.kaggle.com/datasets/thedevastator/jobs-dataset-from-glassdoor",
          target = "_blank",
          class = "btn btn-primary btn-sm",
          tags$i(class = "bi bi-database-fill"), " Link to the Dataset"
        ),
        markdown("

*CS 226 Data Analytics - Stat Using R, AY 2025-2026.*
")))
      )
    )
  )


# ── 6. SERVER ─────────────────────────────────────────────────
server <- function(input, output, session) {

  pal  <- reactive({ if(isTRUE(input$dark_mode == "dark")) PD else PL })
  colr <- reactive({ if(isTRUE(input$dark_mode == "dark")) CD else CL })
  th   <- reactive({ mth(pal()) })

  observeEvent(input$active_tab, {
    nav_select("main_panel", input$active_tab)
  })

  observe({
    updateSliderInput(session, "f_sal2", value = input$f_sal)
    updateSelectInput(session, "f_role2",   selected = input$f_role)
    updateSelectInput(session, "f_sector2", selected = input$f_sector)
    updateSelectInput(session, "f_senior2", selected = input$f_senior)
  })
  observe({
    updateSliderInput(session, "f_sal3", value = input$f_sal)
    updateSelectInput(session, "f_role3",   selected = input$f_role)
    updateSelectInput(session, "f_sector3", selected = input$f_sector)
    updateSelectInput(session, "f_senior3", selected = input$f_senior)
  })

  wrap <- function(p, h = 400) {
    pa <- pal()
    ggplotly(p, height = h, tooltip = "text") %>%
      layout(
        paper_bgcolor = pa$card_bg,
        plot_bgcolor  = pa$card_bg,
        font = list(family = "Plus Jakarta Sans, sans-serif", size = 11, color = pa$text1),
        title = list(
          font = list(family = "Plus Jakarta Sans, sans-serif", size = 12, color = pa$primary),
          x = 0.02, xanchor = "left"
        ),
        hoverlabel = list(
          bgcolor     = pa$hover_bg,
          bordercolor = pa$hover_border,
          font = list(family = "Plus Jakarta Sans, sans-serif", size = 11, color = pa$hover_text)
        ),
        legend = list(
          orientation = "h", x = 0, y = -0.18,
          font = list(color = pa$text3, size = 10)
        ),
        margin = list(l = 10, r = 10, t = 36, b = 10)
      ) %>%
      config(displayModeBar = FALSE)
  }

  # ── Filtered data ──────────────────────────────────────────
  GDF <- reactive({
    d <- df %>% filter(avg_salary >= input$f_sal[1], avg_salary <= input$f_sal[2])
    if(input$f_role   != "All") d <- d %>% filter(role      == input$f_role)
    if(input$f_sector != "All") d <- d %>% filter(Sector    == input$f_sector)
    if(input$f_senior != "All") d <- d %>% filter(seniority == input$f_senior)
    d
  })
  CDF <- reactive({
    d <- df
    if(input$co_size != "All") d <- d %>% filter(as.character(Size) == input$co_size)
    if(input$co_own  != "All") d <- d %>% filter(Type.of.ownership  == input$co_own)
    d
  })
  SST <- reactive({
    d <- GDF()
    tibble(skill = SN, col = SC,
           demand  = sapply(SC, \(s) mean(d[[s]], na.rm = TRUE) * 100),
           premium = sapply(SC, \(s)
             mean(d$avg_salary[d[[s]] == 1], na.rm = TRUE) -
             mean(d$avg_salary[d[[s]] == 0], na.rm = TRUE)))
  })

  # ── KPIs ──────────────────────────────────────────────────
  output$kpi_n      <- renderText(scales::comma(nrow(GDF())))
  output$kpi_med    <- renderText(paste0("$", round(median(GDF()$avg_salary, na.rm = TRUE), 1), "K"))
  output$kpi_iqr    <- renderText({
    d <- GDF()$avg_salary
    paste0("$", round(quantile(d, .25, na.rm=TRUE)), "K – $", round(quantile(d, .75, na.rm=TRUE)), "K")
  })
  output$kpi_spread <- renderText(paste0("$", round(mean(GDF()$sal_spread, na.rm = TRUE), 1), "K"))

  # ── DASHBOARD ─────────────────────────────────────────────
  output$p_dumbbell <- renderPlotly({
    pa <- pal(); t <- th()
    d <- GDF() %>% group_by(role) %>%
      summarise(lo = median(min_salary, na.rm=TRUE), mid = median(avg_salary, na.rm=TRUE),
                hi = median(max_salary, na.rm=TRUE), n = n(), .groups = "drop") %>%
      arrange(mid) %>% mutate(role = factor(role, levels = role),
        tip = paste0("<b>", role, "</b><br>Min: $", round(lo), "K<br>Avg: $", round(mid), "K<br>Max: $", round(hi), "K<br>Postings: ", n))
    validate(need(nrow(d) > 0, "No data."))
    tr <- d %>% slice_max(mid, n=1, with_ties=FALSE)
    p <- ggplot(d, aes(y = role)) +
      geom_segment(aes(x=lo, xend=hi, yend=role, text=tip), color=pa$bg3, linewidth=5, lineend="round") +
      geom_point(aes(x=lo,  text=tip), color=pa$accent,   size=3.5) +
      geom_point(aes(x=mid, text=tip), color=pa$primary,  size=5.5) +
      geom_point(aes(x=hi,  text=tip), color=pa$positive, size=3.5) +
      geom_text(aes(x=mid, label=paste0("$",round(mid),"K")), nudge_y=0.32, size=2.8, fontface="bold", color=pa$primary) +
      annotate("text", x=tr$hi*1.05, y=as.numeric(tr$role), label="Highest paying role",
               hjust=0, size=3.2, color=pa$accent, fontface="bold") +
      scale_x_continuous(labels=dollar_format(suffix="K",prefix="$"), expand=expansion(mult=c(0.02,0.25))) +
      labs(title="Salary Range by Job Role", subtitle="Orange=min  \u00B7  Blue=avg  \u00B7  Green=max",
           x="Salary (K USD)", y=NULL) + t
    wrap(p, 370)
  })

  output$p_role_bar <- renderPlotly({
    pa <- pal(); t <- th()
    d <- GDF() %>% count(role) %>%
      mutate(pct = round(n/sum(n)*100,1),
             tip = paste0("<b>",role,"</b><br>Postings: ",n,"<br>Share: ",pct,"%")) %>%
      arrange(n) %>% mutate(role = factor(role, levels=role))
    validate(need(nrow(d) > 0, "No data."))
    p <- ggplot(d, aes(n, role, fill=n, text=tip)) +
      geom_col(alpha=0.9, width=0.7, show.legend=FALSE) +
      geom_text(aes(label=paste0(n,"  (",pct,"%)")), hjust=-0.08, size=2.8, fontface="bold", color=pa$text1) +
      scale_fill_gradient(low=pa$primary3, high=pa$primary) +
      scale_x_continuous(expand=expansion(mult=c(0,0.35))) +
      labs(title="Job Postings by Role", subtitle="Darker = more postings", x="Number of Postings", y=NULL) + t
    wrap(p, 370)
  })

  output$p_sen_lollipop <- renderPlotly({
    pa <- pal(); t <- th()
    d <- GDF() %>% group_by(seniority) %>%
      summarise(n=n(), med=median(avg_salary, na.rm=TRUE), .groups="drop") %>%
      mutate(pct=round(n/sum(n)*100,1),
             seniority=factor(seniority, levels=c("Junior","Mid-Level","Senior","Executive")),
             tip=paste0("<b>",seniority,"</b><br>Share: ",pct,"%<br>Median: $",round(med),"K<br>Postings: ",n))
    validate(need(nrow(d) > 0, "No data."))
    p <- ggplot(d, aes(pct, seniority, text=tip)) +
      geom_segment(aes(x=0, xend=pct, yend=seniority), color=pa$bg3, linewidth=1.8) +
      geom_point(aes(color=med), size=9, show.legend=FALSE) +
      geom_text(aes(label=paste0(pct,"%")), color="white", size=3, fontface="bold") +
      geom_text(aes(x=pct, label=paste0(" $",round(med),"K")), hjust=0, size=2.8, color=pa$text2) +
      scale_color_gradient(low=pa$primary3, high=pa$accent) +
      scale_x_continuous(expand=expansion(mult=c(0,0.5))) +
      labs(title="Seniority Distribution & Median Salary",
           subtitle="Dot % = share of postings  \u00B7  Label = median salary",
           x="Share of Postings (%)", y=NULL) + t
    wrap(p, 290)
  })

  output$p_sen_box <- renderPlotly({
    pa <- pal(); t <- th()
    d <- GDF(); validate(need(nrow(d) > 0, "No data."))
    ml <- d %>% group_by(seniority) %>% summarise(med=median(avg_salary, na.rm=TRUE), .groups="drop")
    d2 <- d %>% mutate(tip=paste0("<b>",seniority,"</b><br>Salary: $",round(avg_salary),"K"))
    p <- ggplot(d2, aes(seniority, avg_salary, fill=seniority, text=tip)) +
      geom_boxplot(alpha=0.78, outlier.alpha=0.18, outlier.size=1.2, show.legend=FALSE) +
      geom_text(data=ml, aes(y=med, label=paste0("$",round(med),"K"), text=NULL),
                vjust=-0.6, size=3.5, fontface="bold", color=pa$text1) +
      scale_fill_manual(values=colr()) +
      scale_y_continuous(labels=dollar_format(suffix="K",prefix="$")) +
      labs(title="Salary Distribution by Seniority Level",
           subtitle="Box=middle 50%  \u00B7  Line=median", x=NULL, y="Avg Salary (K USD)") + t
    wrap(p, 290)
  })

  # ── LOCATION ─────────────────────────────────────────────
  output$geo_top <- renderText({
    d <- GDF() %>% group_by(job_state) %>%
      summarise(med=median(avg_salary,na.rm=TRUE), n=n(), .groups="drop") %>%
      filter(n>=3) %>% slice_max(med, n=1, with_ties=FALSE)
    if(nrow(d)==0) "N/A" else paste0(d$job_state," ($",round(d$med),"K)")
  })
  output$geo_most <- renderText({
    d <- GDF() %>% count(job_state) %>% slice_max(n, n=1, with_ties=FALSE)
    if(nrow(d)==0) "N/A" else paste0(d$job_state," (",d$n," jobs)")
  })
  output$geo_local  <- renderText({
    v <- mean(GDF()$avg_salary[GDF()$same_state==1], na.rm=TRUE)
    if(is.nan(v)) "N/A" else paste0("$",round(v,1),"K")
  })
  output$geo_remote <- renderText({
    v <- mean(GDF()$avg_salary[GDF()$same_state==0], na.rm=TRUE)
    if(is.nan(v)) "N/A" else paste0("$",round(v,1),"K")
  })

  output$p_state_n <- renderPlotly({
    pa <- pal(); t <- th()
    d <- GDF() %>% count(job_state) %>% slice_max(n, n=15) %>% arrange(n) %>%
      mutate(job_state=factor(job_state, levels=job_state), is_top=n==max(n),
             tip=paste0("<b>",job_state,"</b><br>Postings: ",n))
    validate(need(nrow(d) > 0, "No data."))
    p <- ggplot(d, aes(n, job_state, fill=n, text=tip)) +
      geom_col(show.legend=FALSE, alpha=0.9, width=0.72) +
      geom_text(aes(label=n), hjust=-0.15, size=2.8, fontface="bold", color=pa$text1) +
      geom_text(data=filter(d, is_top), aes(x=n/2, label=" #1 for job volume ", text=NULL),
                hjust=0.5, size=3, color="white", fontface="bold") +
      scale_fill_gradient(low=pa$primary3, high=pa$primary) +
      scale_x_continuous(expand=expansion(mult=c(0,0.22))) +
      labs(title="Top 15 States by Job Volume", subtitle="Darker = more postings", x="Job Postings", y=NULL) + t
    wrap(p, 400)
  })

  output$p_state_sal <- renderPlotly({
    pa <- pal(); t <- th()
    d <- GDF() %>% group_by(job_state) %>%
      summarise(med=median(avg_salary,na.rm=TRUE), n=n(), .groups="drop") %>%
      filter(n>=3) %>% slice_max(med, n=15) %>% arrange(med) %>%
      mutate(job_state=factor(job_state, levels=job_state), is_top=med==max(med),
             tip=paste0("<b>",job_state,"</b><br>Median: $",round(med),"K<br>Postings: ",n))
    validate(need(nrow(d) > 0, "No data."))
    p <- ggplot(d, aes(med, job_state, fill=med, text=tip)) +
      geom_col(show.legend=FALSE, alpha=0.9, width=0.72) +
      geom_text(aes(label=paste0("$",round(med),"K")), hjust=-0.1, size=2.8, fontface="bold", color=pa$text1) +
      geom_text(data=filter(d, is_top), aes(x=med/2, label=" Highest paying ", text=NULL),
                hjust=0.5, size=3, color="white", fontface="bold") +
      scale_fill_gradient(low=pa$primary3, high=pa$primary) +
      scale_x_continuous(expand=expansion(mult=c(0,0.22))) +
      labs(title="Top 15 States by Median Salary",
           subtitle="States with fewer than 3 postings excluded", x="Median Salary (K USD)", y=NULL) + t
    wrap(p, 400)
  })

  output$p_local_box <- renderPlotly({
    pa <- pal(); t <- th()
    d <- GDF() %>% mutate(
      group = ifelse(same_state==1, "Same State", "Different State"),
      tip = paste0("<b>",ifelse(same_state==1,"Same State","Different State"),"</b><br>Salary: $",round(avg_salary),"K"))
    validate(need(nrow(d) > 0, "No data."))
    lm2 <- median(d$avg_salary[d$group=="Same State"],       na.rm=TRUE)
    rm2 <- median(d$avg_salary[d$group=="Different State"],  na.rm=TRUE)
    gap <- round(abs(lm2-rm2), 1)
    p <- ggplot(d, aes(group, avg_salary, fill=group, text=tip)) +
      geom_boxplot(alpha=0.78, outlier.alpha=0.2, show.legend=FALSE, outlier.size=1.2) +
      geom_jitter(aes(color=group), width=0.15, alpha=0.15, size=1.2, show.legend=FALSE) +
      annotate("segment", x=1, xend=2, y=max(lm2,rm2)*1.12, yend=max(lm2,rm2)*1.12,
               color=pa$text3, linewidth=0.6, arrow=arrow(ends="both", length=unit(0.1,"cm"))) +
      annotate("text", x=1.5, y=max(lm2,rm2)*1.17, label=paste0("$",gap,"K gap"),
               size=3.5, fontface="bold", color=pa$text1) +
      scale_fill_manual(values=c("Same State"=pa$primary, "Different State"=pa$accent)) +
      scale_color_manual(values=c("Same State"=pa$primary, "Different State"=pa$accent)) +
      scale_y_continuous(labels=dollar_format(suffix="K",prefix="$")) +
      labs(title="Salary: Local vs Out-of-State",
           subtitle=paste0(ifelse(lm2>=rm2,"Local","Out-of-state")," applicants earn more  \u00B7  Gap: $",gap,"K"),
           x=NULL, y="Avg Salary (K USD)") + t
    wrap(p, 300)
  })

  output$p_local_role <- renderPlotly({
    pa <- pal(); t <- th()
    d <- GDF() %>% mutate(group=ifelse(same_state==1,"Same State","Different State")) %>%
      group_by(role, group) %>% summarise(med=median(avg_salary, na.rm=TRUE), .groups="drop") %>%
      mutate(tip=paste0("<b>",role," — ",group,"</b><br>Median: $",round(med),"K"))
    validate(need(nrow(d) > 0, "No data."))
    p <- ggplot(d, aes(role, med, fill=group, text=tip)) +
      geom_col(position="dodge", alpha=0.88, width=0.7) +
      geom_text(aes(label=paste0("$",round(med),"K")),
                position=position_dodge(width=0.7), vjust=-0.4, size=3, fontface="bold", color=pa$text1) +
      scale_fill_manual(values=c("Same State"=pa$primary, "Different State"=pa$accent), name=NULL) +
      scale_y_continuous(labels=dollar_format(suffix="K",prefix="$"), expand=expansion(mult=c(0,0.18))) +
      labs(title="Local vs Out-of-State Salary by Role",
           subtitle="Blue=same-state  \u00B7  Purple=out-of-state", x=NULL, y="Median Salary (K USD)") + t +
      theme(axis.text.x=element_text(angle=30, hjust=1, color=pa$text3, size=10))
    wrap(p, 300)
  })

  # ── SKILLS ───────────────────────────────────────────────
  output$sk_top_demand  <- renderText({
    s <- SST() %>% slice_max(demand, n=1, with_ties=FALSE)
    paste0(s$skill," (",round(s$demand,0),"% of jobs)")
  })
  output$sk_top_premium <- renderText({
    s <- SST() %>% slice_max(premium, n=1, with_ties=FALSE)
    paste0(s$skill," (+$",round(s$premium,1),"K)")
  })
  output$sk_python <- renderText(paste0(round(mean(GDF()$python_yn, na.rm=TRUE)*100, 1),"%"))
  output$sk_excel  <- renderText(paste0(round(mean(GDF()$excel,     na.rm=TRUE)*100, 1),"%"))

  output$p_skill_demand <- renderPlotly({
    pa <- pal(); t <- th()
    d <- SST() %>% arrange(demand) %>%
      mutate(skill=factor(skill, levels=skill), is_top=demand==max(demand),
             tip=paste0("<b>",skill,"</b><br>Required in: ",round(demand,1),"% of postings"))
    validate(need(nrow(d) > 0, "No data."))
    p <- ggplot(d, aes(demand, skill, fill=demand, text=tip)) +
      geom_col(show.legend=FALSE, alpha=0.88, width=0.65) +
      geom_text(aes(label=paste0(round(demand,1),"%")), hjust=-0.1, size=2.8, fontface="bold", color=pa$text1) +
      geom_text(data=filter(d,is_top), aes(x=demand/2, label=" Most required ", text=NULL),
                hjust=0.5, size=3, color="white", fontface="bold") +
      scale_fill_gradient(low=pa$primary3, high=pa$primary) +
      scale_x_continuous(expand=expansion(mult=c(0,0.28)), limits=c(0,100)) +
      labs(title="Skill Demand Across All Postings",
           subtitle="% of all postings requiring this skill", x="% of Job Postings", y=NULL) + t
    wrap(p, 310)
  })

  output$p_skill_premium <- renderPlotly({
    pa <- pal(); t <- th()
    d <- SST() %>% arrange(premium) %>%
      mutate(skill=factor(skill, levels=skill),
             dir=ifelse(premium>=0,"positive","negative"),
             is_best=premium==max(premium),
             tip=paste0("<b>",skill,"</b><br>Salary boost: ",ifelse(premium>0,"+",""),round(premium,1),"K"))
    validate(need(nrow(d) > 0, "No data."))
    best <- d %>% filter(is_best)
    p <- ggplot(d, aes(premium, skill, fill=dir, text=tip)) +
      geom_col(show.legend=FALSE, alpha=0.88, width=0.65) +
      geom_vline(xintercept=0, linetype="dashed", color=pa$text3) +
      geom_text(aes(label=paste0(ifelse(premium>0,"+",""),round(premium,1),"K")),
                hjust=ifelse(d$premium>=0,-0.15,1.15), size=4, fontface="bold", color=pa$text1) +
      annotate("text", x=best$premium[1]*1.4, y=as.numeric(best$skill[1]),
               label=paste0("Best to learn\n+$",round(best$premium[1],1),"K boost"),
               hjust=0, size=3.2, color=pa$positive, fontface="bold") +
      scale_fill_manual(values=c("positive"=pa$positive,"negative"=pa$negative)) +
      scale_x_continuous(expand=expansion(mult=c(0.2,0.5))) +
      labs(title="Salary Premium by Technical Skill",
           subtitle="Green=salary boost  \u00B7  Red=salary penalty",
           x="Salary Difference vs Jobs Without That Skill (K USD)", y=NULL) + t
    wrap(p, 310)
  })

  output$p_skill_role <- renderPlotly({
    pa <- pal(); t <- th()
    d <- GDF(); validate(need(nrow(d) > 0, "No data."))
    pd <- d %>% group_by(role) %>%
      summarise(across(all_of(SC), \(x) mean(x, na.rm=TRUE)*100), .groups="drop") %>%
      pivot_longer(all_of(SC), names_to="col", values_to="pct") %>%
      mutate(skill=SN[match(col,SC)],
             tip=paste0("<b>",skill," — ",role,"</b><br>Required in: ",round(pct),"% of postings"))
    p <- ggplot(pd, aes(skill, pct, fill=pct, text=tip)) +
      geom_col(alpha=0.9, width=0.72, show.legend=FALSE) +
      geom_text(aes(label=paste0(round(pct),"%")), vjust=-0.3, size=2.5, fontface="bold", color=pa$text1) +
      facet_wrap(~role, ncol=3) +
      scale_fill_gradient(low=pa$primary3, high=pa$primary) +
      scale_y_continuous(expand=expansion(mult=c(0,0.25)), limits=c(0,100)) +
      labs(title="Skill Demand by Job Role",
           subtitle="% of postings in each role requiring this skill", x=NULL, y="% of Postings") + t +
      theme(axis.text.x=element_text(angle=35, hjust=1, size=9, color=pa$text3))
    wrap(p, 360)
  })

  output$p_cooccur <- renderPlotly({
    pa <- pal(); t <- th()
    d <- GDF(); validate(need(nrow(d)>=10, "Not enough data."))
    co <- matrix(0L, 5, 5, dimnames=list(SN,SN))
    for(i in 1:5) for(j in 1:5)
      co[i,j] <- round(mean(d[[SC[i]]]==1 & d[[SC[j]]]==1, na.rm=TRUE)*100)
    co_df <- as.data.frame(as.table(co)) %>%
      rename(S1=Var1, S2=Var2, pct=Freq) %>%
      mutate(is_max=pct==max(pct[S1!=S2]),
             tip=paste0("<b>",S1," + ",S2,"</b><br>Co-occur in: ",pct,"% of postings"))
    p <- ggplot(co_df, aes(S1, S2, fill=pct, text=tip)) +
      geom_tile(color=pa$bg, linewidth=1.2) +
      geom_tile(data=filter(co_df, is_max & S1!=S2), aes(S1,S2),
                fill=NA, color=pa$accent, linewidth=2) +
      geom_text(aes(label=paste0(pct,"%")), size=3.2, fontface="bold",
                color=ifelse(co_df$pct>50,"white",pa$text1)) +
      scale_fill_gradient(low=pa$bg2, high=pa$primary, name="% co-occurring", limits=c(0,100)) +
      labs(title="Skill Co-occurrence Matrix",
           subtitle="Darker=often listed together  \u00B7  Purple border=most common pair",
           x=NULL, y=NULL) + t +
      theme(legend.position="right",
            axis.text=element_text(face="bold", size=11, color=pa$text2))
    wrap(p, 360)
  })

  # ── COMPANY ──────────────────────────────────────────────
  output$p_co_size <- renderPlotly({
    pa <- pal(); t <- th()
    d <- CDF() %>% filter(!is.na(Size)) %>% group_by(Size) %>%
      summarise(med=median(avg_salary,na.rm=TRUE), n=n(), .groups="drop") %>%
      mutate(is_peak=med==max(med),
             tip=paste0("<b>",Size,"</b><br>Median: $",round(med),"K<br>Postings: ",n))
    validate(need(nrow(d) > 0, "No data."))
    p <- ggplot(d, aes(Size, med, text=tip, fill=ifelse(is_peak, pa$accent, pa$bg3))) +
      geom_col(show.legend=FALSE, alpha=0.9, width=0.72) +
      geom_text(aes(label=paste0("$",round(med),"K\n(n=",n,")")), vjust=-0.3, size=3.1, color=pa$text1) +
      geom_text(data=filter(d, is_peak), aes(y=med/2, label="Sweet spot", text=NULL),
                hjust=0.5, size=3.2, color=pa$bg, fontface="bold") +
      scale_fill_identity() +
      scale_y_continuous(expand=expansion(mult=c(0,.22)), labels=dollar_format(suffix="K",prefix="$")) +
      labs(title="Median Salary by Company Size",
           subtitle="Highlighted bar = highest-paying size group", x=NULL, y="Median Salary (K USD)") + t +
      theme(axis.text.x=element_text(angle=30, hjust=1, size=9, color=pa$text3))
    wrap(p, 310)
  })

  output$p_co_own <- renderPlotly({
    pa <- pal(); t <- th()
    d <- CDF() %>% filter(!is.na(Type.of.ownership)) %>% group_by(Type.of.ownership) %>%
      summarise(med=median(avg_salary,na.rm=TRUE), n=n(), .groups="drop") %>%
      filter(n>=5) %>% arrange(med) %>%
      mutate(Type.of.ownership=factor(Type.of.ownership, levels=Type.of.ownership),
             is_top=med==max(med),
             tip=paste0("<b>",Type.of.ownership,"</b><br>Median: $",round(med),"K<br>Postings: ",n))
    validate(need(nrow(d) > 0, "No data."))
    p <- ggplot(d, aes(med, Type.of.ownership, fill=med, text=tip)) +
      geom_col(show.legend=FALSE, alpha=0.9, width=0.7) +
      geom_text(aes(label=paste0("$",round(med),"K  (n=",n,")")), hjust=-0.1, size=3.4, color=pa$text1) +
      geom_text(data=filter(d,is_top), aes(x=med/2, label="Highest paying", text=NULL),
                hjust=0.5, size=3, color="white", fontface="bold") +
      scale_fill_gradient(low=pa$primary3, high=pa$primary) +
      scale_x_continuous(expand=expansion(mult=c(0,.3))) +
      labs(title="Median Salary by Ownership Type",
           subtitle="Only types with 5+ postings shown", x="Median Salary (K USD)", y=NULL) + t
    wrap(p, 310)
  })

  output$p_rating <- renderPlotly({
    pa <- pal(); t <- th()
    d <- CDF() %>% filter(!is.na(Rating)) %>%
      mutate(tip=paste0("<b>",Company.Name,"</b><br>Rating: ",Rating,"\u2605<br>Salary: $",round(avg_salary),"K<br>Role: ",role))
    validate(need(nrow(d) > 5, "Not enough data."))
    ar <- round(mean(d$Rating, na.rm=TRUE), 1)
    sl <- round(coef(lm(avg_salary ~ Rating, data=d))[2], 1)
    p <- ggplot(d, aes(Rating, avg_salary, text=tip)) +
      geom_point(color=pa$primary, alpha=0.3, size=2) +
      geom_smooth(method="lm", se=TRUE, color=pa$accent, fill=paste0(pa$accent,"33"), linewidth=1.2) +
      geom_vline(xintercept=ar, linetype="dashed", color=pa$text3, linewidth=0.7) +
      annotate("text", x=ar, y=Inf, label=paste0(" Avg: ",ar,"\u2605"),
               hjust=0, vjust=1.6, size=3.2, color=pa$text2) +
      annotate("text", x=4.2, y=min(d$avg_salary,na.rm=TRUE)*1.05,
               label=paste0("Each extra \u2605 \u2248 ",ifelse(sl>=0,"+",""),sl,"K"),
               size=3.5, fontface="bold", color=pa$accent, hjust=0) +
      scale_y_continuous(labels=dollar_format(suffix="K",prefix="$")) +
      labs(title="Company Rating vs Average Salary",
           subtitle="Trend line  \u00B7  Shaded=95% CI", x="Glassdoor Rating (1-5)", y="Avg Salary (K USD)") + t
    wrap(p, 310)
  })

  output$p_age_sal <- renderPlotly({
    pa <- pal(); t <- th()
    d <- CDF() %>% filter(!is.na(age_group)) %>% group_by(age_group) %>%
      summarise(med=median(avg_salary,na.rm=TRUE), n=n(), .groups="drop") %>%
      mutate(is_peak=med==max(med),
             tip=paste0("<b>",age_group,"</b><br>Median: $",round(med),"K<br>Postings: ",n))
    validate(need(nrow(d) > 0, "No data."))
    p <- ggplot(d, aes(age_group, med, text=tip, fill=ifelse(is_peak, pa$accent, pa$bg3))) +
      geom_col(show.legend=FALSE, alpha=0.9, width=0.7) +
      geom_text(aes(label=paste0("$",round(med),"K\n(n=",n,")")), vjust=-0.3, size=3.1, color=pa$text1) +
      geom_text(data=filter(d,is_peak), aes(y=med/2, label="Highest paying", text=NULL),
                hjust=0.5, size=3.1, color=pa$bg, fontface="bold") +
      scale_fill_identity() +
      scale_y_continuous(expand=expansion(mult=c(0,.22)), labels=dollar_format(suffix="K",prefix="$")) +
      labs(title="Median Salary by Company Age",
           subtitle="Startups vs legacy firms", x=NULL, y="Median Salary (K USD)") + t +
      theme(axis.text.x=element_text(angle=20, hjust=1, color=pa$text3))
    wrap(p, 310)
  })

  # ── CAREER FIT ───────────────────────────────────────────
  PD2 <- eventReactive(input$pr_go, {
    d <- df
    if(input$pr_role   != "Any") d <- d %>% filter(role      == input$pr_role)
    if(input$pr_state  != "Any") d <- d %>% filter(job_state == input$pr_state)
    if(input$pr_senior != "Any") d <- d %>% filter(seniority == input$pr_senior)
    for(s in input$pr_skills) d <- d %>% filter(.data[[s]] == 1)
    d
  }, ignoreNULL = FALSE)

  output$pr_n     <- renderText(scales::comma(nrow(PD2())))
  output$pr_avg   <- renderText({
    v <- mean(PD2()$avg_salary, na.rm=TRUE)
    if(is.nan(v)) "N/A" else paste0("$",round(v,1),"K")
  })
  output$pr_range <- renderText({
    d <- PD2(); if(nrow(d)==0) return("N/A")
    paste0("$",round(median(d$min_salary,na.rm=TRUE)),"K – $",round(median(d$max_salary,na.rm=TRUE)),"K")
  })

  output$pr_dist <- renderPlotly({
    pa <- pal(); t <- th()
    d <- PD2(); validate(need(nrow(d)>=5, "Not enough matches — broaden your filters."))
    av <- mean(d$avg_salary, na.rm=TRUE)
    d2 <- d %>% mutate(tip=paste0("Salary: $",round(avg_salary),"K"))
    p <- ggplot(d2, aes(avg_salary, text=tip)) +
      geom_histogram(bins=20, fill=pa$primary, color=pa$card_bg, alpha=0.88) +
      geom_vline(xintercept=av, color=pa$accent, linetype="dashed", linewidth=1.2) +
      annotate("text", x=av, y=Inf, label=paste0(" Avg: $",round(av,1),"K"),
               hjust=-0.05, vjust=1.8, color=pa$accent, fontface="bold") +
      scale_x_continuous(labels=dollar_format(suffix="K",prefix="$")) +
      labs(title="Expected Salary Distribution",
           subtitle=paste0(nrow(d)," postings match your profile"),
           x="Avg Salary (K USD)", y="Number of Jobs") + t
    wrap(p, 260)
  })

  output$pr_sector <- renderPlotly({
    pa <- pal(); t <- th()
    d <- PD2() %>% filter(!is.na(Sector)) %>% count(Sector) %>%
      slice_max(n, n=8) %>% arrange(n) %>%
      mutate(Sector=factor(Sector, levels=Sector),
             tip=paste0("<b>",Sector,"</b><br>Matching postings: ",n))
    validate(need(nrow(d)>0, "No sector data."))
    p <- ggplot(d, aes(n, Sector, fill=n, text=tip)) +
      geom_col(show.legend=FALSE, alpha=0.9, width=0.7) +
      geom_text(aes(label=n), hjust=-0.2, size=3.4, color=pa$text1) +
      scale_fill_gradient(low=pa$primary3, high=pa$primary) +
      scale_x_continuous(expand=expansion(mult=c(0,.22))) +
      labs(title="Top Sectors Hiring for Your Profile", x="Matching Job Count", y=NULL) + t
    wrap(p, 260)
  })

  output$pr_gap <- renderPlotly({
    pa <- pal(); t <- th()
    d    <- PD2()
    has  <- input$pr_skills
    mc   <- SC[!SC %in% has]
    mn   <- SN[!SC %in% has]

    if(length(mc) == 0) {
      p <- ggplot() +
        annotate("text", x=0.5, y=0.6, hjust=0.5, vjust=0.5, size=5.5,
                 color=pa$positive, fontface="bold",
                 label="You have all 5 tracked skills!") +
        annotate("text", x=0.5, y=0.4, hjust=0.5, vjust=0.5, size=4,
                 color=pa$text2,
                 label="You're in the top tier of candidates.") +
        xlim(0,1) + ylim(0,1) +
        theme_void() +
        theme(plot.background=element_rect(fill=pa$card_bg, color=NA))
      return(ggplotly(p, height=250) %>% config(displayModeBar=FALSE) %>%
               layout(paper_bgcolor=pa$card_bg, plot_bgcolor=pa$card_bg))
    }

    my_avg <- if(nrow(d) >= 3) mean(d$avg_salary, na.rm=TRUE) else mean(df$avg_salary, na.rm=TRUE)

    gap <- tibble(
      skill = mn,
      gain  = sapply(mc, \(s)
        mean(df$avg_salary[df[[s]] == 1], na.rm=TRUE) - my_avg)
    ) %>% arrange(desc(gain)) %>%
      mutate(
        dir = ifelse(gain >= 0, "positive", "negative"),
        skill = factor(skill, levels=skill),
        tip = paste0("<b>", skill, "</b><br>Potential gain: ",
                     ifelse(gain > 0, "+", ""), round(gain, 1), "K vs your current avg")
      )

    validate(need(nrow(gap) > 0, "No skill gap to display."))

    p <- ggplot(gap, aes(gain, skill, fill=dir, text=tip)) +
      geom_col(show.legend=FALSE, alpha=0.88, width=0.65) +
      geom_text(aes(label=paste0(ifelse(gain > 0, "+", ""), round(gain, 1), "K")),
                hjust=ifelse(gap$gain >= 0, -0.15, 1.15),
                size=4, fontface="bold", color=pa$text1) +
      geom_vline(xintercept=0, linetype="dashed", color=pa$text3, linewidth=0.8) +
      scale_fill_manual(values=c("positive"=pa$positive, "negative"=pa$negative)) +
      scale_x_continuous(expand=expansion(mult=c(0.25, 0.35))) +
      labs(
        title    = "Skill Gap — Estimated Salary Impact",
        subtitle = paste0("Compared to your current estimated avg ($", round(my_avg, 1), "K)"),
        x        = "Potential Salary Gain (K USD)", y=NULL
      ) + t
    wrap(p, 250)
  })
}

shinyApp(ui, server)
