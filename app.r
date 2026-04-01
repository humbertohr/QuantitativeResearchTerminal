library(shiny)
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(zoo)
library(scales)

# ── Palette ─────────────────────────────────────────────────
COL_BG      <- "#0f0f0f"
COL_PANEL   <- "#1a1a1a"
COL_BORDER  <- "#2e2e2e"
COL_YELLOW  <- "#f0c040"
COL_TEXT    <- "#e8e8e8"
COL_SUBTEXT <- "#888888"
COL_NEG     <- "#e05252"
COL_POS     <- "#52c28a"

# ── ggplot theme ────────────────────────────────────────────
theme_quant <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background   = element_rect(fill = COL_BG,    color = NA),
      panel.background  = element_rect(fill = COL_PANEL, color = NA),
      panel.grid.major  = element_line(color = "#252525", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      panel.border      = element_rect(color = COL_BORDER, fill = NA,
                                       linewidth = 0.6),
      axis.text         = element_text(color = COL_SUBTEXT, size = 9,
                                       family = "mono"),
      axis.title        = element_text(color = COL_TEXT, size = 9.5,
                                       family = "mono"),
      plot.title        = element_text(color = COL_TEXT, size = 11,
                                       face = "bold", family = "mono",
                                       margin = margin(b = 4)),
      plot.subtitle     = element_text(color = COL_SUBTEXT, size = 8.5,
                                       family = "mono",
                                       margin = margin(b = 8)),
      legend.background = element_rect(fill = COL_PANEL, color = NA),
      legend.text       = element_text(color = COL_SUBTEXT, size = 8.5,
                                       family = "mono"),
      strip.text        = element_text(color = COL_TEXT, size = 9,
                                       family = "mono"),
      plot.margin       = margin(10, 14, 8, 12)
    )
}

EQUITY_CHOICES <- c(
  "Apple Inc."      = "AAPL",
  "Microsoft Corp." = "MSFT",
  "Amazon.com Inc." = "AMZN",
  "Alphabet Inc."   = "GOOGL",
  "Visa Inc."       = "V",
  "NVIDIA Corp."    = "NVDA"
)

# ============================================================
#  UI
# ============================================================
ui <- fluidPage(

  tags$head(
    tags$style(HTML(paste0("

      @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@300;400;500;600&family=IBM+Plex+Sans:wght@300;400;500&display=swap');

      /* ── Full viewport lock ── */
      *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

      html, body {
        height: 100vh; width: 100vw;
        overflow: hidden;
        background: ", COL_BG, ";
        color: ", COL_TEXT, ";
        font-family: 'IBM Plex Sans', sans-serif;
      }

      /* Kill fluidPage padding */
      .container-fluid {
        padding: 0 !important;
        margin: 0 !important;
        height: 100vh !important;
        display: flex !important;
        flex-direction: column !important;
      }

      /* ══════════════════════════════════
         TOP HEADER BAR
      ══════════════════════════════════ */
      #qrt-header {
        flex-shrink: 0;
        height: 52px;
        background: #111;
        border-bottom: 1px solid ", COL_BORDER, ";
        display: flex;
        align-items: center;
        padding: 0 24px;
        gap: 14px;
      }
      .hdr-logo {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 15px; font-weight: 600;
        color: ", COL_YELLOW, ";
        letter-spacing: 0.08em;
        text-transform: uppercase;
      }
      .hdr-sub {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 9.5px; color: ", COL_SUBTEXT, ";
        letter-spacing: 0.12em; text-transform: uppercase;
      }
      .hdr-gap { flex: 1; }
      .hdr-dot {
        width: 7px; height: 7px; border-radius: 50%;
        background: ", COL_POS, ";
        display: inline-block; margin-right: 6px;
      }
      .hdr-live {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 10px; color: ", COL_SUBTEXT, ";
        text-transform: uppercase; letter-spacing: 0.1em;
      }

      /* ══════════════════════════════════
         BODY ROW  (sidebar | main)
      ══════════════════════════════════ */
      #qrt-body {
        flex: 1;
        display: flex;
        overflow: hidden;
        min-height: 0;
      }

      /* ── Sidebar ── */
      #qrt-sidebar {
        width: 220px;
        flex-shrink: 0;
        background: #111;
        border-right: 1px solid ", COL_BORDER, ";
        padding: 20px 16px;
        overflow-y: auto;
        display: flex;
        flex-direction: column;
      }
      .ctrl-lbl {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 9px; color: ", COL_SUBTEXT, ";
        text-transform: uppercase; letter-spacing: 0.15em;
        margin-bottom: 5px; margin-top: 16px; display: block;
      }
      .ctrl-lbl:first-child { margin-top: 0; }

      #qrt-sidebar .selectize-input {
        background: #1e1e1e !important;
        border: 1px solid ", COL_BORDER, " !important;
        border-radius: 3px !important;
        color: ", COL_TEXT, " !important;
        font-family: 'IBM Plex Mono', monospace !important;
        font-size: 12px !important; padding: 7px 10px !important;
        box-shadow: none !important;
      }
      #qrt-sidebar .selectize-input.focus {
        border-color: ", COL_YELLOW, " !important;
        box-shadow: 0 0 0 2px rgba(240,192,64,0.12) !important;
      }
      #qrt-sidebar .selectize-dropdown {
        background: #1e1e1e !important;
        border: 1px solid ", COL_BORDER, " !important;
        font-family: 'IBM Plex Mono', monospace !important;
        font-size: 12px !important;
      }
      #qrt-sidebar .selectize-dropdown-content .option {
        color: ", COL_TEXT, " !important; padding: 7px 10px !important;
      }
      #qrt-sidebar .selectize-dropdown-content .option:hover,
      #qrt-sidebar .selectize-dropdown-content .option.active {
        background: #2a2a2a !important; color: ", COL_YELLOW, " !important;
      }
      #qrt-sidebar .form-control {
        background: #1e1e1e !important;
        border: 1px solid ", COL_BORDER, " !important;
        border-radius: 3px !important; color: ", COL_TEXT, " !important;
        font-family: 'IBM Plex Mono', monospace !important;
        font-size: 11px !important; padding: 6px 10px !important;
      }
      #qrt-sidebar .form-control:focus {
        border-color: ", COL_YELLOW, " !important;
        box-shadow: 0 0 0 2px rgba(240,192,64,0.12) !important;
        outline: none !important;
      }
      #qrt-sidebar .input-group-btn .btn {
        background: #252525 !important;
        border: 1px solid ", COL_BORDER, " !important;
        color: ", COL_SUBTEXT, " !important;
      }
      #qrt-sidebar .shiny-input-container { width: 100% !important; }

      #btn-go {
        width: 100%; margin-top: 22px;
        background: ", COL_YELLOW, " !important; color: #0a0a0a !important;
        font-family: 'IBM Plex Mono', monospace !important;
        font-size: 11px !important; font-weight: 600 !important;
        letter-spacing: 0.14em !important; text-transform: uppercase !important;
        border: none !important; border-radius: 3px !important;
        padding: 10px 0 !important; cursor: pointer !important;
        transition: opacity 0.15s !important;
      }
      #btn-go:hover { opacity: 0.82 !important; }

      /* ── Main column ── */
      #qrt-main {
        flex: 1;
        display: flex;
        flex-direction: column;
        overflow: hidden;
        min-width: 0;
      }

      /* ── Metric strip ── */
      #qrt-metrics {
        flex-shrink: 0;
        height: 78px;
        display: flex;
        background: #111;
        border-bottom: 1px solid ", COL_BORDER, ";
      }
      .mc {
        flex: 1; padding: 12px 18px;
        border-right: 1px solid ", COL_BORDER, ";
      }
      .mc:last-child { border-right: none; }
      .mc-lbl {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 8.5px; color: ", COL_SUBTEXT, ";
        text-transform: uppercase; letter-spacing: 0.14em;
        margin-bottom: 5px;
      }
      .mc-val {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 20px; font-weight: 500;
        color: ", COL_TEXT, "; line-height: 1;
      }
      .mc-val.pos { color: ", COL_POS, "; }
      .mc-val.neg { color: ", COL_NEG, "; }

      /* ══════════════════════════════════
         HORIZONTAL TAB NAV
         Override Bootstrap list styling
         so tabs render as a horizontal bar
      ══════════════════════════════════ */
      #qrt-tabs {
        flex-shrink: 0;
        background: #111;
        border-bottom: 1px solid ", COL_BORDER, ";
        padding: 0 16px;
      }

      /* The ul.nav.nav-tabs rendered by Shiny */
      #qrt-tabs .nav.nav-tabs {
        display: flex !important;          /* horizontal row */
        flex-direction: row !important;
        flex-wrap: nowrap !important;
        list-style: none !important;       /* kill bullets */
        margin: 0 !important;
        padding: 0 !important;
        border-bottom: none !important;   /* we draw our own */
      }
      #qrt-tabs .nav.nav-tabs > li {
        list-style: none !important;       /* belt-and-braces */
        float: none !important;
        display: block !important;
        margin: 0 !important;
        padding: 0 !important;
      }
      #qrt-tabs .nav.nav-tabs > li > a {
        display: block !important;
        font-family: 'IBM Plex Mono', monospace !important;
        font-size: 10px !important;
        text-transform: uppercase !important;
        letter-spacing: 0.12em !important;
        color: ", COL_SUBTEXT, " !important;
        background: transparent !important;
        border: none !important;
        border-bottom: 2px solid transparent !important;
        padding: 12px 16px !important;
        border-radius: 0 !important;
        white-space: nowrap !important;
        text-decoration: none !important;
        transition: color 0.15s !important;
      }
      #qrt-tabs .nav.nav-tabs > li > a:hover {
        color: ", COL_TEXT, " !important;
        border-bottom-color: #444 !important;
        background: transparent !important;
      }
      #qrt-tabs .nav.nav-tabs > li.active > a,
      #qrt-tabs .nav.nav-tabs > li.active > a:hover,
      #qrt-tabs .nav.nav-tabs > li.active > a:focus {
        color: ", COL_YELLOW, " !important;
        background: transparent !important;
        border: none !important;
        border-bottom: 2px solid ", COL_YELLOW, " !important;
      }

      /* ── Tab content area — scrollable ── */
      #qrt-content {
        flex: 1;
        overflow-y: auto;
        min-height: 0;
        background: ", COL_BG, ";
      }
      /* Shiny wraps content in .tab-content > .tab-pane */
      #qrt-content .tab-content {
        height: auto !important;
        overflow: visible !important;
      }
      #qrt-content .tab-pane {
        height: auto !important;
        overflow: visible !important;
        display: none !important;
      }
      #qrt-content .tab-pane.active {
        display: block !important;
      }
      .tab-scroll {
        padding: 16px;
        background: ", COL_BG, ";
      }

      /* ── Plot cards ── */
      .pcard {
        background: ", COL_PANEL, ";
        border: 1px solid ", COL_BORDER, ";
        border-radius: 4px;
        margin-bottom: 14px;
        overflow: hidden;
      }
      .pcard:last-child { margin-bottom: 0; }
      .pcard-hdr {
        padding: 9px 14px;
        border-bottom: 1px solid ", COL_BORDER, ";
        display: flex; align-items: baseline;
        gap: 10px; flex-wrap: wrap;
      }
      .pcard-title {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 10px; font-weight: 600; color: ", COL_TEXT, ";
        text-transform: uppercase; letter-spacing: 0.12em;
        white-space: nowrap;
      }
      .pcard-desc {
        font-family: 'IBM Plex Sans', sans-serif;
        font-size: 10.5px; color: #666; font-style: italic;
      }
      .pcard-body { padding: 4px 6px 6px; }

      .grid-2 {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 14px;
      }

      /* Performance table */
      .ptbl-wrap { padding: 6px; }
      .ptbl {
        width: 100%; border-collapse: collapse;
        font-family: 'IBM Plex Mono', monospace; font-size: 12px;
      }
      .ptbl th {
        background: #141414; color: ", COL_SUBTEXT, ";
        font-weight: 500; font-size: 9px;
        text-transform: uppercase; letter-spacing: 0.12em;
        padding: 9px 14px; border-bottom: 1px solid ", COL_BORDER, ";
        text-align: left;
      }
      .ptbl td {
        padding: 9px 14px; color: ", COL_TEXT, ";
        border-bottom: 1px solid #1f1f1f;
      }
      .ptbl tr:last-child td { border-bottom: none; }
      .ptbl tr:hover td { background: #1f1f1f; }

      /* Thin scrollbars */
      ::-webkit-scrollbar { width: 5px; height: 5px; }
      ::-webkit-scrollbar-track { background: transparent; }
      ::-webkit-scrollbar-thumb { background: #333; border-radius: 3px; }
      ::-webkit-scrollbar-thumb:hover { background: #444; }

    ")))
  ),

  # ── Header ────────────────────────────────────────────────
  div(id = "qrt-header",
      div(
        div(class = "hdr-logo",
            "QRT \u2014 Quantitative Research Terminal"),
        div(class = "hdr-sub",
            "Equity Analytics Dashboard \u00b7 v2.2")
      ),
      div(class = "hdr-gap"),
      span(class = "hdr-dot"),
      span(class = "hdr-live", "Live Data \u00b7 Yahoo Finance")
  ),

  # ── Body ──────────────────────────────────────────────────
  div(id = "qrt-body",

    # Sidebar
    div(id = "qrt-sidebar",
        span(class = "ctrl-lbl", "Equity"),
        selectInput("ticker", NULL,
                    choices = EQUITY_CHOICES, selected = "AAPL",
                    width = "100%"),
        span(class = "ctrl-lbl", "From"),
        dateInput("date_from", NULL,
                  value = "2015-01-01", format = "yyyy-mm-dd",
                  width = "100%"),
        span(class = "ctrl-lbl", "To"),
        dateInput("date_to", NULL,
                  value = Sys.Date(), format = "yyyy-mm-dd",
                  width = "100%"),
        span(class = "ctrl-lbl", "Roll Window (days)"),
        numericInput("roll_window", NULL,
                     value = 21, min = 5, max = 252, step = 1,
                     width = "100%"),
        tags$button(
          id = "go", type = "button",
          class = "btn action-button",
          style = paste0(
            "width:100%;margin-top:22px;",
            "background:", COL_YELLOW, ";color:#0a0a0a;",
            "font-family:'IBM Plex Mono',monospace;",
            "font-size:11px;font-weight:600;",
            "letter-spacing:0.14em;text-transform:uppercase;",
            "border:none;border-radius:3px;",
            "padding:10px 0;cursor:pointer;"
          ),
          "ANALYZE"
        )
    ),

    # Main
    div(id = "qrt-main",

      # Metric strip
      div(id = "qrt-metrics",
          div(class = "mc",
              div(class = "mc-lbl", "Last Price"),
              div(class = "mc-val", uiOutput("m_price"))),
          div(class = "mc",
              div(class = "mc-lbl", "Annualised Return"),
              div(class = "mc-val", uiOutput("m_ann_ret"))),
          div(class = "mc",
              div(class = "mc-lbl", "Ann. Volatility"),
              div(class = "mc-val", uiOutput("m_vol"))),
          div(class = "mc",
              div(class = "mc-lbl", "Sharpe Ratio"),
              div(class = "mc-val", uiOutput("m_sharpe"))),
          div(class = "mc",
              div(class = "mc-lbl", "Max Drawdown"),
              div(class = "mc-val", uiOutput("m_mdd"))),
          div(class = "mc",
              div(class = "mc-lbl", "Total Return"),
              div(class = "mc-val", uiOutput("m_total")))
      ),

      # Tab nav — in its own div so we can target CSS precisely
      div(id = "qrt-tabs",
          tabsetPanel(
            id = "main_tabs",
            type = "tabs",

            tabPanel("Price & Returns",   value = "tab_pr",  uiOutput("dummy_pr")),
            tabPanel("Volatility",        value = "tab_vol", uiOutput("dummy_vol")),
            tabPanel("Drawdowns & Cumul.",value = "tab_dd",  uiOutput("dummy_dd")),
            tabPanel("Distribution",      value = "tab_dist",uiOutput("dummy_dist")),
            tabPanel("Performance",       value = "tab_perf",uiOutput("dummy_perf"))
          )
      ),

      # Content area — rendered separately so it has its own flex slot
      div(id = "qrt-content",
          # We manually show/hide content based on the active tab
          # by rendering all panes and using CSS display logic via
          # conditionalPanel — simpler: just render all in one scrollable
          # area and use tabsetPanel's built-in show/hide.
          # The trick: we move .tab-content OUT of #qrt-tabs via JS.
          tags$script(HTML("
            // After Shiny renders, move .tab-content from #qrt-tabs
            // into #qrt-content so the nav stays in its own row
            // and the content fills the remaining flex space.
            $(document).on('shiny:sessioninitialized', function() {
              $('#qrt-tabs .tab-content').appendTo('#qrt-content');
            });
          "))
      )
    ) # qrt-main
  )   # qrt-body
)     # fluidPage

# ── Tab content builders ─────────────────────────────────────
tab_price_returns <- function() {
  div(class = "tab-scroll",
    div(class = "pcard",
        div(class = "pcard-hdr",
            span(class = "pcard-title", "Price Series"),
            span(class = "pcard-desc",
                 "Adjusted closing prices. Reflects dividends and stock splits.")),
        div(class = "pcard-body",
            plotOutput("price_plot", height = "240px"))
    ),
    div(class = "pcard",
        div(class = "pcard-hdr",
            span(class = "pcard-title", "Percentage Returns"),
            span(class = "pcard-desc",
                 "Daily log returns (%). Stationarity is required by most financial econometric models.")),
        div(class = "pcard-body",
            plotOutput("returns_plot", height = "240px"))
    )
  )
}

tab_volatility <- function() {
  div(class = "tab-scroll",
    div(class = "pcard",
        div(class = "pcard-hdr",
            span(class = "pcard-title", "Rolling Volatility"),
            span(class = "pcard-desc",
                 "Annualised rolling standard deviation of log returns. Captures time-varying risk regimes.")),
        div(class = "pcard-body",
            plotOutput("vol_plot", height = "240px"))
    ),
    div(class = "pcard",
        div(class = "pcard-hdr",
            span(class = "pcard-title", "Rolling Sharpe Ratio"),
            span(class = "pcard-desc",
                 "Risk-adjusted return metric over a rolling window. Zero risk-free rate assumed.")),
        div(class = "pcard-body",
            plotOutput("sharpe_plot", height = "240px"))
    )
  )
}

tab_drawdowns <- function() {
  div(class = "tab-scroll",
    div(class = "pcard",
        div(class = "pcard-hdr",
            span(class = "pcard-title", "Cumulative Return"),
            span(class = "pcard-desc",
                 "Compounded growth of a $1 investment. Based on the log return series.")),
        div(class = "pcard-body",
            plotOutput("cumret_plot", height = "240px"))
    ),
    div(class = "pcard",
        div(class = "pcard-hdr",
            span(class = "pcard-title", "Drawdown Series"),
            span(class = "pcard-desc",
                 "Percentage decline from each rolling peak. Measures depth and duration of loss periods.")),
        div(class = "pcard-body",
            plotOutput("drawdown_plot", height = "240px"))
    )
  )
}

tab_distribution <- function() {
  div(class = "tab-scroll",
    div(class = "grid-2",
      div(class = "pcard",
          div(class = "pcard-hdr",
              span(class = "pcard-title", "Return Histogram"),
              span(class = "pcard-desc",
                   "Empirical density overlaid with a fitted normal curve.")),
          div(class = "pcard-body",
              plotOutput("hist_plot", height = "310px"))
      ),
      div(class = "pcard",
          div(class = "pcard-hdr",
              span(class = "pcard-title", "Q-Q Plot"),
              span(class = "pcard-desc",
                   "Quantile-quantile plot vs. normal. Deviation indicates fat tails.")),
          div(class = "pcard-body",
              plotOutput("qq_plot", height = "310px"))
      )
    )
  )
}

tab_performance <- function() {
  div(class = "tab-scroll",
    div(class = "pcard",
        div(class = "pcard-hdr",
            span(class = "pcard-title", "Annualised Performance Statistics"),
            span(class = "pcard-desc",
                 "Standard risk and return metrics used in quantitative portfolio analysis.")),
        div(class = "ptbl-wrap",
            tableOutput("perf_table"))
    )
  )
}

# ============================================================
#  SERVER
# ============================================================
server <- function(input, output, session) {

  # Inject tab content into their panes
  output$dummy_pr   <- renderUI(tab_price_returns())
  output$dummy_vol  <- renderUI(tab_volatility())
  output$dummy_dd   <- renderUI(tab_drawdowns())
  output$dummy_dist <- renderUI(tab_distribution())
  output$dummy_perf <- renderUI(tab_performance())

  # ── Data ────────────────────────────────────────────────
  raw_data <- eventReactive(input$go, {
    req(input$ticker, input$date_from, input$date_to)
    withProgress(message = "Fetching market data\u2026", value = 0.5, {
      tryCatch(
        getSymbols(input$ticker, src = "yahoo",
                   from = input$date_from, to = input$date_to,
                   auto.assign = FALSE),
        error = function(e) {
          showNotification(paste("Data error:", e$message), type = "error")
          NULL
        }
      )
    })
  })

  log_ret <- reactive({
    req(raw_data())
    na.omit(Return.calculate(Cl(raw_data()), method = "log"))
  })

  xts_to_df <- function(x, col) {
    data.frame(date  = as.Date(index(x)),
               v     = as.numeric(coredata(x))) |>
      setNames(c("date", col))
  }

  # ── Metrics ─────────────────────────────────────────────
  metrics <- reactive({
    req(log_ret(), raw_data())
    r <- log_ret()
    list(
      ann    = as.numeric(Return.annualized(r, scale = 252)),
      vol    = as.numeric(sd(r, na.rm = TRUE)) * sqrt(252),
      sr     = as.numeric(Return.annualized(r, scale = 252)) /
               (as.numeric(sd(r, na.rm = TRUE)) * sqrt(252)),
      mdd    = as.numeric(maxDrawdown(r)),
      total  = as.numeric(prod(1 + r) - 1),
      last_p = as.numeric(last(Cl(raw_data())))
    )
  })

  fp  <- function(x, d = 2) paste0(ifelse(x >= 0, "+", ""), round(x*100, d), "%")
  fn  <- function(x, d = 2) round(x, d)
  cls <- function(x) if (x >= 0) "pos" else "neg"

  output$m_price   <- renderUI({ req(metrics()); HTML(paste0("$", fn(metrics()$last_p))) })
  output$m_ann_ret <- renderUI({ req(metrics()); v <- metrics()$ann
    HTML(paste0("<span class='", cls(v), "'>", fp(v), "</span>")) })
  output$m_vol     <- renderUI({ req(metrics()); HTML(fp(metrics()$vol)) })
  output$m_sharpe  <- renderUI({ req(metrics()); v <- metrics()$sr
    HTML(paste0("<span class='", cls(v), "'>", fn(v, 3), "</span>")) })
  output$m_mdd     <- renderUI({ req(metrics())
    HTML(paste0("<span class='neg'>", fp(-abs(metrics()$mdd)), "</span>")) })
  output$m_total   <- renderUI({ req(metrics()); v <- metrics()$total
    HTML(paste0("<span class='", cls(v), "'>", fp(v, 1), "</span>")) })

  # ── Plots ────────────────────────────────────────────────
  output$price_plot <- renderPlot({
    req(raw_data())
    df <- xts_to_df(Cl(raw_data()), "price")
    ggplot(df, aes(date, price)) +
      geom_line(color = COL_YELLOW, linewidth = 0.7) +
      scale_y_continuous(labels = dollar_format(prefix = "$"),
                         expand = expansion(mult = c(0.02, 0.05))) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                   expand = expansion(mult = 0.01)) +
      labs(x = NULL, y = "Adjusted Close",
           title = paste(input$ticker, "\u2014 Adjusted Closing Price")) +
      theme_quant()
  }, bg = COL_BG)

  output$returns_plot <- renderPlot({
    req(log_ret())
    df <- xts_to_df(log_ret() * 100, "ret")
    ggplot(df, aes(date, ret, fill = ret >= 0)) +
      geom_col(width = 1, show.legend = FALSE) +
      scale_fill_manual(values = c("TRUE" = COL_YELLOW, "FALSE" = COL_NEG)) +
      scale_y_continuous(labels = function(x) paste0(x, "%"),
                         expand = expansion(mult = c(0.05, 0.05))) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                   expand = expansion(mult = 0.01)) +
      labs(x = NULL, y = "Returns (%)",
           title = paste(input$ticker, "\u2014 Daily Log Returns")) +
      theme_quant()
  }, bg = COL_BG)

  output$vol_plot <- renderPlot({
    req(log_ret())
    w  <- max(5L, as.integer(input$roll_window))
    rv <- rollapply(log_ret(), width = w,
                    FUN = function(x) sd(x) * sqrt(252),
                    fill = NA, align = "right")
    df <- xts_to_df(rv * 100, "vol")
    ggplot(df, aes(date, vol)) +
      geom_line(color = COL_YELLOW, linewidth = 0.75) +
      geom_area(fill = COL_YELLOW, alpha = 0.08) +
      scale_y_continuous(labels = function(x) paste0(x, "%"),
                         expand = expansion(mult = c(0, 0.08))) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                   expand = expansion(mult = 0.01)) +
      labs(x = NULL, y = "Ann. Volatility (%)",
           title = paste0(w, "-Day Rolling Annualised Volatility")) +
      theme_quant()
  }, bg = COL_BG)

  output$sharpe_plot <- renderPlot({
    req(log_ret())
    w  <- max(5L, as.integer(input$roll_window)) * 10L
    r  <- log_ret()
    rs <- rollapply(r, width = w,
                    FUN = function(x) {
                      v <- sd(x, na.rm = TRUE) * sqrt(252)
                      if (v == 0) NA_real_
                      else (mean(x, na.rm = TRUE) * 252) / v
                    },
                    fill = NA, align = "right")
    df <- xts_to_df(rs, "sr")
    df <- df[!is.na(df$sr), ]
    ggplot(df, aes(date, sr, color = sr >= 0)) +
      geom_line(linewidth = 0.75, show.legend = FALSE) +
      geom_hline(yintercept = 0, color = "#444", linewidth = 0.4,
                 linetype = "dashed") +
      scale_color_manual(values = c("TRUE" = COL_YELLOW, "FALSE" = COL_NEG)) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                   expand = expansion(mult = 0.01)) +
      labs(x = NULL, y = "Sharpe Ratio",
           title = paste0(w, "-Day Rolling Sharpe Ratio")) +
      theme_quant()
  }, bg = COL_BG)

  output$cumret_plot <- renderPlot({
    req(log_ret())
    cum <- cumprod(1 + log_ret()) - 1
    df  <- xts_to_df(cum * 100, "cr")
    ggplot(df, aes(date, cr)) +
      geom_line(color = COL_YELLOW, linewidth = 0.8) +
      geom_area(fill = COL_YELLOW, alpha = 0.07) +
      geom_hline(yintercept = 0, color = "#444", linewidth = 0.35,
                 linetype = "dashed") +
      scale_y_continuous(labels = function(x) paste0(x, "%"),
                         expand = expansion(mult = c(0.05, 0.08))) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                   expand = expansion(mult = 0.01)) +
      labs(x = NULL, y = "Cumulative Return (%)",
           title = paste(input$ticker, "\u2014 Cumulative Return")) +
      theme_quant()
  }, bg = COL_BG)

  output$drawdown_plot <- renderPlot({
    req(log_ret())
    dd <- Drawdowns(log_ret())
    df <- xts_to_df(dd * 100, "dd")
    ggplot(df, aes(date, dd)) +
      geom_line(color = COL_NEG, linewidth = 0.65) +
      geom_area(fill = COL_NEG, alpha = 0.15) +
      scale_y_continuous(labels = function(x) paste0(x, "%"),
                         expand = expansion(mult = c(0.08, 0.01))) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                   expand = expansion(mult = 0.01)) +
      labs(x = NULL, y = "Drawdown (%)",
           title = paste(input$ticker, "\u2014 Drawdown from Peak")) +
      theme_quant()
  }, bg = COL_BG)

  output$hist_plot <- renderPlot({
    req(log_ret())
    rv <- as.numeric(log_ret()) * 100
    df <- data.frame(ret = rv)
    mu <- mean(rv); s <- sd(rv)
    ggplot(df, aes(ret)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 60, fill = COL_YELLOW, alpha = 0.6, color = NA) +
      stat_function(fun = dnorm, args = list(mean = mu, sd = s),
                    color = "#ffffff", linewidth = 0.8, linetype = "dashed") +
      scale_x_continuous(labels = function(x) paste0(x, "%")) +
      labs(x = "Daily Log Return (%)", y = "Density",
           title = "Return Distribution",
           subtitle = paste0("\u03bc = ", round(mu, 3),
                             "%   \u03c3 = ", round(s, 3), "%")) +
      theme_quant()
  }, bg = COL_BG)

  output$qq_plot <- renderPlot({
    req(log_ret())
    rv <- as.numeric(log_ret())
    df <- data.frame(sample      = sort(rv),
                     theoretical = qnorm(ppoints(length(rv))))
    ggplot(df, aes(theoretical, sample)) +
      geom_abline(slope = sd(rv), intercept = mean(rv),
                  color = "#555", linewidth = 0.6, linetype = "dashed") +
      geom_point(color = COL_YELLOW, size = 0.7, alpha = 0.55) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
           title = "Normal Q-Q Plot",
           subtitle = "Tail divergence indicates excess kurtosis") +
      theme_quant()
  }, bg = COL_BG)

  output$perf_table <- renderTable({
    req(log_ret())
    r       <- log_ret()
    ann_ret <- as.numeric(Return.annualized(r, scale = 252))
    ann_vol <- as.numeric(sd(r, na.rm = TRUE)) * sqrt(252)
    sr      <- ann_ret / ann_vol
    mdd     <- as.numeric(maxDrawdown(r))
    skw     <- as.numeric(skewness(r))
    krt     <- as.numeric(kurtosis(r))
    var95   <- as.numeric(quantile(r, 0.05))
    var99   <- as.numeric(quantile(r, 0.01))
    es95    <- mean(r[r <= var95])
    total_r <- as.numeric(prod(1 + r) - 1)
    n_days  <- nrow(r)

    data.frame(
      Metric = c("Annualised Return","Annualised Volatility",
                 "Sharpe Ratio (Rf=0)","Max Drawdown","Total Return",
                 "Skewness","Excess Kurtosis","VaR 95% (daily)",
                 "VaR 99% (daily)","Expected Shortfall 95%","Trading Days"),
      Value  = c(
        paste0(round(ann_ret*100,2),"%"), paste0(round(ann_vol*100,2),"%"),
        round(sr,3), paste0(round(-abs(mdd)*100,2),"%"),
        paste0(round(total_r*100,2),"%"), round(skw,4), round(krt,4),
        paste0(round(var95*100,3),"%"), paste0(round(var99*100,3),"%"),
        paste0(round(es95*100,3),"%"), n_days
      ),
      stringsAsFactors = FALSE
    )
  },
  striped = FALSE, hover = FALSE, bordered = FALSE,
  spacing = "m", width = "100%", align = "lr", rownames = FALSE,
  sanitize.text.function = identity,
  container = tags$table(class = "ptbl")
  )

} # end server

shinyApp(ui = ui, server = server)
  
