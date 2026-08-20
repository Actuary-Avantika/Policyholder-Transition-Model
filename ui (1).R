# ============================================================
# UI.R
# Policyholder Transition Model
# ============================================================

ui <- fluidPage(

  # ----------------------------------------------------------
  # CUSTOM CSS
  # ----------------------------------------------------------

  tags$head(

    tags$title(
      "Policyholder Transition Model"
    ),

    tags$style(HTML("

      /* =====================================================
         GLOBAL
         ===================================================== */

      html, body {
        margin: 0;
        padding: 0;
        background: #F6F7F4;
        color: #303638;
        font-family: 'Segoe UI', Arial, sans-serif;
      }

      body {
        overflow-x: hidden;
      }

      .container-fluid {
        padding-left: 28px;
        padding-right: 28px;
      }


      /* =====================================================
         NAVIGATION
         ===================================================== */

      .navbar {
        background: #1D6F69 !important;
        border: none !important;
        border-radius: 0 !important;
        margin-bottom: 0 !important;
        min-height: 84px;
        box-shadow: 0 4px 15px rgba(48,54,56,0.10);
      }

      .navbar-header {
        float: left;
      }

      .navbar-brand {
        color: white !important;
        font-size: 25px !important;
        line-height: 1.15 !important;
        font-weight: 700 !important;
        letter-spacing: 0.2px;
        padding-top: 27px !important;
        padding-bottom: 27px !important;
      }

      /* Push the nav links to the right-hand side of the bar,
         away from the brand, and give each item its own
         breathing room instead of one solid green block. */
      .navbar-nav {
        float: right !important;
        margin-right: 4px;
      }

      .navbar-nav > li {
        float: left;
        margin-left: 4px;
      }

      .navbar-nav > li > a {
        color: rgba(255,255,255,0.92) !important;
        font-size: 12.5px;
        font-weight: 600;
        letter-spacing: 0.2px;
        text-transform: uppercase;
        border-radius: 8px;
        padding: 9px 13px !important;
        margin-top: 22px;
        transition: 0.2s ease;
      }

      .navbar-nav > li > a:hover {
        color: white !important;
        background: rgba(255,255,255,0.14) !important;
      }

      .navbar-nav > .active > a,
      .navbar-nav > .active > a:hover,
      .navbar-nav > .active > a:focus {
        color: #123F3C !important;
        background: #E6BD66 !important;
      }


      /* =====================================================
         MAIN CONTENT
         ===================================================== */

      .main-container {
        padding-top: 30px;
        padding-bottom: 50px;
      }


      /* =====================================================
         PAGE HEADINGS
         ===================================================== */

      .page-title {
        color: #123F3C;
        font-size: 38px;
        font-weight: 700;
        margin-bottom: 8px;
      }

      .page-subtitle {
        color: #64716F;
        font-size: 17px;
        margin-bottom: 28px;
      }

      /* Overview hero: the navbar brand already states the app
         name, so the lead line here just sets context and gets
         a little extra top space + a max-width for readability. */
      .overview-hero {
        padding-top: 44px;
      }

      .overview-lead {
        max-width: 720px;
        font-size: 19px;
        margin-bottom: 34px;
      }

      h2 {
        color: #123F3C;
        font-weight: 700;
      }

      h3 {
        color: #123F3C;
        font-weight: 650;
      }


      /* =====================================================
         CARDS
         ===================================================== */

      .custom-card {
        background: #FFFFFF;
        border: 1px solid #E1E7E3;
        border-radius: 16px;
        padding: 28px 30px;
        margin-bottom: 24px;
        box-shadow: 0 5px 18px rgba(48,54,56,0.06);
        transition: box-shadow 0.2s ease, transform 0.2s ease;
      }

      .custom-card:hover {
        box-shadow: 0 8px 24px rgba(48,54,56,0.09);
      }

      .custom-card h2 {
        margin-top: 0;
        font-size: 22px;
        margin-bottom: 16px;
      }

      .custom-card p {
        line-height: 1.65;
        color: #454C4E;
      }


      /* =====================================================
         KPI CARDS
         ===================================================== */

      .kpi-card {
        background: #FFFFFF;
        border: 1px solid #E1E7E3;
        border-radius: 15px;
        padding: 22px;
        min-height: 135px;
        box-shadow: 0 5px 18px rgba(48,54,56,0.05);
        margin-bottom: 20px;
        transition: transform 0.2s ease, box-shadow 0.2s ease;
      }

      .kpi-card:hover {
        transform: translateY(-3px);
        box-shadow: 0 10px 22px rgba(48,54,56,0.10);
      }

      .kpi-label {
        color: #B98B2E;
        font-size: 14px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }

      .kpi-value {
        color: #123F3C;
        font-size: 31px;
        font-weight: 700;
        margin-top: 8px;
      }

      .kpi-description {
        color: #737E7C;
        font-size: 13px;
        margin-top: 5px;
      }


      /* =====================================================
         FORMULA BOX
         ===================================================== */

      .formula-box {
        background: #F0F4F2;
        border: 1px solid #DCE7E2;
        border-radius: 12px;
        padding: 22px;
        text-align: center;
        color: #123F3C;
        font-size: 20px;
        margin: 18px 0;
      }


      /* =====================================================
         INFO BOX
         ===================================================== */

      .info-box {
        background: #EEF5F2;
        border-left: 5px solid #1D6F69;
        border-radius: 10px;
        padding: 18px 20px;
        margin: 20px 0;
      }

      .warning-box {
        background: #FBF2DE;
        border-left: 5px solid #B98B2E;
        border-radius: 10px;
        padding: 18px 20px;
        margin: 20px 0;
      }


      /* =====================================================
         TABLES
         ===================================================== */

      .table {
        background: white;
        border-radius: 10px;
        overflow: hidden;
      }

      .table > thead > tr > th {
        background: #EEF3F0 !important;
        color: #123F3C !important;
        font-weight: 700;
        border-bottom: 2px solid #D5DFDB !important;
      }

      .table > tbody > tr:hover {
        background: #F5F8F6 !important;
      }

      .table td,
      .table th {
        padding: 12px !important;
      }


      /* =====================================================
         INPUTS
         ===================================================== */

      .form-control {
        border: 1px solid #D5DFDB !important;
        border-radius: 9px !important;
        box-shadow: none !important;
      }

      .form-control:focus {
        border-color: #1D6F69 !important;
        box-shadow: 0 0 0 3px rgba(29,111,105,0.12) !important;
      }

      label {
        color: #123F3C;
        font-weight: 600;
      }


      /* =====================================================
         BUTTONS
         ===================================================== */

      .btn-primary {
        background: #1D6F69 !important;
        border-color: #1D6F69 !important;
        border-radius: 9px !important;
        font-weight: 600;
      }

      .btn-primary:hover {
        background: #145951 !important;
        border-color: #145951 !important;
      }

      .btn-outline {
        background: transparent !important;
        color: #1D6F69 !important;
        border: 1px solid #1D6F69 !important;
        border-radius: 9px !important;
        font-weight: 600;
      }

      .btn-outline:hover {
        background: #EEF5F2 !important;
      }

      .action-row {
        margin-bottom: 18px;
      }

      .action-row .btn {
        margin-right: 10px;
      }


      /* =====================================================
         STATE BADGES
         ===================================================== */

      .state-badge {
        display: inline-block;
        padding: 6px 11px;
        border-radius: 20px;
        font-size: 12px;
        font-weight: 600;
        margin: 3px;
      }

      .active-badge {
        background: #DCEEEB;
        color: #145951;
      }

      .claim-badge {
        background: #F3E6E4;
        color: #8C4A44;
      }

      .lapsed-badge {
        background: #F7EDD3;
        color: #8A6A1E;
      }

      .closed-badge {
        background: #E9EAEB;
        color: #5F6668;
      }


      /* =====================================================
         FOOTER
         ===================================================== */

      .footer {
        text-align: center;
        color: #7A8583;
        padding: 30px 0;
        font-size: 13px;
      }


      /* =====================================================
         PLOTS
         ===================================================== */

      .plot-container {
        background: white;
        border-radius: 14px;
        border: 1px solid #E1E7E3;
        padding: 15px;
      }


      /* =====================================================
         SCROLLBAR
         ===================================================== */

      ::-webkit-scrollbar {
        width: 9px;
      }

      ::-webkit-scrollbar-track {
        background: #F1F3F1;
      }

      ::-webkit-scrollbar-thumb {
        background: #8FB5B0;
        border-radius: 10px;
      }

      ::-webkit-scrollbar-thumb:hover {
        background: #1D6F69;
      }


      /* =====================================================
         MOBILE
         ===================================================== */

      @media (max-width: 768px) {

        .navbar-brand {
          font-size: 22px !important;
          padding-top: 16px !important;
          padding-bottom: 12px !important;
        }

        .navbar-nav {
          float: none !important;
          margin-right: 0;
        }

        .navbar-nav > li {
          float: none;
          margin-left: 0;
        }

        .navbar-nav > li > a {
          margin-top: 0;
          border-radius: 0;
          padding: 14px 18px !important;
        }

        .page-title {
          font-size: 28px;
        }

        .overview-hero {
          padding-top: 26px;
        }

        .overview-lead {
          font-size: 17px;
        }

        .container-fluid {
          padding-left: 15px;
          padding-right: 15px;
        }

      }

    "))
  ),


  # ----------------------------------------------------------
  # NAVBAR
  # ----------------------------------------------------------

  navbarPage(

    title = "Policyholder Transition Model",

    id = "main_nav",

    # ========================================================
    # OVERVIEW
    # ========================================================

    tabPanel(

      title = "Overview",

      div(
        class = "main-container overview-hero",

        div(
          class = "page-subtitle overview-lead",
          "A discrete-time Markov Chain framework for analysing policyholder movements across an insurance portfolio."
        ),

        # ----------------------------------------------------
        # KPI ROW
        # ----------------------------------------------------

        fluidRow(

          column(
            width = 3,

            div(
              class = "kpi-card",

              div(
                class = "kpi-label",
                "Number of States"
              ),

              div(
                class = "kpi-value",
                "4"
              ),

              div(
                class = "kpi-description",
                "Policyholder states"
              )
            )
          ),

          column(
            width = 3,

            div(
              class = "kpi-card",

              div(
                class = "kpi-label",
                "Model Type"
              ),

              div(
                class = "kpi-value",
                "DTMC"
              ),

              div(
                class = "kpi-description",
                "Discrete-time Markov Chain"
              )
            )
          ),

          column(
            width = 3,

            div(
              class = "kpi-card",

              div(
                class = "kpi-label",
                "Initial Active"
              ),

              div(
                class = "kpi-value",
                "100%"
              ),

              div(
                class = "kpi-description",
                "Default starting portfolio"
              )
            )
          ),

          column(
            width = 3,

            div(
              class = "kpi-card",

              div(
                class = "kpi-label",
                "Absorbing State"
              ),

              div(
                class = "kpi-value",
                "Closed"
              ),

              div(
                class = "kpi-description",
                "Cannot be left once entered"
              )
            )
          )
        ),


        # ----------------------------------------------------
        # MODEL DESCRIPTION
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("What does this model do?"),

          p(
            "This model represents an insurance policyholder portfolio ",
            strong("as a discrete-time Markov Chain.")
          ),

          p(
            "At each observation period, a policyholder can move between ",
            "different states such as remaining active, entering a claim state, ",
            "lapsing or becoming closed."
          ),

          div(
            class = "info-box",

            strong("Key idea: "),

            "The probability of the next state depends on the ",
            "policyholder's current state."
          )
        ),


        # ----------------------------------------------------
        # STATES
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("Model states"),

          fluidRow(

            column(
              width = 3,

              div(
                class = "kpi-card",

                span(
                  class = "state-badge active-badge",
                  "ACTIVE"
                ),

                p(
                  "Policy remains in force."
                )
              )
            ),

            column(
              width = 3,

              div(
                class = "kpi-card",

                span(
                  class = "state-badge claim-badge",
                  "CLAIM"
                ),

                p(
                  "Policyholder is in a claim-related state."
                )
              )
            ),

            column(
              width = 3,

              div(
                class = "kpi-card",

                span(
                  class = "state-badge lapsed-badge",
                  "LAPSED"
                ),

                p(
                  "Policy has lapsed."
                )
              )
            ),

            column(
              width = 3,

              div(
                class = "kpi-card",

                span(
                  class = "state-badge closed-badge",
                  "CLOSED"
                ),

                p(
                  "Policy has reached the absorbing state."
                )
              )
            )
          )
        ),


        # ----------------------------------------------------
        # APPLICATION
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("Actuarial application"),

          p(
            "A transition model of this type can be used to project the ",
            "future composition of an insurance portfolio."
          ),

          p(
            "For example, an insurer could use historical policyholder ",
            "experience to estimate transition probabilities and then ",
            "project the expected number of active, claiming, lapsed and ",
            "closed policies over time."
          )
        )

        # Footer is added globally below the navigation.
      )
    ),


    # ========================================================
    # MODEL INPUTS
    # ========================================================

    tabPanel(

      title = "Model Inputs",

      div(
        class = "main-container",

        div(
          class = "page-title",
          "Model Inputs"
        ),

        div(
          class = "page-subtitle",
          "Define the transition structure and initial portfolio."
        ),

        div(
          class = "custom-card",

          h2("Transition matrix"),

          p(
            "Rows represent the current state and columns represent the ",
            "next state."
          ),

          div(
            class = "formula-box",

            HTML(
              "P<sub>ij</sub> = P(X<sub>t+1</sub> = j | X<sub>t</sub> = i)"
            )
          ),

          tableOutput("input_matrix")
        ),


        div(
          class = "custom-card",

          h2("Initial distribution"),

          div(
            class = "formula-box",

            HTML(
              "&pi;<sub>0</sub> = (1, 0, 0, 0)"
            )
          ),

          p(
            "The default model begins with 100% of policyholders in ",
            strong("Active")
            , "."
          ),

          tableOutput("initial_table")
        ),


        div(
          class = "custom-card",

          h2("Model assumptions"),

          tags$ul(

            tags$li(
              "Transition probabilities remain constant over the projection period."
            ),

            tags$li(
              "The model is discrete-time."
            ),

            tags$li(
              "The future state depends on the current state through the specified transition probabilities."
            ),

            tags$li(
              "The base probabilities used here are hypothetical — see ",
              strong("Data & Estimation"),
              " for how they could instead be estimated from observed experience."
            )

          )
        )
      )
    ),


    # ========================================================
    # DATA & ESTIMATION  (NEW)
    # ========================================================

    tabPanel(

      title = "Data & Estimation",

      div(
        class = "main-container",

        div(
          class = "page-title",
          "Data & Estimation"
        ),

        div(
          class = "page-subtitle",
          "How a transition matrix would be estimated from observed policyholder experience."
        ),

        div(
          class = "info-box",

          strong("Why this matters: "),

          "The transition matrix used elsewhere in this app is assumed. In practice, ",
          "an insurer would estimate it from historical policy records. This tab simulates ",
          "a synthetic policyholder panel under the assumed matrix, then re-estimates the ",
          "matrix from that simulated data using the standard maximum-likelihood estimator ",
          "for a time-homogeneous Markov chain."
        ),

        div(
          class = "custom-card",

          h2("Simulate a policyholder panel"),

          fluidRow(

            column(
              width = 4,

              numericInput(
                inputId = "sim_n_policyholders",
                label = "Number of policyholders",
                value = 1000,
                min = 50,
                max = 5000,
                step = 50
              )
            ),

            column(
              width = 4,

              numericInput(
                inputId = "sim_n_periods",
                label = "Number of periods observed",
                value = 10,
                min = 1,
                max = 30,
                step = 1
              )
            ),

            column(
              width = 4,

              numericInput(
                inputId = "sim_seed",
                label = "Random seed",
                value = 123,
                min = 1,
                max = 100000,
                step = 1
              )
            )
          ),

          div(
            class = "action-row",

            actionButton(
              inputId = "run_simulation",
              label = "Simulate & re-estimate",
              class = "btn-primary"
            )
          ),

          p(
            "Each policyholder starts Active and moves between states according to the ",
            "assumed base matrix. The estimator then counts every observed ",
            HTML("i &rarr; j"),
            " move and divides by the total moves out of state i:"
          ),

          div(
            class = "formula-box",

            HTML(
              "P&#770;<sub>ij</sub> = N<sub>ij</sub> / N<sub>i&middot;</sub>"
            )
          )
        ),

        div(
          class = "custom-card",

          h2("Assumed vs. estimated transition matrix"),

          fluidRow(

            column(
              width = 6,

              h3("Assumed (base_matrix)"),

              tableOutput("assumed_matrix_display")
            ),

            column(
              width = 6,

              h3("Estimated from simulated data"),

              tableOutput("estimated_matrix_display")
            )
          ),

          uiOutput("estimation_note")
        )
      )
    ),


    # ========================================================
    # PORTFOLIO PROJECTION
    # ========================================================

    tabPanel(

      title = "Portfolio Projection",

      div(
        class = "main-container",

        div(
          class = "page-title",
          "Portfolio Projection"
        ),

        div(
          class = "page-subtitle",
          "Project the policyholder distribution over multiple transitions."
        ),

        div(
          class = "custom-card",

          fluidRow(

            column(
              width = 4,

              numericInput(
                inputId = "projection_years",
                label = "Projection horizon (years)",
                value = 10,
                min = 1,
                max = 100,
                step = 1
              )
            ),

            column(
              width = 4,

              numericInput(
                inputId = "portfolio_size",
                label = "Initial portfolio size",
                value = 1000,
                min = 1,
                max = 1000000,
                step = 100
              )
            )

          ),

          div(
            class = "formula-box",

            HTML(
              "&pi;<sub>n</sub> = &pi;<sub>0</sub>P<sup>n</sup>"
            )
          ),

          p(
            "This gives the probability distribution of the portfolio after ",
            "n transitions."
          )
        ),


        div(
          class = "custom-card",

          h2("Projected portfolio"),

          plotOutput(
            "portfolio_plot",
            height = "480px"
          )
        ),


        div(
          class = "custom-card",

          h2("Projection at selected horizon"),

          div(
            class = "action-row",

            downloadButton(
              outputId = "download_projection",
              label = "Download as CSV",
              class = "btn-outline"
            )
          ),

          tableOutput(
            "projection_table"
          )
        ),


        div(
          class = "custom-card",

          h2("Illustrative financial impact"),

          div(
            class = "warning-box",

            strong("Schematic only: "),

            "this is a simplified illustration of how the Claim-state probability ",
            "could be turned into a cost figure — it is not a reserving calculation."
          ),

          fluidRow(

            column(
              width = 4,

              numericInput(
                inputId = "avg_claim_cost",
                label = "Average cost per claim period (currency units)",
                value = 500,
                min = 0,
                max = 1000000,
                step = 50
              )
            ),

            column(
              width = 8,

              div(
                class = "kpi-card",

                div(
                  class = "kpi-label",
                  "Total Expected Claim Cost Over Horizon"
                ),

                div(
                  class = "kpi-value",
                  textOutput("total_expected_claim_cost", inline = TRUE)
                ),

                div(
                  class = "kpi-description",
                  "Sum over all years of (expected policyholders in Claim) x (average cost per claim period)"
                )
              )
            )
          ),

          tableOutput("financial_table")
        )
      )
    ),


    # ========================================================
    # TRANSITION ANALYSIS
    # ========================================================

    tabPanel(

      title = "Transition Analysis",

      div(
        class = "main-container",

        div(
          class = "page-title",
          "Transition Analysis"
        ),

        div(
          class = "page-subtitle",
          "Explore the n-step transition probabilities implied by the model."
        ),

        div(
          class = "custom-card",

          numericInput(
            inputId = "n_steps",
            label = "Calculate Pⁿ for n =",
            value = 5,
            min = 0,
            max = 100,
            step = 1
          ),

          div(
            class = "formula-box",

            HTML(
              "P<sup>n</sup> = P &times; P &times; ... &times; P"
            )
          ),

          p(
            "The individual entry Pⁿᵢⱼ gives the probability of being in ",
            "state j after n transitions, conditional on starting in state i."
          ),

          tableOutput(
            "n_step_matrix"
          )
        ),


        div(
          class = "custom-card",

          h2("Interpretation"),

          uiOutput(
            "transition_interpretation"
          )
        )
      )
    ),


    # ========================================================
    # LONG-RUN ANALYSIS
    # ========================================================

    tabPanel(

      title = "Long-Run Analysis",

      div(
        class = "main-container",

        div(
          class = "page-title",
          "Long-Run Analysis"
        ),

        div(
          class = "page-subtitle",
          "Study stationary behaviour, absorbing states and expected time to absorption."
        ),


        # ----------------------------------------------------
        # STATIONARY DISTRIBUTION
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("Stationary distribution"),

          div(
            class = "formula-box",

            HTML(
              "&pi;P = &pi;"
            )
          ),

          p(
            "A stationary distribution describes a probability distribution ",
            "that remains unchanged after another transition."
          ),

          tableOutput(
            "stationary_table"
          ),

          uiOutput(
            "stationary_interpretation"
          )
        ),


        # ----------------------------------------------------
        # ABSORBING STATES
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("Absorbing states"),

          tableOutput(
            "absorbing_table"
          ),

          uiOutput(
            "absorbing_message"
          )
        ),


        # ----------------------------------------------------
        # EXPECTED TIME TO ABSORPTION  (NEW)
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("Expected time to absorption"),

          p(
            "Using the fundamental matrix of the chain, N = (I - Q)",
            HTML("<sup>-1</sup>"),
            ", where Q is the sub-matrix of transition probabilities between ",
            "transient states only, N", HTML("&middot;"), "1 gives the expected ",
            "number of periods a policyholder spends before being absorbed into Closed."
          ),

          div(
            class = "formula-box",

            HTML(
              "N = (I - Q)<sup>-1</sup> &nbsp;&nbsp;|&nbsp;&nbsp; t = N &middot; 1"
            )
          ),

          tableOutput("absorption_time_table")
        ),


        # ----------------------------------------------------
        # LONG RUN
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("Long-run interpretation"),

          p(
            strong("Stationary distribution: "),
            "a distribution satisfying &pi;P = &pi;."
          ),

          p(
            strong("Absorbing state: "),
            "a state that, once entered, cannot be left."
          ),

          p(
            strong("Expected time to absorption: "),
            "the expected number of periods before a policyholder reaches an absorbing state."
          ),

          p(
            strong("Long-run behaviour: "),
            "what the process approaches as the number of transitions becomes large."
          ),

          div(
            class = "warning-box",

            strong("Important: "),

            "Stationary distribution, absorbing states, expected time to absorption ",
            "and long-run behaviour are related concepts, but they are not identical."
          )
        )
      )
    ),


    # ========================================================
    # SCENARIO ANALYSIS
    # ========================================================

    tabPanel(

      title = "Scenario Analysis",

      div(
        class = "main-container",

        div(
          class = "page-title",
          "Scenario Analysis"
        ),

        div(
          class = "page-subtitle",
          "Test how portfolio outcomes change when model assumptions change."
        ),


        div(
          class = "info-box",

          strong("Important: "),

          "These scenarios are illustrative sensitivity analyses. ",
          "They are not forecasts or predictions."
        ),


        # ----------------------------------------------------
        # SCENARIO INPUTS
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("Lapse assumptions"),

          div(
            class = "action-row",

            actionButton(
              inputId = "reset_scenarios",
              label = "Reset to defaults",
              class = "btn-outline"
            )
          ),

          fluidRow(

            column(
              width = 4,

              numericInput(
                inputId = "high_lapse",
                label = "High lapse: Active → Lapsed",
                value = 0.15,
                min = 0,
                max = 1,
                step = 0.01
              )
            ),

            column(
              width = 4,

              numericInput(
                inputId = "base_lapse",
                label = "Base case: Active → Lapsed",
                value = 0.08,
                min = 0,
                max = 1,
                step = 0.01
              )
            ),

            column(
              width = 4,

              numericInput(
                inputId = "low_lapse",
                label = "Low lapse: Active → Lapsed",
                value = 0.03,
                min = 0,
                max = 1,
                step = 0.01
              )
            )
          ),

          uiOutput("scenario_input_warning")
        ),


        # ----------------------------------------------------
        # SCENARIO PLOT
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("Scenario comparison"),

          plotOutput(
            "scenario_plot",
            height = "560px"
          )
        ),


        # ----------------------------------------------------
        # SCENARIO RESULTS
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("Scenario results at projection horizon"),

          div(
            class = "action-row",

            downloadButton(
              outputId = "download_scenarios",
              label = "Download as CSV",
              class = "btn-outline"
            )
          ),

          tableOutput(
            "scenario_results"
          )
        ),


        div(
          class = "custom-card",

          h2("Scenario interpretation"),

          uiOutput(
            "scenario_interpretation"
          )
        )
      )
    ),


    # ========================================================
    # METHODOLOGY
    # ========================================================

    tabPanel(

      title = "Methodology",

      div(
        class = "main-container",

        div(
          class = "page-title",
          "Methodology"
        ),

        div(
          class = "page-subtitle",
          "How the Policyholder Transition Model works."
        ),


        # ----------------------------------------------------
        # WHAT IS MODELLED
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("What is being modelled?"),

          p(
            "The model represents an insurance policyholder portfolio ",
            "as a discrete-time Markov Chain."
          ),

          h3("States"),

          p(
            "The model contains four states:"
          ),

          tags$ul(

            tags$li("Active"),

            tags$li("Claim"),

            tags$li("Lapsed"),

            tags$li("Closed")

          )
        ),


        # ----------------------------------------------------
        # TRANSITION MATRIX
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("Transition matrix"),

          p(
            "The transition matrix contains the probability of moving ",
            "from each current state to each possible next state."
          ),

          div(
            class = "formula-box",

            HTML(
              "P<sub>ij</sub> = P(X<sub>t+1</sub> = j | X<sub>t</sub> = i)"
            )
          ),

          p(
            "Rows represent current states and columns represent next states."
          ),

          tableOutput(
            "method_matrix"
          )
        ),


        # ----------------------------------------------------
        # INITIAL DISTRIBUTION
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("Initial distribution"),

          div(
            class = "formula-box",

            HTML(
              "&pi;<sub>0</sub> = (1, 0, 0, 0)"
            )
          ),

          p(
            "The default model begins with 100% of policyholders in ",
            "the Active state."
          )
        ),


        # ----------------------------------------------------
        # N STEP
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("n-step transition probabilities"),

          p(
            "The n-step transition matrix is obtained by multiplying ",
            "the transition matrix by itself n times."
          ),

          div(
            class = "formula-box",

            HTML(
              "P<sup>n</sup> = P &times; P &times; ... &times; P"
            )
          ),

          p(
            "The individual entry Pⁿᵢⱼ gives the probability of being ",
            "in state j after n transitions, conditional on starting ",
            "in state i."
          )
        ),


        # ----------------------------------------------------
        # PORTFOLIO PROJECTION
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("Portfolio projection"),

          div(
            class = "formula-box",

            HTML(
              "&pi;<sub>n</sub> = &pi;<sub>0</sub>P<sup>n</sup>"
            )
          ),

          p(
            "This gives the probability distribution of the portfolio ",
            "after n transitions."
          )
        ),


        # ----------------------------------------------------
        # STATIONARY
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("Stationary distribution"),

          div(
            class = "formula-box",

            HTML(
              "&pi;P = &pi;"
            )
          ),

          p(
            "A stationary distribution is a distribution that remains ",
            "unchanged under the transition matrix."
          )
        ),


        # ----------------------------------------------------
        # ABSORBING
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("Absorbing states & expected time to absorption"),

          p(
            "A state is absorbing when the process remains in that ",
            "state once it enters it."
          ),

          div(
            class = "formula-box",

            HTML(
              "P<sub>ii</sub> = 1"
            )
          ),

          p(
            "In the default model, Closed is an absorbing state. The expected number ",
            "of periods a policyholder spends in transient states before absorption ",
            "is obtained from the fundamental matrix N = (I - Q)",
            HTML("<sup>-1</sup>"),
            ", where Q contains the transition probabilities restricted to transient states."
          )
        ),


        # ----------------------------------------------------
        # ESTIMATION  (NEW)
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("Estimating a transition matrix from data"),

          p(
            "The base matrix used throughout this app is a hypothetical assumption. ",
            "The Data & Estimation tab illustrates how it could instead be estimated from ",
            "observed policyholder experience, using the maximum-likelihood estimator for ",
            "a time-homogeneous discrete-time Markov chain — the observed count of ",
            HTML("i &rarr; j"),
            " moves divided by the total moves out of state i."
          ),

          div(
            class = "formula-box",

            HTML(
              "P&#770;<sub>ij</sub> = N<sub>ij</sub> / N<sub>i&middot;</sub>"
            )
          )
        ),


        # ----------------------------------------------------
        # ASSUMPTIONS
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("Major assumptions"),

          tags$ul(

            tags$li(
              "Transition probabilities are assumed to remain constant over the projection period."
            ),

            tags$li(
              "The model is discrete-time."
            ),

            tags$li(
              "The future state depends on the current state through the specified transition probabilities."
            ),

            tags$li(
              "The base transition probabilities are hypothetical; the Data & Estimation tab shows how real data would be used instead."
            ),

            tags$li(
              "The financial impact figure on the Portfolio Projection tab is illustrative only, not a reserving calculation."
            )

          )
        ),


        # ----------------------------------------------------
        # LIMITATIONS
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("Limitations"),

          tags$ul(

            tags$li(
              "The model does not use actual insurer policyholder data."
            ),

            tags$li(
              "Transition probabilities are not estimated from real experience (only from simulated data, for demonstration)."
            ),

            tags$li(
              "The current model does not explicitly represent new business."
            ),

            tags$li(
              "Economic conditions and policyholder characteristics are not explicitly modelled."
            ),

            tags$li(
              "Real actuarial models may require additional states, segmentation and time-varying assumptions."
            )

          )
        ),


        # ----------------------------------------------------
        # REAL WORLD APPLICATION
        # ----------------------------------------------------

        div(
          class = "custom-card",

          h2("Real-world actuarial application"),

          p(
            "In a real insurance setting, transition probabilities could ",
            "be estimated using historical policyholder experience."
          ),

          p(
            "For example, an insurer could analyse policy records to estimate ",
            "the probability of remaining active, entering a claim state, ",
            "lapsing or closing during each observation period."
          ),

          p(
            "The resulting transition matrix could then be used for portfolio ",
            "projections, experience analysis, scenario testing, reserving-related ",
            "cost estimates and other actuarial investigations."
          )
        )

        # Footer is added globally below the navigation.
      )
    )
  )
)


# ============================================================
# GLOBAL FOOTER
# ============================================================

footer <- div(
  class = "footer",
  HTML("&copy; 2026 Avantika Vashisht &nbsp;|&nbsp; Policyholder Transition Model &nbsp;|&nbsp; Actuarial Markov Chain Project")
)

ui <- tagList(
  ui,
  footer
)
