# ============================================================
# POLICYHOLDER TRANSITION MODEL
# Discrete-Time Markov Chain | Insurance Portfolio
# ============================================================

# ============================================================
# 1. PACKAGES
# ============================================================

library(shiny)
library(ggplot2)


# ============================================================
# 2. MODEL INPUTS
# ============================================================

states <- c(
  "Active",
  "Claim",
  "Lapsed",
  "Closed"
)

# Base transition matrix
#
# Rows    = current state
# Columns = next state
#
#                 Active Claim Lapsed Closed
# Active           0.47  0.10  0.27   0.16
# Claim            0.42  0.09  0.26   0.23
# Lapsed           0.14  0.02  0.62   0.22
# Closed           0.00  0.00  0.00   1.00

base_matrix <- matrix(
  c(
    0.47, 0.10, 0.27, 0.16,
    0.42, 0.09, 0.26, 0.23,
    0.14, 0.02, 0.62, 0.22,
    0.00, 0.00, 0.00, 1.00
  ),
  nrow = 4,
  byrow = TRUE,
  dimnames = list(states, states)
)

# Initial distribution
initial_distribution <- c(
  Active = 1,
  Claim = 0,
  Lapsed = 0,
  Closed = 0
)


# ============================================================
# 3. HELPER FUNCTIONS
# ============================================================

# ------------------------------------------------------------
# Matrix power
# ------------------------------------------------------------

matrix_power <- function(M, n) {
  
  if (n == 0) {
    return(diag(nrow(M)))
  }
  
  result <- diag(nrow(M))
  
  for (i in seq_len(n)) {
    result <- result %*% M
  }
  
  result
}


# ------------------------------------------------------------
# Create scenario transition matrix
#
# We vary Active -> Lapsed.
#
# To keep the row sum equal to 1, the difference is transferred
# to Active -> Active.
# ------------------------------------------------------------

scenario_matrix <- function(lapse_rate) {
  
  M <- base_matrix
  
  original_lapse <- M["Active", "Lapsed"]
  
  difference <- original_lapse - lapse_rate
  
  M["Active", "Lapsed"] <- lapse_rate
  
  M["Active", "Active"] <-
    M["Active", "Active"] + difference
  
  M
}


# ------------------------------------------------------------
# Calculate stationary distribution
# ------------------------------------------------------------

calculate_stationary <- function(M) {
  
  # Solve:
  #
  # pi P = pi
  #
  # equivalently:
  #
  # (P' - I) pi' = 0
  #
  # Replace one equation with sum(pi) = 1.
  
  A <- t(M) - diag(nrow(M))
  
  A[nrow(A), ] <- 1
  
  b <- c(rep(0, nrow(M) - 1), 1)
  
  solution <- tryCatch(
    solve(A, b),
    error = function(e) rep(NA, nrow(M))
  )
  
  names(solution) <- rownames(M)
  
  # Remove tiny numerical errors
  solution[abs(solution) < 1e-10] <- 0
  
  solution
}


# ------------------------------------------------------------
# Detect absorbing states
# ------------------------------------------------------------

find_absorbing_states <- function(M) {
  
  absorbing <- character(0)
  
  for (i in seq_len(nrow(M))) {
    
    if (
      abs(M[i, i] - 1) < 1e-10 &&
      sum(M[i, -i]) < 1e-10
    ) {
      
      absorbing <- c(
        absorbing,
        rownames(M)[i]
      )
    }
  }
  
  absorbing
}


# ------------------------------------------------------------
# Format matrix for display
# ------------------------------------------------------------

format_matrix <- function(M) {
  
  data.frame(
    State = rownames(M),
    round(M, 4),
    check.names = FALSE,
    row.names = NULL
  )
}


# ------------------------------------------------------------
# Portfolio projection
# ------------------------------------------------------------

project_portfolio <- function(
    M,
    n,
    initial_dist = initial_distribution
) {
  
  Pn <- matrix_power(M, n)
  
  distribution <- as.numeric(
    initial_dist %*% Pn
  )
  
  names(distribution) <- states
  
  distribution
}


# ============================================================
# 4. UI
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
        background: #6B8F87 !important;
        border: none !important;
        border-radius: 0 !important;
        margin-bottom: 0 !important;
        min-height: 96px;
        box-shadow: 0 4px 15px rgba(48,54,56,0.10);
      }

      .navbar-brand {
        color: white !important;
        font-size: 38px !important;
        line-height: 1.15 !important;
        font-weight: 700 !important;
        padding-top: 23px !important;
        padding-bottom: 18px !important;
      }

      .navbar-nav > li > a {
        color: white !important;
        font-size: 15px;
        font-weight: 500;
        padding-top: 36px !important;
        padding-bottom: 36px !important;
        transition: 0.2s ease;
      }

      .navbar-nav > li > a:hover {
        background: rgba(255,255,255,0.12) !important;
      }

      .navbar-nav > .active > a,
      .navbar-nav > .active > a:hover {
        background: rgba(255,255,255,0.16) !important;
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
        color: #5A3D4E;
        font-size: 38px;
        font-weight: 700;
        margin-bottom: 8px;
      }

      .page-subtitle {
        color: #64716F;
        font-size: 17px;
        margin-bottom: 28px;
      }

      h2 {
        color: #5A3D4E;
        font-weight: 700;
      }

      h3 {
        color: #5A3D4E;
        font-weight: 650;
      }


      /* =====================================================
         CARDS
         ===================================================== */

      .custom-card {
        background: #FFFFFF;
        border: 1px solid #E1E7E3;
        border-radius: 16px;
        padding: 26px;
        margin-bottom: 24px;
        box-shadow: 0 5px 18px rgba(48,54,56,0.06);
      }

      .custom-card h2 {
        margin-top: 0;
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
      }

      .kpi-label {
        color: #6B8F87;
        font-size: 14px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }

      .kpi-value {
        color: #5A3D4E;
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
        color: #5A3D4E;
        font-size: 20px;
        margin: 18px 0;
      }


      /* =====================================================
         INFO BOX
         ===================================================== */

      .info-box {
        background: #EEF5F2;
        border-left: 5px solid #6B8F87;
        border-radius: 10px;
        padding: 18px 20px;
        margin: 20px 0;
      }

      .warning-box {
        background: #FBF5EA;
        border-left: 5px solid #C69C6D;
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
        color: #5A3D4E !important;
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
        border-color: #6B8F87 !important;
        box-shadow: 0 0 0 3px rgba(107,143,135,0.12) !important;
      }

      label {
        color: #5A3D4E;
        font-weight: 600;
      }


      /* =====================================================
         BUTTONS
         ===================================================== */

      .btn-primary {
        background: #6B8F87 !important;
        border-color: #6B8F87 !important;
        border-radius: 9px !important;
        font-weight: 600;
      }

      .btn-primary:hover {
        background: #587B73 !important;
        border-color: #587B73 !important;
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
        background: #E2EFEB;
        color: #416D64;
      }

      .claim-badge {
        background: #F1E8EE;
        color: #79546A;
      }

      .lapsed-badge {
        background: #F7EEDD;
        color: #896D42;
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
        background: #A8BCB6;
        border-radius: 10px;
      }

      ::-webkit-scrollbar-thumb:hover {
        background: #6B8F87;
      }


      /* =====================================================
         MOBILE
         ===================================================== */

      @media (max-width: 768px) {

        .navbar-brand {
          font-size: 28px !important;
          padding-top: 18px !important;
          padding-bottom: 14px !important;
        }

        .navbar-nav > li > a {
          padding-top: 16px !important;
          padding-bottom: 16px !important;
        }

        .page-title {
          font-size: 30px;
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
        class = "main-container",
        
        div(
          class = "page-title",
          "Policyholder Transition Model"
        ),
        
        div(
          class = "page-subtitle",
          "A discrete-time Markov Chain framework for analysing policyholder movements."
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
        ),
        
        
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
              "The probabilities used in this project are hypothetical."
            )
            
          )
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
          
          tableOutput(
            "projection_table"
          )
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
          "Study stationary behaviour and absorbing states."
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
            strong("Long-run behaviour: "),
            "what the process approaches as the number of transitions becomes large."
          ),
          
          div(
            class = "warning-box",
            
            strong("Important: "),
            
            "Stationary distribution, absorbing states and long-run behaviour ",
            "are related concepts, but they are not identical."
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
          )
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
          
          h2("Absorbing states"),
          
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
            "In the default model, Closed is an absorbing state."
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
              "The transition probabilities used here are hypothetical."
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
              "Transition probabilities are not estimated from experience."
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
            "projections, experience analysis, scenario testing and other ",
            "actuarial investigations."
          )
        ),
        
        
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

# Add the footer below the active application content.
ui <- tagList(
  ui,
  footer
)


# ============================================================
# 5. SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # INPUT MATRIX
  # ==========================================================
  
  output$input_matrix <- renderTable({
    
    format_matrix(base_matrix)
    
  }, striped = TRUE, bordered = FALSE)
  
  
  # ==========================================================
  # INITIAL DISTRIBUTION TABLE
  # ==========================================================
  
  output$initial_table <- renderTable({
    
    data.frame(
      State = states,
      Initial_Probability = paste0(
        round(initial_distribution * 100, 2),
        "%"
      )
    )
    
  }, striped = TRUE, bordered = FALSE)
  
  
  # ==========================================================
  # PORTFOLIO PROJECTION
  # ==========================================================
  
  projection_distribution <- reactive({
    
    project_portfolio(
      base_matrix,
      input$projection_years
    )
    
  })
  
  
  # ----------------------------------------------------------
  # Projection plot
  # ----------------------------------------------------------
  
  output$portfolio_plot <- renderPlot({
    
    horizon <- input$projection_years
    
    years <- 0:horizon
    
    projection_data <- data.frame()
    
    for (yr in years) {
      
      dist <- project_portfolio(
        base_matrix,
        yr
      )
      
      temp <- data.frame(
        Year = yr,
        State = names(dist),
        Probability = as.numeric(dist)
      )
      
      projection_data <- rbind(
        projection_data,
        temp
      )
    }
    
    
    ggplot(
      projection_data,
      aes(
        x = Year,
        y = Probability,
        group = State,
        linetype = State
      )
    ) +
      
      geom_line(
        linewidth = 1.2
      ) +
      
      geom_point(
        size = 2
      ) +
      
      scale_y_continuous(
        labels = function(x) paste0(
          round(x * 100),
          "%"
        ),
        limits = c(0, 1)
      ) +
      
      scale_x_continuous(
        breaks = pretty(years)
      ) +
      
      labs(
        title = "Projected Portfolio Distribution",
        x = "Year",
        y = "Portfolio Distribution",
        linetype = "State"
      ) +
      
      theme_minimal(base_size = 14) +
      
      theme(
        plot.title = element_text(
          face = "bold",
          color = "#5A3D4E"
        ),
        
        axis.title = element_text(
          color = "#5A3D4E",
          face = "bold"
        ),
        
        legend.title = element_text(
          face = "bold"
        ),
        
        panel.grid.minor = element_blank()
      )
    
  })
  
  
  # ----------------------------------------------------------
  # Projection table
  # ----------------------------------------------------------
  
  output$projection_table <- renderTable({
    
    dist <- projection_distribution()
    
    data.frame(
      
      State = names(dist),
      
      Probability = paste0(
        round(dist * 100, 4),
        "%"
      ),
      
      Expected_Policyholders = round(
        dist * input$portfolio_size,
        2
      )
      
    )
    
  }, striped = TRUE, bordered = FALSE)
  
  # ==========================================================
  # N-STEP TRANSITION MATRIX
  # ==========================================================
  
  output$n_step_matrix <- renderTable({
    
    n <- input$n_steps
    
    # Calculate P^n
    Pn <- matrix_power(
      base_matrix,
      n
    )
    
    # Make sure state names are attached
    state_names <- c(
      "Active",
      "Claim",
      "Lapsed",
      "Closed"
    )
    
    rownames(Pn) <- state_names
    colnames(Pn) <- state_names
    
    # Format for display
    format_matrix(Pn)
    
  }, striped = TRUE, bordered = FALSE)
  
  
  # ----------------------------------------------------------
  # N-step interpretation
  # ----------------------------------------------------------
  
  output$transition_interpretation <- renderUI({
    
    n <- input$n_steps
    
    # Calculate P^n
    Pn <- matrix_power(
      base_matrix,
      n
    )
    
    # Make sure the matrix has state names
    state_names <- c(
      "Active",
      "Claim",
      "Lapsed",
      "Closed"
    )
    
    rownames(Pn) <- state_names
    colnames(Pn) <- state_names
    
    # Extract probabilities safely
    active_to_closed <- Pn[1, 4]
    active_to_active <- Pn[1, 1]
    
    div(
      
      class = "info-box",
      
      p(
        strong(
          paste0(
            "After ",
            n,
            " transition(s): "
          )
        ),
        
        "a policyholder starting in the Active state has a ",
        
        strong(
          paste0(
            round(active_to_closed * 100, 2),
            "%"
          )
        ),
        
        " probability of being Closed."
      ),
      
      p(
        "The probability of still being Active is ",
        
        strong(
          paste0(
            round(active_to_active * 100, 2),
            "%"
          )
        ),
        
        "."
      )
    )
    
  })
  
  
  # ==========================================================
  # STATIONARY DISTRIBUTION
  # ==========================================================
  
  stationary_distribution <- reactive({
    
    calculate_stationary(
      base_matrix
    )
    
  })
  
  
  output$stationary_table <- renderTable({
    
    pi <- stationary_distribution()
    
    data.frame(
      
      State = names(pi),
      
      Stationary_Probability = paste0(
        sprintf(
          "%.4f",
          pi * 100
        ),
        "%"
      )
      
    )
    
  }, striped = TRUE, bordered = FALSE)
  
  
  # ----------------------------------------------------------
  # Stationary interpretation
  # ----------------------------------------------------------
  
  output$stationary_interpretation <- renderUI({
    
    pi <- stationary_distribution()
    
    absorbing <- find_absorbing_states(
      base_matrix
    )
    
    if (length(absorbing) > 0) {
      
      div(
        
        class = "warning-box",
        
        strong("Interpretation: "),
        
        "The current transition structure contains absorbing ",
        "state(s): ",
        
        strong(
          paste(
            absorbing,
            collapse = ", "
          )
        ),
        
        ". Therefore, the long-run result should be interpreted ",
        "in the context of absorption rather than as a stable ",
        "business equilibrium between all states."
      )
      
    } else {
      
      div(
        
        class = "info-box",
        
        strong("Interpretation: "),
        
        "The stationary distribution represents the long-run ",
        "equilibrium distribution implied by the transition matrix."
      )
      
    }
    
  })
  
  
  # ==========================================================
  # ABSORBING STATES
  # ==========================================================
  
  output$absorbing_table <- renderTable({
    
    absorbing <- find_absorbing_states(
      base_matrix
    )
    
    data.frame(
      
      State = states,
      
      Absorbing = ifelse(
        states %in% absorbing,
        "Yes",
        "No"
      )
      
    )
    
  }, striped = TRUE, bordered = FALSE)
  
  
  output$absorbing_message <- renderUI({
    
    absorbing <- find_absorbing_states(
      base_matrix
    )
    
    if (length(absorbing) > 0) {
      
      div(
        
        class = "warning-box",
        
        strong(
          paste0(
            "Absorbing state(s): ",
            paste(absorbing, collapse = ", "),
            ". "
          )
        ),
        
        "Once a policyholder enters an absorbing state, ",
        "the model does not allow them to leave it."
      )
      
    }
    
  })
  
  
  # ==========================================================
  # SCENARIO MATRICES
  # ==========================================================
  
  scenario_matrices <- reactive({
    
    list(
      
      "Base Case" =
        scenario_matrix(
          input$base_lapse
        ),
      
      "High Lapse" =
        scenario_matrix(
          input$high_lapse
        ),
      
      "Low Lapse" =
        scenario_matrix(
          input$low_lapse
        )
      
    )
    
  })
  
  
  # ==========================================================
  # SCENARIO PLOT
  # ==========================================================
  
  output$scenario_plot <- renderPlot({
    
    scenarios <- scenario_matrices()
    
    horizon <- input$projection_years
    
    all_data <- data.frame()
    
    for (scenario_name in names(scenarios)) {
      
      M <- scenarios[[scenario_name]]
      
      for (yr in 0:horizon) {
        
        dist <- project_portfolio(
          M,
          yr
        )
        
        temp <- data.frame(
          
          Scenario = scenario_name,
          
          Year = yr,
          
          State = names(dist),
          
          Probability = as.numeric(dist)
          
        )
        
        all_data <- rbind(
          all_data,
          temp
        )
      }
    }
    
    
    # Focus on the most useful portfolio states
    # for scenario comparison.
    plot_data <- subset(
      all_data,
      State %in% c(
        "Active",
        "Lapsed",
        "Closed"
      )
    )
    
    
    ggplot(
      plot_data,
      aes(
        x = Year,
        y = Probability,
        linetype = State
      )
    ) +
      
      geom_line(
        linewidth = 1.15
      ) +
      
      facet_wrap(
        ~Scenario,
        nrow = 1
      ) +
      
      scale_y_continuous(
        labels = function(x) paste0(
          round(x * 100),
          "%"
        ),
        limits = c(0, 1)
      ) +
      
      labs(
        title = "Impact of Lapse Assumptions",
        x = "Year",
        y = "Portfolio Distribution",
        linetype = "State"
      ) +
      
      theme_minimal(base_size = 14) +
      
      theme(
        
        plot.title = element_text(
          face = "bold",
          color = "#5A3D4E"
        ),
        
        strip.text = element_text(
          face = "bold",
          color = "#5A3D4E"
        ),
        
        axis.title = element_text(
          face = "bold",
          color = "#5A3D4E"
        ),
        
        panel.grid.minor = element_blank(),
        
        legend.position = "bottom"
      )
    
  })
  
  
  # ==========================================================
  # SCENARIO RESULTS
  # ==========================================================
  
  output$scenario_results <- renderTable({
    
    scenarios <- scenario_matrices()
    
    horizon <- input$projection_years
    
    results <- data.frame()
    
    for (scenario_name in names(scenarios)) {
      
      M <- scenarios[[scenario_name]]
      
      dist <- project_portfolio(
        M,
        horizon
      )
      
      temp <- data.frame(
        
        Scenario = scenario_name,
        
        State = names(dist),
        
        Probability = paste0(
          round(dist * 100, 4),
          "%"
        ),
        
        Expected_Policyholders = round(
          dist * input$portfolio_size,
          2
        )
        
      )
      
      results <- rbind(
        results,
        temp
      )
    }
    
    results
    
  }, striped = TRUE, bordered = FALSE)
  
  
  # ==========================================================
  # SCENARIO INTERPRETATION
  # ==========================================================
  
  output$scenario_interpretation <- renderUI({
    
    high_M <- scenario_matrix(
      input$high_lapse
    )
    
    base_M <- scenario_matrix(
      input$base_lapse
    )
    
    low_M <- scenario_matrix(
      input$low_lapse
    )
    
    
    high_dist <- project_portfolio(
      high_M,
      input$projection_years
    )
    
    base_dist <- project_portfolio(
      base_M,
      input$projection_years
    )
    
    low_dist <- project_portfolio(
      low_M,
      input$projection_years
    )
    
    
    div(
      
      class = "info-box",
      
      p(
        
        strong("High lapse scenario: "),
        
        "Increasing the Active → Lapsed transition probability ",
        "increases the proportion of policyholders expected to ",
        "lapse over the projection period."
      ),
      
      p(
        
        strong("Low lapse scenario: "),
        
        "Reducing the Active → Lapsed transition probability ",
        "allows a greater proportion of policyholders to remain ",
        "Active."
      ),
      
      p(
        
        strong("Actuarial interpretation: "),
        
        "Scenario analysis helps assess how sensitive portfolio ",
        "projections are to assumptions about policyholder behaviour."
      ),
      
      p(
        
        strong("At the selected horizon: "),
        
        "the Active proportion is ",
        
        strong(
          paste0(
            round(base_dist["Active"] * 100, 2),
            "%"
          )
        ),
        
        " under the base case, compared with ",
        
        strong(
          paste0(
            round(high_dist["Active"] * 100, 2),
            "%"
          )
        ),
        
        " under high lapse and ",
        
        strong(
          paste0(
            round(low_dist["Active"] * 100, 2),
            "%"
          )
        ),
        
        " under low lapse."
      )
      
    )
    
  })
  
  
  # ==========================================================
  # METHODOLOGY MATRIX
  # ==========================================================
  
  output$method_matrix <- renderTable({
    
    format_matrix(
      base_matrix
    )
    
  }, striped = TRUE, bordered = FALSE)
  
}


# ============================================================
# 6. RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
