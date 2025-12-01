###############################################################################
# Filename: PESMADMIIFinalAllPrefs.R
# PES‐MADM II Decision Support Platform
# - Adjusted only the Alt Partial Entropy Plot to use a stacked bar chart.
###############################################################################

library(shiny)
library(shinythemes)
library(shinyjs)
library(readxl)
library(writexl)
library(ggplot2)
library(dplyr)
library(DT)
library(gridExtra)
library(reshape2)
library(ggrepel)

########################################
# 1) Helper Functions
########################################
round3 <- function(x) round(x, 3)
format3 <- function(x) sprintf("%.3f", x)
log2safe <- function(x, eps = 1e-15) ifelse(x > eps, log2(x), 0)

########################################
# preferenceFunction() for PROMETHEE
########################################
preferenceFunction <- function(d, funcType, q = 0, p = 0, s = 0) {
  if (tolower(funcType) == "usual") {
    return(ifelse(d > 0, 1, 0))
  } else if (tolower(funcType) == "u-shape") {
    if (d <= q) return(0) else return(1)
  } else if (tolower(funcType) == "v-shape") {
    if (d <= 0) return(0)
    else if (d < p) return(d / p)
    else return(1)
  } else if (tolower(funcType) == "level") {
    if (d <= q) return(0)
    else if (d <= p) return(0.5)
    else return(1)
  } else if (tolower(funcType) == "linear") {
    if (d <= q) return(0)
    else if (d <= p) return((d - q) / (p - q))
    else return(1)
  } else if (tolower(funcType) == "gaussian") {
    if (d <= 0) return(0)
    if (abs(s) < 1e-15) return(ifelse(d > 0, 1, 0))
    return(1 - exp(-(d^2) / (2*s^2)))
  } else {
    # fallback: V-shape
    if (d <= 0) return(0)
    else if (d < p) return(d / p)
    else return(1)
  }
}

computePreferenceEnhancedRow <- function(row, funcType, q = 0, p = 0, s = 0) {
  N <- length(row)
  F <- numeric(N)
  for (j in 1:N) {
    diffs <- row[j] - row[-j]
    prefs <- sapply(diffs, function(d) preferenceFunction(d, funcType, q, p, s))
    F[j] <- mean(prefs)
  }
  return(F)
}

########################################
# computeModelOutputs()
########################################
computeModelOutputs <- function(dataMat, sbjW, prefParams, benefitCost, threshMult = 1) {
  M <- nrow(dataMat)
  N <- ncol(dataMat)
  
  # 1) Cost Inversion
  costIndices <- which(sapply(rownames(dataMat), function(x) {
    idx <- which(benefitCost$Criterion == x)
    if (length(idx) == 0) return(FALSE)
    tolower(benefitCost$Type[idx]) == "cost"
  }))
  linearInversionCost <- function(mat, costIdx) {
    matNew <- mat
    for (i in costIdx) {
      rowMax <- max(matNew[i,])
      matNew[i,] <- (rowMax + 1e-15) - matNew[i,]
    }
    return(matNew)
  }
  dataMatTrans <- linearInversionCost(dataMat, costIndices)
  
  # 2) Normalize row-wise
  rowSumsData <- rowSums(dataMatTrans)
  normData <- matrix(0, nrow = M, ncol = N)
  for (i in 1:M) {
    normData[i,] <- dataMatTrans[i,] / (rowSumsData[i] + 1e-15)
  }
  rownames(normData) <- rownames(dataMat)
  colnames(normData) <- colnames(dataMat)
  
  # 3) Preference-Enhanced Probabilities
  prefEnhancedMatrix <- matrix(0, nrow = M, ncol = N)
  rownames(prefEnhancedMatrix) <- rownames(dataMat)
  colnames(prefEnhancedMatrix) <- colnames(dataMat)
  for (i in 1:M) {
    row_i <- normData[i,]
    critName <- rownames(dataMat)[i]
    paramRow <- prefParams[prefParams$Criterion == critName, ]
    if (nrow(paramRow) == 0) {
      funcType <- "v-shape"
      q_val <- 0
      p_val <- 0.1
      s_val <- 0
    } else {
      funcType <- as.character(paramRow$PreferenceFunction)
      q_val <- as.numeric(paramRow$Threshold_q)
      p_val <- as.numeric(paramRow$Threshold_p)
      s_val <- as.numeric(paramRow$Threshold_s)
    }
    # if "Usual", keep F_i=1
    if (tolower(funcType) == "usual") {
      F_i <- rep(1, length(row_i))
    } else {
      F_i <- computePreferenceEnhancedRow(row_i, funcType, q = q_val, p = p_val, s = s_val)
    }
    product <- row_i * F_i
    prefEnhancedMatrix[i,] <- product / (sum(product) + 1e-15)
  }
  condProb <- prefEnhancedMatrix
  
  # 4) Normalized Entropy
  M <- nrow(condProb)
  N <- ncol(condProb)
  h <- numeric(M)
  for (i in 1:M) {
    row_i <- condProb[i,]
    nonz <- row_i > 0
    val <- -sum(row_i[nonz] * log2safe(row_i[nonz])) / log2(N)
    h[i] <- min(max(val, 0), 1)
  }
  d <- 1 - h
  objW <- d / sum(d + 1e-15)
  intW <- (objW * sbjW) / sum(objW * sbjW + 1e-15)
  
  # 5) Conditional Entropy
  condEntropy <- numeric(M)
  for (i in 1:M) {
    row_i <- condProb[i,]
    nonz <- row_i > 0
    cval <- -sum(row_i[nonz] * log2safe(row_i[nonz]))
    condEntropy[i] <- min(max(cval, 0), log2(N))
  }
  
  # NEW: Compute Partial Alternative Entropy Matrix 
  partialAltEntropy <- matrix(0, nrow = M, ncol = N)
  for (i in 1:M) {
    for (j in 1:N) {
      if (condProb[i,j] > 0) {
        partialAltEntropy[i,j] <- - condProb[i,j] * log2safe(condProb[i,j])
      } else {
        partialAltEntropy[i,j] <- 0
      }
    }
  }
  
  # 6) Alternative Scores
  altScores <- numeric(N)
  for (j in 1:N) {
    altScores[j] <- sum(condProb[, j] * intW)
  }
  altNonzero <- altScores[altScores > 0]
  valY <- -sum(altNonzero * log2safe(altScores))
  overallEntropyY <- min(max(valY, 0), log2(N))
  
  totalCondEntropy <- sum(intW * condEntropy)
  normTotalCondEntropy <- totalCondEntropy / (overallEntropyY + 1e-15)
  
  # 7) Joint Entropy S(X,Y)
  jointP <- matrix(0, nrow = M, ncol = N)
  for (i in 1:M) {
    for (j in 1:N) {
      jointP[i,j] <- condProb[i,j] * intW[i]
    }
  }
  jNonz <- jointP[jointP > 0]
  valXY <- -sum(jNonz * log2safe(jNonz))
  SXY <- min(max(valXY, 0), log2(M * N))
  
  # 8) Criteria Entropy S(X)
  Sx <- -sum(intW[intW > 0] * log2safe(intW[intW > 0]))
  Sx <- max(0, Sx)
  
  # 9) Mutual Information J(X;Y)
  Jxy <- Sx + overallEntropyY - SXY
  Jxy <- min(max(Jxy, 0), min(Sx, overallEntropyY))
  
  IXY <- SXY / (overallEntropyY + 1e-15)
  IXY <- pmin(1, pmax(0, IXY))
  
  # 10) Indices: CES, CSF, ADI, NMI, NMGI
  partialMI <- numeric(M)
  for (i in 1:M) {
    pVal <- overallEntropyY - condEntropy[i]
    partialMI[i] <- max(0, pVal)
  }
  CES <- (1 / M) * sum(partialMI / (overallEntropyY + 1e-15))
  CSF <- 1 - (totalCondEntropy / (overallEntropyY + 1e-15))
  ADI <- 1 - (overallEntropyY / (log2(N) + 1e-15))
  NMI <- Jxy / (overallEntropyY + 1e-15)
  
  sumAll <- NMI + CES + CSF + ADI
  NMGI <- 0
  if (sumAll > 1e-15) {
    p_nmi <- NMI / sumAll
    p_ces <- CES / sumAll
    p_csf <- CSF / sumAll
    p_adi <- ADI / sumAll
    k <- 1 / log(4)
    e_nmi <- ifelse(p_nmi > 0, -k * p_nmi * log(p_nmi), 0)
    e_ces <- ifelse(p_ces > 0, -k * p_ces * log(p_ces), 0)
    e_csf <- ifelse(p_csf > 0, -k * p_csf * log(p_csf), 0)
    e_adi <- ifelse(p_adi > 0, -k * p_adi * log(p_adi), 0)
    d_nmi <- 1 - e_nmi
    d_ces <- 1 - e_ces
    d_csf <- 1 - e_csf
    d_adi <- 1 - e_adi
    d_sum <- d_nmi + d_ces + d_csf + d_adi
    if (d_sum > 1e-15) {
      w_nmi <- d_nmi / d_sum
      w_ces <- d_ces / d_sum
      w_csf <- d_csf / d_sum
      w_adi <- d_adi / d_sum
      valNMGI <- w_nmi * NMI + w_ces * CES + w_csf * CSF + w_adi * ADI
      NMGI <- max(0, valNMGI)
    }
  }
  
  critImport <- intW * condEntropy
  
  return(list(
    M = M, N = N,
    xOBJ = objW,
    xINT = intW,
    critImport = critImport,
    altScores = altScores,
    overallEntropyY = overallEntropyY,
    totalCondEntropy = totalCondEntropy,
    normTotalCondEntropy = normTotalCondEntropy,
    SXY = SXY,
    Sx = Sx,
    Jxy = Jxy,
    IXY = IXY,
    NMI = NMI,
    CES = CES,
    CSF = CSF,
    ADI = ADI,
    NMGI = NMGI,
    partialCondEntropy = condEntropy,
    partialAltEntropy = partialAltEntropy
  ))
}

########################################
# generateDetailedEntropyInsights
########################################
generateDetailedEntropyInsights <- function(SXY, SX, SY, IXY, SYX, IYX, NMI, ADI, CES, CSF, NMGI) {
  insights <- c()
  
  if (SXY > 1.5) {
    insights <- c(insights, "<li>A high joint entropy S(X,Y) indicates a complex interaction between criteria and alternatives, with significant informational diversity.</li>")
  } else {
    insights <- c(insights, "<li>A low joint entropy S(X,Y) suggests limited informational diversity among criteria and alternatives.</li>")
  }
  
  if (SX > 1.0) {
    insights <- c(insights, "<li>Criteria entropy S(X) is well-distributed, supporting robust decision-making.</li>")
  } else {
    insights <- c(insights, "<li>A lower S(X) indicates a few criteria dominate the decision process.</li>")
  }
  
  if (SY > 1.0) {
    insights <- c(insights, "<li>The alternatives entropy S(Y) highlights a diverse set of options, offering comprehensive choice.</li>")
  } else {
    insights <- c(insights, "<li>A lower S(Y) indicates limited distinction among alternatives.</li>")
  }
  
  if (IXY > 0.5) {
    insights <- c(insights, "<li>Mutual information I(X;Y) shows criteria effectively explain the alternatives.</li>")
  } else {
    insights <- c(insights, "<li>A lower I(X;Y) suggests the criteria do not fully capture the variability in alternatives.</li>")
  }
  
  if (SYX < 0.5) {
    insights <- c(insights, "<li>The conditional entropy I(Y|X) demonstrates strong uncertainty reduction given the criteria.</li>")
  } else {
    insights <- c(insights, "<li>A higher I(Y|X) implies residual ambiguity about alternatives even after considering the criteria.</li>")
  }
  
  if (IXY > 0.7) {
    insights <- c(insights, "<li>A high normalized joint entropy I(X,Y) indicates a well-balanced distribution of information across criteria and alternatives.</li>")
  } else {
    insights <- c(insights, "<li>A lower I(X,Y) suggests some redundancy or imbalance in the criteria’s ability to distinguish alternatives.</li>")
  }
  
  if (NMI > 0.5) {
    insights <- c(insights, "<li>High NMI (above 0.5) indicates a strong correlation between criteria and alternatives, reflecting robust explanatory power.</li>")
  } else {
    insights <- c(insights, "<li>A lower NMI (0.5 or below) indicates weaker correlation, suggesting the criteria may need refinement.</li>")
  }
  
  if (ADI > 0.5) {
    insights <- c(insights, "<li>A high ADI (above 0.5) reflects strong distinctions among alternatives, aiding a clear decision.</li>")
  } else {
    insights <- c(insights, "<li>A lower ADI (0.5 or below) indicates overlapping alternatives, potentially complicating the final choice.</li>")
  }
  
  if (CES > 0.5) {
    insights <- c(insights, "<li>The high CES (above 0.5) shows criteria effectively reduce uncertainty in the alternatives, enhancing decision clarity.</li>")
  } else {
    insights <- c(insights, "<li>A lower CES (0.5 or below) suggests the criteria have limited impact in reducing uncertainty.</li>")
  }
  
  if (CSF > 0.5) {
    insights <- c(insights, "<li>A high CSF (above 0.5) signifies stable decisions with minimal residual uncertainty.</li>")
  } else {
    insights <- c(insights, "<li>A lower CSF (0.5 or below) implies potential instability, warranting further refinement of criteria or data.</li>")
  }
  
  if (NMGI > 0.5) {
    insights <- c(insights, "<li>A high NMGI (above 0.5) indicates a well-integrated, effective, and robust decision-making process.</li>")
  } else {
    insights <- c(insights, "<li>A lower NMGI (0.5 or below) reveals areas for improvement in synergy among criteria, stability, and clarity.</li>")
  }
  
  paste("<ul>", paste(insights, collapse = ""), "</ul>")
}

########################################
# 2) UI
########################################
ui <- fluidPage(
  theme = shinytheme("slate"),
  useShinyjs(),
  tags$head(tags$style(HTML("
      table.dataTable tbody td { color: #FFFFFF !important; }
      table.dataTable thead th { color: #FFFF00 !important; }
      .dataTables_wrapper .dataTables_length label,
      .dataTables_wrapper .dataTables_filter label,
      .dataTables_wrapper .dataTables_info {
        color: #FFFF00 !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        color: #000000 !important;
        background-color: #CCCCCC !important;
      }
      .dataTables_wrapper .dataTables_length select,
      .dataTables_wrapper .dataTables_filter input {
        background-color: #FFFFFF !important;
        color: #000000 !important;
      }
  "))),
  
  titlePanel("PES‐MADM II Decision Support Platform"),
  
  navbarPage(
    title = "PES‐MADM II",
    id    = "tabs",
    
    # 1) Instructions
    tabPanel("1. Instructions",
             fluidRow(
               column(12,
                      h3("How to Use the PES‐MADM II Platform"),
                      tags$ul(
                        tags$li("Import data in Tab 2, ensuring your PreferenceParams sheet has columns: Criterion, PreferenceFunction, Threshold_q, Threshold_p, Threshold_s."),
                        tags$li("View Baseline results in Tabs 3–5."),
                        tags$li("Use the Sensitivity Analysis tab (Tab 6) to modify scenario parameters and compare results.")
                      ),
                      hr(),
                      h4("Key Notations"),
                      DTOutput("tableNotation")
               )
             )
    ),
    
    # 2) Data Import
    tabPanel("2. Data Import",
             sidebarLayout(
               sidebarPanel(
                 fileInput("fileExcel", "Upload Excel File:", accept = c(".xls", ".xlsx")),
                 numericInput("sheetMatrix", "Sheet # for DataMatrix:", 1, min = 1),
                 numericInput("sheetWeights", "Sheet # for SubjectiveWeights:", 2, min = 1),
                 textInput("sheetPrefParams", "Sheet name for PreferenceParams:", value = "PreferenceParams"),
                 textInput("sheetBenefitCost", "Sheet name for BenefitCost:", value = "BenefitCost"),
                 actionButton("loadData", "Load Data", class = "btn-primary")
               ),
               mainPanel(
                 h4("DataMatrix Preview"),
                 DTOutput("tableMatrixPreview"),
                 h4("SubjectiveWeights Preview"),
                 DTOutput("tableWeightsPreview"),
                 h4("PreferenceParams Preview"),
                 DTOutput("tablePrefParamsPreview"),
                 h4("BenefitCost Preview"),
                 DTOutput("tableBCPreview")
               )
             )
    ),
    
    # 3) Alternative Results
    tabPanel("3. Alternative Results",
             sidebarLayout(
               sidebarPanel(),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Alternatives Table", DTOutput("tableAltScores")),
                   tabPanel("Alternatives Plot", plotOutput("plotAltScores", height = "600px"))
                 )
               )
             )
    ),
    
    # 4) Criteria Results
    tabPanel("4. Criteria Results",
             sidebarLayout(
               sidebarPanel(),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Criteria Table", DTOutput("tableCriteriaResults")),
                   tabPanel("Criteria Plots",
                            tabsetPanel(
                              tabPanel("a) xSBJ Weights", plotOutput("plotxSBJWeights", height = "600px")),
                              tabPanel("b) xOBJ Weights", plotOutput("plotxOBJWeights", height = "600px")),
                              tabPanel("c) xINT Weights", plotOutput("plotxINTWeights", height = "600px")),
                              tabPanel("d) Comparative Weights",
                                       checkboxGroupInput("selectWeights", "Select Weights to Display:",
                                                          choices = list("xSBJ" = "xSBJ", "xOBJ" = "xOBJ", "xINT" = "xINT"),
                                                          selected = c("xSBJ", "xOBJ", "xINT")),
                                       plotOutput("plotComparativeWeights", height = "800px")
                              ),
                              tabPanel("e) Integrated Criteria Importance", plotOutput("plotIntegratedCI", height = "600px"))
                            )
                   )
                 )
               )
             )
    ),
    
    # 5) Entropy Measures & Indices
    tabPanel("5. Entropy Measures & Indices",
             sidebarLayout(
               sidebarPanel(helpText("View baseline global entropies and key indices.")),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Entropy Measures",     DTOutput("tableGlobalEntropies")),
                   tabPanel("Entropy Indices",      DTOutput("tableIndices")),
                   tabPanel("NMGI",                 DTOutput("tableNMGI")),
                   tabPanel("Partial Cond Entropy Table", DTOutput("tablePartialCondEntropy")),
                   tabPanel("Alt Partial Entropy Table", DTOutput("tableAltPartialEntropy")),
                   tabPanel("Entropy Measures Plot", plotOutput("plotEntropyMeasures", height = "600px")),
                   tabPanel("Entropy Indices Plot",  plotOutput("plotIndices", height = "600px")),
                   tabPanel("Partial Cond Entropy Plot", plotOutput("plotPartialCondEntropy", height = "600px")),
                   tabPanel("Alt Partial Entropy Plot", plotOutput("plotAltPartialEntropy", height = "600px"))
                 )
               )
             )
    ),
    
    # 6) Sensitivity Analysis
    tabPanel("6. Sensitivity Analysis",
             sidebarLayout(
               sidebarPanel(
                 h4("Sensitivity Controls"),
                 tabsetPanel(
                   tabPanel("Criteria Weight",
                            selectInput("sens_weight_criterion", "Select Criterion:", choices = NULL),
                            numericInput("sens_weight_value", "New Subjective Weight:", value = 1, step = 0.1),
                            actionButton("apply_weight", "Apply Weight Change")
                   ),
                   tabPanel("Data Matrix Element",
                            selectInput("sens_data_criterion", "Select Criterion:", choices = NULL),
                            selectInput("sens_data_alternative", "Select Alternative:", choices = NULL),
                            numericInput("sens_data_value", "New Data Value (ξ):", value = 1, step = 0.1),
                            actionButton("apply_data", "Apply Data Change")
                   ),
                   tabPanel("Criteria Type",
                            selectInput("sens_type_criterion", "Select Criterion:", choices = NULL),
                            radioButtons("sens_type_value", "Select Type:", choices = c("benefit", "cost"), inline = TRUE),
                            actionButton("apply_type", "Apply Type Change")
                   ),
                   tabPanel("Preference Function",
                            selectInput("sens_pf_criterion", "Select Criterion:", choices = NULL),
                            selectInput("sens_pf_type", "Select Preference Function:",
                                        choices = c("Usual","U-Shape","V-Shape","Level","Linear","Gaussian")),
                            actionButton("apply_pf", "Apply Preference Function Change")
                   ),
                   tabPanel("Threshold",
                            selectInput("sens_threshold_criterion", "Select Criterion:", choices = NULL),
                            numericInput("sens_threshold_q", "New q:", value = 0, step = 0.01),
                            numericInput("sens_threshold_p", "New p:", value = 0, step = 0.01),
                            numericInput("sens_threshold_s", "New s:", value = 0, step = 0.01),
                            actionButton("apply_threshold", "Apply Threshold Change")
                   )
                 ),
                 hr(),
                 actionButton("reset_sensitivity", "Reset to Baseline", class="btn-danger")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Modified Results",
                            DTOutput("tableModAltScores"),
                            DTOutput("tableModCriteriaResults"),
                            plotOutput("plotModAltScores", height = "600px"),
                            plotOutput("plotModEntropy", height = "600px")
                   ),
                   tabPanel("Comparison",
                            DTOutput("tableCompIndices"),
                            plotOutput("plotCompIndices", height = "600px")
                   )
                 )
               )
             )
    ),
    
    # 7) Summary
    tabPanel("7. Summary",
             fluidRow(
               column(3,
                      div(style="text-align:center;",
                          img(src="https://cdn.thecollector.com/wp-content/uploads/2024/07/thinker-auguste-rodin-what-so-special.jpg?width=1400&quality=70",
                              width="160px", height="160px", style="border-radius:50%; border:2px solid black;"),
                          downloadButton("downloadAllSummary", "Download Results (Excel)", class="btn-primary", style="margin-top:15px;")
                      )
               ),
               column(9,
                      h3("Summary of Baseline Results"),
                      uiOutput("summaryResults")
               )
             )
    ),
    
    # 8) Settings
    tabPanel("8. Settings",
             fluidRow(
               column(12,
                      h3("Settings"),
                      fluidRow(
                        column(4,
                               h4("Font Size Adjustment"),
                               sliderInput("fontSize", "Select Font Size:",
                                           min = 12, max = 24, value = 16, step = 1)
                        )
                      )
               )
             )
    )
  )
)

########################################
# 3) SERVER
########################################
server <- function(input, output, session) {
  
  ########################################
  # A) Reactive Storage
  ########################################
  storedDataMat <- reactiveVal(NULL)
  storedSBJw    <- reactiveVal(NULL)
  storedPrefParams <- reactiveVal(NULL)
  storedBenefitCost <- reactiveVal(NULL)
  
  scenario <- reactiveValues(
    dataMat = NULL,
    SBJw = NULL,
    PrefParams = NULL,
    BenefitCost = NULL
  )
  
  # Baseline computations
  baselineResults <- reactive({
    req(storedDataMat(), storedSBJw(), storedPrefParams(), storedBenefitCost())
    computeModelOutputs(
      dataMat = storedDataMat(),
      sbjW = as.numeric(storedSBJw()),
      prefParams = storedPrefParams(),
      benefitCost = storedBenefitCost(),
      threshMult = 1
    )
  })
  
  ########################################
  # B) Data Import
  ########################################
  observeEvent(input$loadData, {
    req(input$fileExcel)
    # DataMatrix
    dfMatrix <- read_excel(input$fileExcel$datapath, sheet = input$sheetMatrix, col_names = TRUE)
    critNames <- dfMatrix[[1]]
    dataMatrix <- as.matrix(dfMatrix[,-1])
    rownames(dataMatrix) <- critNames
    colnames(dataMatrix) <- colnames(dfMatrix)[-1]
    
    # Subjective Weights
    dfWeights <- read_excel(input$fileExcel$datapath, sheet = input$sheetWeights, col_names = TRUE)
    subjectWeights <- sapply(critNames, function(x) {
      idx <- which(dfWeights$Criterion == x)
      if (length(idx) == 0) return(0) else return(dfWeights$Subjective_Weight[idx])
    })
    
    # PreferenceParams
    prefParams <- read_excel(input$fileExcel$datapath, sheet = input$sheetPrefParams, col_names = TRUE)
    # BenefitCost
    benefitCost <- read_excel(input$fileExcel$datapath, sheet = input$sheetBenefitCost, col_names = TRUE)
    
    storedDataMat(dataMatrix)
    storedSBJw(subjectWeights)
    storedPrefParams(prefParams)
    storedBenefitCost(benefitCost)
    
    # Initialize scenario
    scenario$dataMat <- dataMatrix
    scenario$SBJw <- subjectWeights
    scenario$PrefParams <- prefParams
    scenario$BenefitCost <- benefitCost
    
    updateSelectInput(session, "sens_weight_criterion", choices = critNames)
    updateSelectInput(session, "sens_data_criterion", choices = critNames)
    updateSelectInput(session, "sens_type_criterion", choices = critNames)
    updateSelectInput(session, "sens_pf_criterion", choices = critNames)
    updateSelectInput(session, "sens_threshold_criterion", choices = critNames)
    updateSelectInput(session, "sens_data_alternative", choices = colnames(dataMatrix))
    
    showNotification("Data loaded successfully!", type = "message")
  })
  
  # Previews
  output$tableMatrixPreview <- renderDT({
    req(storedDataMat())
    datatable(as.data.frame(storedDataMat()), options = list(pageLength = 5), rownames = TRUE) %>%
      formatRound(columns = 1:ncol(storedDataMat()), digits = 3)
  })
  
  output$tableWeightsPreview <- renderDT({
    req(storedSBJw())
    df <- data.frame(Criterion = rownames(storedDataMat()), Subjective_Weight = storedSBJw())
    datatable(df, options = list(pageLength = 5), rownames = FALSE) %>%
      formatRound(columns = "Subjective_Weight", digits = 3)
  })
  
  output$tablePrefParamsPreview <- renderDT({
    req(storedPrefParams())
    datatable(storedPrefParams(), options = list(pageLength = 5), rownames = FALSE) %>%
      formatRound(columns = c("Threshold_q","Threshold_p","Threshold_s"), digits = 3)
  })
  
  output$tableBCPreview <- renderDT({
    req(storedBenefitCost())
    datatable(storedBenefitCost(), options = list(pageLength = 5), rownames = FALSE)
  })
  
  output$tableNotation <- renderDT({
    df <- data.frame(
      Symbol = c("ξ(μν)","x(μ)^SBJ","x(μ)^OBJ","x(μ)^INT",
                 "S(X,Y)","S(X)","S(Y)","I(X;Y)","I(Y|X)","I(X,Y)",
                 "NMI","ADI","CES","CSF","NMGI"),
      Meaning = c(
        "Performance of criterion μ on alternative ν",
        "Subjective weight",
        "Objective weight (entropy-based)",
        "Integrated weight (product of OBJ & SBJ, normalized)",
        "Joint entropy of X and Y",
        "Entropy of criteria distribution",
        "Entropy of alternatives distribution",
        "Mutual information between X and Y",
        "Normalized conditional entropy of Y given X",
        "Normalized joint entropy of X and Y",
        "Normalized Mutual Information",
        "Alternatives Distinction Index",
        "Criteria Effectiveness Score",
        "Conditional Stability Factor",
        "Net Mutual Growth Index"
      )
    )
    datatable(df, options = list(pageLength = 20))
  })
  
  ########################################
  # C) Baseline Outputs
  ########################################
  output$tableAltScores <- renderDT({
    r <- baselineResults(); req(r)
    df <- data.frame(Alternative = colnames(storedDataMat()), Score = r$altScores)
    df <- df[order(-df$Score), ]
    datatable(df, options = list(pageLength = 10), rownames = FALSE) %>%
      formatRound(columns = "Score", digits = 3)
  })
  
  output$plotAltScores <- renderPlot({
    r <- baselineResults(); req(r)
    df_alt <- data.frame(
      Alternative = colnames(storedDataMat()),
      Score       = r$altScores
    )
    df_alt <- df_alt[order(-df_alt$Score), ]
    ggplot(df_alt, aes(x = reorder(Alternative, -Score), y = Score)) +
      geom_bar(stat = "identity", fill = "coral") +
      geom_text(aes(label = format3(Score)), vjust = -0.3, color = "white", size = 5) +
      labs(title = "Baseline Alternative Scores", x = "Alternative", y = "Score") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#FFFF00", size = 18, face = "bold"),
        axis.title = element_text(color = "#FFFFFF", size = 16),
        axis.text = element_text(color = "#FFFFFF", size = 14),
        panel.background = element_rect(fill = "#222222"),
        plot.background = element_rect(fill = "#222222")
      )
  })
  
  output$tableCriteriaResults <- renderDT({
    r <- baselineResults(); req(r)
    df <- data.frame(
      Criterion = rownames(storedDataMat()),
      xSBJ_Weight = storedSBJw(),
      xOBJ_Weight = r$xOBJ,
      xINT_Weight = r$xINT,
      Integrated_Criteria_Importance = r$critImport
    )
    datatable(df, options = list(pageLength = 10), rownames = FALSE) %>%
      formatRound(columns = c("xSBJ_Weight", "xOBJ_Weight", "xINT_Weight", "Integrated_Criteria_Importance"), digits = 3)
  })
  
  output$plotxSBJWeights <- renderPlot({
    critNames <- rownames(storedDataMat())
    sbj <- storedSBJw()
    df_subj <- data.frame(Criterion = critNames, Weight = sbj)
    df_subj <- df_subj[order(-df_subj$Weight), ]
    ggplot(df_subj, aes(x = reorder(Criterion, -Weight), y = Weight)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      geom_text(aes(label = format3(Weight)), vjust = -0.3, color = "white", size = 5) +
      labs(title = "Baseline xSBJ Criteria Weights", x = "Criterion", y = "Weight") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#FFFF00", size = 18, face = "bold"),
        axis.title = element_text(color = "#FFFFFF", size = 16),
        axis.text = element_text(color = "#FFFFFF", size = 14),
        panel.background = element_rect(fill = "#222222"),
        plot.background = element_rect(fill = "#222222")
      )
  })
  
  output$plotxOBJWeights <- renderPlot({
    r <- baselineResults(); req(r)
    critNames <- rownames(storedDataMat())
    df_obj <- data.frame(Criterion = critNames, Weight = r$xOBJ)
    df_obj <- df_obj[order(-df_obj$Weight), ]
    ggplot(df_obj, aes(x = reorder(Criterion, -Weight), y = Weight)) +
      geom_bar(stat = "identity", fill = "dodgerblue") +
      geom_text(aes(label = format3(Weight)), vjust = -0.3, color = "white", size = 5) +
      labs(title = "Baseline xOBJ Criteria Weights", x = "Criterion", y = "Weight") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#FFFF00", size = 18, face = "bold"),
        axis.title = element_text(color = "#FFFFFF", size = 16),
        axis.text = element_text(color = "#FFFFFF", size = 14),
        panel.background = element_rect(fill = "#222222"),
        plot.background = element_rect(fill = "#222222")
      )
  })
  
  output$plotxINTWeights <- renderPlot({
    r <- baselineResults(); req(r)
    critNames <- rownames(storedDataMat())
    df_int <- data.frame(Criterion = critNames, Weight = r$xINT)
    df_int <- df_int[order(-df_int$Weight), ]
    ggplot(df_int, aes(x = reorder(Criterion, -Weight), y = Weight)) +
      geom_bar(stat = "identity", fill = "orange") +
      geom_text(aes(label = format3(Weight)), vjust = -0.3, color = "white", size = 5) +
      labs(title = "Baseline xINT Criteria Weights", x = "Criterion", y = "Weight") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#FFFF00", size = 18, face = "bold"),
        axis.title = element_text(color = "#FFFFFF", size = 16),
        axis.text = element_text(color = "#FFFFFF", size = 14),
        panel.background = element_rect(fill = "#222222"),
        plot.background = element_rect(fill = "#222222")
      )
  })
  
  output$plotComparativeWeights <- renderPlot({
    r <- baselineResults(); req(r)
    critNames <- rownames(storedDataMat())
    df_comp <- data.frame(
      Criterion = critNames,
      xSBJ = storedSBJw(),
      xOBJ = r$xOBJ,
      xINT = r$xINT
    )
    df_melt <- melt(df_comp, id.vars = "Criterion", variable.name = "Weight_Type", value.name = "Weight")
    selected_weights <- input$selectWeights
    if (is.null(selected_weights)) {
      selected_weights <- c("xSBJ", "xOBJ", "xINT")
    }
    df_melt <- df_melt %>% filter(Weight_Type %in% selected_weights)
    ggplot(df_melt, aes(x = Criterion, y = Weight, color = Weight_Type, group = Weight_Type)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      geom_text_repel(aes(label = format3(Weight)), size = 3, color = "white",
                      box.padding = 0.1, point.padding = 0.1, segment.color = "transparent",
                      max.overlaps = Inf, show.legend = FALSE) +
      labs(title = "Comparative Criteria Weights (Baseline)", x = "Criterion", y = "Weight") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#FFFF00", size = 18, face = "bold"),
        axis.title = element_text(color = "#FFFFFF", size = 16),
        axis.text = element_text(color = "#FFFFFF", size = 14),
        legend.title = element_text(color = "#FFFF00", size = 16),
        legend.text = element_text(color = "#FFFFFF", size = 14),
        panel.background = element_rect(fill = "#222222"),
        plot.background = element_rect(fill = "#222222")
      ) +
      scale_color_manual(values = c("xSBJ" = "darkgreen", "xOBJ" = "dodgerblue", "xINT" = "orange")) +
      coord_cartesian(clip = 'off')
  })
  
  output$plotIntegratedCI <- renderPlot({
    r <- baselineResults(); req(r)
    critNames <- rownames(storedDataMat())
    df_ci <- data.frame(
      Criterion = critNames,
      ICI = r$critImport
    )
    df_ci <- df_ci[order(-df_ci$ICI), ]
    ggplot(df_ci, aes(x = reorder(Criterion, -ICI), y = ICI)) +
      geom_bar(stat = "identity", fill = "cyan") +
      geom_text(aes(label = format3(ICI)), vjust = -0.3, color = "white", size = 5) +
      labs(title = "Baseline Integrated Criteria Importance", x = "Criterion", y = "ICI") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#FFFF00", size = 18, face = "bold"),
        axis.title = element_text(color = "#FFFFFF", size = 16),
        axis.text = element_text(color = "#FFFFFF", size = 14),
        panel.background = element_rect(fill = "#222222"),
        plot.background = element_rect(fill = "#222222")
      )
  })
  
  # 5) Entropy Measures & Indices
  output$tableGlobalEntropies <- renderDT({
    r <- baselineResults(); req(r)
    df <- data.frame(
      "S(Y)"       = r$overallEntropyY,
      "S(Y|X)"     = r$totalCondEntropy,
      "I(Y|X)"     = r$normTotalCondEntropy,
      "S(X,Y)"     = r$SXY,
      "S(X)"       = r$Sx,
      "I(X,Y)"     = r$IXY
    )
    df_t <- as.data.frame(t(df))
    colnames(df_t) <- "Value"
    datatable(df_t, options = list(pageLength = 10), rownames = TRUE) %>%
      formatRound(columns = "Value", digits = 3)
  })
  
  output$tableIndices <- renderDT({
    r <- baselineResults(); req(r)
    df <- data.frame(
      NMI = r$NMI,
      CES = r$CES,
      CSF = r$CSF,
      ADI = r$ADI
    )
    datatable(df, options = list(pageLength = 10), rownames = FALSE) %>%
      formatRound(columns = c("NMI","CES","CSF","ADI"), digits = 3)
  })
  
  output$tableNMGI <- renderDT({
    r <- baselineResults(); req(r)
    df <- data.frame(NMGI = r$NMGI)
    datatable(df, options = list(pageLength = 10), rownames = FALSE) %>%
      formatRound(columns = "NMGI", digits = 3)
  })
  
  output$tablePartialCondEntropy <- renderDT({
    r <- baselineResults(); req(r)
    df <- data.frame(Criterion = rownames(storedDataMat()), Partial_Cond_Entropy = r$partialCondEntropy)
    datatable(df, options = list(pageLength = 10), rownames = FALSE) %>%
      formatRound(columns = "Partial_Cond_Entropy", digits = 3)
  })
  
  output$tableAltPartialEntropy <- renderDT({
    r <- baselineResults(); req(r)
    df <- as.data.frame(r$partialAltEntropy)
    df <- cbind(Criterion = rownames(storedDataMat()), df)
    datatable(df, options = list(pageLength = 10), rownames = FALSE) %>%
      formatRound(columns = colnames(df)[-1], digits = 3)
  })
  
  output$plotEntropyMeasures <- renderPlot({
    r <- baselineResults(); req(r)
    df_entropy <- data.frame(
      Measure = c("S(Y)", "S(Y|X)", "I(Y|X)", "S(X,Y)", "S(X)", "I(X,Y)"),
      Value   = c(r$overallEntropyY, r$totalCondEntropy, r$normTotalCondEntropy,
                  r$SXY, r$Sx, r$IXY)
    )
    df_entropy$Measure <- factor(df_entropy$Measure, levels = df_entropy$Measure)
    ggplot(df_entropy, aes(x = Measure, y = Value, fill = Measure)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = format3(Value)), vjust = -0.3, color = "white", size = 5) +
      labs(title = "Baseline Entropy Measures", x = "Measure", y = "Value") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#FFFF00", size = 18, face = "bold"),
        axis.title = element_text(color = "#FFFFFF", size = 16),
        axis.text = element_text(color = "#FFFFFF", size = 14),
        legend.position = "none",
        panel.background = element_rect(fill = "#222222"),
        plot.background = element_rect(fill = "#222222")
      )
  })
  
  output$plotIndices <- renderPlot({
    r <- baselineResults(); req(r)
    df_idx <- data.frame(
      Index = c("NMI", "CES", "CSF", "ADI", "NMGI"),
      Value = c(r$NMI, r$CES, r$CSF, r$ADI, r$NMGI)
    )
    df_idx <- df_idx[order(-df_idx$Value), ]
    ggplot(df_idx, aes(x = reorder(Index, -Value), y = Value)) +
      geom_bar(stat = "identity", fill = "purple") +
      geom_text(aes(label = format3(Value)), vjust = -0.3, color = "white", size = 5) +
      labs(title = "Baseline Entropy Indices (Descending)", x = "Index", y = "Value") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#FFFF00", size = 18, face = "bold"),
        axis.title = element_text(color = "#FFFFFF", size = 16),
        axis.text = element_text(color = "#FFFFFF", size = 14),
        panel.background = element_rect(fill = "#222222"),
        plot.background = element_rect(fill = "#222222")
      )
  })
  
  output$plotPartialCondEntropy <- renderPlot({
    r <- baselineResults(); req(r)
    df <- data.frame(Criterion = rownames(storedDataMat()), Partial_Cond_Entropy = r$partialCondEntropy)
    ggplot(df, aes(x = reorder(Criterion, -Partial_Cond_Entropy), y = Partial_Cond_Entropy)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = format3(Partial_Cond_Entropy)), vjust = -0.3, color = "white", size = 5) +
      labs(title = "Partial Conditional Entropy per Criterion", x = "Criterion", y = "Entropy") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#FFFF00", size = 18, face = "bold"),
        axis.title = element_text(color = "#FFFFFF", size = 16),
        axis.text = element_text(color = "#FFFFFF", size = 14),
        panel.background = element_rect(fill = "#222222"),
        plot.background = element_rect(fill = "#222222")
      )
  })
  
  # >>> ADJUSTED: Alt Partial Entropy Plot as a STACKED BAR CHART
  output$plotAltPartialEntropy <- renderPlot({
    r <- baselineResults(); req(r)
    altNames <- colnames(storedDataMat())
    critNames <- rownames(storedDataMat())
    df <- as.data.frame(r$partialAltEntropy)
    colnames(df) <- altNames
    df$Criterion <- critNames
    df_long <- reshape2::melt(df, id.vars = "Criterion", variable.name = "Alternative", value.name = "Partial_Entropy")
    
    ggplot(df_long, aes(x = Alternative, y = Partial_Entropy, fill = Criterion)) +
      geom_bar(stat = "identity", position = "stack") +
      # If you want to label the total sum, you can compute it separately, but
      # labeling each stacked portion can cause overlaps if values are small.
      labs(title = "Partial Alternative Entropy (Stacked)", x = "Alternative", y = "Entropy") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#FFFF00", size = 18, face = "bold"),
        axis.title = element_text(color = "#FFFFFF", size = 16),
        axis.text = element_text(color = "#FFFFFF", size = 14),
        panel.background = element_rect(fill = "#222222"),
        plot.background = element_rect(fill = "#222222")
      )
  })
  
  ########################################
  # D) Sensitivity Analysis
  ########################################
  modifiedResults <- reactive({
    req(scenario$dataMat, scenario$SBJw, scenario$PrefParams, scenario$BenefitCost)
    computeModelOutputs(
      dataMat = scenario$dataMat,
      sbjW = as.numeric(scenario$SBJw),
      prefParams = scenario$PrefParams,
      benefitCost = scenario$BenefitCost,
      threshMult = 1
    )
  })
  
  observeEvent(input$apply_weight, {
    req(scenario$SBJw)
    crit <- input$sens_weight_criterion
    newVal <- input$sens_weight_value
    currWeights <- scenario$SBJw
    names(currWeights) <- rownames(scenario$dataMat)
    currWeights[crit] <- newVal
    scenario$SBJw <- currWeights
    showNotification(paste("Subjective weight for", crit, "updated to", newVal), type = "message")
  })
  
  observeEvent(input$apply_data, {
    req(scenario$dataMat)
    crit <- input$sens_data_criterion
    alt <- input$sens_data_alternative
    newVal <- input$sens_data_value
    mat <- scenario$dataMat
    if (crit %in% rownames(mat) && alt %in% colnames(mat)) {
      mat[crit, alt] <- as.numeric(newVal)
      scenario$dataMat <- mat
      showNotification(paste0("Data element (", crit, ",", alt, ") updated to ", format3(newVal)), type = "message")
    } else {
      showNotification("Invalid criterion or alternative selected!", type = "error")
    }
  })
  
  observeEvent(input$apply_type, {
    req(scenario$BenefitCost)
    crit <- input$sens_type_criterion
    newType <- input$sens_type_value
    df <- scenario$BenefitCost
    idx <- which(df$Criterion == crit)
    if (length(idx) > 0) {
      df$Type[idx] <- newType
    } else {
      df <- rbind(df, data.frame(Criterion = crit, Type = newType))
    }
    scenario$BenefitCost <- df
    showNotification(paste("Type for", crit, "set to", newType), type = "message")
  })
  
  observeEvent(input$apply_pf, {
    req(scenario$PrefParams)
    crit <- input$sens_pf_criterion
    newPF <- input$sens_pf_type
    df <- scenario$PrefParams
    idx <- which(df$Criterion == crit)
    if (length(idx) > 0) {
      df$PreferenceFunction[idx] <- newPF
    } else {
      df <- rbind(df, data.frame(Criterion = crit,
                                 PreferenceFunction = newPF,
                                 Threshold_q=0, Threshold_p=0, Threshold_s=0))
    }
    scenario$PrefParams <- df
    showNotification(paste("Preference function for", crit, "set to", newPF), type = "message")
  })
  
  observeEvent(input$apply_threshold, {
    req(scenario$PrefParams)
    crit <- input$sens_threshold_criterion
    newQ <- input$sens_threshold_q
    newP <- input$sens_threshold_p
    newS <- input$sens_threshold_s
    df <- scenario$PrefParams
    idx <- which(df$Criterion == crit)
    if (length(idx) > 0) {
      df$Threshold_q[idx] <- newQ
      df$Threshold_p[idx] <- newP
      df$Threshold_s[idx] <- newS
    } else {
      df <- rbind(df, data.frame(Criterion = crit,
                                 PreferenceFunction = "V-Shape",
                                 Threshold_q=newQ, Threshold_p=newP, Threshold_s=newS))
    }
    scenario$PrefParams <- df
    showNotification(paste("Thresholds for", crit, "set to q=", newQ, ", p=", newP, ", s=", newS), type = "message")
  })
  
  observeEvent(input$reset_sensitivity, {
    req(storedDataMat(), storedSBJw(), storedPrefParams(), storedBenefitCost())
    scenario$dataMat <- storedDataMat()
    scenario$SBJw <- storedSBJw()
    scenario$PrefParams <- storedPrefParams()
    scenario$BenefitCost <- storedBenefitCost()
    showNotification("Sensitivity modifications reset to baseline.", type = "warning")
  })
  
  output$tableModAltScores <- renderDT({
    r <- modifiedResults(); req(r)
    df <- data.frame(Alternative = colnames(scenario$dataMat), Score = r$altScores)
    df <- df[order(-df$Score), ]
    datatable(df, options = list(pageLength = 10), rownames = FALSE) %>%
      formatRound(columns = "Score", digits = 3)
  })
  
  output$tableModCriteriaResults <- renderDT({
    r <- modifiedResults(); req(r)
    df <- data.frame(
      Criterion = rownames(scenario$dataMat),
      xSBJ_Weight = scenario$SBJw,
      xOBJ_Weight = r$xOBJ,
      xINT_Weight = r$xINT,
      Integrated_Criteria_Importance = r$critImport
    )
    datatable(df, options = list(pageLength = 10), rownames = FALSE) %>%
      formatRound(columns = c("xSBJ_Weight", "xOBJ_Weight", "xINT_Weight", "Integrated_Criteria_Importance"), digits = 3)
  })
  
  output$plotModAltScores <- renderPlot({
    r <- modifiedResults(); req(r)
    dfA <- data.frame(Alternative = colnames(scenario$dataMat), Score = r$altScores)
    dfA <- dfA[order(-dfA$Score), ]
    ggplot(dfA, aes(x = reorder(Alternative, -Score), y = Score)) +
      geom_bar(stat = "identity", fill = "coral") +
      geom_text(aes(label = format3(Score)), vjust = -0.3, color = "white", size = 5) +
      labs(title = "Modified Alternative Scores", x = "Alternative", y = "Score") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#FFFF00", size = 18, face = "bold"),
        axis.title = element_text(color = "#FFFFFF", size = 16),
        axis.text = element_text(color = "#FFFFFF", size = 14),
        panel.background = element_rect(fill = "#222222"),
        plot.background = element_rect(fill = "#222222")
      )
  })
  
  output$plotModEntropy <- renderPlot({
    r <- modifiedResults(); req(r)
    df_entropy <- data.frame(
      Metric = c("S(Y)", "S(Y|X)", "I(Y|X)", "S(X,Y)", "S(X)", "NMI", "CES", "CSF", "ADI", "NMGI"),
      Value = c(r$overallEntropyY, r$totalCondEntropy, r$normTotalCondEntropy,
                r$SXY, r$Sx, r$NMI, r$CES, r$CSF, r$ADI, r$NMGI)
    )
    ggplot(df_entropy, aes(x = reorder(Metric, -Value), y = Value, fill = Metric)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = format3(Value)), vjust = -0.3, color = "white", size = 4) +
      labs(title = "Modified Entropy Measures & Indices", x = "Metric", y = "Value") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#FFFF00", size = 18, face = "bold"),
        axis.title = element_text(color = "#FFFFFF", size = 16),
        axis.text = element_text(color = "#FFFFFF", size = 14),
        legend.position = "none",
        panel.background = element_rect(fill = "#222222"),
        plot.background = element_rect(fill = "#222222")
      ) +
      coord_flip()
  })
  
  output$tableCompIndices <- renderDT({
    base <- baselineResults()
    mod <- modifiedResults()
    df <- data.frame(
      Metric = c("S(Y)", "S(Y|X)", "I(Y|X)", "S(X,Y)", "S(X)", "J(X;Y)",
                 "I(X,Y)", "NMI", "CES", "CSF", "ADI", "NMGI"),
      Baseline = c(base$overallEntropyY, base$totalCondEntropy, base$normTotalCondEntropy,
                   base$SXY, base$Sx, base$Jxy, base$IXY, base$NMI, base$CES, base$CSF, base$ADI, base$NMGI),
      Modified = c(mod$overallEntropyY, mod$totalCondEntropy, mod$normTotalCondEntropy,
                   mod$SXY, mod$Sx, mod$Jxy, mod$IXY, mod$NMI, mod$CES, mod$CSF, mod$ADI, mod$NMGI)
    )
    datatable(df, options = list(pageLength = 12), rownames = FALSE) %>%
      formatRound(columns = c("Baseline","Modified"), digits = 3)
  })
  
  output$plotCompIndices <- renderPlot({
    base <- baselineResults()
    mod <- modifiedResults()
    metrics <- c("S(Y)", "S(Y|X)", "I(Y|X)", "S(X,Y)", "S(X)", "J(X;Y)", "I(X,Y)", "NMI", "CES", "CSF", "ADI", "NMGI")
    df <- data.frame(
      Metric = rep(metrics, 2),
      Value = c(
        base$overallEntropyY, base$totalCondEntropy, base$normTotalCondEntropy, base$SXY, base$Sx,
        base$Jxy, base$IXY, base$NMI, base$CES, base$CSF, base$ADI, base$NMGI,
        mod$overallEntropyY, mod$totalCondEntropy, mod$normTotalCondEntropy, mod$SXY, mod$Sx,
        mod$Jxy, mod$IXY, mod$NMI, mod$CES, mod$CSF, mod$ADI, mod$NMGI
      ),
      Scenario = rep(c("Baseline", "Modified"), each = length(metrics))
    )
    df$Metric <- factor(df$Metric, levels = metrics)
    ggplot(df, aes(x = Metric, y = Value, fill = Scenario)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
      labs(title = "Comparison of Baseline vs. Modified Indices", x = "Metric", y = "Value") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#FFFF00", size = 18, face = "bold"),
        axis.title = element_text(color = "#FFFFFF", size = 16),
        axis.text = element_text(color = "#FFFFFF", size = 14),
        panel.background = element_rect(fill = "#222222"),
        plot.background = element_rect(fill = "#222222")
      )
  })
  
  ########################################
  # 7) Summary
  ########################################
  output$summaryResults <- renderUI({
    r <- baselineResults(); req(r)
    altNames <- colnames(storedDataMat())
    dfAlt <- data.frame(Alternative = altNames, Score = r$altScores)
    dfAlt <- dfAlt[order(-dfAlt$Score), ]
    topAlt <- dfAlt$Alternative[1]
    topScore <- dfAlt$Score[1]
    
    critNames <- rownames(storedDataMat())
    dfICI <- data.frame(Criterion = critNames, ICI = r$critImport)
    dfICI <- dfICI[order(-dfICI$ICI), ]
    topCrit <- dfICI$Criterion[1]
    topVal <- dfICI$ICI[1]
    
    detailedInsights <- generateDetailedEntropyInsights(
      SXY = r$SXY,
      SX = r$Sx,
      SY = r$overallEntropyY,
      IXY = r$IXY,
      SYX = r$normTotalCondEntropy,
      IYX = r$Jxy,
      NMI = r$NMI,
      ADI = r$ADI,
      CES = r$CES,
      CSF = r$CSF,
      NMGI = r$NMGI
    )
    
    HTML(paste0(
      "<table style='width:100%; border-collapse:collapse; border:1px solid black;'>",
      "<tr style='background-color:#ffff00;'>",
      "<th colspan='2' style='text-align:left; padding:8px; border:1px solid black; color:black; font-weight:bold;'>Metric</th></tr>",
      
      "<tr><td style='padding:8px; border:1px solid black;'><strong>Optimal Alternative</strong></td>",
      "<td style='padding:8px; border:1px solid black;'>", topAlt, " (Score: <strong>", format3(topScore), "</strong>)</td></tr>",
      
      "<tr><td style='padding:8px; border:1px solid black;'><strong>Most Significant Criterion</strong></td>",
      "<td style='padding:8px; border:1px solid black;'>", topCrit, " (ICI: <strong>", format3(topVal), "</strong>)</td></tr>",
      
      "<tr style='background-color:#ffff00;'>",
      "<th colspan='2' style='text-align:left; padding:8px; border:1px solid black; color:black; font-weight:bold;'>Key Indices</th></tr>",
      
      "<tr><td style='padding:8px; border:1px solid black;'>S(X,Y) - Joint Entropy</td>",
      "<td style='padding:8px; border:1px solid black;'>", format3(r$SXY), "</td></tr>",
      
      "<tr><td style='padding:8px; border:1px solid black;'>S(X) - Criteria Entropy</td>",
      "<td style='padding:8px; border:1px solid black;'>", format3(r$Sx), "</td></tr>",
      
      "<tr><td style='padding:8px; border:1px solid black;'>S(Y) - Alternatives Entropy</td>",
      "<td style='padding:8px; border:1px solid black;'>", format3(r$overallEntropyY), "</td></tr>",
      
      "<tr><td style='padding:8px; border:1px solid black;'>I(X,Y) - Normalized Joint Entropy</td>",
      "<td style='padding:8px; border:1px solid black;'>", format3(r$IXY), "</td></tr>",
      
      "<tr><td style='padding:8px; border:1px solid black;'>I(Y|X) - Conditional Entropy</td>",
      "<td style='padding:8px; border:1px solid black;'>", format3(r$normTotalCondEntropy), "</td></tr>",
      
      "<tr><td style='padding:8px; border:1px solid black;'>I(X;Y) - Mutual Information</td>",
      "<td style='padding:8px; border:1px solid black;'>", format3(r$Jxy), "</td></tr>",
      
      "<tr><td style='padding:8px; border:1px solid black;'>Normalized Mutual Information (NMI)</td>",
      "<td style='padding:8px; border:1px solid black;'>", format3(r$NMI), "</td></tr>",
      
      "<tr><td style='padding:8px; border:1px solid black;'>Alternatives Distinction Index (ADI)</td>",
      "<td style='padding:8px; border:1px solid black;'>", format3(r$ADI), "</td></tr>",
      
      "<tr><td style='padding:8px; border:1px solid black;'>Criteria Effectiveness Score (CES)</td>",
      "<td style='padding:8px; border:1px solid black;'>", format3(r$CES), "</td></tr>",
      
      "<tr><td style='padding:8px; border:1px solid black;'>Conditional Stability Factor (CSF)</td>",
      "<td style='padding:8px; border:1px solid black;'>", format3(r$CSF), "</td></tr>",
      
      "<tr><td style='padding:8px; border:1px solid black;'>Net Mutual Growth Index (NMGI)</td>",
      "<td style='padding:8px; border:1px solid black;'>", format3(r$NMGI), "</td></tr>",
      
      "<tr style='background-color:#ffff00;'>",
      "<th colspan='2' style='text-align:left; padding:8px; border:1px solid black; color:black; font-weight:bold;'>Detailed Insights</th></tr>",
      
      "<tr><td colspan='2' style='padding:8px; border:1px solid black;'>", detailedInsights, "</td></tr>",
      "</table>"
    ))
  })
  
  ########################################
  # 8) Settings - Font Size
  ########################################
  observe({
    fontSize <- input$fontSize
    runjs(sprintf("$('body').css('font-size', '%spx');", fontSize))
  })
  
  ########################################
  # Download All Results
  ########################################
  output$downloadAllSummary <- downloadHandler(
    filename = function(){ "PESMADMII_Summary_Results.xlsx" },
    content = function(file) {
      r <- baselineResults(); req(r)
      df_obj <- data.frame(
        Criterion = rownames(storedDataMat()),
        Objective_Weight = r$xOBJ
      )
      df_int <- data.frame(
        Criterion = rownames(storedDataMat()),
        Subjective_Weight = storedSBJw(),
        Objective_Weight = r$xOBJ,
        Integrated_Weight = r$xINT
      )
      altNames <- colnames(storedDataMat())
      df_alt <- data.frame(Alternative = altNames, Score = r$altScores)
      df_alt <- df_alt[order(-df_alt$Score), ]
      
      df_indices <- data.frame(
        S_Y = r$overallEntropyY,
        Total_Cond_Entropy = r$totalCondEntropy,
        Norm_Cond_Entropy = r$normTotalCondEntropy,
        S_X = r$Sx,
        S_XY = r$SXY,
        J_XY = r$Jxy,
        I_XY = r$IXY,
        NMI = r$NMI,
        CES = r$CES,
        CSF = r$CSF,
        ADI = r$ADI,
        NMGI = r$NMGI
      )
      
      df_partialCondEntropy <- data.frame(
        Criterion = rownames(storedDataMat()),
        Partial_Cond_Entropy = r$partialCondEntropy
      )
      df_altPartialEntropy <- as.data.frame(r$partialAltEntropy)
      df_altPartialEntropy <- cbind(Criterion = rownames(storedDataMat()), df_altPartialEntropy)
      
      writexl::write_xlsx(list(
        Criteria_Objective = df_obj,
        Criteria_Integrated = df_int,
        Alternatives = df_alt,
        Indices_Entropy = df_indices,
        Partial_Cond_Entropy = df_partialCondEntropy,
        Alt_Partial_Entropy = df_altPartialEntropy
      ), path = file)
    }
  )
}

########################################
# 4) Run the App
########################################
shinyApp(ui = ui, server = server)
