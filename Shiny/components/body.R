linebreaks <- function(n){HTML(strrep(br(), n))}

body <- dashboardBody(
  tabItems(
    
    ## introducion
    tabItem(
      tabName = "intro",
      h2("Introduction"),
      h4("This application was developed to predict cardiotoxicity using machine learning."),
      linebreaks(1),
      h4("Cardiotoxicity refers to any heart damage arising from cancer treatment"),
      h4("In this project, Cardiotoxicity was defined as a absolute decline in LVEF by > 10% 
         with an accompanying relative decline in LVGLS > 15%"),
      linebreaks(1),
      h2("FAQs"),
      h4("Principal Investigator : Iksung Cho"),
      h4("Affiliation : Division of Cardiology, Severance Cardiovascular Hospital, Yonsei University College of Medicine"),
      h4("E-mail : iksungcho@yuhs.ac")
      
    ),
    
    ## dashboard
    tabItem(
      tabName = "dashboard",
      fluidRow(
        box(width = 8,
            title = "Set parameters",
            
            column(6, numericInput("age", "Age", 18, min = 18, max = 100),
                   numericInput("HTN", "History of HTN (yes=1, no=0)", 0, min = 0, max = 1),
                   numericInput("DM", "History of DM (yes=1, no=0)", 0, min = 0, max = 1),
                   numericInput("Dyslipidemia", "History of Dyslipidemia (yes=1, no=0)", 0, min = 0, max = 1),
                   numericInput("CAOD", "History of CAOD (yes=1, no=0)", 0, min = 0, max = 1),
                   numericInput("aFib", "History of A.fib (yes=1, no=0)", 0, min = 0, max = 1),
                   numericInput("CKD", "History of CKD (yes=1, no=0)", 0, min = 0, max = 1),
                   numericInput("cancerStage", "Stage", 1, min = 1, max = 4)
            ),
            
            column(6, numericInput("baseEF", "Baseline EF (before chemotherapy)", 55, min = 0, max = 100),
                   numericInput("baseLvGls", "Baseline LVGLS (before chemotherapy)", -15, min = -30, max = 0),
                   numericInput("firstEF", "First EF (after chemotherapy)", 55, min = 0, max = 100),
                   numericInput("firstLvGls", "First EF (after chemotherapy)", -15, min = -30, max = 0),
                   selectInput("RTx", "Radiotherapy", c("No" = "0",
                                                         "Yes(Right)" = "1",
                                                         "Yes(Left)" = "2")),
                   selectInput("cancerType", "Type of cancer",  c("Breast Cancer" = "0",
                                                                  "Scarcoma" = "1")),
                   selectInput("chemoType", "Type of chemotherapy",  c("Herceptin mono" = "0",
                                                                       "Adriamycin mono" = "1",
                                                                       "Herceptin with neoadjuvant Adriamycin" = "2",
                                                                       "Adjuvant Adriamycin with Herceptin" = "3",
                                                                       "Palliative Adriamycin with Herceptin" = "4",
                                                                       "DVALOP (Adriamycin + ICI)" = "5"))
            ),
            
            column(12, actionButton(
              inputId = "submit",
              label = "Submit"),
              align = "right",
              style = "margin-top: 10px; margin-right: 10px;"
            )
        ),
        box(width = 8,
            h4(textOutput("pred_result",inline = T))
        )
      )
    )
  )
)
