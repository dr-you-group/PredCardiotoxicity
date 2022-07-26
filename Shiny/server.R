server <- function(input, output, session) {
  
  testdata <- reactive({
    
    testdata <- c()
    
    testdata$cancerType1 <- ifelse(as.numeric(input$cancerType) == 0, 1, 0)
    testdata$cancerType2 <- ifelse(as.numeric(input$cancerType) == 1, 1, 0)
    testdata$chemoType1 <- ifelse(as.numeric(input$chemoType) == 0, 1, 0)
    testdata$chemoType2 <- ifelse(as.numeric(input$chemoType) == 1, 1, 0)
    testdata$chemoType3 <- ifelse(as.numeric(input$chemoType) == 2, 1, 0)
    testdata$chemoType4 <- ifelse(as.numeric(input$chemoType) == 3, 1, 0)
    testdata$chemoType5 <- ifelse(as.numeric(input$chemoType) == 4, 1, 0)
    testdata$chemoType6 <- ifelse(as.numeric(input$chemoType) == 5, 1, 0)
    testdata$cancerStage1 <- ifelse(input$cancerStage == 1, 1, 0)
    testdata$cancerStage2 <- ifelse(input$cancerStage == 2, 1, 0)
    testdata$cancerStage3 <- ifelse(input$cancerStage == 3, 1, 0)
    testdata$cancerStage4 <- ifelse(input$cancerStage == 4, 1, 0)
    testdata$HTN0 <- ifelse(input$HTN == 0, 1, 0)
    testdata$HTN1 <- ifelse(input$HTN == 1, 1, 0)
    testdata$DM0 <- ifelse(input$DM == 0, 1, 0)
    testdata$DM1 <- ifelse(input$DM == 1, 1, 0)
    testdata$Dyslipidemia0 <- ifelse(input$Dyslipidemia == 0, 1, 0)
    testdata$Dyslipidemia1 <- ifelse(input$Dyslipidemia == 1, 1, 0)
    testdata$CAOD0 <- ifelse(input$CAOD == 0, 1, 0)
    testdata$CAOD1 <- ifelse(input$CAOD == 1, 1, 0)
    testdata$aFib0 <- ifelse(input$aFib == 0, 1, 0)
    testdata$aFib1 <- ifelse(input$aFib== 1, 1, 0)
    testdata$CKD0 <- ifelse(input$CKD == 0, 1, 0)
    testdata$CKD1 <- ifelse(input$CKD == 1, 1, 0)
    testdata$RTx0 <- ifelse(as.numeric(input$RTx) == 0, 1, 0)
    testdata$RTx1 <- ifelse(as.numeric(input$RTx) == 1, 1, 0)
    testdata$RTx2 <- ifelse(as.numeric(input$RTx) == 2, 1, 0)
    testdata$Age <- input$age
    testdata$baseEF <- input$baseEF
    testdata$baseLvGls <- input$baseLvGls
    testdata$deltaEF <- input$baseEF - input$firstEF
    testdata$deltaLvGls <- input$baseLvGls - input$firstLvGls

    testdata <- as.data.frame(testdata)
    
  })
  
  pred <- eventReactive(input$submit, {
    pred_xgb <- predict(fitXgbFinal, newdata=testdata(), type="prob")
    pred <- pred_xgb[,2]
  })
  
  output$pred_result <- renderText({
    if (pred() > threshold)
    {"The risk of cardiotoxicity is high."
    } else {
      "The risk of cardiotoxicity is low."
    }})
  
}
