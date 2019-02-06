Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')

function(input, output,session) {
  
  ### Argument names:
  ArgNames <- reactive({
    Names <- names(formals(input$readFunction)[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  
  # Argument selector:
  output$ArgSelect <- renderUI({
    if (length(ArgNames()) == 0) return(NULL)
    
    selectInput("arg","Argument:",ArgNames())
  })
  
  ## Arg text field:
  output$ArgText <- renderUI({
    fun__arg <- paste0(input$readFunction,"",input$arg)
    
    if (is.null(input$arg)) return(NULL)
    
    Defaults <- formals(input$readFunction)
    
    if (is.null(input[[fun__arg]]))
    {
      textInput(fun__arg, label = "Enter value:", value = deparse(Defaults[[input$arg]]))
    } else {
      textInput(fun__arg, label = "Enter value:", value = input[[fun__arg]])
    }
  })
  
  
  ### Data import:
  Dataset <- reactive({
    if (is.null(input$file)) {
      #User has not uploaded a file yet
      return(NULL)
    }
    
    args <- grep(paste0("^",input$readFunction,""), names(input), value = TRUE)
    
    argList <- list()
    for (i in seq_along(args))
    {
      argList[[i]] <- eval(parse(text = input[[args[i]]]))
    }
    names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
    
    argList <- argList[names(argList) %in% ArgNames()]
    
    Dataset <- as.data.frame(do.call(input$readFunction,c(list(input$file$datapath),argList)))
    
    
  })
  
  start.date <- reactive({input$dateRange[1]})
  end.date <- reactive({input$dateRange[2]})
  Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')
  
  ##RENDERTABLE
  output$rawData <- renderDataTable({
    # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')

    if (!is.null(Dataset())) {
      sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
    }

    #datatable query
    ord.qry <- sprintf("select RequestDate,RequestTimeUtc,
                       FirstName,LastName,City, Department, Distancemi,Durationmin,
                       TotalChargeinKES,ExpenseCode from UberDataTest
                       WHERE RequestDate between '%s' AND '%s'order by RequestDate asc",
                       start.date(), end.date())
    tableu <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, ord.qry)
    })
    # # # Show table:

   
    DT::datatable(tableu(), options = list(pageLength = 10, lengthMenu = c(10, 15, 25)))

  })
  
  ##RENDER TABLE SUMMARY
  output$summaryData <- renderTable({
    
    
    # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')
    
    if (!is.null(Dataset())) {
      sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
    }
    
    #Summaryquery
    summaryQuery <- sprintf("select LastName, sum(Distancemi) as TotalDistance, count(LastName) as Freq,
                              sum(TotalChargeinKES) as TotalCharges from UberDataTest
                              WHERE RequestDate between '%s' AND '%s' group by LastName ",
                            start.date(), end.date())
    
    
    summaryTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, summaryQuery)
    })
    
    #Total Summary
    summaryTable()%>%adorn_totals("row") #From janitor package
    
  })
  
  ###plot Company summary data
  output$SummaryPlot <- renderPlotly({
    if (!is.null(Dataset())) {
      sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
    }
    
    
    #datatable query
    summaryQuery <- sprintf("select LastName, sum(Distancemi) as TotalDistance, count(LastName) as Freq,
                              sum(TotalChargeinKES) as TotalCharges from UberDataTest
                              WHERE RequestDate between '%s' AND '%s' group by LastName ",
                            start.date(), end.date())
    
    summaryTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, summaryQuery)
    })
    fontSpec<-list(
      size = 10)
    
    p1 <- plot_ly(y = summaryTable()$LastName, x = summaryTable()$TotalDistance, type = 'bar', name= "Total Distance") %>%
      layout(title = "TOTAL CHARGES , TOTAL DISTANCE BY EXPENSE CODE",
             xaxis = list(showgrid = T, zeroline = T, showticklabels = T),
             yaxis = list(showgrid = T, zeroline = T, showticklabels = T),
             list(title = "Expense Code"))
              
    
    p2 <- plot_ly(y = summaryTable()$LastName, x = summaryTable()$TotalCharges, type = 'bar', name= "Total Charges") #%>%

    p3 <- plot_ly(y = summaryTable()$LastName, x = summaryTable()$Freq, type = 'bar', name= "Frequency") #%>%
    
    subplot(p1, p2, p3, shareY = TRUE)
    
  })
  

  ####RENDER TABLE SUMMARY
  ####expense code per person
  # 
  # # output$TripTypeData <- renderDataTable({
  # # 
  # # 
  # #   # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')
  # # 
  # #   if (!is.null(Dataset())) {
  # #     sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
  # #   }
  # # 
  # #   #Summaryquery
  # #   TripTypeByPersonQry <- sprintf("select ExpenseCode, LastName, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
  # #                          sum(TotalChargeinKES) as TotalCharges from UberDataTest
  # #                          WHERE RequestDate between '%s' AND '%s'group by ExpenseCode, LastName",
  # #                          start.date(), end.date())
  # # 
  # # 
  # # 
  # #   TripTypeTable <- eventReactive(input$refresh, {
  # #     sqlQuery(Myconnection, TripTypeByPersonQry)
  # # 
  # #   })
  # # 
  # #   TripTypeQryReactive <- reactive({
  # #     (TripTypeTable()[TripTypeTable()$ExpenseCode == input$TypeTrip, ])
  # #   })
  # # 
  # # 
  # #   DT::datatable(TripTypeQryReactive(), options = list(pageLength = 50, dom = 't'))
  # #   #DT::datatable(tableu(), options = list(pageLength = 10, lengthMenu = c(10, 15, 25)))
  # # 
  # # })
  
  
  ####expense code per person PLOT 
  
  output$TripTypePlot <- renderPlotly({
    
    
    TripTypeByPersonQry <- sprintf("select ExpenseCode, LastName, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
                           sum(TotalChargeinKES) as TotalCharges from UberDataTest
                                   WHERE RequestDate between '%s' AND '%s'group by ExpenseCode, LastName",
                                   start.date(), end.date())
    
    
    
    TripTypeTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, TripTypeByPersonQry)
      
    })
    
    TripTypeQryReactive <- reactive({
      (TripTypeTable()[TripTypeTable()$ExpenseCode == input$TypeTrip, ])
    })
    
    
    fontSpec<-list(
      size = 10)
    
    
    ExpPerson.Char <- plot_ly(
      y = TripTypeQryReactive()$LastName, x = TripTypeQryReactive()$TotalCharges, name = "Total Charges",type = "bar") %>%
      
      layout(yaxis = list(title = "Expense Code"), label = TRUE)
    
    ExpPerson.dist <- plot_ly(
      y = TripTypeQryReactive()$LastName, x = TripTypeQryReactive()$TotalDistance, name = "Total Distance", type = "bar")# %>%
    
    ExpPerson.trips <- plot_ly(
      y = TripTypeQryReactive()$LastName, x = TripTypeQryReactive()$Freq, name = "Number of trips", type = "bar") #%>%
    
    
    subplot(ExpPerson.Char, ExpPerson.dist, ExpPerson.trips, shareY = TRUE)
    
    
  })
  
  #####group by Expense 
  
  output$TripTypeSummary <- renderDataTable({


    # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')

    if (!is.null(Dataset())) {
      sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
    }

    #Summaryquery
    TripTypeQry <- sprintf("select ExpenseCode, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
                          sum(TotalChargeinKES) as TotalCharges from UberDataTest
                          WHERE RequestDate between '%s' AND '%s'group by ExpenseCode",
                          start.date(), end.date())



    TripTypeTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, TripTypeQry)

    })

    # TripTypeQryReactive <- reactive({
    #   (TripTypeTable()[TripTypeTable()$ExpenseCode == input$TypeTrip, ]) 
    # })
  
    
    DT::datatable(TripTypeTable(), options = list(pageLength = 50, dom = 't'))
    #DT::datatable(tableu(), options = list(pageLength = 10, lengthMenu = c(10, 15, 25)))

  })
  
  ####PLOT EXPENSEC CODE SUMMARY
  output$ExpenseCodePlot <- renderPlotly({
    
    
    TripTypeQry <- sprintf("select ExpenseCode, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
                                   sum(TotalChargeinKES) as TotalCharges from UberDataTest
                                   WHERE RequestDate between '%s' AND '%s'group by ExpenseCode",
                                   start.date(), end.date())
    
    
    
    TripTypeTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, TripTypeQry)
      
    })
  
    
    fontSpec<-list(
      size = 10)
    
    
    ExpCode.Char <- plot_ly(
      y = TripTypeTable()$ExpenseCode, x = TripTypeTable()$TotalCharges, name = "Total Charges",type = "bar", marker = list(color = '#8B2323')) %>%
      
      layout(yaxis = list(title = "Expense Code"), label = TRUE)
    
    ExpCode.dist <- plot_ly(
      y = TripTypeTable()$ExpenseCode, x = TripTypeTable()$TotalDistance, name = "Total Distance", type = "bar", marker = list(color = '#BB650B'))# %>%
    
    ExpCode.trips <- plot_ly(
      y = TripTypeTable()$ExpenseCode, x = TripTypeTable()$Freq, name = "Number of trips", type = "bar",marker = list(color = '#A9A9A9')) #%>%
    
    
    subplot(ExpCode.Char, ExpCode.dist, ExpCode.trips, shareY = TRUE)
    
    
  })
  
    ##################DEPARTMENT PIE CHART___________________________________________________________________
    output$ExpenseCodePieChart <- renderPlotly({
          # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')
    
    if (!is.null(Dataset())) {
      sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
    }
    TripTypeQry <- sprintf("select ExpenseCode, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
                                   sum(TotalChargeinKES) as TotalCharges from UberDataTest
                                   WHERE RequestDate between '%s' AND '%s'group by ExpenseCode",
                                   start.date(), end.date())
    
    
    
    TripTypeTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, TripTypeQry)
      
    })
    tripCode <- data.frame(TripTypeTable())
    
    # 
    # fontSpec<-list(
    #   size = 10)
    
    plot_ly(tripCode, labels=~tripCode$ExpenseCode, values = ~tripCode$TotalCharges, type = 'pie')%>%
      layout(title = 'Percentage Total Amount Per Expense Code',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, labels = TRUE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  ####Department SUMMARY
  
  output$DepartmentSummary <- renderDataTable({
    
    
    # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')
    
    if (!is.null(Dataset())) {
      sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
    }
    
    #Summaryquery
    DepartmentQry <- sprintf("select Department, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
                           sum(TotalChargeinKES) as TotalCharges from UberDataTest
                           WHERE RequestDate between '%s' AND '%s'group by Department",
                           start.date(), end.date())
    
    
    
    DepartmentTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, DepartmentQry)
      
    })
    
    # TripTypeQryReactive <- reactive({
    #   (TripTypeTable()[TripTypeTable()$ExpenseCode == input$TypeTrip, ]) 
    # })
    
    
    DT::datatable(DepartmentTable(), options = list(pageLength = 50, dom = 't'))
    #DT::datatable(tableu(), options = list(pageLength = 10, lengthMenu = c(10, 15, 25)))
    
  })
  
  
  ###################DEPARTMENT __________________________________________________________
  output$DepartmentPlot <- renderPlotly({
        # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')
    
    if (!is.null(Dataset())) {
      sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
    }
    
    DepartmentQry <- sprintf("select Department, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
                           sum(TotalChargeinKES) as TotalCharges from UberDataTest
                           WHERE RequestDate between '%s' AND '%s'group by Department",
                           start.date(), end.date())
    
    
    
    DepartmentTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, DepartmentQry)
      
    })
    
    
    fontSpec<-list(
      size = 10)
    
    
    Department.Char <- plot_ly(
      y = DepartmentTable()$Department, x = DepartmentTable()$TotalCharges, name = "Total Charges",type = "bar", marker = list(color = '#5F9EA0')) %>%
      
      layout(yaxis = list(title = "Expense Code"), label = TRUE, color = 'rgb(204,204,204)')
    
    Department.dist <- plot_ly(
      y = DepartmentTable()$Department, x = DepartmentTable()$TotalDistance, name = "Total Distance", type = "bar", marker = list(color = '#A2CD5A'))# %>%
    
    Department.trips <- plot_ly(
      y = DepartmentTable()$Department, x = DepartmentTable()$Freq, name = "Number of trips", type = "bar", marker = list(color = '#CD6600')) #%>%
    
    
    subplot(Department.Char, Department.dist, Department.trips, shareY = TRUE)
    
    
  })
 
  ##################DEPARTMENT PIE CHART___________________________________________________________________
    output$DepartmentPieChart <- renderPlotly({
    
    DepartmentQry <- sprintf("select Department, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
                           sum(TotalChargeinKES) as TotalCharges from UberDataTest
                           WHERE RequestDate between '%s' AND '%s'group by Department",
                           start.date(), end.date())
    
    
    
    DepartmentTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, DepartmentQry)
      
    })
    dept <- data.frame(DepartmentTable())
    
    # 
    # fontSpec<-list(
    #   size = 10)
    
    plot_ly(dept, labels=~dept$Department, values = ~dept$TotalCharges, type = 'pie')%>%
      layout(title = 'Percentage Expenditure Per Depatment',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, labels = TRUE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  # ####### ExPENSE CODE PER DEPARTMENT
  output$TripTypePerDepartment <- renderDataTable({


    # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')

    if (!is.null(Dataset())) {
      sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
    }

    #Summaryquery
    TripTypeQry2 <- sprintf("select ExpenseCode, Department, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
                           sum(TotalChargeinKES) as TotalCharges from UberDataTest
                           WHERE RequestDate between '%s' AND '%s'group by ExpenseCode, Department",
                           start.date(), end.date())



    TripTypeTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, TripTypeQry2)

    })

    TripTypeQryReactive <- reactive({
      (TripTypeTable()[TripTypeTable()$ExpenseCode == input$ExpType, ])
    })


    DT::datatable(TripTypeQryReactive(), options = list(pageLength = 50, dom = 't'))

  })

  # ####ExPENSE CODE PER DEPARTMENT
  output$TripTypePerDepartmentPlot <- renderPlotly({
    # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')
    
    if (!is.null(Dataset())) {
      sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
    }
    #Summaryquery
    TripTypeQry2 <- sprintf("select ExpenseCode, Department, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
                            sum(TotalChargeinKES) as TotalCharges from UberDataTest
                            WHERE RequestDate between '%s' AND '%s'group by ExpenseCode, Department",
                            start.date(), end.date())

    TripTypeTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, TripTypeQry2)
    })
    
    TripTypeQryReactive <- reactive({
      (TripTypeTable()[TripTypeTable()$ExpenseCode == input$ExpType, ])
    })
    
    fontSpec<-list(
      size = 10)

    DEPT.char <- plot_ly(
      y = TripTypeQryReactive()$Department, x = TripTypeQryReactive()$TotalCharges, name = "Total Charges",type = "bar", marker = list(color = '#CDC0B0')) %>%
      
      layout(#xaxis = list(title = "Total Charges"),
        yaxis = list(title = "Last Name"))
    
    DEPT.dist <- plot_ly(
      y = TripTypeQryReactive()$Department, x = TripTypeQryReactive()$TotalDistance, name = "Total Distance", type = "bar", marker = list(color = '#BB4500'))# %>%
    
    DEPT.trips <- plot_ly(
      y = TripTypeQryReactive()$Department, x = TripTypeQryReactive()$Freq, name = "Number of Trips", type = "bar", marker = list(color = '#8EE5EE')) #%>%
    
    subplot(DEPT.char, DEPT.dist, DEPT.trips, shareY = TRUE)
  })
  
  ########TripTypePerDepartment PIE CHART
  output$TripTypePerDepartmentPieChart <- renderPlotly({
    
    if (!is.null(Dataset())) {
      sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
    }
    #Summaryquery
    TripTypeQry2 <- sprintf("select ExpenseCode, Department, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
                            sum(TotalChargeinKES) as TotalCharges from UberDataTest
                            WHERE RequestDate between '%s' AND '%s'group by ExpenseCode, Department",
                            start.date(), end.date())
    
    TripTypeTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, TripTypeQry2)
    })
    
    TripTypeQryReactive <- reactive({
      (TripTypeTable()[TripTypeTable()$ExpenseCode == input$ExpType, ])
    })
    
    fontSpec<-list(
      size = 10)
    
    TripTypePERDpt <- data.frame(TripTypeQryReactive())
    
    # 
    # fontSpec<-list(
    #   size = 10)
    
    plot_ly(TripTypePERDpt, labels=~TripTypePERDpt$Department, values = ~TripTypePERDpt$TotalCharges, type = 'pie')%>%
      layout(title = 'Percentage Expenditure Per Depatment',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, labels = TRUE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })

  
  ######DEPARTMENT PER EXPENSE CODE
  output$DepartmentPerTripType <- renderDataTable({
    
    # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')
    if (!is.null(Dataset())) {
      sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
    }
    
    #Summaryquery
    DepartmentQry <- sprintf("select Department, ExpenseCode, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
                             sum(TotalChargeinKES) as TotalCharges from UberDataTest
                             WHERE RequestDate between '%s' AND '%s'group by ExpenseCode, Department",
                             start.date(), end.date())
    
    DepartmentTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, DepartmentQry)
    })
    
    DepartmentTableReactive <- reactive({
      (DepartmentTable()[DepartmentTable()$Department == input$DepartmentCategory1, ])
    })
    
    
    DT::datatable(DepartmentTableReactive(), options = list(pageLength = 50, dom = 't'))
  })
  
  #####DEPARTMENT PER EXPENSE CODE PLOT
  output$DepartmentPerTripTypePlot <- renderPlotly({
    # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')
    
    if (!is.null(Dataset())) {
      sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
    }
    #Summaryquery
    DepartmentQry <- sprintf("select Department, ExpenseCode, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
                            sum(TotalChargeinKES) as TotalCharges from UberDataTest
                            WHERE RequestDate between '%s' AND '%s'group by ExpenseCode, Department",
                            start.date(), end.date())
    
    DepartmentQryTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, DepartmentQry)
    })
    
    DepartmentTableReactive <- reactive({
      (DepartmentQryTable()[DepartmentQryTable()$Department == input$DepartmentCategory1, ])
    })
    
    fontSpec<-list(
      size = 10)
    
    EXP.char <- plot_ly(
      y = DepartmentTableReactive()$ExpenseCode, x = DepartmentTableReactive()$TotalCharges, name = "Total Charges",type = "bar", marker = list(color = '#5F9EA0')) %>%
      
      layout(#xaxis = list(title = "Total Charges"),
        yaxis = list(title = "Last Name"))
    
    EXP.dist <- plot_ly(
      y = DepartmentTableReactive()$ExpenseCode, x = DepartmentTableReactive()$TotalDistance, name = "Total Distance", type = "bar", marker = list(color = '#A2CD5A'))# %>%
    
    EXP.trips <- plot_ly(
      y = DepartmentTableReactive()$ExpenseCode, x = DepartmentTableReactive()$Freq, name = "Number of Trips", type = "bar", marker = list(color = '#CD6600')) #%>%
    
    subplot(EXP.char, EXP.dist, EXP.trips, shareY = TRUE)
    
  })
  
  ################## TripTypePerDepartmentPieChart
  
  output$DepartmentPerTripTypePieChart <- renderPlotly({
    
    
    if (!is.null(Dataset())) {
      sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
    }
    #Summaryquery
    DepartmentQry <- sprintf("select Department, ExpenseCode, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
                             sum(TotalChargeinKES) as TotalCharges from UberDataTest
                             WHERE RequestDate between '%s' AND '%s'group by ExpenseCode, Department",
                             start.date(), end.date())
    
    DepartmentQryTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, DepartmentQry)
    })
    
    DepartmentTableReactive <- reactive({
      (DepartmentQryTable()[DepartmentQryTable()$Department == input$DepartmentCategory1, ])
    })
    
    fontSpec<-list(
      size = 10)
    
    DepartmentEXpCode <- data.frame(DepartmentTableReactive())
    
    # 
    # fontSpec<-list(
    #   size = 10)
    
    plot_ly(DepartmentEXpCode, labels=~DepartmentEXpCode$ExpenseCode, values = ~DepartmentEXpCode$TotalCharges, type = 'pie')%>%
      layout(title = 'Percentage Expenditure Per Expense Code',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, labels = TRUE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  #####DEPARTMENT EMPLOYEE SUMMARY
  output$DepartmentCategoryData <- renderDataTable({

        # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')
    if (!is.null(Dataset())) {
      sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
    }

    DepartmentEmplQry <- sprintf("select Department, LastName, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
                                 sum(TotalChargeinKES) as TotalCharges from UberDataTest
                                 WHERE RequestDate between '%s' AND '%s'group by Department, LastName",
                                 start.date(), end.date())

    DepartmentEmplTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, DepartmentEmplQry)
    })

    DepartmentEmplReactive <- reactive({
      (DepartmentEmplTable()[DepartmentEmplTable()$Department == input$DepartmentCategory2, ])
    })

    DT::datatable(DepartmentEmplReactive(), options = list(pageLength = 50, dom = 't'))
  })


  #####DEPARTMENT EMPLOYEE PLOT
  output$DepartmentEmpl <- renderPlotly({
    
    
    DepartmentEmplQry <- sprintf("select Department, LastName, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
                           sum(TotalChargeinKES) as TotalCharges from UberDataTest
                                   WHERE RequestDate between '%s' AND '%s'group by Department, LastName",
                                   start.date(), end.date())
    
    
    
    DepartmentEmplTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, DepartmentEmplQry)
      
    })
    
    DepartmentEmplReactive <- reactive({
      (DepartmentEmplTable()[DepartmentEmplTable()$Department == input$DepartmentCategory2, ])
    })
    
    
    fontSpec<-list(
      size = 10)
    
    
    DepartmentEmpl.Char <- plot_ly(
      y = DepartmentEmplReactive()$LastName, x = DepartmentEmplReactive()$TotalCharges, name = "Total Charges",type = "bar", marker = list(color = '#EE3B3B')) %>%
      
      layout(yaxis = list(title = "Expense Code"), label = TRUE)
    
    DepartmentEmpl.dist <- plot_ly(
      y = DepartmentEmplReactive()$LastName, x = DepartmentEmplReactive()$TotalDistance, name = "Total Distance", type = "bar", marker = list(color = '#EE7600'))
    
    DepartmentEmpl.trips <- plot_ly(
      y = DepartmentEmplReactive()$LastName, x = DepartmentEmplReactive()$Freq, name = "Number of trips", type = "bar", marker = list(color = '#556B2F')) 
    
    
    subplot(DepartmentEmpl.Char, DepartmentEmpl.dist, DepartmentEmpl.trips, shareY = TRUE)
    
    
  })
  
  
  ####################LAST NAME PER EXPENSE CODE TripCodeData
  
  output$TripCodeData <- renderDataTable({
    
    # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')
    if (!is.null(Dataset())) {
      sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
    }
    
    TripCodeQry <- sprintf("select ExpenseCode, LastName, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
                                 sum(TotalChargeinKES) as TotalCharges from UberDataTest
                                 WHERE RequestDate between '%s' AND '%s'group by ExpenseCode, LastName",
                                 start.date(), end.date())
    
    TripCodeTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, TripCodeQry)
    })
    
    TripCodeReactive <- reactive({
      (TripCodeTable()[TripCodeTable()$ExpenseCode == input$ExpType3, ])
    })
    
    DT::datatable(TripCodeReactive(), options = list(pageLength = 50, dom = 't'))
  })
  
  
  #####DEPARTMENT EMPLOYEE PLOT
  output$TripCodePlot <- renderPlotly({
    
    # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')
    if (!is.null(Dataset())) {
      sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
    }
    
    TripCodeQry <- sprintf("select ExpenseCode, LastName, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
                           sum(TotalChargeinKES) as TotalCharges from UberDataTest
                           WHERE RequestDate between '%s' AND '%s'group by ExpenseCode, LastName",
                           start.date(), end.date())
    
    TripCodeTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, TripCodeQry)
    })
    
    TripCodeReactive <- reactive({
      (TripCodeTable()[TripCodeTable()$ExpenseCode == input$ExpType3, ])
    })
    
    
    fontSpec<-list(
      size = 10)
    
    
    TripCode.Char <- plot_ly(
      y = TripCodeReactive()$LastName, x = TripCodeReactive()$TotalCharges, name = "Total Charges",type = "bar", marker = list(color = '#5F9EA0')) %>%
      
      layout(yaxis = list(title = "Expense Code"), label = TRUE)
    
    TripCode.dist <- plot_ly(
      y = TripCodeReactive()$LastName, x = TripCodeReactive()$TotalDistance, name = "Total Distance", type = "bar", marker = list(color = '#EE3B3B'))
    
    TripCode.trips <- plot_ly(
      y = TripCodeReactive()$LastName, x = TripCodeReactive()$Freq, name = "Number of trips", type = "bar",marker = list(color = '#FFB90F')) 
    
    
    subplot(TripCode.Char, TripCode.dist, TripCode.trips, shareY = TRUE)
    
    
  })
  
  
  ########EXPENSE CODE PER DEPARTMENT SUMMMARY
  ###SUMMARY TABLE
  
  output$ExpToDeptData <- renderDataTable({
    
    
    # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')
    
    if (!is.null(Dataset())) {
      sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
    }
    
    #Summaryquery
    ExpToDeptQry <- sprintf("select LastName, ExpenseCode, Department, sum(Distancemi) as TotalDistance, count(LastName) as Freq,
                              sum(TotalChargeinKES) as TotalCharges from UberDataTest
                            WHERE RequestDate between '%s' AND '%s' group by ExpenseCode, LastName, Department",
                            start.date(), end.date())
    
    
    
    ExpToDeptTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, ExpToDeptQry)
      
    })
    
    ExpReactive <- reactive({
      (ExpToDeptTable()[ExpToDeptTable()$ExpenseCode == input$ExpType2, ])
    })
    
    DeptReactive <- reactive({
      (ExpReactive()[ExpReactive()$Department == input$DeptCtgry, ])
    })
    
    DT::datatable(DeptReactive(), options = list(pageLength = 50, dom = 't'))

  })
  
  
  
  
    #summary data
    output$S.Data <- renderTable({


      # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')

      if (!is.null(Dataset())) {
        sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
      }

      #Summaryquery
      SumQry <- sprintf("select distinct ExpenseCode, sum(Distancemi) as TotalDistance,
                        count(LastName) as freq,
                        sum(TotalChargeinKES) as TotalCharges from UberDataTest
                        WHERE RequestDate between '%s' AND '%s' group by ExpenseCode ",
                        start.date(), end.date())



      summaryTable <- eventReactive(input$refresh, {
        sqlQuery(Myconnection, SumQry)
      })

      #Total Summary
      summaryTable()%>%adorn_totals("row") #From janitor package

    })


    #######plot######

    output$plot5 <- renderPlotly({


      SumQry <- sprintf("select distinct ExpenseCode, sum(Distancemi) as TotalDistance,
                        count(LastName) as freq,
                        sum(TotalChargeinKES) as TotalCharges from UberDataTest
                        WHERE RequestDate between '%s' AND '%s' group by ExpenseCode ",
                        start.date(), end.date())



      summaryTable2 <- eventReactive(input$refresh, {
        sqlQuery(Myconnection, SumQry)
      })


      fontSpec<-list(
        size = 10)


      plot.Totalchar <- plot_ly(
        y = summaryTable2()$ExpenseCode, x = summaryTable2()$TotalCharges, name = "Total Charges",type = "bar") %>%

        layout(yaxis = list(title = "Expense Code"), label = TRUE)

      plot.dist <- plot_ly(
        y = summaryTable2()$ExpenseCode, x = summaryTable2()$TotalDistance, name = "Total Distance", type = "bar")# %>%

      plot.trips <- plot_ly(
        y = summaryTable2()$ExpenseCode, x = summaryTable2()$freq, name = "Number of trips", type = "bar") #%>%


      subplot(plot.Totalchar, plot.dist, plot.trips, shareY = TRUE)

      
    })

    ###########################plot 1


    output$plot2 <- renderPlotly({



      if (!is.null(Dataset())) {
        sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
      }

      #Summaryquery
      summaryQry <- sprintf("select LastName, ExpenseCode as expenseCode, count(LastName) as freq, sum(Distancemi) as TotalDistance,
                          sum(TotalChargeinKES) as TotalCharges from UberDataTest
                          WHERE RequestDate between '%s' AND '%s'group by LastName, ExpenseCode",
                            start.date(), end.date())



      summaryTableExp <- eventReactive(input$refresh, {
        sqlQuery(Myconnection, summaryQry)

      })

      newExpense2 <- reactive({
        (summaryTableExp()[summaryTableExp()$expenseCode == input$TypeTrip, ])

      })
      fontSpec<-list(
        size = 10)
      # })
      
        plot.Totalchar <- plot_ly(
          y = newExpense2()$lastname, x = newExpense2()$TotalCharges, name = "Total Charges",type = "bar") %>%
          
          layout(#xaxis = list(title = "Total Charges"),
            yaxis = list(title = "Last Name"))
        
        plot.dist <- plot_ly(
          y = newExpense2()$lastname, x = newExpense2()$TotalDistance, name = "Total Distance", type = "bar")# %>%
        
        plot.trips <- plot_ly(
          y = newExpense2()$lastname, x = newExpense2()$freq, name = "Number of trips", type = "bar") #%>%
        
        subplot(plot.Totalchar, plot.dist, plot.trips, shareY = TRUE)
        
    })
   #####group by Expense code
    
    output$EmployeeSummary <- renderDataTable({
      
      
      # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')
      
      if (!is.null(Dataset())) {
        sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
      }
      
      #Summaryquery
      summaryQry <- sprintf("select LastName, Department, ExpenseCode, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
                          sum(TotalChargeinKES) as TotalCharges from UberDataTest
                          WHERE RequestDate between '%s' AND '%s'group by ExpenseCode, Department, LastName",
                            start.date(), end.date())
      
      
      
      summaryTable <- eventReactive(input$refresh, {
        sqlQuery(Myconnection, summaryQry)
        
      })
      
      newExpense <- reactive({
        (summaryTable()[summaryTable()$LastName == input$EmployeeName, ])
        
        
      })
      DT ::datatable(newExpense(), options = list(pageLength = 50, dom = 't'))
      #DT::datatable(tableu(), options = list(pageLength = 10, lengthMenu = c(10, 15, 25)))
      
    })
    


    ############################
    output$EmployeeSumPlot <- renderPlotly({
      
      SelectedPerPerson <- sprintf("select LastName, ExpenseCode, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
                                   sum(TotalChargeinKES) as TotalCharges from UberDataTest
                                   WHERE RequestDate between '%s' AND '%s'group by ExpenseCode, LastName",
                                   start.date(), end.date())
      
      
      EmployeeTable <- eventReactive(input$refresh, {
        sqlQuery(Myconnection, SelectedPerPerson)
      })
      
      
      EmployeeReaction <- reactive({
        (EmployeeTable()[EmployeeTable()$LastName == input$EmployeeName, ])
      })
      
      fontSpec<-list(
        size = 10)
      
      
      Employee.char <- plot_ly(
        y = EmployeeReaction()$ExpenseCode, x = EmployeeReaction()$TotalCharges, name = "Total Charges",type = "bar") %>%
        
        layout(#xaxis = list(title = "Total Charges"),
          yaxis = list(title = "Last Name"))
      
      Employee.dist <- plot_ly(
        y = EmployeeReaction()$ExpenseCode, x = EmployeeReaction()$TotalDistance, name = "Total Distance", type = "bar")# %>%
      
      Employee.trips <- plot_ly(
        y = EmployeeReaction()$ExpenseCode, x = EmployeeReaction()$Freq, name = "Number of Trips", type = "bar") #%>%
      
      subplot(Employee.char, Employee.dist, Employee.trips, shareY = TRUE)
      })
    
    
    
    ##################EMPLOYE DATA+++++++++++++++++++++++
        
    output$EmployeeData <- renderDataTable({
      
      
      # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')
      
      if (!is.null(Dataset())) {
        sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
      }
      
      #Summaryquery
      EmpDataQry <- sprintf("select LastName, Distancemi as TotalDistance,
                          TotalChargeinKES as TotalCharges from UberDataTest
                          WHERE RequestDate between '%s' AND '%s'",
                            start.date(), end.date())
      
      
      
      EmplDataTable <- eventReactive(input$refresh, {
        sqlQuery(Myconnection, EmpDataQry)
        
      })
      
      EmplDataReactive <- reactive({
        (EmplDataTable()[EmplDataTable()$LastName == input$Employee, ])
        
        
      })
      DT ::datatable(EmplDataReactive(), options = list(pageLength = 50, dom = 't'))

    })
    
    ############################
    ##
    ##
    ##
    
    # output$EmployeeAnaylitics <- renderDataTable({
    #         if (!is.null(Dataset())) {
    #     sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
    #   }
    # 
    #   #Summaryquery
    #   EmpAnalyticsQry <- sprintf("select, RequestDate, LastName, Distancemi as TotalDistance, Department,
    #                       TotalChargeinKES as TotalCharges, ExpenseCode from UberDataTest
    #                       WHERE RequestDate between '%s' AND '%s'",
    #                         start.date(), end.date())
    # 
    #   EmplDataTable <- eventReactive(input$refresh, {
    #   sqlQuery(Myconnection, EmpAnalyticsQry)
    # 
    #   })
    # 
    #   GroupedMonthlyDepartment <- reactive(DATA %>%
                # mutate(Month = format(as.Date(RequestDate), "%m"), Year = format(as.Date(RequestDate), "%Y")) %>%
                # group_by(Year, Month)%>%
                # summarise(TotalCharge = sum(TotalChargeinKES), Distance = sum(Distancemi)))
    # 
    # 
    #   DT ::datatable(GroupedMonthlyDepartment(), options = list(pageLength = 50, dom = 't'))
    # 
    # 
    # })
   ############################################################################################ 
    
# 
#   # output$EmployeeAnaylitics <- renderDataTable({
#   #           if (!is.null(Dataset())) {
#   #       sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
#   #     }
#   # 
#   #     #Summaryquery
#   #     EmpAnalyticsQry <- sprintf("select RequestDate, LastName, Distancemi as TotalDistance, Department,
#   #                         TotalChargeinKES as TotalChargeinKES, ExpenseCode from UberDataTest
#   #                         WHERE RequestDate between '%s' AND '%s'",
#   #                           start.date(), end.date())
#   # 
#   #     EmplDataTable <- eventReactive(input$refresh, {
#   #     sqlQuery(Myconnection, EmpAnalyticsQry)
#   # 
#   #     })
#   # 
#   #     GroupedMonthlyDepartment <- reactive(data.frame(EmplDataTable()) %>%
#   #               mutate(Month = format(as.Date(RequestDate), "%m"), Year = format(as.Date(RequestDate), "%Y")) %>%
#   #               group_by(Year, Month, Department) %>%
#   #               summarise(TotalChargeinKES = sum(TotalChargeinKES), TotalDistance = sum(TotalDistance)))
#   # 
#   # 
#   #     DT ::datatable(GroupedMonthlyDepartment(), options = list(pageLength = 50, dom = 't'))
#   # 
#   # 
#   #   })
    
  #################################$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$  
    output$EmployeeAnayliticsPlot <- renderPlotly({
      
      if (!is.null(Dataset())) {
        sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
      }

      #Summaryquery
      EmpAnalyticsQry <- sprintf("select RequestDate, LastName, Distancemi as TotalDistance, Department,
                          TotalChargeinKES as TotalCharges, ExpenseCode from UberDataTest
                          WHERE RequestDate between '%s' AND '%s'",
                            start.date(), end.date())

      EmplDataTable <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, EmpAnalyticsQry)

      })

      GroupedMonthlyLastName <- reactive(data.frame(EmplDataTable()) %>%
                mutate(Month = format(as.Date(RequestDate), "%m"), Year = format(as.Date(RequestDate), "%Y")) %>%
                mutate(Month.Year = paste(Year, Month))%>%
                group_by(Month.Year, LastName)%>%
                summarise(TotalCharges = sum(TotalCharges), Distance = sum(TotalDistance)))

      GroupedMonthlyDepartment <- reactive(data.frame(EmplDataTable()) %>%
                mutate(Month = format(as.Date(RequestDate), "%m"), Year = format(as.Date(RequestDate), "%Y")) %>%
                mutate(Month.Year = paste(Year, Month))%>%
                group_by(Month.Year, Department)%>%
                summarise(TotalCharges = sum(TotalCharges), Distance = sum(TotalDistance)))

      GroupedMonthlyCompany <- reactive(data.frame(EmplDataTable()) %>%
                mutate(Month = format(as.Date(RequestDate), "%m"), Year = format(as.Date(RequestDate), "%Y")) %>%
                mutate(Month.Year = paste(Year, Month))%>%
                group_by(Month.Year)%>%
                summarise(TotalCharges = sum(TotalCharges), TotalDistance = sum(TotalDistance)))

       GroupedMonthlyLastNameReactive <- reactive({
        (GroupedMonthlyLastName[GroupedMonthlyLastName$LastName == input$Name1, ])
       })

       GroupedMonthlyDepartmentReactive <- reactive({
         (GroupedMonthlyDepartment[GroupedMonthlyDepartment$Department == input$Dptmnt1, ])
       })
       DepartmentEXpCode11 <- data.frame(GroupedMonthlyDepartmentReactive())
       GroupedMonthlyLastName <- data.frame(GroupedMonthlyLastNameReactive())
       

       # ggplot(data = GroupedMonthlyLastName(), aes(x = Month.Year, y = TotalCharges)) +
       #   geom_line(color = "#00AFBB", size = 1)

# # 
#        ggplot() +
#           geom_line(data = GroupedMonthlyCompany(), aes(y = TotalCharges, x = Month.Year), color = "red") +
#           #geom_line(data = data.frame(GroupedMonthlyDepartment), aes(y = TotalCharges, x = Month.Year), color = "blue") +
#           #geom_line(data = data.frame(GroupedMonthlyDepartmentReactive), aes(y = TotalCharges, x = Month.Year), color = "green") +
#           xlab('YEAR MONTH') +
#           ylab('TOTAL AMOUNT')

       plot_ly(GroupedMonthlyCompany(), x = ~Month.Year, y = ~TotalCharges,  type = 'scatter', mode = 'lines')#%>%
         #add_trace(data.frame(GroupedMonthlyLastNameReactive()), x = ~Month.Year, y = ~TotalCharges, mode = 'lines') %>%
         #add_trace(data.frame(GroupedMonthlyDepartmentReactive()), x = ~Month.Year, y = ~TotalCharges, mode = 'lines')
         #add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers') %>%
         #add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers')
       
    })
    
    ####
    #####
    ############
    
    
    #####group by Expense code
    
    output$ExpenseDepartment <- renderDataTable({
      ##RENDER TABLE SUMMARY

        
        # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')
        
        if (!is.null(Dataset())) {
          sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
        }
        
        #Summaryquery
        summaryQuery <- sprintf("select LastName, ExpenseCode, Department, sum(Distancemi) as TotalDistance, count(LastName) as Freq,
                              sum(TotalChargeinKES) as TotalCharges from UberDataTest
                              WHERE RequestDate between '%s' AND '%s' group by ExpenseCode, LastName, Department",
                                start.date(), end.date())
        
        
        summaryTable <- eventReactive(input$refresh, {
          sqlQuery(Myconnection, summaryQuery)
        })
        

        TypeExpense <- reactive({
          (summaryTable()[summaryTable()$ExpenseCode == input$TripOfTrip, ])
        })
        
        Dept <- reactive({
          (TypeExpense()[TypeExpense()$Department == input$Deptmnt1, ])
        })


        #Total Summary
        DT::datatable(Dept(), options = list(pageLength = 10, dom = 't'))

    })
    
    output$percentages <- renderDataTable({
      ##RENDER TABLE SUMMARY
      
      
      # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')
      
      if (!is.null(Dataset())) {
        sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
      }
      
      #Summaryquery
      summaryQuery <- sprintf("select RequestDate, LastName, Department, Distancemi,
                       TotalChargeinKES,ExpenseCode from UberDataTest
                       WHERE RequestDate between '%s' AND '%s'order by RequestDate asc",
                              start.date(), end.date())
      
      
      summaryTable <- eventReactive(input$refresh, {
        sqlQuery(Myconnection, summaryQuery)
      })
      
      ###percentage amount
      GroupedMonthly<- reactive(summaryTable() %>%
                              mutate(Month = format(as.Date(RequestDate), "%m"), Year = format(as.Date(RequestDate), "%Y")))

      #Total Summary
      DT::datatable(GroupedMonthly(), options = list(pageLength =5, dom = 't'))
      
    })
    
     
  output$TripTypeSum <- renderDataTable({


    # Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')

    if (!is.null(Dataset())) {
      sqlSave(Myconnection, Dataset(), tablename = "UberDataTest", append = T)
    }

    #Summaryquery
    TripTypeQry1 <- sprintf("select ExpenseCode, count(LastName) as Freq, sum(Distancemi) as TotalDistance,
                          sum(TotalChargeinKES) as TotalCharges, from UberDataTest
                          WHERE RequestDate between '%s' AND '%s'group by ExpenseCode",
                          start.date(), end.date())



    TripTypeTable1 <- eventReactive(input$refresh, {
      sqlQuery(Myconnection, TripTypeQry1)

    })
    
    NewData <- reactive(TripTypeTable1() %>%
       mutate(as.numeric(Percentage = TotalCharges / sum(TotalCharges))))

    
    DT::datatable(NewData(), options = list(pageLength = 50, dom = 't'))
    #DT::datatable(tableu(), options = list(pageLength = 10, lengthMenu = c(10, 15, 25)))

  })
  
  
    

}