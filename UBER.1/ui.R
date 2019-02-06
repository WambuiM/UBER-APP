dashboardPage(
  dashboardHeader(title = "UBER DATA ANALYTICS",titleWidth = 250,
                  tags$li(href = 'http://www.innova.co.ke',
                          tags$img(src = 'preview-logo-logo1.png',title = "Company Home",height = "43px",width = "auto"), class = "dropdown")),
  dashboardSidebar(width = 250,
                   sidebarMenu(
                     menuItem("Import Data",tabName = "Importer",icon = icon("upload"),
                              tags$style(type = 'text/css', ".well { max-width: 20em ; }"),
                              # Tags:
                              tags$head(
                                tags$style(type = "text/css", "select[multiple] { width: 100%; height:10em}"),
                                tags$style(type = "text/css", "select { width: 100%}"),
                                tags$style(type = "text/css", "input { width: 19em; max-width:100%}")
                                
                              ),
                              
                              # Select filetype:
                              selectInput("readFunction", "Function to read data:",c(
                                # Base R:
                                "read.csv",
                                "read.table",
                                "read.csv2",
                                "read.delim",
                                "read.delim2"
                              )),
                              
                              # Upload data:
                              fileInput("file", "Upload data-file:"),
                              
                              # Variable selection:
                              htmlOutput("varselect"),
                              
                              br()
                              
                     ),
                     menuItem(dateRangeInput('dateRange', label = 'Date range input',
                                             start = Sys.Date() - 90, end = Sys.Date(), format = "dd-M-yyyy")),
                     menuItem(actionButton("refresh", "LOAD DATA")),
                     br(),
                     menuItem("Data", tabName = "Table",icon = icon("table")),
                     
                     menuItem("Company Analytics", tabName = "companyAnalytics", icon = icon("group"),
                              menuSubItem("Summary", tabName = "ExpCodeDept", icon = icon("table")),
                              menuSubItem("Expense Code summary", tabName = "ExpCodeSumm", icon = icon("table")),
                              menuSubItem("Department Summary", tabName = "DepartmentSumm", icon = icon("table")),
                              menuSubItem("Expense Code Per Department", tabName = "ExpCodePerDept", icon = icon("table")),
                              menuSubItem("Department per Expense Code", tabName = "DeptPerExpCode", icon = icon("table"))
                              
                              
                     ),
                     menuItem("Employee Analytics", tabName = "Individual", icon = icon("group"),
                              menuSubItem("Expenditure Summary", tabName = "ExpCodeVsDepartment", icon = icon("table")),
                              menuSubItem("Employee Summary", tabName = "employeeAnalytics", icon = icon("table"))
                              
                     ),
                     menuItem("predictive Modeling", tabName = "Analytics", icon = icon("group")
                     )
                   ),
                   br()
  ),
  dashboardBody(
 
    tabItems(
        tabItem(tabName = "Table",
          fluidRow(
              box(width = 12,
                    title = "Raw Data Table",
                    collapsible = TRUE, dataTableOutput("rawData")),
                
              box(width = 4,
                  title = "Data Summary", solidHeader = TRUE,
                  tableOutput("summaryData")
                  ),
              
              box(width = 8,
                  title = "ExpenseCode: Expense code, Total Charges, Total Distance", solidHeader = TRUE,
                  collapsible = T, plotlyOutput("SummaryPlot")
                  )
        
        )
       ),
        # tabItem(tabName = "ExpCodeDept",
        #         fluidRow(
        #           box(width = 12,
        #               title = "Expense Code Summary", solidHeader = T,
        #               collapsible = T, dataTableOutput("TripTypeSummary"), plotlyOutput("ExpenseCodePlot"),
        #               HTML("<br/>", "<br/>", "<br/>"),
        #               plotlyOutput("ExpenseCodePieChart")
        #                       ),
        #           
        #           box(width = 12,
        #               title = "Department Summary", solidHeader = T,
        #               collapsible = T, dataTableOutput("DepartmentSummary"), plotlyOutput("DepartmentPlot"), 
        #               HTML("<br/>", "<br/>", "<br/>"),
        #               plotlyOutput("DepartmentPieChart")
        #             ),
        #           
        #           box(width = 12,
        #               title = "Expense Code Per Department", solidHeader = T,
        #               radioButtons("ExpType", label = "Type of Trip:",
        #                            choices = c("MEETING", "OFFICE ADMIN", "SUPPORT", "TRAVEL", "WORKLATE"),
        #                            selected = "MEETING", inline = TRUE),
        #               collapsible = T, dataTableOutput("TripTypePerDepartment"), plotlyOutput("TripTypePerDepartmentPlot"), plotlyOutput("TripTypePerDepartmentPieChart")
        #           ),
        #           
        #           box(width = 12,
        #               title = "Department Per TripType", solidHeader = T,
        #               radioButtons("DepartmentCategory1", label = "Department:",
        #                            choices = c("Engineering", "Projects", "Business Development", "Quality Assurance", "Finance"),
        #                            selected = "Engineering", inline = TRUE),
        #               collapsible = T, dataTableOutput("DepartmentPerTripType"), plotlyOutput("DepartmentPerTripTypePlot"), plotlyOutput("DepartmentPerTripTypePieChart")
        #           )
        # 
        #         )
        #   ),
       tabItem(tabName = "ExpCodeSumm",
               fluidRow(
                 box(width = 12,
                     title = "Expense Code Summary", solidHeader = T,
                     collapsible = T, dataTableOutput("TripTypeSummary"), plotlyOutput("ExpenseCodePlot"),
                     HTML("<br/>", "<br/>", "<br/>"),
                     plotlyOutput("ExpenseCodePieChart")
                 )
               ) 
       
       ),
       
       tabItem(tabName = "DepartmentSumm",
               fluidRow(
                 box(width = 12,
                     title = "Department Summary", solidHeader = T,
                     collapsible = T, dataTableOutput("DepartmentSummary"), plotlyOutput("DepartmentPlot"),
                     HTML("<br/>", "<br/>", "<br/>"),
                     plotlyOutput("DepartmentPieChart")
                     )
               ) 
               
       ),
       
       tabItem(tabName = "ExpCodePerDept",
               fluidRow(
                 box(width = 12,
                     title = "Department Per TripType", solidHeader = T,
                     radioButtons("DepartmentCategory1", label = "Department:",
                     choices = c("Engineering", "Projects", "Business Development", "Quality Assurance", "Finance"),
                                    selected = "Engineering", inline = TRUE),
                          collapsible = T, dataTableOutput("DepartmentPerTripType"), plotlyOutput("DepartmentPerTripTypePlot"), plotlyOutput("DepartmentPerTripTypePieChart")
                     )
               ) 
               
       ),  
       
       tabItem(tabName = "DeptPerExpCode",
               fluidRow(
                 box(width = 12,
                     title = "Expense Code Per Department", solidHeader = T,
                     radioButtons("ExpType", label = "Type of Trip:",
                                  choices = c("MEETING", "OFFICE ADMIN", "SUPPORT", "TRAVEL", "WORKLATE"),
                                  selected = "MEETING", inline = TRUE),
                     collapsible = T, dataTableOutput("TripTypePerDepartment"), plotlyOutput("TripTypePerDepartmentPlot"), plotlyOutput("TripTypePerDepartmentPieChart")
                 )
               ) 
               
       ),
       
       tabItem(tabName = "ExpCodeVsDepartment",
               fluidRow(
               box(width = 12,
                   title = "Departments Category", solidHeader = T,
                   radioButtons("DepartmentCategory2", label = "Department:",
                                choices = c("Engineering", "Projects", "Business Development", "Quality Assurance", "Finance"),
                                selected = "Engineering", inline = TRUE),
                   collapsible = T, dataTableOutput("DepartmentCategoryData"), plotlyOutput("DepartmentEmpl")),
               
               box(width = 12,
                   title = "Trip Type", solidHeader = T,
                   radioButtons("ExpType3", label = "Type of Trip:",
                                choices = c("MEETING", "OFFICE ADMIN", "SUPPORT", "TRAVEL", "WORKLATE"),
                                selected = "MEETING", inline = TRUE),
                   collapsible = T, dataTableOutput("TripCodeData"), plotlyOutput("TripCodePlot")),
               
               
               box(width = 12,
                   title = " Expense Code Per Department", solidHeader = T,
                   radioButtons("DeptCtgry", label = "Department:",
                                choices = c("Engineering", "Projects", "Business Development", "Quality Assurance", "Finance"),
                                selected = "Engineering", inline = TRUE),
                   
                   radioButtons("ExpType2", label = "Type of Trip:",
                                choices = c("MEETING", "OFFICE ADMIN", "SUPPORT", "TRAVEL", "WORKLATE"),
                                selected = "MEETING", inline = TRUE),
                   collapsible = T, dataTableOutput("ExpToDeptData"))
                )
               ),
       
       tabItem(tabName = "employeeAnalytics",
               fluidRow(
                 box(width = 12,
                     tittle = "Employee's Data ",
                     selectInput("EmployeeName", "Employee Name:",
                                 choices = showList),
                     collapsible = T, dataTableOutput("EmployeeSummary")
                     ),
                 
                 box(width = 9,
                     title = "Total Charges, Distance (miles), Number of trips Per Employee",
                     collapsible = T, plotlyOutput("EmployeeSumPlot")
                     ),
                 # box(width = 12,
                 #     tittle = "Employee's Data",
                 #     selectInput("Employee", "Employee Name:",
                 #                 choices = showList),
                 #     collapsible = T, dataTableOutput("EmployeeData")
                 #     ),
                 
                 
                 box(width = 12,
                      title = "Department Summary", solidHeader = T,
                     selectInput("Name1", "Employee Name:",
                                 choices = showList),
                     
                     radioButtons("Dptmnt1", label = "Department:",
                                  choices = c("Engineering", "Projects", "Business Development", "Quality Assurance", "Finance"),
                                  selected = "Engineering", inline = TRUE),
                     
                      collapsible = T, dataTableOutput("EmployeeAnaylitics"), plotlyOutput("EmployeeAnayliticsPlot") 
                    )
                 
                 
               ))
       
       
       # tabItem(tabName = "Analytics",
       #         fluidRow(
       #           box(width = 12,
       #               title = "Expense code, Departments Expenditure", solidHeader = T,
       #               radioButtons("TripOfTrip", label = "Type of Trip:",
       #                            choices = c("MEETING", "OFFICE ADMIN", "SUPPORT", "TRAVEL", "WORKLATE"),
       #                            
       #                            selected = "MEETING", inline = TRUE),
       #               radioButtons("Deptmnt1", label = "Department:",
       #                            choices = c("Engineering", "Projects", "Business Development", "Quality Assurance", "Finance"),
       #                            
       #                            selected = "Quality Assurance", inline = TRUE)
       #               ),
       #               
       #           box(width = 9,
       # 
       #               title = "Expense Code Department",
       #               collapsible = T, dataTableOutput("percentages")
       #               ),
       #                      box(width = 9,
       # 
       #               title = "Expense Code Department",
       #               collapsible = T, dataTableOutput("TripTypeSum")
       #               )
       #           
       #           
       #         ))
       
       
        )

  )
)