library(RODBC)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dygraphs)
library(zoo)
library(DT)
require(dplyr)
library(janitor)
library(forecast)
library(tidyquant)
library(timetk)
library(sweep)
library(ggplot2)

Myconnection <- odbcDriverConnect('driver={SQL Server};server=.;database=UberDB;trusted_connection=true')
showList <- sqlQuery(Myconnection, "select DISTINCT LastName AS EmployeeName from UberDataTest", stringsAsFactors = T)
tripType <- sqlQuery(Myconnection, "select distinct ExpenseCode from UberDataTest", stringsAsFactors = T)

Department <- sqlQuery(Myconnection, "select distinct Department from UberDataTest", stringsAsFactors = T)
#odbcClose(Myconnection)
showList
tripType
Department
#Concat(FirstName,' ',LastName)

# DATA <- sqlQuery(Myconnection, "select RequestDate, LastName,  Distancemi, Department, ExpenseCode, TotalChargeinKES from UberDataTest", stringsAsFactors = T)
# DATA$RequestDate
# 
# DATATIMESERIES <- ts(DATA)
# DATATIMESERIES
# plot.ts(DATATIMESERIES)
# ts.plot(DATATIMESERIES,gpars= list(col=rainbow(5)))
# 
# 
# GroupedMonthly <- DATA %>%
#                 mutate(Month = format(as.Date(RequestDate), "%m"), Year = format(as.Date(RequestDate), "%Y")) %>%
#                 group_by(Year, Month, Department)%>%
#                 #LastName = count(LastName)%>%
#                 summarise(TotalCharge = sum(TotalChargeinKES), Distance = sum(Distancemi))
# transpose(GroupedMonthly)
# 
# plot_ly(GroupedMonthly, labels=~GroupedMonthly$ExpenseCode, values = ~GroupedMonthly$TotalCharge, type = 'pie')
# # 
# # require(plotly)
# # ggplot(GroupedMonthly, aes(x = time, y = TotalCharge, colour = "green")) + geom_line()
# # 
# plot(DATA$RequestDate, DATA$TotalChargeinKES, type= "l",  xlab = "Year", ylab= "Total Charge in KES", col="green" , lwd=2)
# # 
# ts.plot(DATA$TotalChargeinKES, gpars= list(col=rainbow(10)))
# #  
# #  require(graphics)
# # variable = psavert  
# #  
# ggplot(data = GroupedMonthly, aes(x = Year, y = TotalCharge)) +
#   geom_line(aes(color = variable), size = 1) +
#   scale_color_manual(values = c("#00AFBB", "#E7B800")) + theme_minimal()
# 
# 
# p <- ggplot(data = DATA, aes(x = RequestDate, y = TotalChargeinKES)) +
#      geom_line(color = "#00AFBB", size = 1)
# 
# 
# ggplot() +
#   geom_line(data = GroupedMonthlyDepartment, aes(y = TotalCharge, x = Month || Year), color = "red") +
#   geom_line(data = CompanyData, aes(y = TotalCharge, x = Month || Year), color = "blue") +
# #   xlab('data_date') +
# #   ylab('percent.change')
# # paste(GroupedMonthlyDepartment$Month, GroupedMonthlyDepartment$Year)
# # 
# # library(ggplot2)
# # ggplot(DATA, aes(RequestDate, TotalChargeinKES)) + geom_line(color = "#E7B800")
# # # 
# # # 
# # # 
# cmpny <- DATA %>%
#                 mutate(Month = format(as.Date(RequestDate), "%m"), Year = format(as.Date(RequestDate), "%Y")) %>%
#                 mutate(Month.Year = paste(Year, Month))%>%
#                 group_by(Month.Year)%>%
#                 summarise(TotalCharge = sum(TotalChargeinKES), Distance = sum(Distancemi))
# 
# MonDept <- DATA %>%
#             mutate(Month = format(as.Date(RequestDate), "%m"), Year = format(as.Date(RequestDate), "%Y")) %>%
#             mutate(Month.Year = paste(Year, Month))%>%
#             group_by(Month.Year, Department)%>%
#             summarise(TotalCharge = sum(TotalChargeinKES), Distance = sum(Distancemi))
# 
# FilteredDepartment<- MonDept %>% 
#    filter(Department == "Engineering")
# 
# employeeLastname <- DATA %>%
#                       mutate(Month = format(as.Date(RequestDate), "%m"), Year = format(as.Date(RequestDate), "%Y")) %>%
#                       mutate(Month.Year = paste(Year, Month))%>%
#                       group_by(Month.Year, LastName)%>%
#                       summarise(TotalCharge = sum(TotalChargeinKES), Distance = sum(Distancemi))
# 
# FilteredLastName <- employeeLastname %>% 
#                       filter(LastName == "Ngure")

# 

# 
# 
# # 
# plot_ly(FilteredDepartment, x = ~Month.Year, y = ~TotalCharge, name = 'trace 0', type = 'scatter', mode = 'lines')%>%
#        add_trace(cmpny,  y = ~TotalCharge, name = 'trace 1', mode = 'lines+markers')%>%
#        add_trace(FilteredLastName, y = ~TotalCharge, name = 'trace 2', mode = 'markers')
# 
# 
# p = ggplot() + 
#   geom_line(data = FilteredDepartment, aes(x = Month.Year, y = TotalCharge), color = "blue") +
#   geom_line(data = FilteredLastName, aes(x = Month.Year, y = TotalCharge), color = "red") +
#   xlab('Dates') +
#   ylab('percent.change')
# # # 
# # 
# # 
# p <- plot_ly(data, x = ~x, y = ~trace_0, name = 'trace 0', type = 'scatter', mode = 'lines') %>%
#   add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers') %>%
#   add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers')
# 
# 
# 
# 
# 
# 
# p = ggplot() + 
#   geom_line(data = GroupedMonthlyDepartment, aes(x = Month.Year, y = TotalCharge), color = "blue") +
#   #geom_line(data = GroupedMonthlyDepartment, aes(x = dates, y = Difference), color = "red") +
#   xlab('Dates') +
#   ylab('percent.change')
# 
# ggplot(GroupedMonthlyDepartment, aes(Month.Year, TotalCharge)) + geom_line(color = "#E7B800")
# ggplot() + 
#   geom_line(data = GroupedMonthlyDepartment, aes(x = Month.Year, y = TotalCharge), color = "blue") +
#   xlab('Dates') +
#   ylab('percent.change')
# 
# CompanyData <- DATA %>%
#   mutate(Month = format(as.Date(RequestDate), "%m"), Year = format(as.Date(RequestDate), "%Y")) %>%
#   group_by(Year)%>%
#   summarise(TotalCharge = sum(TotalChargeinKES), Distance = sum(Distancemi))
# 
# 
# 
# #mutate(Month.Year = paste(GroupedMonthlyDepartment$))
# 
# 
# CompanyData <- DATA %>%
#                 mutate(Month = format(as.Date(RequestDate), "%m"), Year = format(as.Date(RequestDate), "%Y")) %>%
#                 group_by(Month, Year)%>%
#                 summarise(TotalCharge = sum(TotalChargeinKES), Distance = sum(Distancemi))
# 
# LASTNAMEData <- DATA %>%
#                 mutate(Month = format(as.Date(RequestDate), "%m"), Year = format(as.Date(RequestDate), "%Y")) %>%
#                 group_by(Year, Month, LastName)%>%
#                 summarise(TotalCharge = sum(TotalChargeinKES), Distance = sum(Distancemi))
