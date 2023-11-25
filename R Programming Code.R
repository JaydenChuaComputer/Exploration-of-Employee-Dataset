# Chua Yi Xuan
# TP068394
#
# --------------------------------------------------------------------



# -------------------- Data Import --------------------
# Read data
Employee <- read.csv("C:\\Users\\User10\\Desktop\\PFDA Assignment\\employee_attrition.csv",
                    header = TRUE)

# View data
View(Employee)



# -------------------- Data Cleaning --------------------
# Check for null data
sum(is.na(Employee)) # Extra feature 1



# -------------------- Data Pre-processing --------------------
# View column headers
names(Employee)

# Assign headers
names(Employee) <- c("EMPLOYEE_ID","EMP_RECORD_DATE","DOB","EMP_HIRED_DATE",
                    "EMP_TERMINATION_DATE","EMP_AGE","EMP_SERVICE_LENGTH",
                    "EMP_CITY","EMP_DEPARTMENT","EMP_JOB_TITLE","EMP_STORE_CODE",
                    "EMP_SEX_S","EMP_S_F","EMP_TERMINATION_REASON",
                    "EMP_TERMINATION_TYPE","EMP_STATUS_IN_THE_YEAR",
                    "EMP_STATUS_IN_THE_COMPANY","BUSINESS_UNITS_OF_COMPANY_EMP_STAY")

# View column headers
names(Employee)

# Replace unmeaningful data
Employee$EMP_TERMINATION_DATE <- ifelse(Employee$EMP_TERMINATION_DATE == "1/1/1900",
                                         "1/1/2024",Employee$EMP_TERMINATION_DATE)

# Correct spelling error 
Employee$EMP_TERMINATION_REASON <- ifelse(Employee$EMP_TERMINATION_REASON == "Resignaton",
                                       "Resignation",Employee$EMP_TERMINATION_REASON)

# Assign correct data types
Employee$DOB <- as.Date(Employee$DOB, format = "%m/%d/%Y") # Extra feature 2
Employee$EMP_HIRED_DATE <- as.Date(Employee$EMP_HIRED_DATE, format = "%m/%d/%Y")
Employee$EMP_TERMINATION_DATE <- as.Date(Employee$EMP_TERMINATION_DATE, format = "%m/%d/%Y")
Employee$EMP_AGE <- as.numeric(Employee$EMP_AGE) # Extra feature 3
Employee$EMP_SERVICE_LENGTH <- as.numeric(Employee$EMP_SERVICE_LENGTH)
Employee$EMP_RECORD_DATE <- as.Date(Employee$EMP_RECORD_DATE, format = "%m/%d/%Y %H:%M")

# Keep only data recorded within the year 2015
employees2015 <- subset(Employee, format(EMP_RECORD_DATE, "%Y") == "2015")



# -------------------- Data Transformation  --------------------
# Load dplyr package
library(dplyr)

# Select specific columns, ignore unused columns
Employee <- select(Employee,EMPLOYEE_ID,EMP_RECORD_DATE,DOB,EMP_HIRED_DATE,
                         EMP_TERMINATION_DATE,EMP_AGE,
                         EMP_CITY,EMP_DEPARTMENT,EMP_JOB_TITLE,EMP_STORE_CODE,
                         EMP_SEX_S,EMP_TERMINATION_REASON,
                         EMP_TERMINATION_TYPE,
                         EMP_STATUS_IN_THE_COMPANY,BUSINESS_UNITS_OF_COMPANY_EMP_STAY)

# Create a new column for length of service in years
Employee <- mutate(Employee, EMP_SERVICE_LENGTH_YEARS = 
                    round(as.numeric(as.Date(EMP_TERMINATION_DATE, "%m/%d/%Y") - 
                                       as.Date(EMP_HIRED_DATE, "%m/%d/%Y"))/365, 2))

# Summarise the details of active employees in the company
Summary_by_department <- summarise(Employee[Employee[!duplicated(Employee$EMPLOYEE_ID, fromLast = TRUE),]$
                                              EMP_STATUS_IN_THE_COMPANY == "ACTIVE", ], 
                                  Avg_age = mean(EMP_AGE), 
                                  Avg_service_length = mean(EMP_SERVICE_LENGTH_YEARS), 
                                  Number_of_store = length(unique(EMP_STORE_CODE)), 
                                  Number_of_city = length(unique(EMP_CITY)), 
                                  Number_of_department = length(unique(EMP_DEPARTMENT)))

# Piping some functions together to show some details that can retrieved for the data
Employee %>%
  select(EMPLOYEE_ID, EMP_HIRED_DATE, EMP_TERMINATION_DATE, EMP_AGE, EMP_CITY, EMP_DEPARTMENT) %>%
  filter(EMP_TERMINATION_DATE != "2024-01-01") %>%
  subset(EMP_AGE < 30) %>%
  sample_n(10) %>%
  mutate(EMP_SERVICE_LENGTH_YEARS = 
           round(as.numeric(as.Date(EMP_TERMINATION_DATE, "%m/%d/%Y") - 
                              as.Date(EMP_HIRED_DATE, "%m/%d/%Y"))/365, 2)) %>%
  arrange(EMP_SERVICE_LENGTH_YEARS)



#-------------------- Data Analysis --------------------
library(ggplot2)
library(plotly)
library(patchwork)
library(gridExtra)
library(plotrix)



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#---------------------------------- Question 1 ---------------------------------
# Question 1: Is the termination of Employee in the company be affected by the others factors?

# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`

  # Analysis 1.1: How many employees are terminated in the company?
  
  # Filter data for terminated employees
  terminatedEmployees <- Employee[!duplicated(Employee$EMPLOYEE_ID, fromLast = TRUE),] # Extra feature 4

  # Count the numbers of employees for different status
  statusCounts <- table(terminatedEmployees$EMP_STATUS_IN_THE_COMPANY)
  
  # Create a data frame 
  statusData <- data.frame(Status = names(statusCounts), Count = as.numeric(statusCounts))
  
  # Change the status from fully capital to only first letter is in capital
  statusData$Status <- gsub("ACTIVE", "Active", statusData$Status) # Extra feature 5
  statusData$Status <- gsub("TERMINATED", "Terminated", statusData$Status)
  
  # Create the pie chart
  ggplot(statusData, aes(x = "", y = Count, fill = Status)) +
    geom_bar(stat = "identity", color = "white", width = 2) +
    coord_polar(theta = "y") + # Extra feature 6
    labs(x = "", y = "", fill = "Employee Status",
         title = "Status of Employees in the Company") +
    theme_void() + # Extra feature 7
    guides(fill = guide_legend(title = "Employee Status")) +
    geom_text(aes(label = paste0(round(Count / sum(Count) * 100), "%\n", Count)),
              position = position_stack(vjust = 0.5), color = "gray40", size = 4)
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`

  # Analysis 1.2: How does the birth date of employees affect their likelihood of termination from the company?
  
  # Filter data for terminated employees
  terminatedEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Extract year and month from DOB
  terminatedEmployees$YearMonth <- format(as.Date(terminatedEmployees$DOB), "%Y-%m")
  
  # Count the number of terminated employees by DOB
  terminationCounts <- with(terminatedEmployees, table(YearMonth))
  
  # Convert the table to a data frame
  terminationData <- as.data.frame(terminationCounts)
  terminationData$YearMonth <- as.Date(paste0(terminationData$YearMonth, "-01")) # Extra feature 8
  
  # Create a sequence of dates by every 6 months
  start_date <- min(terminationData$YearMonth)
  end_date <- max(terminationData$YearMonth)
  dates <- seq(start_date, end_date, by = "6 months") # Extra feature 9
  
  # Subset the data for every 6 months
  terminationData_sub <- terminationData[terminationData$YearMonth %in% dates, ] # Extra feature 10
  
  # Calculate summary statistics
  max_value <- max(terminationData_sub$Freq)
  min_value <- min(terminationData_sub$Freq)
  mean_value <- mean(terminationData_sub$Freq)
  
  # Create the line graph
  ggplot(terminationData_sub, aes(x = YearMonth, y = Freq, group = 1)) +
    geom_line(color = "blueviolet") +
    labs(x = "Date of Birth", y = "Number of Employees Terminated", 
         title = "Impact of Date of Birth on Termination") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # Extra feature 12
    scale_x_date(date_breaks = "6 months", date_labels = "%Y-%B") + 
    geom_text(aes(label = paste("Max:", max_value)), x = max(terminationData_sub$YearMonth), 
              y = max_value, color = "gray40") +
    geom_text(aes(label = paste("Min:", min_value)), x = min(terminationData_sub$YearMonth), 
              y = min_value, color = "gray40") +
    geom_text(aes(label = paste("Mean:", round(mean_value, 2))), x = mean(terminationData_sub$YearMonth), 
              y = mean_value, color = "gray40")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`

  # Analysis 1.3: How does the age of employees affect their likelihood of termination from the company?
  
  # Filter data for terminated employees
  terminatedEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Count the number of terminated employees by employees' age
  terminationCounts <- with(terminatedEmployees, table(EMP_AGE)) # Extra feature 13
  
  # Convert the table to a data frame
  terminationData <- as.data.frame(terminationCounts)
  
  # Arrange the data in terminationData in acsending order based on employees' age
  terminationData <- terminationData[order(terminationData$EMP_AGE), ] # Extra feature 14
  
  # Convert employees' age data to numeric format
  terminationData$EMP_AGE <- as.integer(terminationData$EMP_AGE)
  
  terminationData$EMP_AGE <- terminationData$EMP_AGE + 18
  
  # Calculate summary statistics
  max_value <- max(terminationData$Freq)
  min_value <- min(terminationData$Freq)
  mean_value <- mean(terminationData$Freq)
  
  # Create the lollipop graph 
  ggplot(terminationData, aes(x = EMP_AGE, y = Freq)) +
    geom_segment(aes(x = EMP_AGE, xend = EMP_AGE, y = 0, yend = Freq), color = "violet") + # Extra feature 15
    geom_point(color = "blueviolet", size = 3) +
    labs(x = "Employee Age", y = "Number of Employees Terminated", 
         title = "Impact of Employee Age on Termination") +
    theme_minimal() +
    geom_text(aes(label = paste("Max:", max_value)), x = max(terminationData$EMP_AGE), 
              y = max_value, vjust = -1, color = "gray40") +
    geom_text(aes(label = paste("Min:", min_value)), x = min(terminationData$EMP_AGE), 
              y = min_value, hjust = 0.3, vjust = -1.3, color = "gray40") +
    geom_text(aes(label = paste("Mean:", round(mean_value, 2))), x = mean(terminationData$EMP_AGE), 
              y = mean_value,vjust = 1, color = "gray40")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 1.4: How does the hire date of employees affect their likelihood of termination from the company?
  
  # Filter data for terminated employees
  terminatedEmployees = Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Extract month and year from hired date
  terminatedEmployees$Year <- format(as.Date(terminatedEmployees$EMP_HIRED_DATE), "%Y")
  
  # Count the number of terminated employees by hired date
  terminationCounts = with(terminatedEmployees, table(Year))
  
  # Convert the table to a data frame
  terminationData = as.data.frame(terminationCounts)
  
  # Arrange the data in terminationData in ascending order based on employees' hired year
  terminationData = terminationData[order(terminationData$Year), ]
  
  # Create the line graph
  ggplot(terminationData, aes(x = Year, y = Freq, group = 1)) +
    geom_point(color = "blue", size = 3.5) + 
    geom_line(color = "blueviolet", size = 0.5) +
    labs(x = "Hired year", y = "Number of Employees Terminated", 
         title = "Impact of Hired year on Termination") +
    theme_minimal() # Extra feature 11
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 1.5: What is the number of employee terminations at different times? 

  # Filter data for terminated employees
  terminatedEmployees = Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Extract year from termination date
  terminatedEmployees$Year <- format(as.Date(terminatedEmployees$EMP_TERMINATION_DATE), "%Y")
  
  # Count the number of terminated employees by terminated date
  terminationCounts = with(terminatedEmployees, table(Year))
  
  # Convert the table to a data frame
  terminationData = as.data.frame(terminationCounts)
  
  # Arrange the data in terminationData in ascending order based on employees' termination year
  terminationData = terminationData[order(terminationData$Year), ]
  
  # Create the line graph
  ggplot(terminationData, aes(x = Year, y = Freq, group = 1)) +
    geom_line(color = "blueviolet", size = 0.5) + 
    geom_point(color = "blue", size = 3.5) +
    labs(x = "Termination date", y = "Number of Employees Terminated", 
         title = "Number of employee terminations at different times") +
    theme_minimal()
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 1.6: What is the relationship between the terminated employees and their length of service in the company?
  
  # Filter data for terminated employees
  terminatedEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Calculate summary statistics
  max_value <- max(terminatedEmployees$EMP_SERVICE_LENGTH_YEARS)
  min_value <- min(terminatedEmployees$EMP_SERVICE_LENGTH_YEARS)
  mean_value <- mean(terminatedEmployees$EMP_SERVICE_LENGTH_YEARS)
  
  # Create the box plot
  ggplot(terminatedEmployees, aes(x = "", y = EMP_SERVICE_LENGTH_YEARS)) +
    geom_boxplot(fill = "lightblue", color = "darkblue") +
    stat_summary(fun = function(x) round(mean(x), 1), geom = "text", # Extra feature 16
                 aes(label = paste("Mean:", after_stat(y)))) +
    stat_summary(geom = "text", fun = max, label = paste("Max:", max_value), 
                 vjust = -1, color = "gray40") +
    stat_summary(geom = "text", fun = min, label = paste("Min:", min_value), 
                 vjust = 1, color = "gray40") +
    labs(x = "", y = "Length of service (years)", 
         title = "Relationship between terminated status and length of service") +
    theme_minimal()
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 1.7: What is the relationship between the terminated employees and the city they work at?
  
  # Filter data for terminated employees
  terminatedEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Count the number of terminated employees by city
  terminationCounts <- table(terminatedEmployees$EMP_CITY)
  
  # Convert the table to a data frame
  terminationData <- data.frame(City = names(terminationCounts), Count = terminationCounts)
  
  # Create the bar chart
  ggplot(terminationData, aes(x = City, y = Count.Freq)) +
    geom_bar(stat = "identity", fill = "lightblue", color = "blue") +
    labs(x = "City", y = "Number of terminated employees",
         title = "Relationship between employees termination status and city") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(aes(label = Count.Freq), vjust = -0.5, color = "gray40")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 1.8: What is the relationship between terminated employees and the department they work at?
  
  # Filter data for terminated employees
  terminatedEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Count the number of terminated employees by department
  terminationCounts <- with(terminatedEmployees, table(EMP_DEPARTMENT))
  
  # Convert the table to a data frame
  terminationData <- as.data.frame(terminationCounts)
  
  # Create the bar chart
  ggplot(terminationData, aes(x = EMP_DEPARTMENT, y = Freq)) +
    geom_bar(stat = "identity", fill = "lightblue", color = "blue") +
    labs(x = "Department", y = "Number of terminated employees",
         title = "Relationship between employees termination status and dDepartment") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(aes(label = Freq), vjust = -0.5, color = "gray40")  
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 1.9: What is the relationship between terminated employees, the city, and the department they work at?
  
  # Filter data for terminated employees
  terminatedEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Count the number of terminated employees by city and department
  terminationCounts <- with(terminatedEmployees, table(EMP_CITY, EMP_DEPARTMENT))
  
  # Convert the table to a data frame
  terminationData <- as.data.frame(terminationCounts)
  
  # Create the grouped line chart
  ggplot(terminationData, aes(x = EMP_CITY, y = Freq, color = EMP_DEPARTMENT, group = EMP_DEPARTMENT)) +
    geom_line() +
    labs(x = "City", y = "Number of terminated employees",
         title = "Terminated employees by city and department") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 1.10: What is the relationship between terminated employees and their job titles in the company?

  # Filter data for terminated employees
  terminatedEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Count the number of terminated employees by job title
  terminationCounts <- table(terminatedEmployees$EMP_JOB_TITLE)
  
  # Convert the table to a data frame
  terminationData <- as.data.frame(terminationCounts)
  
  # Create the pie chart 
  plot_ly(terminationData, labels = terminationData$Var1, values = terminationData$Freq, type = "pie", # Extra feature 17
          textposition = "inside", insidetextfont = list(color = "whitesmoke")) %>%
    layout(title = "Relationship between terminated status and employees\' job titles", # Extra feature 18 & 19
           scene = list(aspectratio = list(x = 1, y = 1, z = 0.7)))
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 1.11: What is the relationship between terminated employees and the store code they work at?
  
  # Filter data for terminated employees
  terminatedEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Count the number of terminated employees by store code
  terminationCounts <- table(terminatedEmployees$EMP_STORE_CODE)
  
  # Convert the table to a data frame
  terminationData <- as.data.frame(terminationCounts)
  
  # Arrange terminationData into decesending order base on Freq
  terminationData <- terminationData[order(terminationData$Freq, decreasing = TRUE), ]
  
  # Rename the column from var1 to EMP_STORE_CODE
  colnames(terminationData)[1] <- "EMP_STORE_CODE"
  
  # Create the bar chart
  ggplot(terminationData, aes(x = EMP_STORE_CODE, y = Freq)) +
    geom_bar(stat = "identity", fill = "lightcyan", color = "cyan") +
    labs(x = "Store code", y = "Number of terminated employees",
         title = "Relationship between terminated Employees and their store code") +
    theme_minimal() +
    geom_text(aes(label = Freq), vjust = 0.5, color = "gray40")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 1.12: What is the relationship between terminated employees and their gender?
  
  # Filter data for terminated employees
  terminatedEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Count the number of terminated employees by gender
  genderCounts <- table(terminatedEmployees$EMP_SEX_S)
  
  # Create the variable needed for pie chart
  colors <- c("pink", "steelblue1")
  labels <- names(genderCounts)
  counts <- as.numeric(genderCounts)
  
  # Calculate the percentage of each gender
  percentages <- round(counts / sum(counts) * 100, 1)
  
  # Create labels with counts and percentages
  labels_with_info <- paste0(labels, " (", counts, ", ", percentages, "%)")
  
  # Create the pie chart
  pie(counts, radius = 0.9, labels, col = colors, clockwise = TRUE,
      main = "Terminated employees and their gender")
  
  # Provide legend
  legend("bottomright", labels_with_info, fill = colors, cex = 0.6) # Extra feature 20
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 1.13: How does the age of employees affect their likelihood of termination from the company based on gender?
  
  # Filter data for terminated employees
  terminatedEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Create age groups
  terminatedEmployees$AgeGroup <- cut(terminatedEmployees$EMP_AGE, breaks = c(0, 20, 30, 40, 50, 60, Inf), # Extra feature 21
                                      labels = c("0-20", "20-30", "30-40", "40-50", "50-60", "60+"))
  
  # Count the number of terminated employees by age group, termination status, and gender
  terminationCounts <- with(terminatedEmployees, table(AgeGroup, EMP_STATUS_IN_THE_COMPANY, EMP_SEX_S))
  
  # Convert the table to a data frame
  terminationData <- as.data.frame(terminationCounts)
  
  # Create the grouped bar chart
  ggplot(terminationData, aes(x = AgeGroup, y = Freq, fill = EMP_STATUS_IN_THE_COMPANY )) +
    geom_bar(stat = "identity", fill = "lightgreen", color = "darkgreen") +
    facet_grid(. ~ EMP_SEX_S) + # Extra feature 22
    labs(x = "Age group", y = "Number of employees terminated", fill = "Termination Status",
         title = "Relationship between age, termination status and gender") +
    theme_minimal() +
    guides(fill = "none") +
    geom_text(aes(label = Freq), vjust = 0.5, color = "gray40")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 1.14: What types of termination reasons lead employees to leave the company?
  
    # Filter data for terminated employees
    terminatedEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
    
    # Count the number of terminated employees by termination reason and termination status
    terminationCounts <- table(terminatedEmployees$EMP_TERMINATION_REASON, 
                               terminatedEmployees$EMP_STATUS_IN_THE_COMPANY)
    
    # Convert the table to a data frame
    terminationData <- as.data.frame(terminationCounts)
    
    # Rename the column names
    colnames(terminationData) <- c("TerminationReason", "TerminationStatus", "Freq")
    
    # Create the pie chart
    ggplot(terminationData, aes(x = "", y = Freq, fill = TerminationReason)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(x = "", y = "", fill = "Termination Reason",
           title = "Reasons that lead employees to leave the company") +
      theme_void() +
      guides(fill = guide_legend(title = "Termination Reason")) +
      geom_text(aes(label = paste0(round(Freq / sum(Freq) * 100), "%","\n",Freq)),
                position = position_stack(vjust = 0.5), color = "gray40") 
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 1.15: What types of termination type lead the employees to leave the company?
  
  # Filter data for terminated employees
  terminatedEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Count the number of employees for each termination type and termination status
  terminationCounts <- table(terminatedEmployees$EMP_TERMINATION_TYPE, 
                             terminatedEmployees$EMP_STATUS_IN_THE_COMPANY) # Extra feature 23
  
  # Create the stacked bar chart with adjusted legend text size
  barplot(terminationCounts, main = "Termination type that lead employees to leave company", # Extra feature 24
          ylab = "Number of employees", col = c("red", "green"))
  
  # Create the legend
  legend("center", legend = rownames(terminationCounts), fill = c("red", "green"), 
         cex = 0.6, title = "Termination type")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 1.16: What is the relationship between terminated employees and the business unit they work at?

  # Filter data for terminated employees
  terminatedEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Count the number of terminated employees by business unit
  terminationCounts <- table(terminatedEmployees$BUSINESS_UNIT)
  
  # Create variables for pie chart
  labels <- names(terminationCounts)
  counts <- as.numeric(terminationCounts)
  
  # Calculate the percentage of different business units
  percentages <- round(counts / sum(counts) * 100, 1)
  
  # Create labels with counts and percentages
  labels_with_info <- paste0(labels, " (", counts, ", ", percentages, "%)") 
  
  # Create the pie chart
  pie(counts, main = "Terminated employees by business unit", labels = labels_with_info,
      col = c("#008080","#FFD700"), radius = 0.8)
  
  # Add a legend
  legend("bottomright", legend = labels_with_info,
         fill = c("#008080","#FFD700"), cex = 0.8)
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 1.17: What is the relationship between terminated employees, their age and the city they work at?

  # Filter data for terminated employees
  terminatedEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Create the box plot
  ggplot(terminatedEmployees, aes(x = EMP_CITY, y = EMP_AGE)) +
    geom_boxplot() +
    labs(x = "City", y = "Age", title = "Age distribution of terminated employees by city") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#---------------------------------- Question 2 ---------------------------------
# Question 2: What is the relationship with different age and the other data attributes in the year 2015? 
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 2.1: What is the mean age of employees in different departments in the year 2015?
  
  # Calculate the mean age by department
  meanAgeByDept <- aggregate(EMP_AGE ~ EMP_DEPARTMENT, employees2015, mean) # Extra feature 25
  
  # Create the bar chart
  ggplot(meanAgeByDept, aes(x = EMP_DEPARTMENT, y = EMP_AGE)) +
    geom_bar(stat = "identity", fill = "lightblue", color = "slateblue4") +
    geom_text(aes(label = round(EMP_AGE, 1)), vjust = -0.5, color = "gray40") + 
    labs(x = "Department", y = "Mean age", 
         title = "Mean age of employees by department in year 2015") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 2.2: What is the mean age of employees in different cities in the year 2015?
  
  # Calculate the mean age by city
  meanAgeByCity <- aggregate(EMP_AGE ~ EMP_CITY, employees2015, mean)
  
  # Create the bar chart
  ggplot(meanAgeByCity, aes(x = EMP_CITY, y = EMP_AGE)) +
    geom_bar(stat = "identity", fill = "lightblue", color = "slateblue4") +
    geom_text(aes(label = round(EMP_AGE, 1)), vjust = -0.3) +
    labs(x = "City", y = "Mean age", 
         title = "Mean age of employees by City in year 2015") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 2.3: What is the mean age of employees with different job titles in the year 2015?
  
  # Calculate the mean age by city
  meanAgeByJobTitles <- aggregate(EMP_AGE ~ EMP_JOB_TITLE, employees2015, mean)
  
  # Create the bar chart
  ggplot(meanAgeByJobTitles, aes(x = EMP_JOB_TITLE, y = EMP_AGE)) +
    geom_bar(stat = "identity", fill = "lightblue", color = "slateblue4") +
    geom_text(aes(label = round(EMP_AGE, 1)), vjust = -0.3, color = "gray40") +
    labs(x = "Job titles", y = "Mean age", 
         title = "Mean age of employees by job titles in year 2015") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 2.4: What is the mean age of employees if the business unit is the headquarters in the year 2015?
  
  # Calculate the mean age by city
  meanAgeByBusinessUnit <- aggregate(EMP_AGE ~ BUSINESS_UNITS_OF_COMPANY_EMP_STAY, 
                                     employees2015, mean)
  
  # Create the bar chart
  ggplot(meanAgeByBusinessUnit, aes(x = BUSINESS_UNITS_OF_COMPANY_EMP_STAY, y = EMP_AGE)) +
    geom_bar(stat = "identity", fill = "lightblue", color = "slateblue4") +
    geom_text(aes(label = round(EMP_AGE, 1)), vjust = -0.3, color = "gray40") +
    labs(x = "Business unit", y = "Mean age", title = "Mean age of employees by business unit in year 2015") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 2.5: What is the mean age of employees in different departments if the business unit is the headquarters in the year 2015?
  
  # Filter data for the year 2015 and business unit as headquarters
  hqEmployees2015 <- employees2015[employees2015$BUSINESS_UNITS_OF_COMPANY_EMP_STAY == "HEADOFFICE", ]
  
  # Calculate mean age by department
  meanAgeByDeptHQ <- aggregate(EMP_AGE ~ EMP_DEPARTMENT, hqEmployees2015, mean)
  
  # Create the bar chart
  ggplot(meanAgeByDeptHQ, aes(x = EMP_DEPARTMENT, y = EMP_AGE)) +
    geom_bar(stat = "identity", fill = "lightblue", color = "slateblue4") +
    geom_text(aes(label = round(EMP_AGE, 1)), vjust = -0.3, color = "gray40") +
    labs(x = "Department", y = "Mean age", 
         title = "Mean age of employees in different departments (Headquarters, 2015)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 2.6: What is the mean age of employees with different job titles if the business unit is the headquarters in the year 2015?
  
  # Filter data for the year 2015 and business unit as headquarters
  hqEmployees2015 <- employees2015[employees2015$BUSINESS_UNITS_OF_COMPANY_EMP_STAY == "HEADOFFICE", ]
  
  # Calculate mean age by job title
  meanAgeByJobTitleHQ <- aggregate(EMP_AGE ~ EMP_JOB_TITLE, hqEmployees2015, mean)
  
  # Create the bar chart 
  ggplot(meanAgeByJobTitleHQ, aes(x = EMP_JOB_TITLE, y = EMP_AGE)) +
    geom_bar(stat = "identity", fill = "lightblue", color = "slateblue4") +
    geom_text(aes(label = round(EMP_AGE, 1)), vjust = -0.3, color = "gray40") +
    labs(x = "Job title", y = "Mean age", 
         title = "Mean age of employees with different job titles (Headquarters, 2015)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 2.7: How many employees are there in different departments if their age is below 21 years old in the year 2015?
  
  # Calculate how many employees' age is below 21 years old
  nrow(employees2015[employees2015$EMP_AGE < 21, ])
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 2.8: How many employees are there in different departments if their age is between 21 to 60 years old in the year 2015?
  
  # Filter data for the year 2015 and age below 21
  youngEmployees2015 <- employees2015[employees2015$EMP_AGE >= 21 & employees2015$EMP_AGE <= 60, ]
  
  # Count the number of employees by department
  employeeCountByDept <- table(youngEmployees2015$EMP_DEPARTMENT)
  
  # Create a data frame for the chart
  chartData <- data.frame(department = names(employeeCountByDept),
                          count = as.numeric(employeeCountByDept))
  
  # Create the bar chart
  ggplot(chartData, aes(x = department, y = count, fill = department)) +
    geom_bar(stat = "identity", width = 0.6, color = "white") +
    geom_text(aes(label = count), color = "gray40", size = 4, vjust = -0.5) +  
    coord_flip() +
    labs(x = "Department", y = "Number of employees",
         fill = "Department",
         title = "Number of employees (Age between 21 and 60) in different departments in year 2015") +
    theme_minimal() +
    theme(legend.position = "none")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 2.9: How many employees are there in different departments if their age is over 60 years old in the year 2015?
  
  # Filter data for the year 2015 and age over 60
  olderEmployees2015 <- employees2015[employees2015$EMP_AGE > 60, ]
  
  # Count the number of employees by department
  employeeCountByDept <- table(olderEmployees2015$EMP_DEPARTMENT)
  
  # Create a data frame for the chart
  chartData <- data.frame(department = names(employeeCountByDept),
                          count = as.numeric(employeeCountByDept))
  
  # Create the bar chart
  ggplot(chartData, aes(x = department, y = count, fill = department)) +
    geom_bar(stat = "identity", width = 0.6, color = "white") +
    geom_text(aes(label = count), color = "gray40", size = 4, vjust = -0.5) + 
    coord_flip() +
    labs(x = "Department", y = "Number of employees",
         fill = "Department",
         title = "Number of employees (Age over 60) in Different Departments in year 2015") +
    theme_minimal() +
    theme(legend.position = "none")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 2.10: How many employees are there in different cities if their age is below 21 years old in the year 2015?
  
  # Calculate how many employees' age is below 21 years old
  nrow(employees2015[employees2015$EMP_AGE < 21, ])
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
 
  # Analysis 2.11: How many employees are there in different cities if their age is between 21 to 60 years old in the year 2015?
  
  # Filter data for the year 2015 and age below 21
  youngEmployees2015 <- employees2015[employees2015$EMP_AGE >= 21 & employees2015$EMP_AGE <= 60, ]
  
  # Count the number of employees by department
  employeeCountByCity <- table(youngEmployees2015$EMP_CITY)
  
  # Create a data frame for the chart
  chartData <- data.frame(city = names(employeeCountByCity),
                          count = as.numeric(employeeCountByCity))
  
  # Create the bar chart
  ggplot(chartData, aes(x = city, y = count, fill = city)) +
    geom_bar(stat = "identity", width = 0.6, color = "white") +
    geom_text(aes(label = count), color = "gray40", size = 4, vjust = -0.5) +
    coord_flip() +
    labs(x = "City", y = "Number of employees",
         fill = "City",
         title = "Number of employees (Age between 21 and 60) in different departments in year 2015") +
    theme_minimal() +
    theme(legend.position = "none")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
 
  # Analysis 2.12: How many employees are there in different cities if their age is over 60 years old in the year 2015?
  
  # Filter data for the year 2015 and age over 60
  olderEmployees2015 <- employees2015[employees2015$EMP_AGE > 60, ]
  
  # Count the number of employees by city
  employeeCountByCity <- table(olderEmployees2015$EMP_CITY)
  
  # Create a data frame for the chart
  chartData <- data.frame(city = names(employeeCountByCity),
                          count = as.numeric(employeeCountByCity))
  
  # Create the bar chart
  ggplot(chartData, aes(x = city, y = count, fill = city)) +
    geom_bar(stat = "identity", width = 0.6, color = "white") +
    geom_text(aes(label = count), color = "gray40", size = 4, vjust = -0.5) +
    coord_flip() +
    labs(x = "City", y = "Number of employees",
         fill = "City",
         title = "Number of employees (Age over 60) in different dities in year 2015") +
    theme_minimal() +
    theme(legend.position = "none")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
 
  # Analysis 2.13: How many employees are there for different job titles if their age is below 21 years old in the year 2015?
  
  # Calculate how many employees' age is below 21 years old
  nrow(employees2015[employees2015$EMP_AGE < 21, ])
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
 
  # Analysis 2.14: How many employees are there for different job titles if their age is between 21 to 60 years old in the year 2015?
  
  # Filter data for the year 2015 and age below 21
  youngEmployees2015 <- employees2015[employees2015$EMP_AGE >= 21 & employees2015$EMP_AGE <= 60, ]
  
  # Count the number of employees by department
  employeeCountByTitle <- table(youngEmployees2015$EMP_JOB_TITLE)
  
  # Create a data frame for the chart
  chartData <- data.frame(title = names(employeeCountByTitle),
                          count = as.numeric(employeeCountByTitle))
  
  # Create the bar chart
  ggplot(chartData, aes(x = title, y = count, fill = title)) +
    geom_bar(stat = "identity", width = 0.6, color = "white") +
    geom_text(aes(label = count), color = "gray40", size = 4, vjust = -0.5) +
    coord_flip() +
    labs(x = "Job Titles", y = "Number of employees",
         fill = "Job titles",
         title = "Number of employees (Age between 21 and 60) in different job titles in year 2015") +
    theme_minimal() +
    theme(legend.position = "none")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
 
  # Analysis 2.15: How many employees are there for different job titles if their age is over 60 years old in the year 2015?
  
  # Filter data for the year 2015 and age over 60
  olderEmployees2015 <- employees2015[employees2015$EMP_AGE > 60, ]
  
  # Count the number of employees by city
  employeeCountByTitle <- table(olderEmployees2015$EMP_JOB_TITLE)
  
  # Create a data frame for the chart
  chartData <- data.frame(title = names(employeeCountByTitle),
                          count = as.numeric(employeeCountByTitle))
  
  # Create the bar chart
  ggplot(chartData, aes(x = title, y = count, fill = title)) +
    geom_bar(stat = "identity", width = 0.6, color = "white") +
    geom_text(aes(label = count), color = "gray40", size = 4, vjust = -0.5) + 
    coord_flip() +
    labs(x = "Title", y = "Number of employees",
         fill = "Title",
         title = "Number of employees (Age over 60) in different job title in year 2015") +
    theme_minimal() +
    theme(legend.position = "none")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
 
  # Analysis 2.16: What is the relationship between employees' age and gender in the year 2015?
  
  # Calculate the mean values
  mean_values <- aggregate(EMP_AGE ~ EMP_SEX_S, data = employees2015, FUN = mean)
  mean_values
  
  # Calculate the median values
  median_values <- aggregate(EMP_AGE ~ EMP_SEX_S, data = employees2015, FUN = median) # Extra feature 26
  median_values
  
  # Create the violin plot with statistical summaries
  ggplot(employees2015, aes(x = EMP_SEX_S, y = EMP_AGE, fill = EMP_SEX_S)) +
    geom_violin() + # Extra feature 27
    stat_summary(fun = "mean", geom = "point", shape = 18, size = 5, color = "red",
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun = "median", geom = "point", shape = 11, size = 3, color = "blue",
      position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "median_hilow", geom = "errorbar", width = 0.2, 
      position = position_dodge(width = 0.9)) +
    labs(x = "Gender", y = "Age", fill = "Gender",
      title = "Distribution of employees' age by gender (2015)") +
    theme_minimal()
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 2.17: What is the number of employees joining the company at different ages?
  
  # Create a histogram plot
  histogram_plot <- ggplot(Employee[!duplicated(Employee$EMPLOYEE_ID, fromFirst = TRUE),], 
                           aes(x = EMP_AGE)) +
    geom_histogram(binwidth = 5, fill = "skyblue", color = "white") +
    labs(x = "Age", y = "Number of employees", 
         title = "Number of employees joining at different ages") +
    theme_minimal()
  
  # Create a density plot
  density_plot <- ggplot(Employee[!duplicated(Employee$EMPLOYEE_ID, fromFirst = TRUE),], 
                         aes(x = EMP_AGE, fill = "pink")) +
    geom_density(alpha = 0.6) +
    labs(x = "Age", y = "Density", title = "Density of Employees Joining at Different Ages") +
    theme_minimal() +
    guides(fill = FALSE)
  
  # Combine two plot
  combined_plot <- histogram_plot + density_plot
  
  # Show the plot
  combined_plot
  
  
  
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#---------------------------------- Question 3 ---------------------------------
# Question 3: What is the relationship with different data attributes for employees who terminated their job in the company due to retirement? 
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 3.1: How many employees terminated their job due to retirement?
  
  # Filter data for employees who terminated their job due to retirement
  retiredEmployees <- Employee[Employee$EMP_TERMINATION_REASON == "Retirement", ]
  
  # Calculate the data
  count(retiredEmployees)
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`

  # Analysis 3.2: What is the mean age of employees who retire from the company?
  
  # Filter data for terminated employees
  terminatedEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Calculate the mean age by termination reason
  meanAgeData <- aggregate(EMP_AGE ~ EMP_TERMINATION_REASON, terminatedEmployees, mean)
  
  # Create the box plot with mean age and labels
  ggplot(terminatedEmployees, aes(x = EMP_TERMINATION_REASON, y = EMP_AGE, 
                                  fill = EMP_TERMINATION_REASON)) +
    geom_boxplot() +
    geom_text(data = meanAgeData, aes(label = round(EMP_AGE), y = EMP_AGE),
              vjust = -1, color = "gray40", size = 3.5, fontface = "bold") +
    labs(x = "Termination Reason", y = "Age",
         fill = "Termination Reason",
         title = "Age Distribution by Termination Reason with Mean Age") +
    theme_minimal()
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`

  # Analysis 3.3: What is the age of employees who retire from the company?
  
  # Filter data for employees who retire
  retiredEmployees <- Employee[Employee$EMP_TERMINATION_REASON == "Retirement", ]
  
  # Count the number of employees by age
  ageCounts <- table(retiredEmployees$EMP_AGE)
  
  # Create a data frame for the chart
  chartData <- data.frame(age = as.numeric(names(ageCounts)), count = as.numeric(ageCounts))
  
  # Calculate the percentage of each age group
  chartData$percentage <- chartData$count / sum(chartData$count) * 100
  
  # Create the pie chart
  ggplot(chartData, aes(x = "", y = percentage, fill = factor(age))) +
    geom_bar(stat = "identity", width = 5, color = "black") +
    coord_polar("y", start = 0) +
    labs(x = NULL, y = NULL, fill = "Age",
         title = "Age Distribution of Employees Retiring") +
    theme_void() +
    guides(fill = guide_legend(title = "Age")) +
    geom_text(aes(label = paste0(count, " (", round(percentage), "%)")),
              position = position_stack(vjust = 0.5), color = "gray40")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 3.4: What is the mean age of employees who retire from different departments?
  
  # Filter data for employees who retire
  retiredEmployees <- Employee[Employee$EMP_TERMINATION_REASON == "Retirement", ]
  
  # Calculate the mean age by department
  meanAgeByDepartment <- aggregate(EMP_AGE ~ EMP_DEPARTMENT, data = retiredEmployees, 
                                   FUN = mean)
  
  # Show the colors
  colors() # Extra feature 28
  
  # Create the bar chart
  meanAgeChart <- ggplot(meanAgeByDepartment, aes(x = EMP_DEPARTMENT, y = EMP_AGE)) +
    geom_bar(stat = "identity", fill = "#2ca02c", color = "#444444") +  # Extra feature 29
    labs(x = "Department", y = "Mean Age", title = "Mean Age of Employees Retiring by Department") +
    geom_text(aes(label = round(EMP_AGE, 1)), vjust = -0.5, color = "gray40") + # Extra feature 30
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Call the chart
  meanAgeChart
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 3.5: Based on the department, what is the age of employees who retire from the company?
  
  # Filter data for employees who retire
  retiredEmployees <- Employee[Employee$EMP_TERMINATION_REASON == "Retirement", ]
  
  # Count the number of employees by age and department
  ageCounts <- table(retiredEmployees$EMP_AGE, retiredEmployees$EMP_DEPARTMENT)
  
  # Convert the table to a data frame
  ageData <- as.data.frame(ageCounts)
  
  # Assign names to columns
  colnames(ageData) <- c("Age", "Department", "Count")
  
  # Create the bar chart
  ageDistributionChart  <- ggplot(ageData, aes(x = Department, y = Count, fill = factor(Age))) +
    geom_bar(stat = "identity", position = "stack") +
    labs(x = "Department", y = "Number of Employees", fill = "Age",
         title = "Age of Employees Retiring by Department") +
    theme_minimal() +
    guides(fill = guide_legend(title = "Age")) +
    geom_text(aes(label = Count),
              position = position_stack(vjust = 0.5), color = "gray40") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Call the chart
  ageDistributionChart
  
  # Combine the graph with previous analysis
  grid.arrange(meanAgeChart, ageDistributionChart, nrow = 1) # Extra feature 31
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 3.6: What is the mean age of employees who retire from different departments, based on their gender?
  
  # Filter data for retired employees
  retiredEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED" &
                                 Employee$EMP_TERMINATION_REASON == "Retirement", ]
  
  # Calculate the mean age by department and gender
  meanAgeByDepartmentGender <- aggregate(EMP_AGE ~ EMP_DEPARTMENT + EMP_SEX_S, 
                                         data = retiredEmployees, FUN = mean)
  
  # Create the bar chart
  ggplot(meanAgeByDepartmentGender, aes(x = EMP_DEPARTMENT, y = EMP_AGE, fill = EMP_SEX_S)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Department", y = "Mean Age", fill = "Gender",
         title = "Mean Age of Retired Employees by Department and Gender") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(aes(label = round(EMP_AGE, 1)), position = position_dodge(width = 0.9), 
              vjust = -0.5, color = "gray40")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 3.7: What is the mean age of employees who retire from different cities, based on their gender?
  
  # Filter data for retired employees
  retiredEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED" &
                                 Employee$EMP_TERMINATION_REASON == "Retirement", ]
  
  # Calculate the mean age by department and gender
  meanAgeByCityGender <- aggregate(EMP_AGE ~ EMP_CITY + EMP_SEX_S, data = retiredEmployees, FUN = mean)
  
  # Create the bar chart
  ggplot(meanAgeByCityGender, aes(x = EMP_CITY, y = EMP_AGE, fill = EMP_SEX_S)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "City", y = "Mean Age", fill = "Gender",
         title = "Mean Age of Retired Employees by City and Gender") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(aes(label = round(EMP_AGE, 1)), position = position_dodge(width = 0.9), 
              vjust = -0.5, color = "gray40")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 3.8: What is the mean age of employees who retire from different job titles, based on their gender?
  
  # Filter data for retired employees
  retiredEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED" &
                                 Employee$EMP_TERMINATION_REASON == "Retirement", ]
  
  # Calculate the mean age by department and gender
  meanAgeByTitleGender <- aggregate(EMP_AGE ~ EMP_JOB_TITLE + EMP_SEX_S, data = retiredEmployees, 
                                    FUN = mean)
  
  # Create the bar chart
  ggplot(meanAgeByTitleGender, aes(x = EMP_JOB_TITLE, y = EMP_AGE, fill = EMP_SEX_S)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Job Titles", y = "Mean Age", fill = "Gender",
         title = "Mean Age of Retired Employees by Job titles and Gender") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(aes(label = round(EMP_AGE, 1)), position = position_dodge(width = 0.9), 
              vjust = -0.5, color = "gray40")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 3.9: What is the service length of employees who retire from the company?
  
  # Filter data for retired employees
  retiredEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED" &
                                 Employee$EMP_TERMINATION_REASON == "Retirement", ]
  
  # Create the line plot
  ggplot(retiredEmployees, aes(x = EMP_SERVICE_LENGTH_YEARS, y = EMP_AGE)) +
    geom_line(color = "wheat", alpha = 0.6) +
    labs(x = "Service Length", y = "Age", title = "Service Length vs. Age of Retired Employees") +
    theme_minimal() 
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 3.10: What is the percentage rate of employees who retire from the company?
  
  # Filter data for terminated employees
  terminatedEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Count the number of terminated employees by termination reason
  terminationCounts <- table(terminatedEmployees$EMP_TERMINATION_REASON)
  
  # Convert the table to a data frame
  terminationData <- as.data.frame(terminationCounts)
  
  # Assign names for different columns
  colnames(terminationData) <- c("TerminationReason", "Freq")
  
  # Create the 3D pie chart
  pie3D(terminationData$Freq, labels = terminationData$TerminationReason,
        explode = 0.1, main = "Termination Reason Distribution",
        col = rainbow(length(terminationData$Freq)))
  
  # Add legend beside the pie chart
  legend("bottomright", legend = paste0(terminationData$TerminationReason, 
                                        " (", terminationData$Freq, ")"), 
         fill = rainbow(length(terminationData$Freq)), cex = 0.4)
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 3.11: Based on employees' gender, what is the percentage rate of employees who retire from the company?
  
  # Filter data for terminated employees
  terminatedEmployees_F <- terminatedEmployees[terminatedEmployees$EMP_SEX_S == "F", ]
  
  # Count the number of terminated employees by termination reason
  terminationCounts_F <- table(terminatedEmployees_F$EMP_TERMINATION_REASON)
  
  # Convert the table to a data frame
  terminationData_F <- as.data.frame(terminationCounts_F)
  
  # Assign names for different columns
  colnames(terminationData_F) <- c("TerminationReason", "Freq")
  
  # Create the 3D pie chart
  pie3D(terminationData_F$Freq, labels = terminationData_F$TerminationReason,
        explode = 0.1, main = "Termination Reason Distribution (Female Employees)",
        col = rainbow(length(terminationData_F$Freq)))
  
  # Add legend beside the pie chart
  legend("bottomright", legend = paste0(terminationData_F$TerminationReason, 
                                        " (", terminationData_F$Freq, ")"), 
         fill = rainbow(length(terminationData_F$Freq)), cex = 0.4)
  
  #-------------------------------------------------------------------------------------  
  
  # Filter data for terminated employees
  terminatedEmployees_M <- terminatedEmployees[terminatedEmployees$EMP_SEX_S == "M", ]
  
  # Count the number of terminated employees by termination reason
  terminationCounts_M <- table(terminatedEmployees_M$EMP_TERMINATION_REASON)
  
  # Convert the table to a data frame
  terminationData_M <- as.data.frame(terminationCounts_M)
  
  # Assign names for different columns
  colnames(terminationData_M) <- c("TerminationReason", "Freq")
  
  # Create the 3D pie chart
  pie3D(terminationData_M$Freq, labels = terminationData_M$TerminationReason,
        explode = 0.1, main = "Termination Reason Distribution (Male Employees)",
        col = rainbow(length(terminationData_M$Freq)))
  
  # Add legend beside the pie chart
  legend("bottomright", legend = paste0(terminationData_M$TerminationReason, 
                                        " (", terminationData_M$Freq, ")"), 
         fill = rainbow(length(terminationData_M$Freq)), cex = 0.4)
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 3.12: Based on employees' business units, what is the age of employees who retire from the company?
  
  # Filter data for retired employees
  retiredEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED" &
                                 Employee$EMP_TERMINATION_REASON == "Retirement", ]
  
  # Create the violin plot
  ggplot(retiredEmployees, aes(x = BUSINESS_UNITS_OF_COMPANY_EMP_STAY, y = EMP_AGE)) +
    geom_violin(fill = "springgreen1", color = "black") +
    labs(x = "Business Unit", y = "Age", 
         title = "Age of Retired Employees by Business Unit") +
    theme_minimal()
  
  
  
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#---------------------------------- Question 4 ---------------------------------
# Question 4: What is the relationship between different data attributes and employees who voluntarily terminate their job in the company?  

# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 4.1: What is the percentage rate of employees who terminated their job from the company voluntarily?
  
  # Filter data for terminated employees
  terminatedEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Count the number of voluntary and involuntary terminations
  terminationCounts <- table(terminatedEmployees$EMP_TERMINATION_TYPE)
  
  # Calculate the percentage of voluntary and involuntary terminations
  voluntaryPercentage <- terminationCounts["Voluntary"] / sum(terminationCounts) * 100
  involuntaryPercentage <- terminationCounts["Involuntary"] / sum(terminationCounts) * 100
  
  # Create the pie chart
  pie(terminationCounts, labels = c(paste0("Involuntary\n", terminationCounts["Involuntary"], 
                                           " people\n", round(involuntaryPercentage, 2), "%"), 
                                    paste0("Voluntary\n", terminationCounts["Voluntary"], 
                                           " people\n", round(voluntaryPercentage, 2), "%")),
      col = c("purple", "green"), main = "Termination Type Distribution")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 4.2: What is the difference between the genders of employees who terminated their job from the company voluntarily?
  
  # Filter data for terminated employees
  terminatedEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Filter data for voluntary terminations
  voluntaryTerminations <- terminatedEmployees[terminatedEmployees$EMP_TERMINATION_TYPE == "Voluntary", ]
  
  # Create a stacked bar chart
  barplot(table(voluntaryTerminations$EMP_SEX_S, voluntaryTerminations$EMP_SEX_S),
          col = c("lightblue1", "pink"), legend = TRUE,
          xlab = "Gender", ylab = "Count", main = "Voluntary Termination by Gender")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 4.3: What is the relationship between the age of employees who terminated their job from the company voluntarily based on their gender?
  
  # Filter data for terminated employees
  terminatedEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Filter data for voluntary terminations
  voluntaryTerminations <- terminatedEmployees[terminatedEmployees$EMP_TERMINATION_TYPE == "Voluntary", ]
  
  # Create a density plot
  ggplot(voluntaryTerminations, aes(x = EMP_AGE, fill = EMP_SEX_S)) +
    geom_density(alpha = 0.5) + # Extra feature 32
    labs(x = "Age", y = "Density", title = "Voluntary Termination by Gender") +
    scale_fill_manual(values = c("lightgreen", "lightpink")) # Extra feature 33

# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 4.4: What is the percentage rate of different termination reasons for employees who terminated their job from the company voluntarily?
  
  # Filter data for terminated employees
  terminatedEmployees <- Employee[Employee$EMP_STATUS_IN_THE_COMPANY == "TERMINATED", ]
  
  # Filter data for voluntary terminations
  voluntaryTerminations <- terminatedEmployees[terminatedEmployees$EMP_TERMINATION_TYPE == "Voluntary", ]
  
  # Calculate the percentage of each termination reason
  terminationCounts <- table(voluntaryTerminations$EMP_TERMINATION_REASON)
  percentage <- terminationCounts / sum(terminationCounts) * 100
  
  # Create a bar chart
  barplot(percentage, col = rainbow(length(percentage)),
          xlab = "Termination Reason", ylab = "Percentage",
          main = "Termination Reasons (Voluntary)")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 4.5: What is the termination date of employees who terminated their job from the company voluntarily?
  
  # Filter data for voluntary terminations
  voluntaryTerminations <- Employee[Employee$EMP_TERMINATION_TYPE == "Voluntary", ]
  
  # Create a histogram of termination dates
  ggplot(voluntaryTerminations, aes(x = EMP_TERMINATION_DATE)) +
    geom_histogram(binwidth = 30, aes(fill=..count..)) +
    labs(x = "Termination Date", y = "Count", title = "Termination Dates of Voluntary Terminations") +
    theme_minimal() + 
    scale_fill_gradient("Count", low = "green", high = "red") # Extra feature 34
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 4.6: What is the service length of employees who terminated their job from the company voluntarily based on their gender?
  
  # Filter data for voluntary terminations
  voluntaryTerminations <- Employee[Employee$EMP_TERMINATION_TYPE == "Voluntary", ]
  
  # Create a scatter plot of service length by gender
  ggplot(voluntaryTerminations, aes(x = EMP_SERVICE_LENGTH_YEARS, y = EMP_AGE, color = EMP_SEX_S)) +
    geom_point(alpha = 0.6) + 
    labs(x = "Service Length (Years)", y = "Age", 
         title = "Service Length vs. Age of Voluntarily Terminated Employees by Gender") +
    theme_minimal()
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 4.7: What is the city of employees who terminated their job from the company voluntarily?
  
  # Filter data for voluntary terminations
  voluntaryTerminations <- Employee[Employee$EMP_TERMINATION_TYPE == "Voluntary", ]
  
  # Count the occurrences of each city
  cityCounts <- table(voluntaryTerminations$EMP_CITY)
  
  # Convert the city counts to a data frame
  cityData <- data.frame(City = names(cityCounts), Count = as.numeric(cityCounts))
  
  # Sort the cities by count in descending order
  cityData <- cityData[order(cityData$Count, decreasing = TRUE), ]
  
  # Create a bar chart using ggplot
  ggplot(cityData, aes(x = reorder(City, Count), y = Count, fill = Count)) +
    geom_bar(stat = "identity") +
    labs(x = "City", y = "Count", title = "City of Voluntarily Terminated Employees") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_gradient(low = "lightblue", high = "darkblue")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 4.8: What is the department of employees who terminated their job from the company voluntarily based on their gender?
  
  # Filter data for voluntary terminations
  voluntaryTerminations <- Employee[Employee$EMP_TERMINATION_TYPE == "Voluntary", ]
  
  # Count the occurrences of each department by gender
  departmentCounts <- table(voluntaryTerminations$EMP_DEPARTMENT, voluntaryTerminations$EMP_SEX_S)
  
  # Convert department counts to a data frame
  departmentData <- as.data.frame.table(departmentCounts)
  names(departmentData) <- c("Department", "Gender", "Count")
  
  # Create a stacked bar chart of department by gender
  ggplot(departmentData, aes(x = Department, y = Count, fill = Gender)) +
    geom_bar(stat = "identity", position = "stack") + # Extra feature 35
    labs(x = "Department", y = "Count", 
         title = "Department of Voluntarily Terminated Employees by Gender") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(aes(label = Count),
      position = position_stack(vjust = 0.5), color = "gray40")
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 4.9: What is the job title of employees who have terminated their job from the company voluntarily?
  
  # Filter data for voluntary terminations
  voluntaryTerminations <- Employee[Employee$EMP_TERMINATION_TYPE == "Voluntary", ]
  
  # Count the occurrences of each job title
  jobTitleCounts <- table(voluntaryTerminations$EMP_JOB_TITLE)
  
  # Convert job title counts to a data frame
  jobTitleData <- data.frame(JobTitle = names(jobTitleCounts), Count = jobTitleCounts)
  
  # Create a horizontal bar chart of job titles
  ggplot(jobTitleData, aes(x = Count.Freq, y = reorder(JobTitle, Count.Freq))) +
    geom_bar(stat = "identity", fill = "violetred") +
    labs(x = "Count", y = "Job Title", title = "Job Titles of Voluntarily Terminated Employees") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`

  
  
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#---------------------------------- Question 5 ---------------------------------
# Question 5: What is the relationship for different data attributes and the working experiences of the employees in the year 2015? 

# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 5.1: How long of working experiences do the employees have in the year 2015?
  
  # Create a histogram of working experiences
  histogram <- ggplot(employees2015, aes(x = EMP_SERVICE_LENGTH_YEARS)) +
    geom_histogram(fill = "#b2df8a", color = "#33a02c", bins = 10) +
    labs(x = "Working Experience (Years)", y = "Count", 
         title = "Working Experiences of Employees in 2015") +
    theme_minimal()
  
  # Create a violin plot of working experiences
  violin <- ggplot(employees2015, aes(x = "", y = EMP_SERVICE_LENGTH_YEARS)) +
    geom_violin(fill = "#cab2d6", color = "#6a3d9a") +
    labs(y = "Working Experience (Years)", title = "Working Experiences of Employees in 2015") +
    theme_minimal()
  
  # Show graph
  histogram + violin
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 5.2: What is the average of working experiences do the employees have in the year 2015?
  
  # Calculate the average working experience
  round(mean(employees2015$EMP_SERVICE_LENGTH_YEARS),2)
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 5.3: What is the relationship between the employees' age and working experiences in the year 2015?
  
  # Line plot of average working experience by age
  avg_experience <- aggregate(EMP_SERVICE_LENGTH_YEARS ~ EMP_AGE, employees2015, mean)
  ggplot(avg_experience, aes(x = EMP_SERVICE_LENGTH_YEARS, y = EMP_AGE)) +
    geom_line(color = "tan") +
    labs(x = "Average Working Experience (Years)", y = "Age",
         title = "Relationship between Age and Average Working Experience in 2015") +
    theme_minimal() 
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 5.4: How long of working experiences does the employees have for different genders in the year 2015?
  
  # Grouped violin plot of working experiences by gender in 2015
  ggplot(employees2015, aes(x = EMP_SEX_S, y = EMP_SERVICE_LENGTH_YEARS, fill = EMP_SEX_S)) +
    geom_violin() +
    stat_summary(fun = median, geom = "text", aes(label = paste("Median:", 
                                                                round(after_stat(y), 2))),
                 vjust = 0.5, color = "gray40") +
    labs(x = "Gender", y = "Working Experience (Years)",
         title = "Working Experiences of Employees by Gender in 2015") +
    theme_minimal()
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 5.5: What is the average of working experiences does the employees have for different genders in the year 2015?
  
  # Bar chart of average working experiences by gender
  averageExperiences <- aggregate(EMP_SERVICE_LENGTH_YEARS ~ EMP_SEX_S, employees2015, mean)
  ggplot(averageExperiences, aes(x = EMP_SEX_S, y = EMP_SERVICE_LENGTH_YEARS)) +
    geom_bar(stat = "identity", fill = "tomato", color = "black") +
    geom_text(aes(label = round(EMP_SERVICE_LENGTH_YEARS, 2)),
              vjust = -0.5, color = "gray40") +  # Add text labels with rounded values
    labs(x = "Gender", y = "Average Working Experience (Years)",
         title = "Average Working Experiences by Gender in 2015") +
    theme_minimal()
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 5.6: What is the relationship between the employees' age and working experiences for different genders in the year 2015?
  
  # Create scatter plot with gender-separated trend lines
  scatterPlot <- ggplot(employees2015, aes(x = EMP_AGE, y = EMP_SERVICE_LENGTH_YEARS, color = EMP_SEX_S)) +
    geom_point(alpha = 0.6) +
    geom_smooth(aes(color = EMP_SEX_S), method = "lm", se = FALSE) +
    labs(x = "Age", y = "Working Experience (Years)",
         title = "Relationship between Age and Working Experience by Gender in 2015") +
    theme_minimal()
  
  # Create line plot showing average working experiences by age and gender:
  averageExperiences <- aggregate(EMP_SERVICE_LENGTH_YEARS ~ EMP_AGE + EMP_SEX_S, 
                                  employees2015, mean)
  linePlot <- ggplot(averageExperiences, aes(x = EMP_AGE, y = EMP_SERVICE_LENGTH_YEARS, 
                                             color = EMP_SEX_S)) +
    geom_line() +
    labs(x = "Age", y = "Average Working Experience (Years)",
         title = "Average Working Experiences by Age and Gender in 2015") +
    theme_minimal()
  
  scatterPlot + linePlot
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 5.7: What is the average of working experiences do the employees have for different departments in the year 2015?
  
  ggplot(employees2015, aes(x = EMP_DEPARTMENT, y = EMP_SERVICE_LENGTH_YEARS)) +
    geom_point(color = "salmon", alpha = 0.6) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "gray40", 
                 show.legend = FALSE) +
    stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)),
                 vjust = -1.5, color = "gray40") +
    labs(x = "Department", y = "Working Experience (Years)",
         title = "Working Experiences by Department in 2015") +
    theme_minimal()
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
  
  # Analysis 5.8: What is the relationship between the employee's business unit and working experiences for different genders in the year 2015?
  
  # Create scatter Plot with Jitter:
  scatterPlotJitter <- ggplot(employees2015, aes(x = BUSINESS_UNITS_OF_COMPANY_EMP_STAY , 
                                                 y = EMP_SERVICE_LENGTH_YEARS, color = EMP_SEX_S)) +
    geom_jitter(alpha = 0.6) + # Extra feature 36
    labs(x = "Business Unit", y = "Working Experience (Years)",
         title = "Working Experiences by Business Unit and Gender in 2015") +
    theme_minimal()
  
  # Create the boxplot
  boxPlot <- ggplot(employees2015, aes(x = BUSINESS_UNITS_OF_COMPANY_EMP_STAY, 
                                       y = EMP_SERVICE_LENGTH_YEARS, fill = EMP_SEX_S)) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "gray40", 
                 position = position_dodge(width = 0.75)) +
    stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)),
                 vjust = -1.5, color = "gray40") +
    labs(x = "Business Unit", y = "Working Experience (Years)",
         title = "Working Experiences by Business Unit and Gender in 2015") +
    theme_minimal()
  
  scatterPlotJitter + boxPlot
  
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`
# ~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`~`!@#$%^&*()_+-={}|[]\:"<>?;',./`