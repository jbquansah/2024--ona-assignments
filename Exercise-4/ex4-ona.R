###Using USPTO patent examiner data,
#Create variable for application processing time
#‘app_proc_time’ that measures the number of days (or weeks) from application filing date, until the final decision
#on it (patented or abandoned)
#2. Use linear regression models `lm()` to estimate the
#relationship between centrality and `app_proc_time`
#– Make sure to control for other characteristics of the examiner
#that you think might influence that relationship
#3. Does this relationship differ by examiner gender?
#  – Hint: Include an interaction term `gender x centrality` into
#your models
#4. Discuss your findings and their implication for the USPTO



library(tidygraph)
library(igraph)
library(readr)
library(ggraph)
library(igraph)
library(ggplot2)
library(dplyr)
data_path = "~/GitHub/desktop-tutorial/Exercise-3/"
edges <- read_csv(paste0(data_path,"edges.csv"))
applications <- read_csv(paste0(data_path,"cleaned_applications.csv"))


#Drop pending applications
applications <- applications[applications$disposal_type != "PEN", ]
app_subset <- subset(applications, disposal_type %in% c("ISS", "ABN"))

#create application processing time which if disposal type is iss (filing - issue date), and abn (filing - abandon date)
app_subset$decision_date <- ifelse(app_subset$disposal_type == "ISS", app_subset$patent_issue_date,
                             ifelse(app_subset$disposal_type == "ABN", app_subset$abandon_date, NA))

app_subset$app_proc_time <- as.numeric(difftime(as.Date(app_subset$decision_date), 
                                                as.Date(app_subset$filing_date),
                                                units = "days"))

app_subset<-app_subset%>%
  select(application_number, examiner_id, gender, race, tenure_days, app_proc_time, disposal_type)

# Count NA values per column
count_missing <- colSums(is.na(app_subset))
print(count_missing)

app_subset<-na.omit(app_subset)


#How many have a negative value - Why would I have a negative value
summaryd<-summary(app_subset)
print(summaryd)

#Drop negative values from app_proc_time
app_subset <- app_subset[app_subset$app_proc_time >= 0, ]


#Create edges dataset
edges<- edges %>%
  filter(application_number %in% app_subset$application_number)
colnames(edges)[3:4] <- c("from", "to")
#edges<-edges%>%
#  select(from, to)
edges<-na.omit(edges)  #Remove NA values
edges <- edges[, c("from", "to", setdiff(names(edges), c("from", "to")))]

g <- graph_from_data_frame(edges, directed = TRUE)
degree <- degree(g)                        # Degree centrality
closeness <- closeness(g)                  # Closeness centrality
betweenness <- betweenness(g)              # Betweeness Centrality

#Creation of 
centrality <- data.frame(examiner_id    = V(g)$name,
                         degree      = degree,
                         closeness   = closeness,
                         betweenness = betweenness)

#Merge centralities with app_subset
merged_data <- merge(app_subset, centrality, by= "examiner_id")


# Count NA values per column
count_missing <- colSums(is.na(merged_data))
print(count_missing)


#Regression
#Dummy variables
merged_data$gender= as.factor(merged_data$gender)
merged_data$race= as.factor(merged_data$race)

merged_data<-na.omit(merged_data)

reg1 <- lm(app_proc_time ~ betweenness+closeness+degree+tenure_days+disposal_type, data=merged_data)
summary(reg1)

reg2 <- lm(app_proc_time ~ gender*betweenness+closeness+degree+tenure_days+disposal_type, data=merged_data)
summary(reg2)

reg3 <- lm(app_proc_time ~ betweenness+gender*closeness+degree+tenure_days+disposal_type, data=merged_data)
summary(reg3)

reg4 <- lm(app_proc_time ~ betweenness+closeness+gender*degree+tenure_days+disposal_type, data=merged_data)
summary(reg3)

reg5 <- lm(app_proc_time ~ gender*betweenness + gender*closeness + gender*degree +tenure_days, data=merged_data)
summary(reg5)


