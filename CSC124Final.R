#**************PART ONE

# Q1
# read in data
iris_df <- read.table("http://apollo.waketech.edu/~frank/csc124/iris.txt", sep = "\t", header = FALSE)

# set column names
colnames(iris_df) <- c('sepal_length','sepal_width','petal_length','petal_width','species')
head(iris_df)

# display speciese column as a facot
species_col <- as.factor(iris_df$species)
species_col

# Q2
# create variable to represent petal area
iris_df$petal_area <- iris_df$petal_length * iris_df$petal_width
iris_df$petal_area

# Q3
# create a new subset of iris_df w/o species column
iris_df_new <- subset(iris_df, select = c(sepal_length, sepal_width, petal_length, petal_width))
head(iris_df_new)
# new subset's avg value for each column
iris_df_avg <- colMeans(iris_df_new)
iris_df_avg
# new subset's std dev for each column
iris_df_sd <- sapply(iris_df_new, sd)
iris_df_sd
# new subset's range for each column
iris_df_min <- sapply(iris_df_new, min)
iris_df_max <- sapply(iris_df_new, max)
iris_df_range <- t(data.frame(iris_df_min,iris_df_max))
iris_df_range

# Q4
# boxplot number of species
png(file = "boxplot.png")
boxplot(petal_width ~ species_col, 
        data = iris_df, 
        xlab = "Species",
        ylab = "Width of Petals", 
        main = "Iris Petal_width and Species Count")
dev.off()
# histogram of petal area
# the word doc says to show distribution of the petal_area variable,
# but the histograms on the doc say width and length??
# So i'm just going to do them all....
# petal area distribution
png(file = "histogram_petal_area.png")
hist(iris_df$petal_area,xlab = "Weight",col = "yellow",border = "black")
dev.off()
# petal width distribution
png(file = "histogram_petal_width.png")
hist(iris_df$petal_width,xlab = "Weight",col = "yellow",border = "black")
dev.off()
# petal length distribution
png(file = "histogram_petal_length.png")
hist(iris_df$petal_length,xlab = "Weight",col = "yellow",border = "black")
dev.off()
# scatterplot of length and width
input <- iris_df[,c('petal_length','petal_width')]
png(file = "scatterplot.png")
plot(x = input$petal_length,y = input$petal_width,
      xlab = "Petal Length",
      ylab = "Petal Width",
      main = "Petal Width vs Petal Length"
)
dev.off()

#**********************PART TWO

# Q5
# load duluth temps
temperature_df <- read.table("duluth_rocheter_temperature.csv", sep = ",", header = TRUE)
head(temperature_df)
#create a plot showing city year max temps between 11/12/17 - 11/16/17- color by cities
max_p <- ggplot(data = temperature_df, aes(x = DD, y = MAXIMUM, colour = City)) + geom_point(na.rm = FALSE)
max_p
#create a plot showing city year min temps between 11/12/17 - 11/16/17- color by cities
min_p <- ggplot(data = temperature_df, aes(x = DD, y = MINIMUM, colour = City)) + geom_point(na.rm = FALSE)
min_p
#create a plot showing city last year max and min temps between 11/12/17 - 11/16/17- color by cities
temp_p <- ggplot(data = temperature_df, aes(x = DD, y = LAST.YEAR, colour = City)) + geom_point(na.rm = FALSE)
temp_p

# Q6
# connect to xamplite MySQL db
con <- dbConnect(RMySQL::MySQL(),
                 dbname ='test',
                 port = 3306,
                 user = 'wbip',
                 password = 'wbip123')
con
# list named table from test db
table_names <- dbListTables(con)
table <- lapply(table_names, dbReadTable, conn = con)
table
# create df from subset of list 'table'
# here is one way to do it by calling data.frame 
# on the correct index of the list 'table'
personnel_df <- data.frame(table[1])
personnel_df
# or you can use the dbReadTable function to save one
# of the tables as a dataframe
personnel_df2 <- dbReadTable(con, 'personnel')
personnel_df2
# create a plot to display personnel's hrwage on histogram
hist(personnel_df$hourlyWage, xlab = "Hourly Wage",col = "red", main = "Employee Hourly Wage Distribution")
# aggregate each jobtitle's maximum, minimum, and average hourlyWage
# avg
aggregate(hourlyWage ~ jobTitle, data = personnel_df, mean)
#max
aggregate(hourlyWage ~ jobTitle, data = personnel_df, max)
#min
aggregate(hourlyWage ~ jobTitle, data = personnel_df, min)