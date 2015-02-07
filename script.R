# load data
data <- read.csv(unz("activity.zip", "activity.csv"))

# converts NA steps to zero
data[is.na(data$steps),1] <- 0

data <- read.csv(unzip("activity.zip", "activity.csv"))

# split per each day
s <- split(data, data$date)

# ..and find array with total of steps per each day
s.sum<-sapply(s, function(r) { sum(r$steps,na.rm=T)})

# part 1

# 1.1 histogram with totals per each day
hist(s.sum)

# 2.1 mean and median
mean(s.sum)
median(s.sum)

# part 2

# 2.1 time series mean numeber of steps for each interval across all days 
ts <- sapply(split(data$steps, data$interval), function(x) { mean(x, na.rm=T) } )

plot(attributes(ts)[[1]], ts, type="l")

# 2.2 find interval on time-serie above with maximum average numer of steps 

ts[which.max(ts)]
attributes(ts)[[1]][which.max(ts)]

# part 3

# calculate the number of missing values
r.good <- sum(!is.na(data$steps))
r.missing <- sum(is.na(data$steps))

r.missing/r.good*100

# ..for each day, it is all good or all NA
# so fill missing days with average for corresponding weekday

# include weekdays in main datasource
data$weekday <- weekdays(strptime(data[,2],"%Y-%m-%d"))

# split data in weekdays
s.wday <- split(data, data$weekday)

# optain names of weedays
w.names <- attributes(s.wday)[[1]]

# for each weekday, compute corresponding time series of mean number of steps per interval
ts.w <- list()
i <- 0 
for (v in s.wday) { 
    i <- i+1;
    ts.w[[i]] <- sapply(split(v$steps, v$interval), function(r) { mean (r, na.rm=T)} )  
}

# plot for each day of the week the average number of steps/intervals for that day of the week
par(mfrow = c(2, 4))
for (i in 1:7) {
    plot(attributes(ts.w[[i]])[[1]], ts.w[[i]], type="l", main=w.names[i])
}

# x: string containing date in the format YYYY-mm-dd
# w.names: vector containing days of week, whose indexes correspond to tables with 
# average number of steps for that day of the week / interval

get.day.index <- function (x, w.names) {
    x.wd <- weekdays(strptime(x, "%Y-%m-%d")); 
    which(w.names==x.wd)
}

# create replica of original splits
s.new <- s
for (i in 1:length(s.new)) {
    is.missing <- sum(is.na(s.new[[i]]$steps)) != 0    
    print(is.missing)
    if (is.missing) {
        x <- s.new[[i]]
        day.index <- get.day.index(x$date[1],w.names)
        print(day.index)
        x$steps <- ts.w[[day.index]]
        s.new[[i]] <- x
    }
}

# test if is weekend
weekdays(strptime(data[,2],"%Y-%m-%d")) %in% c("Saturday", "Sunday")
