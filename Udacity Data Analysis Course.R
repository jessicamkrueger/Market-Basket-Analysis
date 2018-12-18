library(ggplot2)
library(dplyr)
library(reshape2)

# Write code to create a new data frame,
# called 'pf.fc_by_age_gender', that contains
# information on each age AND gender group.

# The data frame should contain the following variables:

#    mean_friend_count,
#    median_friend_count,
#    n (the number of users in each age and gender grouping)

# Here is an example of the structure of your data frame. Your
# data values will be different. Note that if you are grouping by
# more than one variable, you will probably need to call the
# ungroup() function. 

#   age gender mean_friend_count median_friend_count    n
# 1  13 female          247.2953                 150  207
# 2  13   male          184.2342                  61  265
# 3  14 female          329.1938                 245  834
# 4  14   male          157.1204                  88 1201

# See the Instructor Note for two hints.

pf.fc_by_age_gender <- group_by(pf, age, gender) %>%
  filter(!is.na(gender)) %>%
  summarise(mean_friend_count = mean(friend_count), 
            median_friend_count = median(friend_count), 
            n = n())

#plot line graph of median friend count by gender
ggplot(pf.fc_by_age_gender, aes(age, median_friend_count, color = gender)) +
  geom_line()

#reshape data using dcast and melt
pf.fc_by_age_gender.wide <- dcast(pf.fc_by_age_gender, 
                                  age ~ gender, 
                                  value.var = "median_friend_count")

# Plot the ratio of the female to male median
# friend counts using the data frame
# pf.fc_by_age_gender.wide.

# Think about what geom you should use.
# Add a horizontal line to the plot with
# a y intercept of 1, which will be the
# base line. Look up the documentation
# for geom_hline to do that. Use the parameter
# linetype in geom_hline to make the
# line dashed.

# The linetype parameter can take the values 0-6:
# 0 = blank, 1 = solid, 2 = dashed
# 3 = dotted, 4 = dotdash, 5 = longdash
# 6 = twodash

pf.fc_by_age_gender.wide <- mutate(pf.fc_by_age_gender.wide, ratio = female/male)
ggplot(pf.fc_by_age_gender.wide, aes(x=age, y=ratio)) +
  geom_line() +
  geom_hline(yintercept = 1, alpha = .3, linetype = 2)

#OR you can create the ratio directly in the graph
ggplot(pf.fc_by_age_gender.wide, aes(x=age, y=female/male)) +
  geom_line() +
  geom_hline(yintercept = 1, alpha = .3, linetype = 2)

# Create a variable called year_joined
# in the pf data frame using the variable
# tenure and 2014 as the reference year.

# The variable year joined should contain the year
# that a user joined facebook.

# See the Instructor Notes for three hints if you get
# stuck. Scroll down slowly to see one hint at a time
# if you would like some guidance.

# This programming exercise WILL BE automatically graded.
pf <- mutate(pf, year_joined = floor(2014- tenure/365))
summary(pf$year_joined)
table(pf$year_joined)

# Create a new variable in the data frame
# called year_joined.bucket by using
# the cut function on the variable year_joined.

# You need to create the following buckets for the
# new variable, year_joined.bucket

#        (2004, 2009]
#        (2009, 2011]
#        (2011, 2012]
#        (2012, 2014]

# Note that a parenthesis means exclude the year and a
# bracket means include the year.

pf$year_joined.bucket <- cut(pf$year_joined, breaks = c(2004, 2009, 2011, 2012, 2014))

# Create a line graph of friend_count vs. age
# so that each year_joined.bucket is a line
# tracking the median user friend_count across
# age. This means you should have four different
# lines on your plot.

# You should subset the data to exclude the users
# whose year_joined.bucket is NA.

ggplot(data = subset(pf, !is.na(year_joined.bucket)), 
       aes(x=age, y=friend_count)) +
  geom_line(aes(color=year_joined.bucket), 
            stat = 'summary', 
            fun.y = median)

# Write code to do the following:

# (1) Add another geom_line to code below
# to plot the grand mean of the friend count vs age.

# (2) Exclude any users whose year_joined.bucket is NA.

# (3) Use a different line type for the grand mean.

# As a reminder, the parameter linetype can take the values 0-6:

# 0 = blank, 1 = solid, 2 = dashed
# 3 = dotted, 4 = dotdash, 5 = longdash
# 6 = twodash

ggplot(data = subset(pf, !is.na(year_joined.bucket)), 
       aes(x=age, y=friend_count)) +
  geom_line(aes(color=year_joined.bucket), 
            stat = 'summary', 
            fun.y = mean) +
  geom_line(stat = 'summary', fun.y = mean, linetype = 2) #plots overall mean


#calculate friending rate per day of tenure, excluding anyone with less than 1 day
pf_FR <- subset(pf, tenure > 0) 
pf_FR$friending_rate <- pf_FR$friend_count/pf_FR$tenure
summary(pf_FR$friending_rate)


# Create a line graph of mean of friendships_initiated per day (of tenure)
# vs. tenure colored by year_joined.bucket.

# You need to make use of the variables tenure,
# friendships_initiated, and year_joined.bucket.

# You also need to subset the data to only consider user with at least
# one day of tenure.

pfFI <- subset(pf, pf$tenure > 0)
ggplot(pfFI, aes(x=tenure, y=friendships_initiated/tenure)) +
  geom_line(aes(color = year_joined.bucket), 
            stat = "summary",
            fun.y = mean)

#try with a smoother instead of line to reduce noise in graph
pfFI <- subset(pf, pf$tenure > 0)
ggplot(pfFI, aes(x=tenure, y=friendships_initiated/tenure)) +
  geom_smooth(aes(color = year_joined.bucket))


#Yogurt data set activities
yo <- read.csv("yogurt.csv")
summary(yo)
str(yo)

yo$id <- as.factor(yo$id)

ggplot(yo, aes(price)) +
  geom_histogram(binwidth = 10)

# Create a new variable called all.purchases,
# which gives the total counts of yogurt for
# each observation or household.

# One way to do this is using the transform
# and run the examples of code at the bottom of the
# documentation to figure out what it does.

# The transform function produces a data frame
# so if you use it then save the result to 'yo'!

# OR you can figure out another way to create the
# variable.
colnames(yo)
yo <- mutate(yo, all.purchases = strawberry+blueberry+pina.colada+plain+mixed.berry)


# Create a scatterplot of price vs time.

# This will be an example of a time series plot.

# Resolve overplotting issues by using
# techniques you learned in Lesson 4.

# What are some things that you notice?
ggplot(yo, aes(x=time, y=price)) +
  geom_point(alpha = .03)

#create plot sampling households and price vs. time wiht diff. points indicating
#size of purchase
set.seed(123)
sample.ids <- sample(levels(yo$id), 16)

yo$all.purchases <- as.integer(yo$all.purchases)

ggplot(aes(x=time, y=price),
       data = subset(yo, id %in% sample.ids)) +
  facet_wrap(~id) +
  geom_line() +
  geom_point(aes(size = all.purchases), pch = 1)


#Lesson 8 - Explore Many Variables
# Create a histogram of diamond prices.
# Facet the histogram by diamond color
# and use cut to color the histogram bars.

# The plot should look something like this.
# http://i.imgur.com/b5xyrOu.jpg

# Note: In the link, a color palette of type
# 'qual' was used to color the histogram using
# scale_fill_brewer(type = 'qual')

diamonds <- diamonds
ggplot(diamonds, aes(price)) +
  geom_histogram(aes(fill = cut), binwidth = 2500) +
  facet_wrap(~color)

# Create a scatterplot of diamond price vs.
# table and color the points by the cut of
# the diamond.

ggplot(diamonds, aes(x=table, y=price)) +
  geom_point(aes(color = cut)) +
  scale_x_continuous(limits = c(50, 80), breaks = seq(50, 80, 2))

# Create a scatterplot of diamond price vs.
# volume (x * y * z) and color the points by
# the clarity of diamonds. Use scale on the y-axis
# to take the log10 of price. You should also
# omit the top 1% of diamond volumes from the plot.

# Note: Volume is a very rough approximation of
# a diamond's actual volume.
diamonds <- mutate(diamonds, volume = x*y*z)
ggplot(aes(x=volume, y=price),
       data = subset(diamonds, volume <= quantile(volume, .99)))+ #saying subset 
  #where volume is less than or equal to the 99th quantile of volume (which is 354.4266)
  geom_point(aes(color = clarity)) +
  scale_y_log10() +
  scale_color_brewer(type = 'div')



# Your task is to create a new variable called 'prop_initiated'
# in the Pseudo-Facebook data set. The variable should contain
# the proportion of friendships that the user initiated.
pf <- mutate(pf, prop_initiated = friendships_initiated/friend_count)
summary(pf$prop_initiated)

# Create a line graph of the median proportion of
# friendships initiated ('prop_initiated') vs.
# tenure and color the line segment by
# year_joined.bucket.

# Recall, we created year_joined.bucket in Lesson 5
# by first creating year_joined from the variable tenure.
# Then, we used the cut function on year_joined to create
# four bins or cohorts of users.

# (2004, 2009]
# (2009, 2011]
# (2011, 2012]
# (2012, 2014]

ggplot(aes(x=tenure, y=prop_initiated),
       data = subset(pf, !is.na(pf$prop_initiated))) +
  geom_line(aes(color = year_joined.bucket), 
            stat = "summary",
            fun.y = median)
  
#smooth previous plot
ggplot(aes(x=tenure, y=prop_initiated),
       data = subset(pf, !is.na(pf$prop_initiated))) +
  geom_smooth(aes(color = year_joined.bucket))

#find mean prop_intiated of 2012-2014 bucket
meanPI <- pf %>%
  group_by(year_joined.bucket) %>%
  filter(friend_count > 0) %>%
  summarize(meanPI = mean(prop_initiated))


# Create a scatter plot of the price/carat ratio
# of diamonds. The variable x should be
# assigned to cut. The points should be colored
# by diamond color, and the plot should be
# faceted by clarity.

# The plot should look something like this.
# http://i.imgur.com/YzbWkHT.jpg.

# Note: In the link, a color palette of type
# 'div' was used to color the histogram using
# scale_color_brewer(type = 'div')

ggplot(diamonds, aes(x=cut, y=price/carat)) +
  geom_jitter(aes(color = color)) +
  facet_wrap(~clarity) +
  scale_color_brewer(type='div')
  
