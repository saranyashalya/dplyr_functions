##Ref: http://stcorp.nl/R_course/tutorial_dplyr.html

head(mtcars)

#filter - select rows in data.frame that fit one or more  logical expressions.
mtcars %>% filter(cyl==8, qsec>16)

#arrange, sort data.frame according to one or more columns.
mtcars %>% arrange(cyl, mpg)

#select, select columns, or rename existing columns
mtcars %>% dplyr::select( mpg,wt)

mtcars %>% dplyr::select(starts_with('c'))
mtcars %>% dplyr::select(ends_with('yl'))
mtcars %>% dplyr::select(contains('ar'))
mtcars %>% dplyr::select(one_of('mpg','cyl'))

mtcars %>% dplyr::select(c('mpg','ab')) ## This will error out as there is no column 'ab'
mtcars %>% dplyr::select(one_of('mpg','ab'))  ## gives warning but prints the columns that are available

# rename, rename columns, only keep those
mtcars %>% rename(new_mpg = mpg, new_cyl =cyl)

#distinct, select unique rows based on the content of one or more columns
mtcars %>% distinct(cyl)

#mutate, edit or add columns, but keep all columns not mentioned in the function
dat = dplyr::select(mtcars, mpg, wt)
mutate(dat, new_col = mpg*wt, new_col2 = mpg+wt)

dat %>% mutate(mpg = mpg+wt)

#transmute, edit or add columns, but only keep columns that are mentioned in the function
dat %>% transmute(new_col=mpg+wt)

# summarise , summarise columns
mtcars %>% summarize(mean_weight = mean(wt), max_milepergallon = max(mpg), min_cylinder = min(cyl))

#summarise_each, summarise multiple functions, multiple columns at once.
mtcars %>% summarize_each(funs(mean,sd), mpg, wt)

#sample_n, sample n rows from data.frame
sample_n(mtcars, 10)

#sample_frac, sample frac fraction of rows.
mtcars %>% sample_frac(0.2)

#glimpse, get a quick overview of the data.
glimpse(mtcars)

#do, execute R expression
result = do(mtcars, model = lm(mpg ~ wt, data = .))
result$model

### gather and spread
library(tidyr)
temp_df <- data.frame(temp = runif(3,100,105),
                      rain_station1 = runif(3, 5,10),
                      rain_station2 = runif(3, 5,10),
                      rain_station3 = runif(3,5,10))


temp_df %>% tidyr::gather(Station_Name,Value, c(rain_station1,rain_station2,rain_station3))
gathered_temp_df <- temp_df %>% tidyr::gather(Station_Name,Value, -temp)

gathered_temp_df %>% spread(Station_Name, Value)

#Grouping and performing operations per group - split, apply and combine

mtcars_grouped = group_by(mtcars , am)

summarize(mtcars_grouped, mean_weight = mean(wt), max_mpg = max(mpg), min_cylinder = min(cyl))

# How to string these small operations together

#nesting calls
summarize(group_by(filter(mtcars, wt>3.15),cyl, am), mn = mean(mpg))

# multiple lines
mtcars_filtered <- filter(mtcars, wt >3.15)
mtcars_grouped <- group_by(mtcars_filtered, cyl,am)
summarize(mtcars_grouped, mn = mean(mpg))

# piping
mtcars %>% filter(wt >3.15) %>% group_by(cyl,am) %>% summarize(mn =mean(mpg))


#### exercises
#Using mtcars, select the cars that have a weight of more than 3 tonnes, and calculate the mean number of horse power.
mtcars %>% filter(wt > 3) %>% summarize(horsep_mean = mean(hp))

#How many cars of each unique number of gears are present in the mtcars dataset. Provide two solutions, one using summarise and one using tally
mtcars %>% group_by(gear) %>% summarize('Number of cars' = n())

mtcars %>% count(gear)

mtcars %>% group_by(gear) %>% tally(n())

#
library(reshape2)
dat = melt(EuStockMarkets)

#Remove the first column.

dat1 <- dat %>% dplyr::select(-one_of("Var1"))

#Rename the remaining two columns to market_id and closing_price.
dat2<- dat1 %>% rename("market_id"= Var2, "closing_price"=value)

#Determine which stock exchange has the highest mean closing price.

dat2 %>% group_by(market_id) %>% summarize("highest closing price"= max(closing_price))

#Which stock exchange varies most over time?
dat2 %>% group_by(market_id) %>% summarize("highest sd closing price"= sd(closing_price)) %>% ungroup() %>% summarize(max(`highest sd closing price`))


#
df <- data.frame(
  x = sample(10, 100, rep = TRUE),
  y = sample(10, 100, rep = TRUE)
)
#How many duplicate rows are present?

library(tidyverse)
nrow(df[duplicated(df),])

## 
library(nycflights13)
data(flights)

#How many flights there are per month.

flights %>% count(month)

#Which destination has the largest amount of incoming flights.
flights %>% group_by(dest) %>% summarize(n()) 

#Which carrier has the most air time.

flights %>% group_by(carrier) %>% summarize(sum(air_time))

#Which carrier flies the fastest on average.

flights %>% group_by(carrier) %>% summarize(mean(hour))

#Which carrier has the longest delay times? Can these delay times be related to the length of the flight? Come up with a new column that quantifies this relation.

flights %>% group_by(carrier) %>% summarize("mean_delay" = mean(arr_delay, na.rm=TRUE),"mean_hour" =mean(air_time, na.rm = TRUE)) %>% mutate ("Relation"= mean_delay/mean_hour)

