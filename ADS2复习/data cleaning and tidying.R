# from lixianghua W6
### 1.  Explore our data

head(wnv)
summary(wnv)
str(wnv)
class(wnv)
class(wnv$TEST.DATE)


### Data cleaning

## 2.1 missing values

# if NA in dataframe, call it
anyNA(df)
db[apply(is.na(df), 1, any), ]
db[rowSums(is.na(df)) > 0, ]

which(!complete.cases(db)) # check rows containing missing values
db[!complete.cases(db), ]

# Shall we remove them? Xianghua decided to drop them out.
pgp3= pgp3[complete.cases(pgp3), ]
anyNA(pgp3) # after done, check again

# for "character" type column
length(wnv[wnv$LOCATION=="", "LOCATION"]) 
# check whether the column need to do this
# if it is (supplementary) comment, I think it does not need.

apply(is.na(db),2,which) # very useful
# this is to find the row, col of NA
# output like:
# $Measurement
# integer(0)
# $Glucose
# [1] 32 37 38 39 40 47 48 52 53

# dimytro
t1d_drug <- t1d_drug %>%
  drop_na()

# NOTE: you need to document!
# Since there is no data or clues of how to fill up those missing data, Wanlu
# decided to delete those data points. Before deleting those rows, in total
# there are 103 observations (rows), while only 92 remained after the cleaning.

## 2.2 duplicates

# if duplicated in dataframe, call it
anyDuplicated()
duplicated_rows <- duplicated(df) # logical vector
which(duplicated_rows)
# if want to include the repeated lines that appear for the first time,
# i.e. mark all the duplicated rows
all_duplicated_rows <- duplicated(df) | duplicated(df, fromLast = TRUE)
which(all_duplicated_rows)
duplicated_df <- df[duplicated_rows, ]  
all_duplicated_df <- df[all_duplicated_rows, ] 
# when print the rows, both logical vector and index are accepted

# From lixianghua
which(duplicated(pgp3_og)) # ok there are duplicated rows. 
idx1= which(duplicated(pgp3_og))
idx2= which(duplicated(pgp3_og, fromLast = T))
# Since it only gives one set of index duplicated rather than both. Let's check 
pgp3_og[c(idx1, idx2),  ] # Xianghua thinks it is safe to keep only one copy
pgp3<- pgp3_og[-idx1, ]# remove one set of the duplciated rows.  
# Let's check if all the duplicated rows are removed. 
which(duplicated(pgp3)) # ok. 


# From liuwanlu, documentation
# From this results, we can see *ID=50, 93, 26* are lines duplicated. So we
# want to delete the duplicated copy.

# 2.3 data type
str(pgp3) # What do you think? Shall we change any of them? 
table(pgp3$sex)  # hard to understand 
table(as.character(pgp3$sex))
# 1 and 2. From the source of data we know 1 is male and 2 is female. 

# The SampleID should be factors, NOT integers. 
# The elisa od values shouldn't be factors. better to change them to numeric values. 
# The sex varaible is not easily interpretable. will change them to M and F. 
# covert SampleID, age.F, sex to readable factor
pgp3$SampleID <- as.character(pgp3$SampleID)
pgp3$elisa.od <- as.numeric(as.character(pgp3$elisa.od))
# Here, we change the factor to character first.
# What would happen if you don't convert to character first? 
pgp3$elisa.pre.od <- as.numeric(as.character(pgp3$elisa.pre.od))
pgp3$sex <- gsub("2","F",as.character(pgp3$sex))
pgp3$sex <- as.factor(pgp3$sex)

# change the name of the column
names(wnv)[1] <- "YEAR"

# after done, check again
summary(pgp3)


## 2.4 outliers and strange patterns

# Please explore and plot the way you want.
# lixianghua
ggplot(data=pgp3) + geom_boxplot(aes(x=sex, y=elisa.od))
ggplot(data=pgp3) + geom_boxplot(aes(x=age.f, y=elisa.pre.od))

# liuwanlu: add ID label to the points
plot(x=data.noNA.noDup$carat,y=data.noNA.noDup$volume,
     pch=20,col="darkgoldenrod4",
     las=1,xlab="carat",ylab="volume",
     main="diamond carat ~ volume")
text(data.noNA.noDup$carat, data.noNA.noDup$volume,
     labels=data.noNA.noDup$ID,col="dimgray",
     cex= 0.7, pos=4)
# Here we found two strange data point, ID=7 and ID=28. We decide to take a
# look at those two IDs.
print(data.noNA.noDup[which(data.noNA.noDup$ID=="7"),])
print(data.noNA.noDup[which(data.noNA.noDup$ID=="28"),])

# duplicate check can also be done on certain columns
rvse.dup.idx=which(duplicated(data.noNA.noDup[,9:11],fromLast = TRUE))
# e.g. check whether two points share the same coordinate (x,y,z)

# After ploting, find One outlier. Which is it? 
wnv[wnv$NUMBER.OF.MOSQUITOES> 70, ]

# 2.5 Correct typos. 

summary(data.noNA.noDup.noStrg$cut)
print(data.noNA.noDup.noStrg[which(data.noNA.noDup.noStrg$cut=="Idea"),])
data.noNA.noDup.noStrg.notypo$cut[which(data.noNA.noDup.noStrg.notypo$cut=="Idea")]=
  "Ideal"
summary(data.noNA.noDup.noStrg.notypo$cut) # after done, check again
