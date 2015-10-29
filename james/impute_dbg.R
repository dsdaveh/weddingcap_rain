library(dplyr)
source( "../team/data-prep.R")

train.sample.dummy <- train.sample1000
#Group to ID level
ID_dummy <- train.sample.dummy %>%
    select(Id, Ref) %>%
    group_by(Id) %>%
    summarise(Ref = mean(Ref, na.rm = TRUE))
#Remove IDs with only NAs
ID_dummy_noNAs <- ID_dummy %>% 
    filter(Ref != "NaN")
#From Original set - keep only IDs having at least 1 non-NA
train.sample.dummy.noHorizNAs <- inner_join(train.sample.dummy, ID_dummy_noNAs, by = "Id")  #LOC1

#DAH: Your problem starts at LOC1 because Ref is common to both data.frames 
#   R handles that by renaming both Ref columns as Ref.x and Ref.y so at LOC2 below 
#   your if statement is failing because ...$Ref[i] references  a column that doesn't exist

# Here's another way of doing what do above
tsdnH <- train.sample1000 %>%
    group_by(Id) %>%
    mutate( allNA = all( is.na(Ref)) ) %>%
    filter( ! allNA )
train.sample.dummy.noHorizNAs <- tsdnH

#DAH:  The if statements below also fail for i=1, since ...[i-1] is not defined


#impute remaining values vertically by imputing surrounding values
 

for (i in 1:nrow(train.sample.dummy.noHorizNAs))
{
    if((is.na(train.sample.dummy.noHorizNAs$Ref[i])== TRUE) &&     
       train.sample.dummy.noHorizNAs$Id[i] != train.sample.dummy.noHorizNAs$Id[i-1])      #LOC2
    {
        n<- 1
        while (is.na(train.sample.dummy.noHorizNAs$Ref[i+n]))
        {
            n++
        }
        train.sample.dummy.noHorizNAs$Ref[i] = train.sample.dummy.noHorizNAs$Ref[i+n]
    } else if( (is.na(train.sample.dummy.noHorizNAs$Ref[i])== TRUE) && 
               train.sample.dummy.noHorizNAs$Id[i] = train.sample.dummy.noHorizNAs$Id[i-1]) #LOC3
    {
        train.sample.dummy.noHorizNAs$Ref[i] = train.sample.dummy.noHorizNAs$Ref[i-1]
    } else {
        train.sample.dummy.noHorizNAs$Ref[i] = train.sample.dummy.noHorizNAs$Ref[i]
    }
}