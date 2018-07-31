##Housekeeping ####
rm(list=ls())
setwd('~\\Education\\CU\\MSBX 5415-001 Advanced Data Analytics\\Project')
require(data.table)
require(ggplot2)
require(purrr)
require(dplyr)
require(tidyverse)
require(Hmisc)
require(corrplot)
library(caret)

##  read in required files ####
book_data <- fread('Data\\bookings1.csv')
met_stat_areas <- fread('Data\\AirportMetStatArea.csv') #provides the CBSA code
crimestats = fread("Data\\CrimeStats.csv")

income1 <- fread('Data\\aMSA_may2005_dl.csv',
                 na.strings = c('#','*','**','***'))
income2 <- fread('Data\\MSA_may2005_dl_1.csv',
                 na.strings = c('#','*','**','***'))
income3 <- fread('Data\\MSA_may2005_dl_2.csv',
                 na.strings = c('#','*','**','***'))
income4 <- fread('Data\\MSA_may2005_dl_3.csv',
                 na.strings = c('#','*','**','***'))

#Data Cleaning ####
#clean book_data
#remove outlier ID = 433978
book_data <- filter(book_data, ID != 433978)

#convert date fields to dates
book_data$TicketDate <- as.Date(book_data$TicketDate, format = '%m/%d/%Y')
book_data$DepartureDate <- as.Date(book_data$DepartureDate, format = '%m/%d/%Y')

#add variables for ticket_lag
book_data$ticket_lag <- as.numeric(book_data$DepartureDate-book_data$TicketDate)

#aggregate by day, calculate avgCostPerMile on records with Fare > 0
#Step 1
new.data <- filter(book_data, Fare != 0) %>%
    group_by(InvClass, Origin, Destination, Channel, DepartureDate) %>%
    summarise(avgCostPerMile = mean(Fare/PassengerMiles),
              avgLag = mean(ticket_lag)) %>%
    ungroup()

zeronew.data <- filter(book_data, Fare == 0) %>%
    group_by(InvClass, Origin, Destination, Channel, DepartureDate) %>%
    summarise(avgCostPerMile = mean(Fare/PassengerMiles),
              avgLag = mean(ticket_lag)) %>%
    ungroup()

#find what we can that matches on all variables
grpVars <- c('InvClass', 'Channel','Origin', 'Destination', 'DepartureDate')
impCostPerMile <- left_join(zeronew.data, new.data,
                                    by = grpVars[1:5])

zeronew.data$avgCostPerMile <- impCostPerMile$avgCostPerMile.y

foundAvgCostPerMile <- filter(zeronew.data, avgCostPerMile > 0)
new.data <- rbind(new.data, foundAvgCostPerMile)
zeronew.data <- filter(zeronew.data, avgCostPerMile == 0 | is.na(avgCostPerMile))

#Step 2 - match on successively fewer variables
for (i in 2:4) {
    impCostPerMile <- left_join(zeronew.data, new.data, by = grpVars[i:5])
    
    finalImp <- filter(impCostPerMile,
                       avgCostPerMile.y > 0 | !is.na(avgCostPerMile.y)) %>%
        group_by(.dots = grpVars[i:5]) %>%
        summarise(avgCostPerMile = mean(avgCostPerMile.y),
                  avgLag = mean(avgLag.y)) %>%
        ungroup()
    
    finalJoin <- left_join(zeronew.data, finalImp, by = grpVars[i:5])
    
    zeronew.data$avgCostPerMile <- finalJoin$avgCostPerMile.y
    
    foundAvgCostPerMile <- filter(zeronew.data, avgCostPerMile > 0)
    new.data <- rbind(new.data, foundAvgCostPerMile)
    zeronew.data <- filter(zeronew.data,
                           avgCostPerMile == 0 | is.na(avgCostPerMile))
    
}

new.data <- as.data.table(new.data)

#clean up met_stat_areas
met_stat_areas <- met_stat_areas[met_stat_areas$AIRPORT_CODE != '' & !is.na(met_stat_areas$CBSA_CODE),]

#clean up crimestats
#add variables for violent crime and property crime
crimestats$ttl_vcrimert <- mutate(crimestats, ttl_vcrimert = (mmurd05 + mrape05 + massg05 +
                                  massa05)/(mpop05/100000)) %>%
    select(ttl_vcrimert)

crimestats$ttl_propcrimert <- mutate(crimestats, ttl_propcrimert =
                                         (mrobt05 + mrobg05 + mburg05 +
                                              mlarc05 + mmoto05)/(mpop05/100000)) %>%
    select(ttl_propcrimert)

crimestats <- select(crimestats, cbsa05, mpop05, mrmurd05, mrrape05, mrrobt05,
                     mrrobg05, mrassa05, mrassg05, mrburg05, mrlarc05, mrmoto05,
                     ttl_vcrimert, ttl_propcrimert)

## set variables
#income variables
keep.vars <- c('PRIM_STATE', 'AREA', 'AREA_NAME', 'OCC_CODE', 'OCC_TITLE',
               'TOT_EMP', 'H_MEAN', 'A_MEAN', 'H_MEDIAN', 'A_MEDIAN')

#occupations
keep.occ <- c('All Occupations',
              'Transportation and material moving occupations')

#rejected for lack of data
#'Air traffic controllers',
#'Airline pilots, copilots, and flight engineers',
#'Aircraft mechanics and service technicians', 
#'Avionics technicians',
#'Flight attendants'

occ.colNames <- c('all_occ', 'trans_occ')
#rejected for lack of data
#'air_cntrl', 'pilots', 'mechs','avionics', 'attend'

#reduce variables, combine income files and limit to only all occupation rows
income1 <- select(income1, keep.vars)
income2 <- select(income2, keep.vars)
income3 <- select(income3, keep.vars)
income4 <- select(income4, keep.vars)

income <- rbind(income1, income2, income3, income4)
income <- filter(income, income$OCC_TITLE %in% keep.occ)

#change CBSA codes in income table to match CBSA codes in met_stat_areas table
income$AREA[income$AREA == 71650] <- 14460
income$AREA[income$AREA == 77200] <- 39300
income$AREA[income$AREA == 74950] <- 31700

#remove blank records in met_stat_areas and change CBSA codes to match income table
met_stat_areas <- met_stat_areas[met_stat_areas$AIRPORT_CODE!='']
met_stat_areas$CBSA_CODE[met_stat_areas$CBSA_CODE == 31080] <- 31100
met_stat_areas$CBSA_CODE[met_stat_areas$CBSA_CODE == 46520] <- 26180

#put origin and destination airport codes into new.data
new.data$origin_cbsa_code <- left_join(new.data, met_stat_areas,
                                        by = c('Origin' = 'AIRPORT_CODE')) %>%
                            select(CBSA_CODE)
new.data$dest_cbsa_code <- left_join(new.data, met_stat_areas,
                                      by = c('Destination' = 'AIRPORT_CODE')) %>%
                            select(CBSA_CODE)

#remove bookings to or from foreign countries, and bookings with no passengers
remove.codes <- c('AMS', 'ATH', 'BOM', 'CDG', 'DEL', 'FCO', 'FRA', 'LGW', 'LHR',
                  'THR', 'TLV', 'WAW', 'OGG')
new.data <- filter(new.data, !(Origin %in% remove.codes))
new.data <- filter(new.data, !(Destination %in% remove.codes))

#add mean and median income data for selected occupations by origin and dest
#airport
for (i in seq_along(occ.colNames)) {
    org.avg <- paste('org_avg_inc_', occ.colNames[i], sep='')
    dest.avg <- paste('dest_avg_inc_', occ.colNames[i], sep='')
    org.med <- paste('org_med_inc_', occ.colNames[i], sep='')
    dest.med <- paste('dest_med_inc_', occ.colNames[i], sep='')

    new.data[,eval(org.avg)] <- left_join(new.data,
                                           income[income$OCC_TITLE==keep.occ[i],],
                                           by = c('origin_cbsa_code' = 'AREA')) %>%
                                    select(A_MEAN)

    new.data[,eval(dest.avg)] <- left_join(new.data,
                                            income[income$OCC_TITLE==keep.occ[i],],
                                            by = c('dest_cbsa_code' = 'AREA')) %>%
                                    select(A_MEAN)

    new.data[,eval(org.med)] <- left_join(new.data,
                                           income[income$OCC_TITLE==keep.occ[i],],
                                           by = c('origin_cbsa_code' = 'AREA')) %>%
                                    select(A_MEDIAN)

    new.data[,eval(dest.med)] <- left_join(new.data,
                                            income[income$OCC_TITLE==keep.occ[i],],
                                            by = c('dest_cbsa_code' = 'AREA')) %>%
                                    select(A_MEDIAN)
}

#add crime statistics to new.data
crime_cols <- colnames(crimestats)[-1]
for (i in seq_along(crime_cols)) {
    org <- paste('org_', crime_cols[i], sep='')
    dest <- paste('dest_', crime_cols[i], sep='')
    
    new.data[,eval(org)] <- left_join(new.data, crimestats,
                                       by = c('origin_cbsa_code' = 'cbsa05')) %>%
        select(eval(crime_cols)[i])
    
    new.data[,eval(dest)] <- left_join(new.data,crimestats,
                                        by = c('dest_cbsa_code' = 'cbsa05')) %>%
        select(eval(crime_cols)[i])
}

#cleanup
rm(list= ls()[!(ls() %in% c('new.data'))])

#Variable Analysis ####
#add a weekday variable - 1=Sunday
new.data$wday <- wday(new.data$DepartureDate)

#remove variables not needed in any model
new.data <- select(new.data, -DepartureDate, -origin_cbsa_code, -dest_cbsa_code)

#save data without dummies

saveRDS(new.data, file = 'newdata.rds')
set.seed(200)
train = sample(nrow(new.data),0.6*nrow(new.data))
train.data <- new.data[train,]
test.data <- new.data[-train,]

saveRDS(train.data, file = 'traindatanodummies.rds')
saveRDS(test.data, file = 'testdatanodummies.rds')

#create dummy variables for categorical variables
charCols <- colnames(new.data[,sapply(new.data, is.character)])
for (i in charCols) {
    for(level in unique(new.data[eval(i)])){
        new.data[paste("dummy", level, sep = "_")] <- ifelse(new.data[eval(i)] == level, 1, 0)
    }
}

#remove the categorical variables
new.data <- select(new.data, -c(1:4))

#save model datasets
train.data <- new.data[train,]
test.data <- new.data[-train,]

saveRDS(train.data, file = 'traindata.rds')
saveRDS(test.data, file = 'testdata.rds')
