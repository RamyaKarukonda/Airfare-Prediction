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
book_data <- readRDS('newdata.rds')

#variable analysis - Airports ####
byOrigin <- book_data %>%
    group_by(Origin) %>%
    summarise(avgCostPerMile = mean(avgCostPerMile)) %>%
    ungroup()
summary(byOrigin)
sd(byOrigin$avgCostPerMile)

ggplot(data = byOrigin, aes(byOrigin$avgCostPerMile)) +
    geom_histogram(breaks = seq(0, .3, by = .05),
                   col = 'blue',
                   fill = 'blue',
                   alpha = .2) +
    labs(title = "Origin City Avg. CPM") +
    labs(x = 'Avg Cost Per Mile', y = 'Count')

byDest <- book_data %>%
    group_by(Destination) %>%
    summarise(avgCostPerMile = mean(avgCostPerMile)) %>%
    ungroup()
summary(byDest)
sd(byDest$avgCostPerMile)

ggplot(data = byDest, aes(byDest$avgCostPerMile)) +
    geom_histogram(breaks = seq(0, .6, by = .05),
                   col = 'blue',
                   fill = 'blue',
                   alpha = .2) +
    labs(title = "Destination City Avg. CPM") +
    labs(x = 'Avg Cost Per Mile', y = 'Count')

cityPairs <- book_data %>%
    group_by(Origin, Destination) %>%
    summarise(avgCostPerMile = mean(avgCostPerMile)) %>%
    ungroup()
summary(cityPairs)
sd(cityPairs$avgCostPerMile)

ggplot(data = cityPairs, aes(cityPairs$avgCostPerMile)) +
    geom_histogram(breaks = seq(0, 3, by = .05),
                   col = 'blue',
                   fill = 'blue',
                   alpha = .2) +
    labs(title = "City Pair Avg. CPM") +
    labs(x = 'Avg Cost Per Mile', y = 'Count')

# variable analysis - Channel
byChannel <- book_data %>%
    group_by(Channel) %>%
    summarise(avgCostPerMile = mean(avgCostPerMile)) %>%
    ungroup()
summary(byChannel)
sd(byChannel$avgCostPerMile)

ggplot(data = byChannel, aes(byChannel$avgCostPerMile)) +
    geom_histogram(breaks = seq(0, .25, by = .05),
                   col = 'blue',
                   fill = 'blue',
                   alpha = .2) +
    labs(title = "Channel Avg. CPM") +
    labs(x = 'Avg Cost Per Mile', y = 'Count')

# variable analysis - InvClass
byInvClass <- book_data %>%
    group_by(InvClass) %>%
    summarise(avgCostPerMile = mean(avgCostPerMile)) %>%
    ungroup()
summary(byInvClass)
sd(byInvClass$avgCostPerMile)

ggplot(data = byInvClass, aes(byInvClass$avgCostPerMile)) +
    geom_histogram(breaks = seq(0, .8, by = .1),
                   col = 'blue',
                   fill = 'blue',
                   alpha = .2) +
    labs(title = "Inventory Class CPM") +
    labs(x = 'Avg Cost Per Mile', y = 'Count')

# variable analysis - avgLag
byavgLag <- book_data %>%
    group_by(avgLag) %>%
    summarise(avgCostPerMile = mean(avgCostPerMile)) %>%
    ungroup()
summary(byavgLag)
sd(byavgLag$avgCostPerMile)

binLag <- byavgLag %>%
    mutate(LagBin=cut(byavgLag$avgLag,
                        breaks=c(-Inf, 0.9, 1, 7, 14, 21, 28, 56, 84, 112, 140,
                                 Inf),
                        labels=c("SameDay","PriorDay","OneWeek", "TwoWeeks",
                                 "ThreeWeeks", "FourWeeks", "TwoMonths",
                                 "ThreeMonths", "FourMonths", "FiveMonths",
                                 "OverFiveMonths"))) %>%
    select(avgCostPerMile, LagBin)

ggplot(data = binLag, aes(LagBin)) +
    geom_bar(
                   col = 'blue',
                   fill = 'blue',
                   alpha = .2) +
    labs(title = "Avg Lag (days) CPM") +
    labs(x = 'Avg Cost Per Mile', y = 'Count')

# avgLag Binned
binLagBD <- book_data %>%
    mutate(LagBin=cut(book_data$avgLag,
                      breaks=c(-Inf, 0.9, 1, 7, 14, 21, 28, 56, 84, 112, 140,
                               Inf),
                      labels=c("SameDay","PriorDay","OneWeek", "TwoWeeks",
                               "ThreeWeeks", "FourWeeks", "TwoMonths",
                               "ThreeMonths", "FourMonths", "FiveMonths",
                               "OverFiveMonths")))

byavgBinLag <- binLagBD %>%
    group_by(LagBin) %>%
    summarise(avgCostPerMile = mean(avgCostPerMile)) %>%
    ungroup()
summary(byavgBinLag)
sd(byavgBinLag$avgCostPerMile)

ggplot(data = byavgBinLag, aes(LagBin,avgCostPerMile)) +
    geom_bar(stat = 'identity',
             col = 'blue',
             fill = 'blue',
             alpha = .2) +
    labs(title = "CPM by Days in Advance Purchased") +
    labs(x = 'Lag', y = 'Avg. Cost Per Mile')

