# Import data collected from Google Forms survey.
# Data translated by Emad Dawaas. Code written by Karen Dyson

## ----- Start up ------------------------------------

data.questions <-
  read.csv(
    "../Data_allparts_english.csv",
    header = TRUE,
    stringsAsFactors = FALSE,
    strip.white = TRUE
  )
data <- data.questions[ -1, ]
#str(data)

library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(likert)

ggColors <- c("#a6cee3", "#1f78b4")

## Global variables
options(scipen = 999)    

likertLevels <- c("Much less",
                  "Less often",
                  "About the same",
                  "More often",
                  "Much more often")

minVal <- 25

grouping <- data.frame(
  Less = c("Much less", "Less often"),
  More = c("More often", "Much more often")
)

## Demographic summaries

table(data$gender)

table(data$maritalStatus)

table(data$age)

table(data$housing)

table(data$townType)

table(data$governorate)

table(data$employment)

table(data$workLocation)

table(data$education)

table(data$beforeIncome)

table(data$afterIncome)

table(data$beforeIncome, data$afterIncome)
table(data$age, data$gender)


## Filtering

data <- data[which(rowSums(data[2:12] == "") < 6), ]
data <- data[which(data$age != "Less than 18"), ]
data$education[data$education %in% c("Master", "PhD or more")] <- "Masters or More"
data$afterIncome[data$afterIncome == "Dramatically increased"] <- "Increased"



## ----- Functions --------------------------------------
# 
# likertTable = activityGreenspaceLikert
# resultsTable = activityGreenspaceResults
# questionColumns = 2:5
# demographicColumns = 6:16

addChiSquared <- function(likertTable, resultsTable, questionColumns, demographicColumns, minVal = 25) {
  i = min(demographicColumns) # row
  j = min(questionColumns) # column
  
  a = 1 # the first row in the resultsTable that should have results assigned.
  c = 4 # first column that should have corrected pvalues
  
  # Iterate through each demographic variable
  for (i in min(demographicColumns):max(demographicColumns)) {
    
    b = 2 # the first column in the resultsTable that should have results assigned to it
    
    for (j in min(questionColumns):max(questionColumns)) {
      
      # Create a temp table and filter out blanks.
      tempTable <- likertTable[ , c(j,i)]
      tempTable <- tempTable[ tempTable[2] != "" & !is.na(tempTable[1]) , ]

      # Check to see if any group has less than min responses.
      testSize <- tempTable %>% count(tempTable[1:2]) %>% group_by(across(.cols = 2)) %>% summarise(n = sum(n))
      
      tooSmall <- testSize[testSize[2] < minVal, 1]
      print(unlist(tooSmall))
      # print(colnames(tempTable)) # use to debug
      
      # Based on this, either assign a "can't be tested"/NA indicator or the values for the chi-squared test.
      if (length(setdiff(unique(tempTable[, 2]), tooSmall)) < 2) {
        resultsTable[a, b] <- NA
        resultsTable[a, b + 1] <- NA
        
      } else {
        tempTable <- tempTable[!(tempTable[, 2] %in% tooSmall), ]
        
        # perform the chi squared test
        tempChi <- chisq.test(tempTable[, 1], tempTable[, 2])
        
        # add statistic value and pvalue to the table.
        resultsTable[a, b] <- tempChi$statistic
        resultsTable[a, b + 1] <- tempChi$p.value
        
      } # end of else
      
      # Use this if you want to export a Pivot table
      # pivot <- pivot_wider(testSize,
      #                         names_from = colnames(testSize[1]),
      #                         values_from = n)
      
      # Clean up
      remove(testSize, tooSmall)
      
      # add 2 so next results go in the proper place
      b = b + 3
    } # end question loop
    
    a = a + 1 # go to next row for next set of demographic info
    
  } # end demographic loop
  
  # add corrected pvalue
  
  z = 1
  for (z in 1:length(questionColumns)) {
    
    tempCorrected <- p.adjust(as.vector(unlist(resultsTable[ , c - 1])), method = "holm")
    
    resultsTable[ , c] <- tempCorrected
    
    c = c + 3
    
  } # end addition of corrected pvalues
  
  return(resultsTable)
  
} # end function


# questionColumn <- 3
# demographicColumn <- 8

# This function needs to compare between categories within a question to see which categories are significantly different.  
posthocChiSquared <- function(likertTable, questionColumn, demographicColumn, minVal = 25, correction = TRUE) {
  tempTable <- likertTable[ , c(questionColumn, demographicColumn)]
  tempTable <- tempTable[ tempTable[2] != "" & !is.na(tempTable[1]) , ]

  # Check to see if any group has less than min allowed responses.
  testSize <- tempTable %>% count(tempTable[1:2]) %>% group_by(across(.cols = 2)) %>% summarise(n = sum(n))
  
  tooSmall <- testSize[testSize[2] < minVal, 1]
  print(unlist(tooSmall))

  # Filter out too small categories
  tempTable <- tempTable[!(tempTable[, 2] %in% tooSmall), ]
  # and make sure demographic is a factor (this gets lost sometimes)
  if (is.factor(tempTable[,2]) == FALSE) {tempTable[,2] <- as.factor(tempTable[,2])}
  
  # Make sure that the number of unique is more than two
  if (nlevels(tempTable[,2]) < 3) {stop(print("Too few levels"))}
  
  numLevels <- nlevels(tempTable[,2])
  
  # create the results table
    #create matrix with correct number of columns
    resultsTable <- matrix(rep(999, times = numLevels^2),
                           ncol = numLevels, byrow = TRUE)
    
    #define column names and row names of matrix
    tempLevels <- levels(tempTable[,2])
    colnames(resultsTable) <- tempLevels
    rownames(resultsTable) <- tempLevels
  
  # for each [i,j] pair of factors add the pvalue for chisquared test
  i = 1 # row
  j = 1 # column
  for (i in 1:numLevels) {
    for (j in 1:numLevels) {
      if (i != j) {
        # subset for i and j levels
        testTable <-
          tempTable[tempTable[, 2] %in% tempLevels[c(i, j)] ,]
        
        # run test and assign pvalue to i,j spot
        resultsTable[i, j] <-
          chisq.test(testTable[, 1], testTable[, 2])$p.value
        
      } else {
        resultsTable[i, j] <- NA
      }
      
    } # end column loop
    
  } # end row loop
  
  # remove the lower triangle--can probably make this a filter but this is easy
  resultsTable[lower.tri(resultsTable, diag = FALSE)] <- NA
  
  # correct pvalues
  if (correction == TRUE) {
    resultsTable <-
      matrix(
        p.adjust(as.vector(resultsTable), method = 'holm'),
        ncol = numLevels,
        dimnames = list(tempLevels, tempLevels)
      )
  }
  #convert matrix to a tibble
  resultsTable <- as_tibble(resultsTable, rownames = "levels")
  
  return(resultsTable)
  
} # end function
  
  



## ------ Gardens ------------------------------------

gardens <- data[, which(grepl("ID|Garden", colnames(data)))]

table(gardens$haveGarden)
# The two non-responses do have gardnes (responded to other questions in series)

gardens <- gardens[ gardens$haveGarden != "No" , ]
table(gardens$haveGarden)

table(gardens$timeInGardenYN)

# of the non-responses 6 answered like "No" and 2 as "Yes".
gardens$timeInGardenYN[gardens$timeInGardenYN == "" & gardens$whyNoTimeInGarden != ""] <- "No"
gardens$timeInGardenYN[gardens$timeInGardenYN == "" & gardens$whyNoTimeInGarden == ""] <- "Yes"

table(gardens$whyNoTimeInGarden[gardens$timeInGardenYN == "No"])

gardens <- gardens[gardens$timeInGardenYN == "Yes", ]

## ----- Gardens: With whom ----------------------------------------

# Who do respondents spend time with in their gardens?
# Compare number before vs. after.

inGardens <-
  gardens[, which(grepl("ID|alone|family|kids|neighbors|friends", colnames(gardens)))] %>%
  pivot_longer(cols = -participantID) %>%
  separate_rows(value, sep = ", ") %>% 
  filter(value != "") 

inGardens$value <- factor(inGardens$value, levels = c("Before the pandemic", "After the pandemic occurred"))

inGardensTable <- count(inGardens, name, value) %>%
  pivot_wider(names_from = name, values_from = n, values_fill = list(n = 0))

# Using apply to fill these in is clumsy based on how chisq.test outputs the information. So, just assign.
inGardensTable <- inGardensTable %>%
  add_row(
    value = "X-squared",
    aloneInGarden = chisq.test(inGardensTable[1:2, 2])$statistic,
    familyInGarden = chisq.test(inGardensTable[1:2, 3])$statistic,
    friendsInGarden = chisq.test(inGardensTable[1:2, 4])$statistic,
    kidsInGarden = chisq.test(inGardensTable[1:2, 5])$statistic,
    neighborsInGarden = chisq.test(inGardensTable[1:2, 6])$statistic
  ) %>%
  add_row(
    value = "p-value",
    aloneInGarden = chisq.test(inGardensTable[1:2, 2])$p.value,
    familyInGarden = chisq.test(inGardensTable[1:2, 3])$p.value,
    friendsInGarden = chisq.test(inGardensTable[1:2, 4])$p.value,
    kidsInGarden = chisq.test(inGardensTable[1:2, 5])$p.value,
    neighborsInGarden = chisq.test(inGardensTable[1:2, 6])$p.value
  ) 

temp <- p.adjust(inGardensTable[ 4, -1])

inGardensTable <- inGardensTable %>%
  add_row(
    value = "Corrected p-value",
    aloneInGarden = temp[1],
    familyInGarden = temp[2],
    friendsInGarden = temp[3],
    kidsInGarden = temp[4],
    neighborsInGarden = temp[5]
  )

ggplot(inGardens) + 
  geom_bar(aes(x = name, fill = value), position = position_dodge2()) +
  labs(x = "", y = "Count", title = "Who do respondents spend time with in their garden?") +
  scale_x_discrete(labels = c("Alone", "With Family", "With Friends", "With Children", "With Neighbors")) +
  scale_fill_manual(values = ggColors, name = "") +
  theme(legend.position = "bottom") +
  geom_segment(aes(x = 2.75, y = 281+20, xend = 3.25, yend = 281+20)) +
  annotate(geom = "text", x = 3, y = 281+25, label = "***") +
  geom_segment(aes(x = 3.75, y = 295, xend = 4.25, yend = 295)) +
  annotate(geom = "text", x = 4, y = 300, label = "*") +
  geom_segment(aes(x = 4.75, y = 228+20, xend = 5.25, yend = 228+20)) +
  annotate(geom = "text", x = 5, y = 228+25, label = "***")
  
ggsave(filename = "graphs/inGardensWho.jpeg", device = "jpeg",
       units = "in", width = 5.5, height = 4)


## ----- Gardens: Activities -------------------------------------

## Comparing activities in gardens--Likert scale

activityGardens <-
  gardens[, which(grepl("ID|after", colnames(gardens)))] %>%
  pivot_longer(cols = -participantID) %>%
  filter( value != "") %>%
  filter( value != "I don't do this activity")
  
activityGardens$value <- factor(activityGardens$value, levels = likertLevels)

activityGardensTable <- count(activityGardens, name, value) %>%
  pivot_wider(names_from = name, values_from = n, values_fill = list(n = 0))

ggplot(activityGardens) + 
  geom_bar(aes(x = name, fill = value))


activityGardens <-
  pivot_wider(activityGardens, names_from = name, values_from = value)

# I don't know why likert function doesn't like tibbles, but it doesn't. Here's the workaround.
activityGardensLikert <- as.data.frame(lapply(activityGardens[-1],
                                              function(x) {
                                                factor(x, levels = likertLevels)
                                              }))

names(activityGardensLikert) <- c(
  "After the COVID-19 pandemic, you use your home garden for relaxing, picnicking, socializing and gathering with family and friends:",
  "After the COVID-19 pandemic, you use your home garden for exercising, sporting and walking:",
  "After the COVID-19 pandemic, you plant vegetables, legumes plants and crops in your home garden:",
  "After the COVID-19 pandemic, you plant and take care of ornamental trees and plants in your home garden:",
  "After the COVID-19 pandemic, you feed birds, birdwatch and do photography in your home garden:",
  "After the COVID-19 pandemic, you search for nests and hunt birds in your home garden:",
  "After the COVID-19 pandemic, you plant and take care of olive trees and other fruiting trees in your home garden:",
  "After the COVID-19 pandemic, you landscape, maintain retaining walls, cut the lawn and plow in your home garden:",
  "After the COVID-19 pandemic, you take care of livestock and domestic birds in your home garden:"
)

activityGardensLikert <- activityGardensLikert[, c(1,2,5,3,4,7,8,6,9)]

activityGardensLikert <- cbind(activityGardens[1], activityGardensLikert)
activityGardensLikert <- left_join(activityGardensLikert, data[1:12], by = "participantID")


likert::likert(
  activityGardensLikert[2:10]
)


likert.bar.plot(
  likert(activityGardensLikert[2:10]),
  wrap = 65,
  neutral.color = "white"
)

# Passive appreciation
likert.bar.plot(
  likert(activityGardensLikert[, c(2:4)]),
  wrap = 50,
  neutral.color = "white"
)

# Active cultivation
likert.bar.plot(
  likert(activityGardensLikert[, c(5:8)]),
  wrap = 50,
  neutral.color = "white"
)

# Extractive or harmful activities
likert.bar.plot(
  likert(activityGardensLikert[, c(9:10)]),
  wrap = 50,
  neutral.color = "white"
)

## ----- Gardens: Activities & Less/More tests ---------------------------------

## goal: to create a table with counts and chi squared test of "less oftens" vs "more
## oftens". Want similar output to e.g. the 'with whom' tests and graphs

## Build a table that has summed "more" and "less" for each question.

activityGardensLessMore <- activityGardens %>%
  pivot_longer(cols = -participantID) %>%
  filter(value != "") %>%
  filter(value != "About the same")

activityGardensLessMore$value[activityGardensLessMore$value == "Much less"] <-
  "Less often"
activityGardensLessMore$value[activityGardensLessMore$value == "Much more often"] <-
  "More often"

activityGardensLMGraph <- activityGardensLessMore

activityGardensLessMore$value <-
  factor(activityGardensLessMore$value,
         levels = c("Less often", "More often"))

activityGardensLessMore <-
  count(activityGardensLessMore, name, value) %>%
  pivot_wider(
    names_from = name,
    values_from = n,
    values_fill = list(n = 0)
  )


# Using apply to fill these in is clumsy based on how chisq.test outputs the
# information. So, just assign.
activityGardensLessMore <- activityGardensLessMore %>%
  add_row(
    value = "X-squared",
    afterGardenBirdPhotography = chisq.test(activityGardensLessMore[1:2, 2])$statistic,
    afterGardenExercising = chisq.test(activityGardensLessMore[1:2, 3])$statistic,
    afterGardenGardening = chisq.test(activityGardensLessMore[1:2, 4])$statistic,
    afterGardenHarvesting = chisq.test(activityGardensLessMore[1:2, 5])$statistic,
    afterGardenLandscaping = chisq.test(activityGardensLessMore[1:2, 6])$statistic,
    afterGardenLivestock = chisq.test(activityGardensLessMore[1:2, 7])$statistic,
    afterGardenOrchard = chisq.test(activityGardensLessMore[1:2, 8])$statistic,
    afterGardenOrnamentals = chisq.test(activityGardensLessMore[1:2, 9])$statistic,
    afterGardenRelaxing = chisq.test(activityGardensLessMore[1:2, 10])$statistic
  ) %>%
  add_row(
    value = "p-value",
    afterGardenBirdPhotography = chisq.test(activityGardensLessMore[1:2, 2])$p.value,
    afterGardenExercising = chisq.test(activityGardensLessMore[1:2, 3])$p.value,
    afterGardenGardening = chisq.test(activityGardensLessMore[1:2, 4])$p.value,
    afterGardenHarvesting = chisq.test(activityGardensLessMore[1:2, 5])$p.value,
    afterGardenLandscaping = chisq.test(activityGardensLessMore[1:2, 6])$p.value,
    afterGardenLivestock = chisq.test(activityGardensLessMore[1:2, 7])$p.value,
    afterGardenOrchard = chisq.test(activityGardensLessMore[1:2, 8])$p.value,
    afterGardenOrnamentals = chisq.test(activityGardensLessMore[1:2, 9])$p.value,
    afterGardenRelaxing = chisq.test(activityGardensLessMore[1:2, 10])$p.value
  ) 

temp <- p.adjust(activityGardensLessMore[ 4, -1])

activityGardensLessMore <- activityGardensLessMore %>%
  add_row(
    value = "Corrected p-value",
    afterGardenBirdPhotography = temp[1],
    afterGardenExercising = temp[2],
    afterGardenGardening = temp[3],
    afterGardenHarvesting = temp[4],
    afterGardenLandscaping = temp[5],
    afterGardenLivestock = temp[6],
    afterGardenOrchard = temp[7],
    afterGardenOrnamentals = temp[8],
    afterGardenRelaxing = temp[9]
  )

activityGardensLessMore[ 1:2, -1] <- round(activityGardensLessMore[ 1:2, -1], 0)
activityGardensLessMore[ 3, -1] <- round(activityGardensLessMore[ 3, -1], 3)


activityGardensLMGraph$name <-
  factor(
    activityGardensLMGraph$name, 
    levels = c(
      "afterGardenRelaxing",
      "afterGardenExercising",
      "afterGardenBirdPhotography",
      "afterGardenGardening",
      "afterGardenOrnamentals",
      "afterGardenOrchard",
      "afterGardenLandscaping",
      "afterGardenHarvesting",
      "afterGardenLivestock"
    )
  )

ggplot(activityGardensLMGraph) + 
  geom_bar(aes(x = name, fill = value), position = position_dodge2()) +
  labs(x = "", y = "Count", title = "Do respondents perform activities in gardens \nless or more often after the COVID-19 pandemic?") +
  scale_x_discrete(labels = c(
      "Relaxing",
      "Exercising",
      "Birds and \nPhotography",
      "Tending \nVegetables",
      "Tending \nOrnamentals",
      "Tending \nOrchards",
      "Landscaping",
      "Nest/Bird \nHunting",
      "Tending \nLivestock"
    
  )) +
  scale_fill_manual(values = ggColors, name = "") +
  theme(legend.position = "bottom") +
  geom_segment(aes(x = 0.75, y = 304+20, xend = 1.25, yend = 304+20)) +
  annotate(geom = "text", x = 1, y = 304+25, label = "***") +
  geom_segment(aes(x = 1.75, y = 231+20, xend = 2.25, yend = 231+20)) +
  annotate(geom = "text", x = 2, y = 231+25, label = "***") +
  geom_segment(aes(x = 2.75, y = 246+20, xend = 3.25, yend = 246+20)) +
  annotate(geom = "text", x = 3, y = 246+25, label = "***") +
  geom_segment(aes(x = 3.75, y = 280+20, xend = 4.25, yend = 280+20)) +
  annotate(geom = "text", x = 4, y = 280+25, label = "***") +
  geom_segment(aes(x = 4.75, y = 309+20, xend = 5.25, yend = 309+20)) +
  annotate(geom = "text", x = 5, y = 309+25, label = "***") +
  geom_segment(aes(x = 5.75, y = 225+20, xend = 6.25, yend = 225+20)) +
  annotate(geom = "text", x = 6, y = 225+25, label = "***") +
  geom_segment(aes(x = 6.75, y = 302+20, xend = 7.25, yend = 302+20)) +
  annotate(geom = "text", x = 7, y = 302+25, label = "***") +
  geom_segment(aes(x = 8.75, y = 117+20, xend = 9.25, yend = 117+20)) +
  annotate(geom = "text", x = 9, y = 117+25, label = "***")

ggsave(filename = "../graphs/activityGardens.jpeg", device = "jpeg",
       units = "in", width = 8, height = 4)




## ----- Gardens: Activities & Demographic tests -------------------------------------

# Create Tibble for results.

activityGardensResults <-
  tibble(
    demographicVariable = c(
      "Gender",
      "Marital Status",
      "Age",
      "Housing",
      "Town Type",
      "Governorate",
      "Employment",
      "Work Location",
      "Education",
      "Before Income",
      "After Income"
    ),
    afterGardensRelaxing_CHI = rep(0.0, length(demographicVariable)),
    afterGardensRelaxing_PVAL = rep(0.0, length(demographicVariable)),
    afterGardensRelaxing_CORPVAL = rep(0.0, length(demographicVariable)),
    afterGardensExercise_CHI = rep(0.0, length(demographicVariable)),
    afterGardensExercise_PVAL = rep(0.0, length(demographicVariable)),
    afterGardensExercise_CORPVAL = rep(0.0, length(demographicVariable)),
    afterGardensBirdPhotography_CHI = rep(0.0, length(demographicVariable)),
    afterGardensBirdPhotography_PVAL = rep(0.0, length(demographicVariable)),
    afterGardensBirdPhotography_CORPVAL = rep(0.0, length(demographicVariable)),
    
    afterGardensGardening_CHI = rep(0.0, length(demographicVariable)),
    afterGardensGardening_PVAL = rep(0.0, length(demographicVariable)),
    afterGardensGardening_CORPVAL = rep(0.0, length(demographicVariable)),
    afterGardensOrnamentals_CHI = rep(0.0, length(demographicVariable)),
    afterGardensOrnamentals_PVAL = rep(0.0, length(demographicVariable)),
    afterGardensOrnamentals_CORPVAL = rep(0.0, length(demographicVariable)),
    afterGardensOrchard_CHI = rep(0.0, length(demographicVariable)),
    afterGardensOrchard_PVAL = rep(0.0, length(demographicVariable)),
    afterGardensOrchard_CORPVAL = rep(0.0, length(demographicVariable)),
    afterGardensLandscaping_CHI = rep(0.0, length(demographicVariable)),
    afterGardensLandscaping_PVAL = rep(0.0, length(demographicVariable)),
    afterGardensLandscaping_CORPVAL = rep(0.0, length(demographicVariable)),
    
    afterGardensHarvesting_CHI = rep(0.0, length(demographicVariable)),
    afterGardensHarvesting_PVAL = rep(0.0, length(demographicVariable)),
    afterGardensHarvesting_CORPVAL = rep(0.0, length(demographicVariable)),
    afterGardensLivestock_CHI = rep(0.0, length(demographicVariable)),
    afterGardensLivestock_PVAL = rep(0.0, length(demographicVariable)),
    afterGardensLivestock_CORPVAL = rep(0.0, length(demographicVariable))
  )




## Create table of ChiSq tests

activityGardensResultsChiSq <-
  addChiSquared(
    activityGardensLikert,
    activityGardensResults,
    questionColumns = 2:10,
    demographicColumns = 11:ncol(activityGardensLikert),
    minVal = minVal
  )




## Graph different likert plots that are significant.
  likert.bar.plot(
    l = likert::likert(
      activityGardensLikert[7],
      grouping = as.factor(activityGardensLikert$education)
    ),
    group.order = c("Highschool or Less", "Diploma", "Bachelor", "Masters or More")
  )
  # appears to be the Diploma category.

activityGardensLikert[,19] <- factor(activityGardensLikert[,19], levels = c("Highschool or Less", "Diploma", "Bachelor", "Masters or More"))

posthocChiSquared(likertTable = activityGardensLikert,
                  questionColumn = 7,
                  demographicColumn = 19
                  )
  
  remove(activityGardensResults)


## ---- Urban Parks -----------------------------------

greenspace <- data[, which(grepl("ID|Greenspace", colnames(data)))]

table(greenspace$visitGreenspace)
# The one non-response is a "No"

table(greenspace$whyNoVisitGreenspace)

greenspace <- greenspace[greenspace$visitGreenspace == "Yes", ]

table(greenspace$visitGreenspace)

## ------ Urban Parks: With whom -------------------------

## Now look at with whom people spend time in urban parks .
# Compare number before vs. after.

inGreenspace <-
  greenspace[, which(grepl("ID|alone|family|kids|neighbors|friends", colnames(greenspace)))] %>%
  pivot_longer(cols = -participantID) %>%
  separate_rows(value, sep = ", ") %>% 
  filter(value != "") 

inGreenspace$value <- factor(inGreenspace$value, levels = c("Before the pandemic", "After the pandemic occurred"))

inGreenspaceTable <- count(inGreenspace, name, value) %>%
  pivot_wider(names_from = name, values_from = n, values_fill = list(n = 0))

# Assign values from Chisq
inGreenspaceTable <- inGreenspaceTable %>%
  add_row(
    value = "X-squared",
    aloneGreenspace = chisq.test(inGreenspaceTable[1:2, 2])$statistic,
    familyGreenspace = chisq.test(inGreenspaceTable[1:2, 3])$statistic,
    friendsGreenspace = chisq.test(inGreenspaceTable[1:2, 4])$statistic,
    kidsGreenspace = chisq.test(inGreenspaceTable[1:2, 5])$statistic,
    neighborsGreenspace = chisq.test(inGreenspaceTable[1:2, 6])$statistic
  ) %>%
  add_row(
    value = "p-value",
    aloneGreenspace = chisq.test(inGreenspaceTable[1:2, 2])$p.value,
    familyGreenspace = chisq.test(inGreenspaceTable[1:2, 3])$p.value,
    friendsGreenspace = chisq.test(inGreenspaceTable[1:2, 4])$p.value,
    kidsGreenspace = chisq.test(inGreenspaceTable[1:2, 5])$p.value,
    neighborsGreenspace = chisq.test(inGreenspaceTable[1:2, 6])$p.value
  ) 

temp <- p.adjust(inGreenspaceTable[ 4, -1])

inGreenspaceTable <- inGreenspaceTable %>%
  add_row(
    value = "Corrected p-value",
    aloneGreenspace = temp[1],
    familyGreenspace = temp[2],
    friendsGreenspace = temp[3],
    kidsGreenspace = temp[4],
    neighborsGreenspace = temp[5]
  )

# line should be .5 x wide and 20 y units above the tallest bar; text is
# centered at whole number and 5 y units above the line.


ggplot(inGreenspace) + 
  geom_bar(aes(x = name, fill = value), position = position_dodge2()) +
  labs(x = "", y = "Count", title = "Who do respondents spend time with in urban parks?") +
  scale_x_discrete(labels = c("Alone", "With Family", "With Friends", "With Children", "With Neighbors")) +
  scale_fill_manual(values = ggColors, name = "") +
  theme(legend.position = "bottom") +
  geom_segment(aes(x = 0.75, y = 247+20, xend = 1.25, yend = 247+20)) +
  annotate(geom = "text", x = 1, y = 247+25, label = "***") +
  geom_segment(aes(x = 1.75, y = 538+20, xend = 2.25, yend = 538+20)) +
  annotate(geom = "text", x = 2, y = 538+25, label = "**") +
  geom_segment(aes(x = 2.75, y = 450+20, xend = 3.25, yend = 450+20)) +
  annotate(geom = "text", x = 3, y = 450+25, label = "***") +
  geom_segment(aes(x = 3.75, y = 299+20, xend = 4.25, yend = 299+20)) +
  annotate(geom = "text", x = 4, y = 299+25, label = "**") +
  geom_segment(aes(x = 4.75, y = 220+20, xend = 5.25, yend = 220+20)) +
  annotate(geom = "text", x = 5, y = 220+25, label = "***")

ggsave(filename = "../graphs/inGreenspace.jpeg", device = "jpeg",
       units = "in", width = 5.5, height = 4)


## ----- Urban parks: Transportation -----------------------

## transportation to urban parks

transitGreenspace <- 
  greenspace[, which(grepl("ID|walking|bicycling|car|bus|taxi", colnames(greenspace)))] %>%
  pivot_longer(cols = -participantID) %>%
  separate_rows(value, sep = ", ") %>% 
  filter(value != "") 

transitGreenspace$value <- factor(transitGreenspace$value, levels = c("Before the pandemic", "After the pandemic occurred"))

transitGreenspaceTable <- count(transitGreenspace, name, value) %>%
  pivot_wider(names_from = name, values_from = n, values_fill = list(n = 0))


# Assign values from Chisq
transitGreenspaceTable <- transitGreenspaceTable %>%
  add_row(
    value = "X-squared",
    bicyclingGreenspace = chisq.test(transitGreenspaceTable[1:2, 2])$statistic,
    busGreenspace = chisq.test(transitGreenspaceTable[1:2, 3])$statistic,
    carGreenspace = chisq.test(transitGreenspaceTable[1:2, 4])$statistic,
    taxiGreenspace = chisq.test(transitGreenspaceTable[1:2, 5])$statistic,
    walkingGreenspace = chisq.test(transitGreenspaceTable[1:2, 6])$statistic
  ) %>%
  add_row(
    value = "p-value",
    bicyclingGreenspace = chisq.test(transitGreenspaceTable[1:2, 2])$p.value,
    busGreenspace = chisq.test(transitGreenspaceTable[1:2, 3])$p.value,
    carGreenspace = chisq.test(transitGreenspaceTable[1:2, 4])$p.value,
    taxiGreenspace = chisq.test(transitGreenspaceTable[1:2, 5])$p.value,
    walkingGreenspace = chisq.test(transitGreenspaceTable[1:2, 6])$p.value
  ) 

temp <- p.adjust(transitGreenspaceTable[ 4, -1])

transitGreenspaceTable <- transitGreenspaceTable %>%
  add_row(
    value = "Corrected p-value",
    bicyclingGreenspace = temp[1],
    busGreenspace = temp[2],
    carGreenspace = temp[3],
    taxiGreenspace = temp[4],
    walkingGreenspace = temp[5]
  )

# line should be .5 x wide and 20 y units above the tallest bar; text is
# centered at whole number and 5 y units above the line.


ggplot(transitGreenspace) + 
  geom_bar(aes(x = name, fill = value), position = position_dodge2()) +
  labs(x = "", y = "Count", title = "How do respondents travel to urban parks?") +
  scale_x_discrete(labels = c("By Bicycling", "By Bus", "By Car", "By Taxi", "By Walking")) +
  scale_fill_manual(values = ggColors, name = "") +
  theme(legend.position = "bottom") +
  geom_segment(aes(x = 1.75, y = 126+20, xend = 2.25, yend = 126+20)) +
  annotate(geom = "text", x = 2, y = 126+25, label = "***") +
  geom_segment(aes(x = 3.75, y = 162+20, xend = 4.25, yend = 162+20)) +
  annotate(geom = "text", x = 4, y = 162+25, label = "*")

ggsave(filename = "../graphs/transitGreenspace.jpeg", device = "jpeg",
       units = "in", width = 5.5, height = 4)


## ----- Urban parks: Travel distance --------------------------
## How far do respondents travel to urban parks?

distanceGreenspace <- 
  greenspace[, which(grepl("ID|distance", colnames(greenspace)))] %>%
  pivot_longer(cols = -participantID) %>%
  separate_rows(value, sep = ", ") %>% 
  filter(value != "") 

distanceGreenspace$value <- factor(distanceGreenspace$value, levels = c("Before the pandemic", "After the pandemic occurred"))

distanceGreenspaceTable <- count(distanceGreenspace, name, value) %>%
  pivot_wider(names_from = name, values_from = n, values_fill = list(n = 0))
distanceGreenspaceTable <- distanceGreenspaceTable[ , c(1,5,2,3,4)]

# Assign values from Chisq
distanceGreenspaceTable <- distanceGreenspaceTable %>%
  add_row(
    value = "X-squared",
    distanceGreenspace_under0.5 = chisq.test(distanceGreenspaceTable[1:2, 2])$statistic,
    distanceGreenspace_0.5to1 = chisq.test(distanceGreenspaceTable[1:2, 3])$statistic,
    distanceGreenspace_1to2 = chisq.test(distanceGreenspaceTable[1:2, 4])$statistic,
    distanceGreenspace_over2 = chisq.test(distanceGreenspaceTable[1:2, 5])$statistic
  ) %>%
  add_row(
    value = "p-value",
    distanceGreenspace_under0.5 = chisq.test(distanceGreenspaceTable[1:2, 2])$p.value,
    distanceGreenspace_0.5to1 = chisq.test(distanceGreenspaceTable[1:2, 3])$p.value,
    distanceGreenspace_1to2 = chisq.test(distanceGreenspaceTable[1:2, 4])$p.value,
    distanceGreenspace_over2 = chisq.test(distanceGreenspaceTable[1:2, 5])$p.value
  ) 

temp <- p.adjust(distanceGreenspaceTable[ 4, -1])

distanceGreenspaceTable <- distanceGreenspaceTable %>%
  add_row(
    value = "Corrected p-value",
    distanceGreenspace_under0.5 = temp[1],
    distanceGreenspace_0.5to1 = temp[2],
    distanceGreenspace_1to2 = temp[3],
    distanceGreenspace_over2 = temp[4]
  )

table(data$beforeIncome, data$distanceGreenspace_0.5to1)

## None are significant.


## ----- Urban parks: Activities ----------------------------

## Comparing activities in greenspace--Likert scale

activityGreenspace <-
  greenspace[, which(grepl("ID|after", colnames(greenspace)))] %>%
  pivot_longer(cols = -participantID) %>%
  filter(value != "") %>%
  filter(value != "I don't do this activity")


activityGreenspace$value <-
  factor(
    activityGreenspace$value,
    levels = likertLevels
  )

activityGreenspaceTable <- count(activityGreenspace, name, value) %>%
  pivot_wider(names_from = name, values_from = n, values_fill = list(n = 0))

ggplot(activityGreenspace) + 
  geom_bar(aes(x = name, fill = value))


activityGreenspace <-
  pivot_wider(activityGreenspace, names_from = name, values_from = value)

activityGreenspaceLikert <- as.data.frame(lapply(activityGreenspace[-1],
                                              function(x) {factor(x,levels=likertLevels, exclude=NA)}))

names(activityGreenspaceLikert) <- c(
  "After the COVID-19 pandemic, you use urban parks for exercising, sporting and walking:",
  "After the COVID-19 pandemic, you use urban parks for relaxing, picnicking, socializing and gathering with family and friends:",
  "After the COVID-19 pandemic, you feed birds, birdwatch and do photography in urban parks:",
  "After the COVID-19 pandemic, you search for nests and hunt birds in urban parks:"
)

activityGreenspaceLikert <- cbind(activityGreenspace[1], activityGreenspaceLikert)
activityGreenspaceLikert <- left_join(activityGreenspaceLikert, data[1:12], by = "participantID")

## Plot all questions

likert.bar.plot(
  likert(activityGreenspaceLikert[, c(2, 3, 4, 5)]),
  wrap = 50,
  neutral.color = "white"
)

## Plot groups of questions

# Passive appreciation
likert.bar.plot(
  likert(activityGreenspaceLikert[, c(2:4)]),
  wrap = 50,
  neutral.color = "white"
)

# Extractive or harmful activities
likert.bar.plot(
  likert(activityGreenspaceLikert[5]),
  wrap = 50,
  neutral.color = "white"
)



## ----- Urban parks: Activities & Less/More tests ---------------------------------

## goal: to create a table with counts and chi squared test of "less oftens" vs
## "more oftens". Want similar output to e.g. the 'with whom' tests and graphs

## Build a table that has summed "more" and "less" for each question.

activityGreenspaceLessMore <- activityGreenspace %>%
  pivot_longer(cols = -participantID) %>%
  filter(value != "") %>%
  filter(value != "About the same")

activityGreenspaceLessMore$value[activityGreenspaceLessMore$value == "Much less"] <-
  "Less often"
activityGreenspaceLessMore$value[activityGreenspaceLessMore$value == "Much more often"] <-
  "More often"

activityGreenspaceLessMore$value <-
  factor(activityGreenspaceLessMore$value,
         levels = c("Less often", "More often"))

activityGreenspaceLMGraph <- activityGreenspaceLessMore

activityGreenspaceLessMore <-
  count(activityGreenspaceLessMore, name, value) %>%
  pivot_wider(
    names_from = name,
    values_from = n,
    values_fill = list(n = 0)
  )


# Using apply to fill these in is clumsy based on how chisq.test outputs the
# information. So, just assign.
activityGreenspaceLessMore <- activityGreenspaceLessMore %>%
  add_row(
    value = "X-squared",
    afterGreenspaceBirdPhotography = chisq.test(activityGreenspaceLessMore[1:2, 2])$statistic,
    afterGreenspaceExercise = chisq.test(activityGreenspaceLessMore[1:2, 3])$statistic,
    afterGreenspaceHarvesting = chisq.test(activityGreenspaceLessMore[1:2, 4])$statistic,
    afterGreenspaceRelaxing = chisq.test(activityGreenspaceLessMore[1:2, 5])$statistic
  ) %>%
  add_row(
    value = "p-value",
    afterGreenspaceBirdPhotography = chisq.test(activityGreenspaceLessMore[1:2, 2])$p.value,
    afterGreenspaceExercise = chisq.test(activityGreenspaceLessMore[1:2, 3])$p.value,
    afterGreenspaceHarvesting = chisq.test(activityGreenspaceLessMore[1:2, 4])$p.value,
    afterGreenspaceRelaxing = chisq.test(activityGreenspaceLessMore[1:2, 5])$p.value
  ) 

temp <- p.adjust(activityGreenspaceLessMore[ 4, -1])

activityGreenspaceLessMore <- activityGreenspaceLessMore %>%
  add_row(
    value = "Corrected p-value",
    afterGreenspaceBirdPhotography = temp[1],
    afterGreenspaceExercise = temp[2],
    afterGreenspaceHarvesting = temp[3],
    afterGreenspaceRelaxing = temp[4]
  )


activityGreenspaceLMGraph$name <-
  factor(
    activityGreenspaceLMGraph$name, 
    levels = c(
      "afterGreenspaceRelaxing",
      "afterGreenspaceExercise",
      "afterGreenspaceBirdPhotography",
      "afterGreenspaceHarvesting"
    )
  )

ggplot(activityGreenspaceLMGraph) + 
  geom_bar(aes(x = name, fill = value), position = position_dodge2()) +
  labs(x = "", y = "Count", title = "Do respondents perform activities in urban parks \nless or more often after the COVID-19 pandemic?") +
  scale_x_discrete(labels = c(
    "Relaxing",
    "Exercising",
    "Birds and \nPhotography",
    "Nest/Bird \nHunting"
    
  )) +
  scale_fill_manual(values = ggColors, name = "") +
  theme(legend.position = "bottom") +
  geom_segment(aes(x = 0.75, y = 312+20, xend = 1.25, yend = 312+20)) +
  annotate(geom = "text", x = 1, y = 312+25, label = "***") +
  geom_segment(aes(x = 3.75, y = 66+20, xend = 4.25, yend = 66+20)) +
  annotate(geom = "text", x = 4, y = 66+25, label = "***")

ggsave(filename = "../graphs/activityGreenspace.jpeg", device = "jpeg",
       units = "in", width = 5, height = 4)






## ----- Urban parks: Activities & Demographic tests -------------------------------------

# Create Tibble for results.

activityGreenspaceResults <-
  tibble(
    demographicVariable = c(
      "Gender",
      "Marital Status",
      "Age",
      "Housing",
      "Town Type",
      "Governorate",
      "Employment",
      "Work Location",
      "Education",
      "Before Income",
      "After Income"
    ),
    afterGreenspaceExercise_CHI = rep(0.0, length(demographicVariable)),
    afterGreenspaceExercise_PVAL = rep(0.0, length(demographicVariable)),
    afterGreenspaceExercise_CORPVAL = rep(0.0, length(demographicVariable)),
    afterGreenspaceRelaxing_CHI = rep(0.0, length(demographicVariable)),
    afterGreenspaceRelaxing_PVAL = rep(0.0, length(demographicVariable)),
    afterGreenspaceRelaxing_CORPVAL = rep(0.0, length(demographicVariable)),
    afterGreenspaceBirdPhotography_CHI = rep(0.0, length(demographicVariable)),
    afterGreenspaceBirdPhotography_PVAL = rep(0.0, length(demographicVariable)),
    afterGreenspaceBirdPhotography_CORPVAL = rep(0.0, length(demographicVariable)),
    
    afterGreenspaceHarvesting_CHI = rep(0.0, length(demographicVariable)),
    afterGreenspaceHarvesting_PVAL = rep(0.0, length(demographicVariable)),
    afterGreenspaceHarvesting_CORPVAL = rep(0.0, length(demographicVariable))
  )

## Create table of ChiSq tests

activityGreenspaceResultsChiSq <-
  addChiSquared(
    activityGreenspaceLikert,
    activityGreenspaceResults,
    questionColumns = 2:5,
    demographicColumns = 6:ncol(activityGreenspaceLikert),
    minVal = minVal
  )


## Graph different likert plots that are significant.
likert.bar.plot(
  likert::likert(
    activityGreenspaceLikert[3:5],
    grouping = as.factor(activityGreenspaceLikert$maritalStatus)
  ),
  group.order = c("Single", "Married")
)



## ----- Natural Areas -----------------------------------

natural <- data[, which(grepl("ID|Natural", colnames(data)))]

table(natural$visitNatural)
# The one non-response is a "No", the other is just a non-response

table(natural$whyNoVisitNatural)

natural <- natural[natural$visitNatural == "Yes", ]

table(natural$visitNatural)

## ----- Natural Areas: With whom ----------------------------

## Now look at with whom people spend time when visiting natural areas.
# Compare number before vs. after.

inNatural <-
  natural[, which(grepl("ID|alone|family|kids|neighbors|friends", colnames(natural)))] %>%
  pivot_longer(cols = -participantID) %>%
  separate_rows(value, sep = ", ") %>% 
  filter(value != "") 

inNatural$value <- factor(inNatural$value, levels = c("Before the pandemic", "After the pandemic occurred"))

inNaturalTable <- count(inNatural, name, value) %>%
  pivot_wider(names_from = name, values_from = n, values_fill = list(n = 0))

# Assign values from Chisq
inNaturalTable <- inNaturalTable %>%
  add_row(
    value = "X-squared",
    aloneNatural = chisq.test(inNaturalTable[1:2, 2])$statistic,
    familyNatural = chisq.test(inNaturalTable[1:2, 3])$statistic,
    friendsNatural = chisq.test(inNaturalTable[1:2, 4])$statistic,
    kidsNatural = chisq.test(inNaturalTable[1:2, 5])$statistic,
    neighborsNatural = chisq.test(inNaturalTable[1:2, 6])$statistic
  ) %>%
  add_row(
    value = "p-value",
    aloneNatural = chisq.test(inNaturalTable[1:2, 2])$p.value,
    familyNatural = chisq.test(inNaturalTable[1:2, 3])$p.value,
    friendsNatural = chisq.test(inNaturalTable[1:2, 4])$p.value,
    kidsNatural = chisq.test(inNaturalTable[1:2, 5])$p.value,
    neighborsNatural = chisq.test(inNaturalTable[1:2, 6])$p.value
  ) 

temp <- p.adjust(inNaturalTable[ 4, -1])

inNaturalTable <- inNaturalTable %>%
  add_row(
    value = "Corrected p-value",
    aloneNatural = temp[1],
    familyNatural = temp[2],
    friendsNatural = temp[3],
    kidsNatural = temp[4],
    neighborsNatural = temp[5]
  )

# All are significant, though at different levels.

# line should be .5 x wide and 20 y units above the tallest bar; text is
# centered at whole number and 5 y units above the line.

ggplot(inNatural) +
  geom_bar(aes(x = name, fill = value), position = position_dodge2()) +
  labs(x = "", y = "Count", title = "Who do respondents spend time with in natural areas?") +
  scale_x_discrete(labels = c(
    "Alone",
    "With Family",
    "With Friends",
    "With Children",
    "With Neighbors"
  ))+
  scale_fill_manual(values = ggColors, name = "") +
  theme(legend.position = "bottom") +
  geom_segment(aes(x = 0.75, y = 226+20, xend = 1.25, yend = 226+20)) +
  annotate(geom = "text", x = 1, y = 226+25, label = "**") +
  geom_segment(aes(x = 1.75, y = 697+20, xend = 2.25, yend = 697+20)) +
  annotate(geom = "text", x = 2, y = 697+25, label = "*") +
  geom_segment(aes(x = 2.75, y = 528+20, xend = 3.25, yend = 528+20)) +
  annotate(geom = "text", x = 3, y = 528+25, label = "***") +
  geom_segment(aes(x = 3.75, y = 306+20, xend = 4.25, yend = 306+20)) +
  annotate(geom = "text", x = 4, y = 306+25, label = "*") +
  geom_segment(aes(x = 4.75, y = 204+20, xend = 5.25, yend = 204+20)) +
  annotate(geom = "text", x = 5, y = 204+25, label = "***")

ggsave(filename = "../graphs/inNatural.jpeg", device = "jpeg",
       units = "in", width =5.5, height = 4)


## ----- Natural areas: Transportation -----------------------

## transportation to natural areas

transitNatural <- 
  natural[, which(grepl("ID|walking|bicycling|car|bus|taxi", colnames(natural)))] %>%
  pivot_longer(cols = -participantID) %>%
  separate_rows(value, sep = ", ") %>% 
  filter(value != "") 

transitNatural$value <- factor(transitNatural$value, levels = c("Before the pandemic", "After the pandemic occurred"))

transitNaturalTable <- count(transitNatural, name, value) %>%
  pivot_wider(names_from = name, values_from = n, values_fill = list(n = 0))


# Assign values from Chisquared
transitNaturalTable <- transitNaturalTable %>%
  add_row(
    value = "X-squared",
    bicyclingNatural = chisq.test(transitNaturalTable[1:2, 2])$statistic,
    busNatural = chisq.test(transitNaturalTable[1:2, 3])$statistic,
    carNatural = chisq.test(transitNaturalTable[1:2, 4])$statistic,
    taxiNatural = chisq.test(transitNaturalTable[1:2, 5])$statistic,
    walkingNatural = chisq.test(transitNaturalTable[1:2, 6])$statistic
  ) %>%
  add_row(
    value = "p-value",
    bicyclingNatural = chisq.test(transitNaturalTable[1:2, 2])$p.value,
    busNatural = chisq.test(transitNaturalTable[1:2, 3])$p.value,
    carNatural = chisq.test(transitNaturalTable[1:2, 4])$p.value,
    taxiNatural = chisq.test(transitNaturalTable[1:2, 5])$p.value,
    walkingNatural = chisq.test(transitNaturalTable[1:2, 6])$p.value
  ) 

temp <- p.adjust(transitNaturalTable[ 4, -1])

transitNaturalTable <- transitNaturalTable %>%
  add_row(
    value = "Corrected p-value",
    bicyclingNatural = temp[1],
    busNatural = temp[2],
    carNatural = temp[3],
    taxiNatural = temp[4],
    walkingNatural = temp[5]
  )

# line should be .5 x wide and 20 y units above the tallest bar; text is
# centered at whole number and 5 y units above the line.


ggplot(transitNatural) + 
  geom_bar(aes(x = name, fill = value), position = position_dodge2()) +
  labs(x = "", y = "Count", title = "How do respondents travel to natural areas?") +
  scale_x_discrete(labels = c("By Bicycling", "By Bus", "By Car", "By Taxi", "By Walking")) +
  scale_fill_manual(values = ggColors, name = "") +
  theme(legend.position = "bottom") +
  geom_segment(aes(x = 1.75, y = 159+20, xend = 2.25, yend = 159+20)) +
  annotate(geom = "text", x = 2, y = 159+25, label = "**") +
  geom_segment(aes(x = 3.75, y = 164+20, xend = 4.25, yend = 164+20)) +
  annotate(geom = "text", x = 4, y = 164+25, label = "***")

ggsave(filename = "../graphs/transitNatural.jpeg", device = "jpeg",
       units = "in", width = 5.5, height = 4)



## ----- Natural ares: Travel distance ------------------------------

## How far do respondents travel to natural areas?

distanceNatural <- 
  natural[, which(grepl("ID|distance", colnames(natural)))] %>%
  pivot_longer(cols = -participantID) %>%
  separate_rows(value, sep = ", ") %>% 
  filter(value != "") 

distanceNatural$value <- factor(distanceNatural$value, levels = c("Before the pandemic", "After the pandemic occurred"))

distanceNaturalTable <- count(distanceNatural, name, value) %>%
  pivot_wider(names_from = name, values_from = n, values_fill = list(n = 0))
distanceNaturalTable <- distanceNaturalTable[ , c(1,6,2:5)]

# Assign values from Chisquared
distanceNaturalTable <- distanceNaturalTable %>%
  add_row(
    value = "X-squared",
    distanceNatural_under0.5 = chisq.test(distanceNaturalTable[1:2, 2])$statistic,
    distanceNatural_0.5to1 = chisq.test(distanceNaturalTable[1:2, 3])$statistic,
    distanceNatural_1to2 = chisq.test(distanceNaturalTable[1:2, 4])$statistic,
    distanceNatural_2to4 = chisq.test(distanceNaturalTable[1:2, 5])$statistic, 
    distanceNatural_over4 = chisq.test(distanceNaturalTable[1:2, 6])$statistic
  ) %>%
  add_row(
    value = "p-value",
    distanceNatural_under0.5 = chisq.test(distanceNaturalTable[1:2, 2])$p.value,
    distanceNatural_0.5to1 = chisq.test(distanceNaturalTable[1:2, 3])$p.value,
    distanceNatural_1to2 = chisq.test(distanceNaturalTable[1:2, 4])$p.value,
    distanceNatural_2to4 = chisq.test(distanceNaturalTable[1:2, 5])$p.value,
    distanceNatural_over4 = chisq.test(distanceNaturalTable[1:2, 6])$p.value
  ) 

temp <- p.adjust(distanceNaturalTable[ 4, -1])

distanceNaturalTable <- distanceNaturalTable %>%
  add_row(
    value = "Corrected p-value",
    distanceNatural_under0.5 = temp[1],
    distanceNatural_0.5to1 = temp[2],
    distanceNatural_1to2 = temp[3],
    distanceNatural_2to4 = temp[4],
    distanceNatural_over4 = temp[5]
  )

table(data$beforeIncome, data$distanceNatural_0.5to1)

## None are significant, though before correction under 5 and 2 to 5 are significant. overall a slight decrease across the board, none have increased.

## ----- Natural areas: Activities ----------------------------

## Comparing activities in natural areas--Likert scale

activityNatural <-
  natural[, which(grepl("ID|after", colnames(natural)))] %>%
  pivot_longer(cols = -participantID) %>%
  filter(value != "") %>%
  filter(value != "I don't do this activity")


activityNatural$value <-
  factor(
    activityNatural$value,
    levels = likertLevels
  )

activityNaturalTable <- count(activityNatural, name, value) %>%
  pivot_wider(names_from = name, values_from = n, values_fill = list(n = 0))

ggplot(activityNatural) + 
  geom_bar(aes(x = name, fill = value))


activityNatural <-
  pivot_wider(activityNatural,
              names_from = name,
              values_from = value) %>%
  select(
    participantID,
    afterNaturalExercise,
    afterNaturalRelaxing,
    afterNaturalBirdPhotography,
    afterNaturalScenery,
    afterNaturalHistoric,
    afterNaturalLandscaping,
    afterNaturalCultivation,
    afterNaturalOrchard,
    afterNaturalGathering,
    afterNaturalCamping,
    afterNaturalLivestock,
    afterNaturalHarvesting
  )

activityNaturalLikert <- as.data.frame(lapply(activityNatural[-1],
                                                 function(x) {factor(x,levels=likertLevels, exclude=NA)}))

names(activityNaturalLikert) <- c(
  "After the COVID-19 pandemic, you use natural areas for exercising, sporting, running, juggling and walking:",
  "After the COVID-19 pandemic, you use natural areas for relaxing, picnicking and socializing:",
  "After the COVID-19 pandemic, you visit natural areas for wildife and bird watching and photography:",
  "After the COVID-19 pandemic, you visit natural areas to appreciate scenery from your car:",
  "After the COVID-19 pandemic, you visit natural areas to visit historic and heritage site and exploring new natural sites:",
  "After the COVID-19 pandemic, you visit natural areas to plow and/or landscape private land:",
  "After the COVID-19 pandemic, you visit natural areas to cultivate and harvest crops:",
  "After the COVID-19 pandemic, you visit natural areas to plant and take care of olive trees and other fruiting trees:",
  "After the COVID-19 pandemic, you visit natural areas to gather wild food and plants:",
  "After the COVID-19 pandemic, you visit natural areas to camp:",
  "After the COVID-19 pandemic, you visit natural areas to take care of and graze livestock and/or beekeeping:",
  "After the COVID-19 pandemic, you search for nests and hunt in natural areas:"
)

activityNaturalLikert <- cbind(activityNatural[1], activityNaturalLikert)
activityNaturalLikert <- left_join(activityNaturalLikert, data[1:12], by = "participantID")

likert::likert(activityNaturalLikert[, c(2:13)])

likert.bar.plot(
  likert(activityNaturalLikert[, c(2:13)]),
  wrap = 50,
  neutral.color = "white"
)

# Passive appreciation
likert.bar.plot(
  likert(activityNaturalLikert[, c(2:6)]),
  wrap = 50,
  neutral.color = "white"
)

# Active cultivation
likert.bar.plot(
  likert(activityNaturalLikert[, c(7:10)]),
  wrap = 50,
  neutral.color = "white"
)

# Extractive or harmful activities
likert.bar.plot(
  likert(activityNaturalLikert[, c(11:13)]),
  wrap = 50,
  neutral.color = "white"
)

## ----- Natural areas: Activities & Less/More tests ---------------------------------

## goal: to create a table with counts and chi squared test of "less oftens" vs "more
## oftens". Want similar output to e.g. the 'with whom' tests and graphs

## Build a table that has summed "more" and "less" for each question.

activityNaturalLessMore <- activityNatural %>%
  pivot_longer(cols = -participantID) %>%
  filter(value != "") %>%
  filter(value != "About the same")

activityNaturalLessMore$value[activityNaturalLessMore$value == "Much less"] <-
  "Less often"
activityNaturalLessMore$value[activityNaturalLessMore$value == "Much more often"] <-
  "More often"

activityNaturalLMGraph <- activityNaturalLessMore

activityNaturalLessMore$value <-
  factor(activityNaturalLessMore$value,
         levels = c("Less often", "More often"))

activityNaturalLessMore <-
  count(activityNaturalLessMore, name, value) %>%
  pivot_wider(
    names_from = name,
    values_from = n,
    values_fill = list(n = 0)
  )


# Using apply to fill these in is clumsy based on how chisq.test outputs the
# information. So, just assign.
activityNaturalLessMore <- activityNaturalLessMore %>%
  add_row(
    value = "X-squared",
    afterNaturalBirdPhotography = chisq.test(activityNaturalLessMore[1:2, 2])$statistic,
    afterNaturalCamping = chisq.test(activityNaturalLessMore[1:2, 3])$statistic,
    afterNaturalCultivation = chisq.test(activityNaturalLessMore[1:2, 4])$statistic,
    afterNaturalExercise = chisq.test(activityNaturalLessMore[1:2, 5])$statistic,
    afterNaturalGathering = chisq.test(activityNaturalLessMore[1:2, 6])$statistic,
    afterNaturalHarvesting = chisq.test(activityNaturalLessMore[1:2, 7])$statistic,
    afterNaturalHistoric = chisq.test(activityNaturalLessMore[1:2, 8])$statistic,
    afterNaturalLandscaping = chisq.test(activityNaturalLessMore[1:2, 9])$statistic,
    afterNaturalLivestock = chisq.test(activityNaturalLessMore[1:2, 10])$statistic,
    afterNaturalOrchard = chisq.test(activityNaturalLessMore[1:2, 11])$statistic,
    afterNaturalRelaxing = chisq.test(activityNaturalLessMore[1:2, 12])$statistic,
    afterNaturalScenery = chisq.test(activityNaturalLessMore[1:2, 13])$statistic
  ) %>%
  add_row(
    value = "p-value",
    afterNaturalBirdPhotography = chisq.test(activityNaturalLessMore[1:2, 2])$p.value,
    afterNaturalCamping = chisq.test(activityNaturalLessMore[1:2, 3])$p.value,
    afterNaturalCultivation = chisq.test(activityNaturalLessMore[1:2, 4])$p.value,
    afterNaturalExercise = chisq.test(activityNaturalLessMore[1:2, 5])$p.value,
    afterNaturalGathering = chisq.test(activityNaturalLessMore[1:2, 6])$p.value,
    afterNaturalHarvesting = chisq.test(activityNaturalLessMore[1:2, 7])$p.value,
    afterNaturalHistoric = chisq.test(activityNaturalLessMore[1:2, 8])$p.value,
    afterNaturalLandscaping = chisq.test(activityNaturalLessMore[1:2, 9])$p.value,
    afterNaturalLivestock = chisq.test(activityNaturalLessMore[1:2, 10])$p.value,
    afterNaturalOrchard = chisq.test(activityNaturalLessMore[1:2, 11])$p.value,
    afterNaturalRelaxing = chisq.test(activityNaturalLessMore[1:2, 12])$p.value,
    afterNaturalScenery = chisq.test(activityNaturalLessMore[1:2, 13])$p.value
  ) 

temp <- p.adjust(activityNaturalLessMore[ 4, -1])

activityNaturalLessMore <- activityNaturalLessMore %>%
  add_row(
    value = "Corrected p-value",
    afterNaturalBirdPhotography = temp[1],
    afterNaturalCamping = temp[2],
    afterNaturalCultivation = temp[3],
    afterNaturalExercise = temp[4],
    afterNaturalGathering = temp[5],
    afterNaturalHarvesting = temp[6],
    afterNaturalHistoric = temp[7],
    afterNaturalLandscaping = temp[8],
    afterNaturalLivestock = temp[9],
    afterNaturalOrchard = temp[10],
    afterNaturalRelaxing = temp[11],
    afterNaturalScenery = temp[12]
  )


activityNaturalLMGraph$name <-
  factor(
    activityNaturalLMGraph$name, 
    levels = c(
      "afterNaturalExercise",
      "afterNaturalRelaxing",
      "afterNaturalBirdPhotography",
      "afterNaturalScenery",
      "afterNaturalHistoric",
      "afterNaturalLandscaping",
      "afterNaturalCultivation",
      "afterNaturalOrchard",
      "afterNaturalGathering",
      "afterNaturalCamping",
      "afterNaturalLivestock",
      "afterNaturalHarvesting"
    )
  )

ggplot(activityNaturalLMGraph) + 
  geom_bar(aes(x = name, fill = value), position = position_dodge2()) +
  labs(x = "", y = "Count", title = "Do respondents perform activities in natural areas \nless or more often after the COVID-19 pandemic?") +
  scale_x_discrete(labels = c(
    "Exercising",
    "Relaxing",
    "Birds and \nPhotography",
    "Viewing \nScenery",
    "Historic \nSites",
    "Landscaping", # significant **
    "Cultivating \nCrops", # *
    "Tending \nOrchards", # **
    "Gathering \nPlants",
    "Camping", # **
    "Tending \nLivestock",
    "Nest/Bird \nHunting"
  )) +
  scale_fill_manual(values = ggColors, name = "") +
  theme(legend.position = "bottom") +

  geom_segment(aes(x = 5.75, y = 171+20, xend = 6.25, yend = 171+20)) +
  annotate(geom = "text", x = 6, y = 171+25, label = "**") +
  geom_segment(aes(x = 6.75, y = 182+20, xend = 7.25, yend = 182+20)) +
  annotate(geom = "text", x = 7, y = 182+25, label = "*") +
  geom_segment(aes(x = 7.75, y = 206+20, xend = 8.25, yend = 206+20)) +
  annotate(geom = "text", x = 8, y = 206+25, label = "**") +

  
  geom_segment(aes(x = 9.75, y = 136+20, xend = 10.25, yend = 136+20)) +
  annotate(geom = "text", x = 10, y = 136+25, label = "**") +
  
  geom_segment(aes(x = 11.75, y = 76+20, xend = 12.25, yend = 76+20)) +
  annotate(geom = "text", x = 12, y = 76+25, label = "^")

ggsave(filename = "../graphs/activityNatural.jpeg", device = "jpeg",
       units = "in", width = 9.5, height = 4.5)



## ----- Natural areas: Activities & Demographic tests -------------------------------------

# Create Tibble for results.

activityNaturalResults <-
  tibble(
    demographicVariable = c(
      "Gender",
      "Marital Status",
      "Age",
      "Housing",
      "Town Type",
      "Governorate",
      "Employment",
      "Work Location",
      "Education",
      "Before Income",
      "After Income"
    ),
    afterNaturalExercise_CHI = rep(0.0, length(demographicVariable)),
    afterNaturalExercise_PVAL = rep(0.0, length(demographicVariable)),
    afterNaturalExercise_CORPVAL = rep(0.0, length(demographicVariable)),
    afterNaturalRelaxing_CHI = rep(0.0, length(demographicVariable)),
    afterNaturalRelaxing_PVAL = rep(0.0, length(demographicVariable)),
    afterNaturalRelaxing_CORPVAL = rep(0.0, length(demographicVariable)),
    afterNaturalBirdPhotography_CHI = rep(0.0, length(demographicVariable)),
    afterNaturalBirdPhotography_PVAL = rep(0.0, length(demographicVariable)),
    afterNaturalBirdPhotography_CORPVAL = rep(0.0, length(demographicVariable)),
    afterNaturalScenery_CHI = rep(0.0, length(demographicVariable)),
    afterNaturalScenery_PVAL = rep(0.0, length(demographicVariable)),
    afterNaturalScenery_CORPVAL = rep(0.0, length(demographicVariable)),
    afterNaturalHistoric_CHI = rep(0.0, length(demographicVariable)),
    afterNaturalHistoric_PVAL = rep(0.0, length(demographicVariable)),
    afterNaturalHistoric_CORPVAL = rep(0.0, length(demographicVariable)),
    
    afterNaturalLandscaping_CHI = rep(0.0, length(demographicVariable)),
    afterNaturalLandscaping_PVAL = rep(0.0, length(demographicVariable)),
    afterNaturalLandscaping_CORPVAL = rep(0.0, length(demographicVariable)),
    afterNaturalCultivation_CHI = rep(0.0, length(demographicVariable)),
    afterNaturalCultivation_PVAL = rep(0.0, length(demographicVariable)),
    afterNaturalCultivation_CORPVAL = rep(0.0, length(demographicVariable)),
    afterNaturalOrchard_CHI = rep(0.0, length(demographicVariable)),
    afterNaturalOrchard_PVAL = rep(0.0, length(demographicVariable)),
    afterNaturalOrchard_CORPVAL = rep(0.0, length(demographicVariable)),
    
    afterNaturalGathering_CHI = rep(0.0, length(demographicVariable)),
    afterNaturalGathering_PVAL = rep(0.0, length(demographicVariable)),
    afterNaturalGathering_CORPVAL = rep(0.0, length(demographicVariable)),
    afterNaturalCamping_CHI = rep(0.0, length(demographicVariable)),
    afterNaturalCamping_PVAL = rep(0.0, length(demographicVariable)),
    afterNaturalCamping_CORPVAL = rep(0.0, length(demographicVariable)),
    afterNaturalLivestock_CHI = rep(0.0, length(demographicVariable)),
    afterNaturalLivestock_PVAL = rep(0.0, length(demographicVariable)),
    afterNaturalLivestock_CORPVAL = rep(0.0, length(demographicVariable)),
    afterNaturalHarvesting_CHI = rep(0.0, length(demographicVariable)),
    afterNaturalHarvesting_PVAL = rep(0.0, length(demographicVariable)),
    afterNaturalHarvesting_CORPVAL = rep(0.0, length(demographicVariable))

  )


## Create table of ChiSq tests

activityNaturalResultsChiSq <-
  addChiSquared(
    activityNaturalLikert,
    activityNaturalResults,
    questionColumns = 2:13,
    demographicColumns = 14:ncol(activityNaturalLikert),
    minVal = minVal
  )



## Graph different likert plots that are significant & run post hoc tests
likert.bar.plot(
  likert::likert(
    activityNaturalLikert[c(2:6,13)],
    grouping = as.factor(activityNaturalLikert$maritalStatus)
  ),
  group.order = c("Single", "Married")
)

likert.bar.plot(
  likert::likert(
    activityNaturalLikert[c(3,8)],
    grouping = as.factor(activityNaturalLikert$housing)
  ),
  group.order = c("Apartment", "House")
)

likert.bar.plot(
  likert::likert(
    activityNaturalLikert[c(7,9)],
    grouping = as.factor(activityNaturalLikert$townType)
  ),
  group.order = c("Village", "Town", "City", "Refugee Camp")
)

activityNaturalLikert[, 18] <-
  factor(
    activityNaturalLikert[, 18],
    levels = c("Village", "Town", "City", "Refugee Camp")
  )

posthocChiSquared(likertTable = activityNaturalLikert,
                  questionColumn = 7,
                  demographicColumn = 18
)

posthocChiSquared(likertTable = activityNaturalLikert,
                  questionColumn = 9,
                  demographicColumn = 18
)


likert.bar.plot(
  likert::likert(
    activityNaturalLikert[9],
    grouping = as.factor(activityNaturalLikert$education)
  ),
  group.order = c("Highschool or Less", "Diploma", "Bachelor", "Masters or More")
)



activityNaturalLikert[, 22] <-
  factor(
    activityNaturalLikert[, 22],
    levels = c("Highschool or Less", "Diploma", "Bachelor", "Masters or More")
  )

posthocChiSquared(
  likertTable = activityNaturalLikert,
  questionColumn = 9,
  demographicColumn = 22
)


likert.bar.plot(
  likert::likert(
    activityNaturalLikert[6],
    grouping = as.factor(activityNaturalLikert$afterIncome)
  ), 
  group.order = c("I have no income", "Severely decreased", "Decreased", "The same", "Increased")
)


activityNaturalLikert[, 24] <-
  factor(
    activityNaturalLikert[, 24],
    levels = c("I have no income", "Severely decreased", "Decreased", "The same", "Increased")
  )

posthocChiSquared(
  likertTable = activityNaturalLikert,
  questionColumn = 6,
  demographicColumn = 24
)



## ----- END ---------------------------------
