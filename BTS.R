### Bayesian Truth Serum

participants <- 18 # number of individuals

chan <- c(c("No chance","Possible, but extremely unlikely"),
          c("Possible, but probably won't happen","50-50","Likely, but not certain"),
          c("Nearly certain","Absolutely certain"))

conf <-  c("Very confident",
           "Somewhat confident",
           "Not confident",
           "A total guess")

# These are the questions from the survey results. Each element in this list is a series of 5 numbers
# The numbers refer to 1: the BTS question, 2: the confidence (not used in calculation), 3: prediction that others
# would say 'no chance', etc. 4: prediction that others would say 'possible', etc. 5: prediction that others would
# say 'nearly certain', etc.

question_columns <- list(btsq1=c(11:15),
                         btsq2=c(16:20),
                         btsq3=c(27,28,31:33),
                         btsq4=c(37:41),
                         btsq5=c(42:46),
                         btsq6=c(53,54,57:59),
                         btsq7=c(63:67),
                         btsq8=c(70,71,74:76),
                         btsq9=c(80:84),
                         btsq10=c(87,88,91:93),
                         btsq11=c(96:100),
                         btsq12=c(101:105))

bts_results <- list()

responses <- list(id=c('4125','0397','8657','4715','6401','5785','4556','3865','7454',
                       '6961','8170','8547','1263','0870','1849','9002','2001','8600'),
                  soph_offense=c('US',NA,'RU','RU',NA,'CN','US','CN','RU',
                                 'RU','RU',NA,'CN','US','US','US','RU','RU'),
                  unsoph_offense=c('IQ',NA,'IR','IQ','alq','KP','KP','IQ','KP',
                                   'IQ','KP',NA,'KP',NA,'IQ','KP','aum','IQ'),
                  soph_defense=c('US',NA,'US','US','US','US','US','US','US',
                                 'US','US','US','US',NA,'US','US',NA,'US'),
                  unsoph_defense=c('IQ',NA,'AU',NA,NA,NA,NA,NA,'SE',
                                   'KR','NG',NA,NA,NA,'VN','TR',NA,'KR'))

# For loop over all the BTS questions, 12 total
for (i in 1:length(question_columns)){
  
  bts_results[[i]] <- list()
  
  # Saving data for question
  question <- list(answer=c(external[[question_columns[[i]][[1]]]][1:participants+1]),
                    answer_confidence=external[[question_columns[[i]][[2]]]][1:participants+1],
                    expert_nochance=external[[question_columns[[i]][[3]]]][1:participants+1],
                    expert_possible=external[[question_columns[[i]][[4]]]][1:participants+1],
                    expert_certain=external[[question_columns[[i]][[5]]]][1:participants+1])
  
  # Cleaning up question responses
  for (x in 3:5){
    
    question[[x]] <- sub("\\D*","",question[[x]]) # get rid of non-digits
    question[[x]] <- sub("%","",question[[x]]) # get rid of % signs
    question[[x]] <- sub("","0",question[[x]]) # if empty, replace with 0
    question[[x]] <- as.numeric(question[[x]]) # numeric data type
    
  }
  
  #Reorganizing question by responses
  question_byresponse <- list()
  
  for (x in 1:participants){
    
    matched <- grepl(question$answer[[x]],chan)
    
    if (isTRUE(matched[1]) || isTRUE(matched[2])){
      answer <- c(1,0,0)
    } else{
      if (isTRUE(matched[6]) || isTRUE(matched[7])){
        answer <- c(0,0,1)
      } else{
        answer <- c(0,1,0)
      }
    }
    
    question_byresponse[[x]] <- list(answer,
                                     question$expert_nochance[[x]]/100,
                                     question$expert_possible[[x]]/100,
                                     question$expert_certain[[x]]/100)
  }
  
  # Calculating average endorsements parameter
  sumvec <- c(0,0,0)
  for (x in 1:participants){sumvec <- sumvec + question_byresponse[[x]][[1]]}
  
  avevec1 <- sumvec/participants # average endorsements
  bts_results[[i]][[1]] <- avevec1 # add to bts-results
  
  # Calculating geometric mean of predictions
  logvec <- c(0,0,0)
  for (x in 1:participants){
    prelogvec <- c(question_byresponse[[x]][[2]],question_byresponse[[x]][[3]],question_byresponse[[x]][[4]])
    logvec <- logvec + log1p(prelogvec)
  }
  
  geovec1 <- exp(logvec/participants) # geometric mean of predictions
  bts_results[[i]][[2]] <- geovec1 # add to bts-results
  
  # Calculate BTS Scores
  bts_scores <- c()
  ave_bts <- list()
  for (x in 1:participants){
    
    bts_part1 <- question_byresponse[[x]][[1]] * log1p(avevec1/geovec1)
    bts_part2 <- avevec1 * (log1p(c(question_byresponse[[x]][[2]],question_byresponse[[x]][[3]],question_byresponse[[x]][[4]])) - log1p(avevec1))
    bts_score <- Reduce("+",bts_part1) + Reduce("+",bts_part2)
    bts_scores[x] <- bts_score
    
    # To compute the average BTS in following lines
    ave_bts[[x]] <- question_byresponse[[x]][[1]] * bts_score
  }
  
  ave_bts_sum <- Reduce("+",ave_bts)
  ave_bts <- ave_bts_sum/(participants*avevec1)
  
  bts_results[[i]][[3]] <- bts_scores # add to bts-results
  bts_results[[i]][[4]] <- ave_bts # add to bts-results
  
  # Find participants that answered the bts max question
  max_ave_bts <- which.max(ave_bts)
  if (max_ave_bts == 1){
    ans_range <- c(1,2)
  } else{
    if (max_ave_bts == 3){
      ans_range <- c(6,7)
    } else{
      ans_range <- c(3,4,5)
    }
  }
  
  experts <- c()
  for (x in 1:participants){
    if (question$answer[[x]] %in% chan[ans_range]){
      experts[x] <- responses$id[[x]]
    }else{
      experts[x] <- ""
    }
  }
  bts_results[[i]][[5]] <- experts # add to bts-results
}


# Processing into data frames and files #

filenamesList <- c("average","geometric","bts_score","bts_average","experts") # 5 total CSV file names

matrixnamesList <- paste0("BTSQ",1:12) # For the column names, 12 total

# For loop over 1:5 for each file
for (x in 1:length(bts_results[[1]])){
  
  bts_DF1 <- data.frame(bts_results[[1]][[x]],
                        bts_results[[2]][[x]],
                        bts_results[[3]][[x]],
                        bts_results[[4]][[x]],
                        bts_results[[5]][[x]],
                        bts_results[[6]][[x]],
                        bts_results[[7]][[x]],
                        bts_results[[8]][[x]],
                        bts_results[[9]][[x]],
                        bts_results[[10]][[x]],
                        bts_results[[11]][[x]],
                        bts_results[[12]][[x]])
  
  names(bts_DF1) <- matrixnamesList
  
  write.csv(bts_DF1,
            file = paste0("C:/Users/MKegley/Documents/MyCodingProjects/Fermi/Surveys/output/external/external_output_",filenamesList[x],".csv"),
            row.names = FALSE, col.names = FALSE)
}




### Internal Surveys Analysis

participants <- 8 # number of respondents

chan1 <- c(c(0.0,0.32),c(0.33,0.66),c(0.67,1.00))

chan2 <- c(c(0.0001,0.1),c(0.1,0.5),c(0.5,0.99))

conf <-  c("Very confident",
           "Somewhat confident",
           "Not confident",
           "A total guess")

# These are the questions from the external survey results. Each element in this list is a series of 5 numbers
# The numbers refer to 1: the BTS question, 2: the confidence (not used in calculation), 3: prediction that others
# would say 'no chance', etc. 4: prediction that others would say 'possible', etc. 5: prediction that others would
# say 'nearly certain', etc.

question_columns <- list(btsq1=c(10:14),
                         btsq2=c(15:19),
                         btsq3=c(21:25),
                         btsq4=c(26:30),
                         btsq5=c(32:36),
                         btsq6=c(37:41),
                         btsq7=c(43:47),
                         btsq8=c(48:52))

bts_results <- list()

responses <- list(id=c('6065','3661','6747','9708','3187','4396','7939','8773'))



# For loop over all the BTS questions, 8 total

for (i in 1:length(question_columns)){
  
  bts_results[[i]] <- list()
  
  # Saving data for question
  question <- list(answer=c(internal[[question_columns[[i]][[1]]]][1:participants+1]),
                   answer_confidence=internal[[question_columns[[i]][[2]]]][1:participants+1],
                   expert_nochance=internal[[question_columns[[i]][[3]]]][1:participants+1],
                   expert_possible=internal[[question_columns[[i]][[4]]]][1:participants+1],
                   expert_certain=internal[[question_columns[[i]][[5]]]][1:participants+1])
  
  # Cleaning up answers
  
  question[[1]] <- sub("\\D*","",question[[1]]) # get rid of non-digits
  question[[1]] <- sub("%","",question[[1]]) # get rid of % signs
  question[[1]] <- sub("","0",question[[1]]) # if empty, replace with 0
  question[[1]] <- as.numeric(question[[1]])/100 # numeric data type, convert % to decimal
  
  # Cleaning up chance estimations
  for (x in 3:5){
    
    question[[x]] <- sub("\\D*","",question[[x]]) # get rid of non-digits
    question[[x]] <- sub("%","",question[[x]]) # get rid of % signs
    question[[x]] <- sub("","0",question[[x]]) # if empty, replace with 0
    question[[x]] <- as.numeric(question[[x]])/100 # numeric data type
    
  }
  
  #Reorganizing question by responses
  question_byresponse <- list()
  
  for (x in 1:participants){
    
    if (i %in% c(1,2,3,4,7,8)){
      if (question$answer[[x]] >= chan1[1] && question$answer[[x]] <= chan1[2]){
        answer <- c(1,0,0)
      } else{
        if (question$answer[[x]] >= chan1[3] && question$answer[[x]] <= chan1[4]){
          answer <- c(0,1,0)
        } else{
          answer <- c(0,0,1)
        }
      }
    }else{
      if (question$answer[[x]] >= chan2[1] && question$answer[[x]] <= chan2[2]){
        answer <- c(1,0,0)
      } else{
        if (question$answer[[x]] >= chan2[3] && question$answer[[x]] <= chan2[4]){
          answer <- c(0,1,0)
        } else{
          answer <- c(0,0,1)
        }
      }
    }
    
    question_byresponse[[x]] <- list(answer,
                                     question$expert_nochance[[x]],
                                     question$expert_possible[[x]],
                                     question$expert_certain[[x]])
  }
  
  # Calculating average endorsements parameter
  sumvec <- c(0,0,0)
  for (x in 1:participants){sumvec <- sumvec + question_byresponse[[x]][[1]]}
  
  avevec1 <- sumvec/participants # average endorsements
  bts_results[[i]][[1]] <- avevec1 # add to bts-results
  
  # Calculating geometric mean of predictions
  logvec <- c(0,0,0)
  for (x in 1:participants){
    prelogvec <- c(question_byresponse[[x]][[2]],question_byresponse[[x]][[3]],question_byresponse[[x]][[4]])
    logvec <- logvec + log1p(prelogvec)
  }
  
  geovec1 <- exp(logvec/participants) # geometric mean of predictions
  bts_results[[i]][[2]] <- geovec1 # add to bts-results
  
  # Calculate BTS Scores
  bts_scores <- c()
  ave_bts <- list()
  for (x in 1:participants){
    
    bts_part1 <- question_byresponse[[x]][[1]] * log1p(avevec1/geovec1)
    bts_part2 <- avevec1 * (log1p(c(question_byresponse[[x]][[2]],question_byresponse[[x]][[3]],question_byresponse[[x]][[4]])) - log1p(avevec1))
    bts_score <- Reduce("+",bts_part1) + Reduce("+",bts_part2)
    bts_scores[x] <- bts_score
    
    # To compute the average BTS in following lines
    ave_bts[[x]] <- question_byresponse[[x]][[1]] * bts_score
  }
  
  ave_bts_sum <- Reduce("+",ave_bts)
  ave_bts <- ave_bts_sum/(participants*avevec1)
  
  bts_results[[i]][[3]] <- bts_scores # add to bts-results
  bts_results[[i]][[4]] <- ave_bts # add to bts-results
  
  # Find participants that answered the bts max question
  max_ave_bts <- which.max(ave_bts)
  if (max_ave_bts == 1){
    ans_range <- c(1,2)
  } else{
    if (max_ave_bts == 2){
      ans_range <- c(3,4)
    } else{
      ans_range <- c(5,6)
    }
  }
  
  experts <- c()
  for (x in 1:participants){
    
    if (i %in% c(1,2,3,4,7,8)){
      if (question$answer[[x]] >= chan1[ans_range[1]] && question$answer[[x]] <= chan1[ans_range[2]]){
        experts[x] <- responses$id[[x]]
      }else{
        experts[x] <- ""
      }
    }else{
      if (question$answer[[x]] >= chan2[ans_range[1]] && question$answer[[x]] <= chan2[ans_range[2]]){
        experts[x] <- responses$id[[x]]
      }else{
        experts[x] <- ""
      }
    }
  }
  
  bts_results[[i]][[5]] <- experts # add to bts-results

}

# Processing into data frames and files #

filenamesList <- c("average","geometric","bts_score","bts_average","experts") # 5 total CSV file names

matrixnamesList <- paste0("BTSQ",1:8) # For the column names, 12 total

# For loop over 1:5 for each file
for (x in 1:length(bts_results[[1]])){
  
  bts_DF1 <- data.frame(bts_results[[1]][[x]],
                        bts_results[[2]][[x]],
                        bts_results[[3]][[x]],
                        bts_results[[4]][[x]],
                        bts_results[[5]][[x]],
                        bts_results[[6]][[x]],
                        bts_results[[7]][[x]],
                        bts_results[[8]][[x]])
  
  names(bts_DF1) <- matrixnamesList
  
  write.csv(bts_DF1,
            file = paste0("output_bts",filenamesList[x],".csv"),
            row.names = FALSE, col.names = FALSE)
}