#Install Required Packages
library(httr)
library(stringr)
library(tidyverse)
library(dplyr)
library(readr)
library(magick)
library(base64enc)
library(jsonlite)

#Some background reading: https://osf.io/preprints/psyarxiv/sekf5

#Note: code we are using was adapted by this blog post: https://rpubs.com/nirmal/setting_chat_gpt_R. 
#We highly recommend you read over that blog post in detail if you are stuck at any of these steps 
#First, you must get your ChatGPT API key from here: https://platform.openai.com/overview 

#Set WD 
setwd("/Users/steverathje/Desktop/GBT")

#Then, put your API key in the quotes below: 
my_API <- "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"
Sys.setenv(OPENAI_API_KEY = my_API)

#The "hey_chatGPT function will help you access the API and prompt GPT 
hey_chatGPT <- function(answer_my_question) {
  chat_GPT_answer <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", my_API)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo-0301",
      temperature = 0,
      messages = list(
        list(
          role = "user",
          content = answer_my_question
        )
      )
    )
  )
  str_trim(content(chat_GPT_answer)$choices[[1]]$message$content)
}

#Read in your dataset
data <- read_csv("Data/SemEval2017ErrorsFixed.csv", col_names = FALSE)
View(data)

#Recode variables
colnames(data) <- c("id", "human", "text", "gpt")
data$gpt <- NA #Create a "gpt" variable
data$human <- dplyr::recode(data$human, "positive" = 1, "neutral" = 2, "negative" = 3)

View(data)

#Run a loop over your dataset and prompt ChatGPT 
for (i in 1:10) {
  print(i)
  question <- "Is the sentiment of this text positive, neutral, or negative? Answer only with a number: 1 if positive, 2 if neutral, and 3 if negative. Here is the text:"
  text <- data[i,3]       
  concat <- paste(question, text)
  result <- hey_chatGPT(concat)
  while(length(result) == 0){
    result <- hey_chatGPT(concat)
    print(result)
  }
  print(result)
  data$gpt[i] <- result
}

#Take only the first string from gpt and convert to a numeric 
data$gpt <- substr(data$gpt, 1, 1)  
data$gpt <- as.numeric(data$gpt)

###Below is code for performance evaluation metrics (accuracy, F1, and recall)

###More on these performance metrics here: https://towardsdatascience.com/accuracy-precision-recall-or-f1-331fb37c5cb9 

#Accuracy function
accuracy <- function(data){
  res_table <- table(data$human, data$gpt)
  acc <- sum(diag(res_table))/sum(res_table)
  return(acc)
}

#Compute Accuracy
accuracy(data)

#Run functions and compute average recall and F1
res_pos_AR <- metrics(data,1)
res_neu_AR <- metrics(data,2)
res_neg_AR <- metrics(data,3)

#Compute Average Recall
avg_rec_AR <- mean(c(res_pos_AR[2],res_neu_AR[2],res_neg_AR[2]))
avg_rec_AR

#Compute Average F1
avg_f1_AR <- mean(c(res_pos_AR[3],res_neg_AR[3]))
avg_f1_AR

#What about different prompts? 
for (i in 1:10) {
  print(i)
  question <- "How negative or positive is this tweet on a 1-7 scale? Answer only with a number, with 1 being 'very negative' and 7 being 'very positive.' Here is the tweet:"
  text <- data[i,3]       
  concat <- paste(question, text)
  result <- hey_chatGPT(concat)
  while(length(result) == 0){
    result <- hey_chatGPT(concat)
    print(result)
  }
  print(result)
  data$gpt[i] <- result
}

#What about GPT for vision??? 

# Function to interact with ChatGPT Vision API using a file
hey_chatGPT_vision_with_file <- function(question_text, image_file_path) {
  # Convert BMP to JPEG and encode it to Base64
  #img <- image_read(image_file_path)
  #img <- readJPEG(image_file_path)
  #image_convert(format = "jpeg")
  #temp_file <- tempfile(fileext = ".jpg")
  #image_write(img, temp_file)
  base64_image <- base64enc::base64encode(image_file_path)
  
  # Prepare the payload with the base64 encoded image
  payload <- list(
    model = "gpt-4-vision-preview",
    temperature = 0,
    messages = list(
      list(
        role = "user",
        content = list(
          list(type = "text", text = question_text),
          list(
            type = "image_url",
            image_url = list(
              url = paste("data:image/jpeg;base64,", base64_image, sep="")
            )
          )
        )
      )
    ),
    max_tokens = 300
  )
  
  # Set up the API request
  chat_GPT_vision_answer <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")), `Content-Type` = "application/json"),
    body = jsonlite::toJSON(payload, auto_unbox = TRUE),
    encode = "json"
  )
  
  # Check response status and return content
  response_status <- httr::http_status(chat_GPT_vision_answer)
  
  if (response_status$category == "Success") {
    response_content <- httr::content(chat_GPT_vision_answer, "parsed")
    if (!is.null(response_content$choices[[1]]$message$content)) {
      return(response_content$choices[[1]]$message$content)
    } else {
      return("Response content is NULL")
    }
  } else {
    return(paste("Error:", response_status$reason, "\nFull Response:", capture.output(print(chat_GPT_vision_answer))))
  }
}

prompt_valence <- #"On a scale of 1-7, how might a person feel while viewing this photo, 
  "On a scale of 1-7, what is the valence of this photo?
  1 = very negative
  2 = moderately negative
  3 = somewhat negative
  4 = neutral
  5 = somewhat positive
  6 = moderately positive
  7 = very positive
Please try your best to give an answer, and please answer only with a number."

prompt_arousal <- #"On a scale of 1-7, how might a person feel while viewing this photo, 
  "Please rate this picture in terms of the amount of emotion (arousal) that it evokes. 
  In other words, we would like to know how much emotional intensity the picture creates; 
  whether the picture captures something good or bad doesn’t matter. 
  We are interested only in the degree of excitement, energy, or intensity of feeling it represents.
  Use the higher side of the scale to mark your answer if the picture represents something that is strongly emotional. 
  The words that we might use to describe the emotional state that the picture creates are aroused, alert, activated, charged, or energized.
  Use the lower side of the scale to mark your answer if the picture represents something that is not strongly emotional. 
  The words that we might use to describe the emotional state that the picture creates are unaroused, slow, still, de-energized, calm, or peaceful.
  Use the middle of the scale to indicate an image that is moderately arousing or halfway through the two extremes.
  Please use the full range of the scale to make your responses rather than relying on only a few points.
  On a scale of 1-7, what is the arousal level of this photo?
  1 = very low
  2 = moderately low
  3 = somewhat low
  4 = neutral
  5 = somewhat high
  6 = moderately high
  7 = very high
Please try your best to give an answer, and please answer only with a number."

response <- hey_chatGPT_vision_with_file(prompt_valence, "Happy1.jpg")


