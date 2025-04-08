### Extract text from pdfs

pacman::p_load(pdftools)

pdfs <- list.files('data/sentiment-analysis/policy_pdfs', full.names = T)

output_folder = 'data/sentiment-analysis/policy_text_extracted'

# Ensure output folder exists
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Function to clean extracted text by handling \n issues
clean_text <- function(text) {
  text <- gsub("\n{2,}", "\n\n", text)  # Keep paragraph breaks (double \n)
  text <- gsub("(?<!\\n)\\n(?!\\n)", " ", text, perl = TRUE)  # Replace single \n with space
  text <- gsub("\\s+", " ", text)  # Remove excessive spaces
  return(trimws(text))  # Trim leading/trailing spaces
}

pdf_file <- pdfs[1]
# Process each PDF file
for (pdf_file in pdfs) {
  text <- pdf_text(pdf_file)  # Extract text from the PDF
  text_combined <- paste(text, collapse = "\n")  # Combine text from all pages
  
  # Clean text by handling \n characters
  cleaned_text <- clean_text(text_combined)
  
  # Generate output .txt filename
  output_filename <- paste0(output_folder, "/", tools::file_path_sans_ext(basename(pdf_file)), ".txt")
  
  # Save extracted text to a .txt file
  writeLines(text_combined, output_filename)
  
  # Print status message
  message("Processed: ", pdf_file, " -> ", output_filename)
  
}



#### Set up large language model ----

# devtools::install_github("AlbertRapp/tidychatmodels")


# load in tidyverse and tidychatmodels
library(tidyverse)
library(tidychatmodels)

# Esit R environment variables to add openAI key
key <- names(read.delim('mistral-api-key.txt'))
Sys.setenv(MISTRAL_DEV_KEY = key)
'mistral-embed' # is this free?

# create chat with minstral bot
my_api_key <- Sys.getenv(key)

# create chatbot
chatbot1 <- create_chat('mistral', Sys.getenv('MISTRAL_DEV_KEY')) %>% 
  add_model('open-mistral-nemo') %>% 
  add_params('temperature' = 0.1) %>% # complexity of response
  add_message(role = 'system', 
              message = 'You answer questions.'
              )


# create output location
output_folder = 'data/sentiment-analysis/policy_text_open_mistral_nemo'

# Ensure output folder exists
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}


# define function to read in text files cleanly
clean_read <- function(x){clean_text(paste(read_lines(x),collapse = '\n'))}

# generate random text for comparison in LMM
pacman::p_load(OpenRepGrid)
writeLines(clean_text(paste(OpenRepGrid::randomSentences(100, 20), collapse = '\n')), 'data/sentiment-analysis/policy_text_extracted/random.txt')

# get all files 
policy_text <- list.files('data/sentiment-analysis/policy_text_extracted/')

policy_subset <- policy_text[c(1,2,3,5,6)]


query <- c('You answer the following question only based on the provided text, what is the relationship between biodiversity and poverty?', 
           'You answer the question what is the relationship between biodiversity and poverty? You use only the provided text, and no previous knowledge, to answer this question', 
           'You answer the question what is the relationship between biodiversity and poverty? To help answer this, use the provided text.')

for(i in 1){
  
  for(tokens in c(1000, 10000)){
    
    for(temp in c(0.1, 0.2, 0.3)){
      
      for(doc in policy_subset){
  
  output_folder <- paste0('data/sentiment-analysis/policy_text_open_mistral_nemo', 'temp', temp, 'query', i, 'token', tokens)
  
  # Ensure output folder exists
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  
  chatbot1 <- create_chat('mistral', Sys.getenv('MISTRAL_DEV_KEY')) %>% 
    add_model('open-mistral-nemo') %>% 
    add_params('temperature' = temp, 'max_tokens' = tokens) %>% # complexity of response
    add_message(role = 'system', 
                message = query[i]
    )
  
  # if(file.exists(paste0(output_folder, '/', str_replace(doc, '.txt', '.RDS')))){next}

   # add request to chatbot
   question_for_chat <- paste(clean_read(paste0('data/sentiment-analysis/policy_text_extracted/', 
                                                doc)))
   
   # add message to chatbot
   chatbot1_message_added <- chatbot1 %>% 
     add_message(role = 'user', 
                 message = question_for_chat)
   
   # run chat
   chatbot1_results <- chatbot1_message_added %>% 
     perform_chat()
   
   # save results as text file
   chatbot1_results %>% 
     extract_chat(silent = T) %>% 
     pluck("message", 3) %>% 
     write_lines(., paste0(output_folder, '/', doc))
   
   # save all results including role and query
   chatbot1_results %>% 
     extract_chat(silent = T) %>% 
     as_data_frame() %>% 
     saveRDS(., file = paste0(output_folder, '/', str_replace(doc, '.txt', '.RDS')))
   
   # print results for short evaluations
   chatbot1_results %>% 
     extract_chat(silent = T) %>% 
     pluck("message", 3) %>% 
     cat
  
   rm(chatbot1)
   Sys.sleep(5)
}
}
  }
}



#### Next, compile all the outputs and summarise the main sentiment of the text ----

meta_folders <- c('policy_text_open_mistral_nemotemp0.1query1token1000', 
                  'policy_text_open_mistral_nemotemp0.1query1token10000', 
                  'policy_text_open_mistral_nemotemp0.2query1token1000', 
                  'policy_text_open_mistral_nemotemp0.2query1token10000', 
                  'policy_text_open_mistral_nemotemp0.3query1token1000', 
                  'policy_text_open_mistral_nemotemp0.3query1token10000')

all_files <- unlist(flatten(lapply(meta_folders,
       function(x)
       list.files(paste0('data/sentiment-analysis/', x), pattern = '.RDS'))))


# read in all files - input this text as the metasummary to chatgpt4.0-turbo
bind_rows(lapply(paste0('data/sentiment-analysis/',meta_folders,
              '/', all_files[!is.na(str_extract(all_files, 'COP'))]), readRDS)) %>% 
  filter(role == 'assistant') %>% 
  pull(message) %>% 
  paste(collapse = "\n") %>% 
  clean_text %>% 
  gsub('\\\\', '', .) %>% 
  gsub('[^A-Za-z .1-9]', '', .)

# rio 1992
bind_rows(lapply(paste0('data/sentiment-analysis/',meta_folders,
                        '/', all_files[!is.na(str_extract(all_files, 'Rio'))]), readRDS)) %>% 
  filter(role == 'assistant') %>% 
  pull(message) %>% 
  paste(collapse = "\n") %>% 
  clean_text %>% 
  gsub('\\\\', '', .) %>% 
  gsub('[^A-Za-z .1-9]', '', .)


bind_rows(lapply(paste0('data/sentiment-analysis/',meta_folders,
                        '/', all_files[!is.na(str_extract(all_files, 'Aichii'))]), readRDS)) %>% 
  filter(role == 'assistant') %>% 
  pull(message) %>% 
  paste(collapse = "\n") %>% 
  clean_text %>% 
  gsub('\\\\', '', .) %>% 
  gsub('[^A-Za-z .1-9]', '', .)


bind_rows(lapply(paste0('data/sentiment-analysis/',meta_folders,
                        '/', all_files[!is.na(str_extract(all_files, 'GBF'))]), readRDS)) %>% 
  filter(role == 'assistant') %>% 
  pull(message) %>% 
  paste(collapse = "\n") %>% 
  clean_text %>% 
  gsub('\\\\', '', .) %>% 
  gsub('[^A-Za-z .1-9]', '', .)




