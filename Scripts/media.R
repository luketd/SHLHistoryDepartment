library(httr)
library(rvest)
library(xml2)
library(dplyr)
library(stringr)


# URL to the forum page
forum_url <- "https://simulationhockey.com/forumdisplay.php?fid=403"
#forum_url <- "https://simulationhockey.com/forumdisplay.php?fid=545"

# Send an HTTP GET request to get the last page number
response_last <- httr::GET(forum_url)
if (httr::status_code(response_last) != 200) {
  cat("Failed to retrieve the last page number. Status code:", httr::status_code(response_last), "\n")
  # Handle the error as needed
} else {
  # Extract the last page number
  last_page <- response_last %>%
    content(as = "text") %>%
    read_html() %>%
    html_node(".pagination_last") %>%
    html_text() %>%
    as.numeric()
}

# Initialize empty vectors to store data
ThreadName <- c()
User <- c()
Replies <- c()
tid_values <- c()

#smjhl: 687
#shl: 776
# Loop through each page
for (i in 1:687) {
  page_url <- paste0(forum_url, "&page=", i)
  response_page <- httr::GET(page_url)

  if (httr::status_code(response_page) != 200) {
    cat("Failed to retrieve page ", i, ". Status code:", httr::status_code(response_page), "\n")
    # Handle the error as needed
  } else {
    # Extract the page content
    content_page <- response_page %>%
      content(as = "text")

    # Parse the HTML content
    parsed_html <- read_html(content_page)

    # Extract data and append to vectors
    ThreadName <- c(ThreadName, parsed_html %>% html_nodes(".subject_new a") %>% html_text())
    User <- c(User, parsed_html %>% html_nodes(".author") %>% html_text())
    Replies <- c(Replies, parsed_html %>% html_nodes(".navh .float_right a") %>% html_text())
    tid_values <- c(tid_values, parsed_html %>% html_nodes(".subject_new a") %>%
                      html_attr("href") %>%
                      gsub("showthread.php\\?tid=([0-9]+)", "\\1", .))

    #if( length(ThreadName) != length(User)){
    #  print("Found the mismatch dummy")
    #  print(i)
    #}

  }
}

fixingNamesSMJHL <- data.frame(ThreadName, User, Replies, tid_values)

# Create a dataframe
#df <- data.frame(ThreadName, User, Replies, tid_values)

fixingNamesSMJHL <- fixingNamesSMJHL %>%
  mutate(FixUser = str_extract(User, "(?<=Topic started by )\\S+"))

dfSMJHL$date = 0
dfSMJHL$wordCount = 0

for (x in 1:13740){
  postLink <- paste0("https://simulationhockey.com/showthread.php?tid=",dfSMJHL$tid_values[x])

  response_page <- httr::GET(postLink)

  content_page <- response_page %>%
    content(as = "text")

  parsed_html <- read_html(content_page)

  post_date <- parsed_html %>%
    html_node(".post_date") %>%
    html_text()

  datetime <- strptime(post_date, format = "%m-%d-%Y, %I:%M %p")

# Extract the date
  date_only <- format(datetime, "%Y-%m-%d")

  post_body <- parsed_html %>%
    html_node(":not(.hide) :not(.option) .post_body.scaleimages") %>%
    html_text()


  xml_remove(xml_find_all(parsed_html, "//div[@class='hide']"))

  post_body_element <- parsed_html %>%
    html_node(".post_body.scaleimages")

# Extract the modified HTML structure
  post_body_html <- as.character(post_body_element)

  text <- gsub("<.*?>", "", post_body_html)
# Remove special characters and extra spaces
  text <- gsub("[[:punct:]]", " ", text)
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)

  words <- strsplit(text, " ")[[1]]

  text <- length(words)

  dfSMJHL$date[x] <- date_only
  dfSMJHL$wordCount[x] <- text

}

saveRDS(dfSMJHL, "SMJHL_Media")

dfSMJHL$ThreadName <- gsub("[\r\n]", " ", dfSMJHL$ThreadName)
dfSMJHL$User <- gsub("[\r\n]", "", dfSMJHL$User)

dfSMJHL$ThreadName <- gsub(",", "", dfSMJHL$ThreadName)
dfSMJHL$User <- gsub(",", "", dfSMJHL$User)

dfSMJHL <- dfSMJHL %>%
  mutate(FixUser = fixingNamesSMJHL$FixUser)

write.csv(dfSMJHL, "SMJHL_Media.csv", row.names = FALSE)

df <- readRDS("SHL_Media")

df <- df %>%
  mutate(FixUser = fixingNames$FixUser)

df$ThreadName <- gsub("[\r\n]", " ", df$ThreadName)
df$User <- gsub("[\r\n]", "", df$User)

df$ThreadName <- gsub(",", "", df$ThreadName)
df$User <- gsub(",", "", df$User)

write.csv(df, "SHL_Media.csv", row.names = FALSE)
