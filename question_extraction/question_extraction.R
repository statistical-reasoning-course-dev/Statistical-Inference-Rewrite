# Question extraction script
# Extract the questions and answers from

# install.packages("rvest")
# library("rvest")

library("stringr")

file = "11-mediation.Rmd"
chapter = str_extract(file, "[0-9]+")

document <- readLines(file)

# Extract question numbers

# Find lines with questions
questions.string <- str_extract(document, "question[0-9]+\\.[0-9]+\\.[0-9]+")

questions.number <- str_replace(questions.string, "question", "")

questions.number <- na.omit(questions.number)

questions.number <- as.vector(questions.number)

## find question and answer line

fileConn <- paste0("question_extraction/chapter", chapter ,"QA.html")
cat("<html><body>", file = fileConn)
cat(paste("<h1>Chapter", chapter, "</h1>"), file = fileConn, append=TRUE)


n.questions <- length(questions.number)

for(i in 1:n.questions ) {

# Checked correct selection with grep below
str_extract(document, paste0(questions.number[i], '("|\\))'))


question.answer <- str_which(document, paste0(questions.number[i], '("|\\))'))

question.text <- document[question.answer[1]:question.answer[2]]
answer.text   <- document[question.answer[3]:question.answer[4]]



cat(paste("<h2>Question", questions.number[i], "</h2>"), file = fileConn, append=TRUE)

cat(paste("<p>", question.text, "<p>"), file = fileConn, append=TRUE)

cat(paste("<p>", answer.text, "<p>"), file = fileConn, append=TRUE)

}

cat("</body></html>", file = fileConn, append=TRUE)



