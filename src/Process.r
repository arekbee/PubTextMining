
library(tm)
library(NLP)

uri <-file.path(".", "src/pdfs", "paper-to-review.pdf")
pdf <- readPDF(control = list(text = "-layout"))(elem = list(uri = uri),
                                                 language = "en",
                                                 id = "id1")

cor <- VCorpus(URISource(uri, mode = "", encoding = "UTF-8"),
               readerControl = list(reader = readPDF(engine = "ghostscript")))

inspect(cor)
getTransformations()
cor <- tm_map(cor, content_transformer(tolower))

cor <- tm_map(cor, removeNumbers)
cor <- tm_map(cor, removePunctuation)
cor <- tm_map(cor, stripWhitespace)

#cor <- tm_map(cor, removeSignature)

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
cor <- tm_map(cor, removeURL)


stopwords("english")
cor <- tm_map(cor, removeWords, stopwords("english"))


replace.str <- content_transformer(function (   x   , pattern ) gsub(pattern, " ", x))

cor <- tm_map(cor, replace.str, "â€“")
cor <- tm_map(cor, replace.str, "@")


cor <-  tm_map(cor, stemDocument)



inspect(cor)

cor  <- tm_map(cor, PlainTextDocument)
content(cor)

dtm <- DocumentTermMatrix(cor)
inspect(dtm)

freq <- colSums(as.matrix(dtm))
ord <- order(freq)
freq[tail(ord)] # Most frequent terms

tdm <- TermDocumentMatrix(cor,  control = list(wordLengths = c(1, Inf)))

findFreqTerms(dtm, 5)
findAssocs(dtm, "sigma", 0.01)

inspect(removeSparseTerms(dtm, 0.4))

dim(dtm)


dtms <- removeSparseTerms(dtm, 0.1)
dim(dtms)
inspect(dtms)
freq <- colSums(as.matrix(dtms)
                
                
df <- data.frame(term = names(freq), freq = freq)  

library(ggplot2)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
      xlab("Terms") + ylab("Count") + coord_flip()
                