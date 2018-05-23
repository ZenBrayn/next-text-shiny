
library(tm)
library(lubridate)

# The tokenizers
SentenceTokenizer <- function(x) {
  RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1, delimiters = ".;"))
}

GramTokenizer <- function(x, n) {
  RWeka::NGramTokenizer(x, RWeka::Weka_control(min = n, max = n, delimiters = " \\r\\n\\t.,;:\"()?!")) 
}

cleanupText <- function(inpstr) {
  inpstr <- removeNumbers(inpstr)
  inpstr <- stripWhitespace(inpstr)
  inpstr <- tolower(inpstr)
  return(inpstr)
}

# This is the main tokenization function
getNGramTokens <- function(inpstr, ngram=1) {
  # Clean-up
  inpstr <- cleanupText(inpstr)

  # First parse into individual sentences
  tokens <- SentenceTokenizer(inpstr)
  # Now remove any remaining punctuation
  tokens <- removePunctuation(tokens)

  # Now parse into the NGram tokens
  tokens <- GramTokenizer(tokens, ngram)

  return(tokens)
}


countNGrams <- function(fp, n = 1) {
  # This is a vector to store the counts
  ngram.cnts <- c()

  # Open the file for reading
  inp <- file(fp, "r")
  line <- readLines(inp)

  # Tokenize
  tokens <- getNGramTokens(line, ngram = n)

  # Count them up
  ngram.cnts <- table(tokens)

  close(inp)

  ngram.cnts <- sort(ngram.cnts, decreasing=TRUE)
  return(ngram.cnts)
}



countNGramsNLinesPerRead <- function(fp, n = 1, nlines.per.read=1) {
  # This is a vector to store the counts
  ngram.cnts <- c()

  # Open the file for reading
  inp <- file(fp, "r")
  line <- readLines(inp, nlines.per.read)

  line.cntr <- 1

  while (length(line) > 0) {
    if (line.cntr %% 1000 == 0) {
      print(paste(fp, ": line ", line.cntr, ", ", now(), ", ", length(ngram.cnts), " entries", sep=""))
    }

    # Tokenize
    tokens <- getNGramTokens(line, ngram = n)

    # Count them up
    token.cnts <- table(tokens)
    # If there's nothing to count, move on to the next one
    if (length(token.cnts) == 0) {
      line <- readLines(inp, 1)
      line.cntr <- line.cntr + 1
      next
    }
    
    # Count up into overall vectors
    for (nm in names(token.cnts)) {
      ngram.cnts[nm] <- sum(ngram.cnts[nm], token.cnts[nm], na.rm=TRUE)
    }

    # Read the next line
    line <- readLines(inp, 1)
    line.cntr <- line.cntr + 1
  }

  close(inp)

  ngram.cnts <- sort(ngram.cnts, decreasing=TRUE)
  return(ngram.cnts)
}


countNGramsSplitFiles <- function(data.dir, n = 1) {
  all.files <- list.files(data.dir)

  ngram.cnts <- data.frame()

  for (f in all.files) {
    print(paste(n, ": ", f, sep=""))
    
    fp <- file.path(data.dir, f)
    cnts <- countNGrams(fp, n)
    cnt.df <- data.frame(token=names(cnts), cnt=cnts)

    # Get a cumulative data frame
    tdf <- rbind(ngram.cnts, cnt.df)
    # Add up the counts
    tdf.cnt <- with(tdf, tapply(cnt, token, sum))
    ngram.cnts <- data.frame(token=names(tdf.cnt), cnt=tdf.cnt)
  }

  ngram.cnts <- ngram.cnts[order(ngram.cnts$cnt, decreasing=TRUE),]

  return(ngram.cnts)
}

countAndSaveNGramData <- function(data.dir, n=1) {
  n.map <- c("1"="uni", "2"="bi", "3"="tri", "4"="quad")

  ngram.cnts <- countNGramsSplitFiles(data.dir, n)
  write.table(ngram.cnts, paste(n.map[as.character(n)], "gram_count_df.tsv", sep=""), row.names=FALSE, quote=FALSE, sep="\t")

  ngram.cnts.vector <- as.numeric(ngram.cnts$cnt)
  names(ngram.cnts.vector) <- as.character(ngram.cnts$token)
  saveRDS(ngram.cnts.vector, paste(n.map[as.character(n)], "gram_counts.rds", sep=""))
}


