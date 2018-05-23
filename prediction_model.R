source("ngram_utils.R")

# A mapping function to get the name of the files
# using integer n
n.map <- c("1"="uni", "2"="bi", "3"="tri", "4"="quad")

loadNGramCounts <- function(n, src) {
  if (src == "news") {
    data.dir <- file.path("NGramCounts", "News")
  } else if (src == "blogs") {
    data.dir <- file.path("NGramCounts", "Blogs")
  } else if (src == "twitter") {
    data.dir <- file.path("NGramCounts", "Twitter")
  } else {
    stop("Source is not correct; must be {news, blogs, twitter}")
  }

  ngram.cnts <- list()
  for (ni in 1:n) {
    fn <- paste(n.map[as.character(ni)], "gram_counts.rds", sep="")
    fp <- file.path(data.dir, fn)
    print(paste("Loading ", fp, sep=""))
    cnts <- readRDS(fp)
    ngram.cnts[[ni]] <- cnts
  }

  return(ngram.cnts)
}

initializeModelFromSource <- function(n, src) {
  print(paste("Initializing ", n.map[as.character(n)], "-gram model using source ", src, sep=""))

  # Load the 1:n gram counts
  ngram.cnts <- loadNGramCounts(n, src)

  mdl <- list()
  mdl$n <- n
  mdl$src <- src
  mdl$ngram.cnts <- ngram.cnts

  return(mdl)
}

initializeModel <- function(mdl.data) {
  print(paste("Reading model data from", mdl.data))

  # The model data contains the ngram counts
  ngram.cnts <- readRDS(mdl.data)
  # Figure out n from the length of the model
  n <- length(ngram.cnts)

  src <- mdl.data

  mdl <- list()
  mdl$n <- n
  mdl$src <- src
  mdl$ngram.cnts <- ngram.cnts

  return(mdl)
}

tokenizeInputText <- function(txt, n) {
  tokens <- getNGramTokens(txt, n)
  # Get the last one
  token <- tokens[length(tokens)]
  return(token)
}

predictNextWordTopN <- function(mdl, txt, top.n, output.cnts=TRUE, output.probs=TRUE) {
  # Special case for uni-gram model
  # Just return the most frequent word
  if (mdl$n == 1) {
    ngram.cnts <- mdl$ngram.cnts[[1]]
    idx.sel <- which(ngram.cnts == max(ngram.cnts))
    top.hit <- names(ngram.cnts[idx.sel])
    top.hit.cnt <- max(ngram.cnts)
    names(top.hit.cnt) <- top.hit

    if (output.probs) {
      top.hit.cnt <- top.hit.cnt / top.hit.cnt
    }

    if (output.cnts) {
      return(top.hit.cnt)
    } else {
      return(top.hit)
    }
  }

  # First tokenize the input text
  # and get the last mdl$n - 1 words
  token <- tokenizeInputText(txt, (mdl$n - 1))

  # add a trailing space to the token
  # This will be the lookup key in the mdl$n gram counts
  token <- paste(token, " ", sep="")

  # Look up the token to get all the possibilities
  ngram.cnts <- mdl$ngram.cnts[[mdl$n]]
  idx.sel <- which(grepl(paste("^", token, sep=""), names(ngram.cnts)))

  if (length(idx.sel) == 0) {
    return(NULL)
  }

  # Get the top n hits
  ngram.cnts <- ngram.cnts[idx.sel]
  # This is the probability normalizaiton factor
  tot.cnts <- sum(ngram.cnts)

  ngram.cnts <- sort(ngram.cnts, decreasing=TRUE)
  if (length(ngram.cnts) >= top.n) {
    n <- top.n
  } else {
    n <- length(ngram.cnts)
  }

  top.hits <- ngram.cnts[1:n]
  top.hits <- names(top.hits)

  # Split into unigrams and get the last one
  nxt.words <- lapply(top.hits, getNGramTokens, 1)
  nxt.words <- sapply(nxt.words, function(x) x[length(x)])

  nxt.word.cnts <- ngram.cnts[1:n]
  names(nxt.word.cnts) <- nxt.words
  if (output.probs) {
    nxt.word.cnts <- nxt.word.cnts / tot.cnts
  }

  if (output.cnts) {
    return(nxt.word.cnts)
  } else {
    return(nxt.words)
  }
}


# predictNextWord <- function(mdl, txt) {
#   # Special case for uni-gram model
#   # Just return the most frequent word
#   if (mdl$n == 1) {
#     ngram.cnts <- mdl$ngram.cnts[[1]]
#     idx.sel <- which(ngram.cnts == max(ngram.cnts))
#     top.hit <- names(ngram.cnts[idx.sel])
#     return(top.hit)
#   }

#   # First tokenize the input text
#   # and get the last mdl$n - 1 words
#   token <- tokenizeInputText(txt, (mdl$n - 1))

#   # add a trailing space to the token
#   # This will be the lookup key in the mdl$n gram counts
#   token <- paste(token, " ", sep="")

#   # Look up the token to get all the possibilities
#   ngram.cnts <- mdl$ngram.cnts[[mdl$n]]
#   idx.sel <- which(grepl(paste("^", token, sep=""), names(ngram.cnts)))

#   if (length(idx.sel) == 0) {
#     return(NA)
#   }

#   # Get the top hit
#   top.hit <- which(ngram.cnts[idx.sel] == max(ngram.cnts[idx.sel]))
#   top.hit <- names(top.hit)

#   # Split into unigrams and get the last one
#   nxt.word <- getNGramTokens(top.hit, 1)
#   nxt.word <- nxt.word[length(nxt.word)]

#   return(nxt.word)
# }

predictNextWord <- function(mdl, txt, output.cnts=TRUE, output.probs=TRUE) {
  return(predictNextWordTopN(mdl, txt, 1, output.cnts, output.probs))
}

predictNextWordWithBackoff <- function(mdl, txt, output.cnts=TRUE, output.probs=TRUE) {
  print(paste("Trying ", mdl$n, "-gram model", sep=""))
  nxt.word <- predictNextWord(mdl, txt, output.cnts, output.probs)

  if (is.null(nxt.word)) {
    new.mdl <- mdl
    new.mdl$n <- new.mdl$n - 1
    predictNextWordWithBackoff(new.mdl, txt, output.cnts)
  } else {
    return(nxt.word)
  }
}

predictNextWordWithBackoffTopN <- function(mdl, txt, top.n, output.cnts=TRUE, output.probs=TRUE) {
  print(paste("Trying ", mdl$n, "-gram model", sep=""))
  nxt.words <- predictNextWordTopN(mdl, txt, top.n, output.cnts, output.probs)

  if (is.null(nxt.words)) {
    new.mdl <- mdl
    new.mdl$n <- new.mdl$n - 1
    predictNextWordWithBackoffTopN(new.mdl, txt, top.n, output.cnts, output.probs)
  } else {
    return(list(nxt.words = nxt.words, mdl.n = mdl$n))
  }
}

plotNextWordFreqs <- function(next_words) {
  freq.df <- data.frame(word=names(next_words), freq=as.numeric(next_words))
  freq.df <- freq.df[order(freq.df$freq, decreasing=TRUE),]
  freq.df$word <- factor(freq.df$word, levels=unique(freq.df$word))
  
  p.1 <- ggplot(freq.df, aes(word, freq)) + geom_bar(stat="identity") +
          xlab("Word") + ylab("Probability") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          ggtitle("Top Word Probabilities")
  
  return(p.1)
}


