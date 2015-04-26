
library(shiny)
library(tm)

load('unigramdf.RData')
load('bipred.RData')
load('tripred.RData')
load('quadpred.RData')

load('dict.RData')


# function to clean EN text
cleanEnText <- function (txt, bRemoveStopWords) {
  txt <- tolower(txt)
  #cat(sprintf("before: %s\n", txt), file=stderr())
  
  txt <- gsub("'m ", " am ", txt)
  txt <- gsub("'s ", " ", txt)
  txt <- gsub("'ll ", " will ", txt)
  txt <- gsub("'ve ", " have ", txt)
  txt <- gsub("'d ", " would ", txt)
  txt <- gsub("n't ", " not ", txt)
  
  txt <- gsub(" r ", " are ", txt)
  txt <- gsub(" u ", " you ", txt)
  txt <- gsub(" n ", " and ", txt)
  
  txt <- gsub(" wld ", " would ", txt)
  txt <- gsub(" cld ", " could ", txt)
  txt <- gsub(" thru ", " through ", txt)
  
  txt <- gsub(" d; ", " ", txt)
  txt <- gsub(" p; ", " ", txt)
  
  
  # cleaning the data:
  # 1. convert to lower case --- expected to be done
  #txt <- tolower(txt)
  # 2. remove extra whitespace
  txt <- stripWhitespace(txt)
  # 3. remove bad words
  if(bRemoveStopWords) txt <- removeWords(txt, stopwords('english'))
  # 4. remove punctuation
  txt <- removePunctuation(txt)
  # 5. remove numbers
  txt <- removeNumbers(txt)
  
  # stem documents
  #txt <- stemDocument(txt)

  #cat(sprintf("after: %s\n", txt), file=stderr())
  return( txt )
}

getTopNGram <- function( ngram.df , term, n_top ) {
  ret.df <- data.frame(pred=character(0), count= integer(0))
  
  res.df <- ngram.df[which(term==ngram.df$pre),]
  n <- nrow(res.df)
  #cat(as.character(res.df[1:5,]), file=stderr())
  #cat(sprintf("\nlooking for:%s , here found=%d\n", term, n), file=stderr())
  
  if( n > 0 ) {
    topTerms <- res.df[order(res.df$count, decreasing=TRUE), c('pred', 'count')]
  
    if(0 != n_top)  n <- ifelse(n_top > n, n, n_top)
  
    topTerms$pred  <- as.character(topTerms$pred)
    topTerms$count <- as.integer(topTerms$count)
    
    ret.df <- topTerms[1:n,]
  }
  
  return( ret.df )
}


predictWordNgram <- function( sentence, grep_pred ) {
  res1.ngram <- data.frame(pred=character(0), count= integer(0))
  res2.ngram <- data.frame(pred=character(0), count= integer(0))
  top.ngram  <- data.frame(pred=character(0), count= integer(0))
  
  en_txt <- ''
  
  en_txt <- cleanEnText(sentence, TRUE)
  res1.ngram <- predictor(en_txt, grep_pred)
  iRes1 <- nrow(res1.ngram)
  
  en_txt <- cleanEnText(sentence, FALSE)
  res2.ngram <- predictor(en_txt, grep_pred)  
  iRes2 <- nrow(res2.ngram)
  
  if( iRes1 > 0 )  top.ngram <- res1.ngram
  if( iRes2 > 0 )  top.ngram <- rbind(top.ngram, res2.ngram)
    
  if( (iRes1>0) && (iRes2>0) ) top.ngram <- aggregate(count ~ pred, top.ngram, FUN=sum)
  if( (iRes1 + iRes2) > 0 ) top.ngram <- top.ngram[order(top.ngram$count, decreasing=TRUE), ]
  
  #cat(sprintf("iRes1=%d, iRes2=%d\n", iRes1, iRes2), file=stderr())
  return( top.ngram )
}

predictor <- function( en_txt, grep_pred ) {
  top.ngram <- data.frame(pred=character(0), count= integer(0))
  
  iBiChoice   <- 0
  iTriChoice  <- 0
  iQuadChoice <- 0
  
  no_ngram     <- 0
  no_trigram   <- 0
  no_quadigram <- 0
  
    
  words <- unlist(strsplit(en_txt, split=" "))
  len   <- length(words)
  
  
  top.trigram <- getTopNGram(triterm.pred, paste(words[len-1],words[len]), 0)
  if( '' != grep_pred ) {
    rows <- grep(grep_pred, top.trigram$pred)
    top.trigram <- top.trigram[rows,]
  }
  iTriChoice <- nrow(top.trigram[complete.cases(top.trigram),])
  #cat(sprintf("actually looking: %s [%s], found=%d\n", paste(words[len-1],words[len]), grep_pred, iTriChoice), file=stderr())
  
  
  if( iTriChoice > 0) { #possible to have quadigram
    top.quadigram <- getTopNGram(quaditerm.pred, paste(words[len-2],paste(words[len-1],words[len])), 0)
    if( '' != grep_pred ) {
      rows <- grep(grep_pred, top.quadigram$pred)
      top.quadigram <- top.quadigram[rows,]
    }
    iQuadChoice <- nrow(top.quadigram[complete.cases(top.quadigram),])
  }
  
  if( 0 != iQuadChoice ) {
    no_quadigram <- ifelse( (iQuadChoice >= 2), 2, 1 )
    top.ngram <- top.quadigram[order(top.quadigram$count, decreasing=TRUE), ][1:no_quadigram,]
    no_ngram <- no_quadigram
  }
  
  if( 0 != iTriChoice ) {
    no_trigram <- ifelse( ((iTriChoice+no_quadigram) >= 3),
                          ifelse( (iTriChoice >= 3), 3, iTriChoice ),
                          iTriChoice )
    top.ngram <- rbind(top.ngram, top.trigram[order(top.trigram$count, decreasing=TRUE), ][1:no_trigram,])
    
    top.ngram <- aggregate(count~pred, top.ngram, FUN=sum)
    no_ngram  <- nrow(top.ngram[complete.cases(top.ngram),])
  }
  
  
  if( no_ngram < 3 ) { #still need bigram
    top.bigram <- getTopNGram(biterm.pred, words[len], 0)
    if( '' != grep_pred ) {
      rows <- grep(grep_pred, top.bigram$pred)
      top.bigram <- top.bigram[rows,]
    }
    iBiChoice <- nrow(top.bigram[complete.cases(top.bigram),])
    
    if( iBiChoice > 0) {
      no_bigram <- ifelse( ((iBiChoice+no_ngram) >= 3), (3-no_ngram), iBiChoice ) #limit bigram
      top.ngram <- rbind(top.ngram, top.bigram[order(top.bigram$count, decreasing=TRUE), ][1:no_bigram,])
      
      top.ngram <- aggregate(count~pred, top.ngram, FUN=sum)
    }
  }

  #cat(sprintf("iQuadChoice=%d, iTriChoice=%d, iBiChoice=%d\n", iQuadChoice, iTriChoice, iBiChoice), file=stderr())
  return( top.ngram )
}




predictWord <- function( txt ) {
  s <- '<h4>...</h4>'
  
  if(' ' == substr(txt, nchar(txt), nchar(txt))) {
    word.predict <- predictWordNgram(txt, '')
    
    if( nrow(word.predict[complete.cases(word.predict),]) > 0 ) {
      s <- sprintf("<h3><strong>[1] %s</strong></h3>  <em>[2] %s , [3] %s</em>",    
                  word.predict[1,]$pred,word.predict[2,]$pred,word.predict[3,]$pred)
    }
  }
  else if( nchar(txt) > 0 ) {
    words <- unlist(strsplit(txt, split=" "))
    len   <- length(words)
    
    word.predict <- predictWordNgram(paste(paste(words[1:len-1], collapse=" "), ""), sprintf("^%s",words[len]))
    no_res <- nrow(word.predict[complete.cases(word.predict),])
    
    if( 0 == no_res ) { #no result
       #check Unigram and Dictionary(??)
      rows   <- grep(sprintf("^%s", words[len]), unigram.df$term)
      no_res <- length(rows)
    
      if( 0 == no_res ) { #no result from ngram, check Dictionary
        rows <- grep(sprintf("^%s", words[len]), dict)
        no_res <- length(rows)
        
        if( no_res > 0 ) { #result from dictionary
          word.predict <- dict[rows]
          
          s <- sprintf("<h3><strong>[1] %s</strong></h3>  <em>[2] %s , [3] %s</em>",    
                       word.predict[1],word.predict[2],word.predict[3])
        }
        #else, no result from everywhere!
      }
      else { #result from unigram
        word.predict <- unigram.df[rows,]
        word.predict <- word.predict[order(word.predict$count, decreasing=TRUE), ]

        s <- sprintf("<h3><strong>[1] %s</strong></h3>  <em>[2] %s , [3] %s</em>",    
                     word.predict[1,]$term,word.predict[2,]$term,word.predict[3,]$term)
      }
    }
    else { #result from ngram
      s <- sprintf("<h3><strong>[1] %s</strong></h3>  <em>[2] %s , [3] %s</em>",    
                   word.predict[1,]$pred,word.predict[2,]$pred,word.predict[3,]$pred)
    }
  }
  
  as.character(s)
}