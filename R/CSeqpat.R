#' Mining Frequent Contiguous Sequential Patterns in a Text Corpus
#'
#'
#' Takes in the filepath and minimum support and performs pattern mining
#' @param filepath Path to the text file/text corpus
#' @param phraselenmin Minimum number of words in a phrase
#' @param phraselenmax Maximum number of words in a phrase
#' @param minsupport Minimum absolute support for mining the patterns
#' @param docdelim Document delimiter in the corpus
#' @param stopword Remove stopwords from the document corpus (boolean)
#' @param stemword Perform stemming on the document corpus (boolean)
#' @param lower Lower case all words in document corpus (boolean)
#' @param removepunc Remove punctuations from document corpus (boolean)
#' @return A dataframe containing the frequent phrase patterns with their absolute support
#' @import utils
#' @import tm
#' @import NLP
#' @export
#' @examples
#' test1 <- c("hoagie institution food year road ",
#' "place little dated opened weekend fresh food")
#' tf <- tempfile()
#' writeLines(test1, tf)
#' CSeqpat(tf,1,2,2,"\t",TRUE,FALSE,TRUE,FALSE)
CSeqpat <- function (filepath, phraselenmin=1, phraselenmax=99999, minsupport=1, docdelim, stopword=FALSE, stemword=FALSE, lower=FALSE, removepunc=FALSE) {

  loadNamespace("tm")
  loadNamespace("NLP")

  Terms_vector <- vector()
  Terms_temp_df <- data.frame()
  Terms_final_seq_pattern_df <- data.frame()

  n <- phraselenmin

  docs_inp <- utils::read.csv(filepath,header=FALSE,sep = docdelim,col.names=c("text"),stringsAsFactors = FALSE)

  if(lower==TRUE) {
    docs_inp[] <- lapply(docs_inp, tolower)
  }

  docs_inp <- cbind(doc_id=seq(1:nrow(docs_inp)),docs_inp)
  vcorp <- tm::VCorpus(tm::DataframeSource(docs_inp))

  vcorp <- tm::tm_map(vcorp,stripWhitespace)

  if(stopword==TRUE) {
    vcorp <- tm::tm_map(vcorp, removeWords, stopwords("english"))
  }

  if(stemword==TRUE) {
    vcorp <- tm::tm_map(vcorp, stemDocument)
  }

  if(removepunc==TRUE) {
    vcorp <- tm::tm_map(vcorp, removePunctuation)
  }

  repeat {

    MgramTokenizer <- function(x) unlist(lapply(NLP::ngrams(NLP::words(x), n), paste, collapse = " "), use.names = FALSE)

    tdmm <- tm::TermDocumentMatrix(vcorp, control = list(tokenize = MgramTokenizer))

    Terms_temp_df <- as.data.frame(tm::findFreqTerms(tdmm,minsupport),stringsAsFactors = FALSE)

    if (length(Terms_temp_df) == 0 || (length(Terms_temp_df) > 0 && is.na(Terms_temp_df[1,1])) || n > phraselenmax)
    {
      break
    }
    else
    {
      n <- n+1
    }

    for (k in 1 : nrow(Terms_temp_df)) {

      sum_df <- as.data.frame(tm::tm_term_score(tdmm,Terms_temp_df[k,1]))

      for (sum_row in 1 : nrow(sum_df))
      {
        if (sum_df[sum_row,1] > 1) { sum_df[sum_row,1] <- 1
        }
      }

      Terms_final_seq_pattern_df <- rbind.data.frame(Terms_final_seq_pattern_df, c(Terms_temp_df[k,1],colSums(sum_df)[[1]]),stringsAsFactors = FALSE)

    }

  }


  if(length(Terms_final_seq_pattern_df) > 0) {

    colnames(Terms_final_seq_pattern_df) <- c("Freq_Phrases","Support")

    Terms_final_seq_pattern_df <- Terms_final_seq_pattern_df[Terms_final_seq_pattern_df$Support >= minsupport,]

  }

  return(Terms_final_seq_pattern_df)

}
