library(tm)


#dictionary code taken from from http://bit.ly/2wgkBpv

ObfuscateR <- R6Class(
  "ObfuscateR",
  public = list(
  #ctor
  initialize =function(reportParams){
  
    
    private$reportParams=reportParams
  
    
    ##load dictionary data
    private$first.male <- read.table("dist.male.first.txt", stringsAsFactors = F)
    private$first.female <-
      read.table("dist.female.first.txt", stringsAsFactors = F)
    private$last.names <- read.table("dist.all.last.txt", stringsAsFactors = F)
  
    
    ##name table columns
    names(private$first.male) <- c("name", "perc", "cum.perc", "rank")
    names(private$first.female) <- c("name", "perc", "cum.perc", "rank")
    names(private$last.names) <- c("name", "perc", "cum.perc", "rank")
    
    #set dictionary exceptions
    private$exceptions <-read.table("exceptions.txt",stringsAsFactors = F)$V1    
    
 
    ##remove exceptions from dictionaries
    private$first.male <-
      subset(private$first.male,!tolower(name) %in% private$exceptions)
    private$first.female <-
      subset(private$first.female,!tolower(name) %in% private$exceptions)
    private$last.names <-
      subset(private$last.names,!tolower(name) %in% private$exceptions)
    private$first.names = c(private$first.male$name, private$first.female$name)
   
    
    
  },
  ##print out result message text 
  obfuscate= function(instanceCollection, pattern) {
    
    instanceCollection <- head(as.data.frame(instanceCollection),n = private$reportParams$instancesToShow)
    rowCount <-  nrow(instanceCollection)
    writeLines(paste0("Instances to process: ", "\t", rowCount))
    
    if (rowCount > 0)
      for (i in 1:nrow(instanceCollection)) {
        writeLines("")
        row <- instanceCollection[i, ]
        pos = regexpr(pattern, row$resultmessage)
        rowchar <- as.character(row$resultmessage)
        
        for (i in 1:private$reportParams$extractedTextLength)
          rowchar <- paste0(rowchar, ' ')
        
        #extract text from start of identifier
        rowchar <-
          substr(rowchar, as.integer(pos[1]), as.integer(pos[1]) + private$reportParams$extractedTextLength)
        
        if(reportParams$showOriginal==TRUE)
        {
          writeLines(paste0(" Row:",as.integer(row$rownumber)+1), " Original")
          writeLines(strwrap(rowchar, width=private$reportParams$wrapTextWidth))
        }
        
        
        possibleEntities = lapply(rowchar, self$get.any.names)
        
        for (RO in unlist(possibleEntities)[!(unlist(possibleEntities) %in% stopwords("en"))])
        {
          rowchar <- gsub(paste0('\\<', RO, '\\>'), '#', rowchar)
          
        }
        #obfuscate all number
        rowchar <- gsub('[[:digit:]]', '#', as.character(rowchar))
        writeLines(paste0("Row:",as.integer(row$rownumber)+1 , " Obfuscated"))
        writeLines(strwrap(rowchar, width=private$reportParams$wrapTextWidth))
        
      }
    writeLines("")
    
    
  },
  ##check text for person names
  get.any.names=function(text){
    text <- gsub("'s", "", text)
    text.split <- strsplit(text, " ")[[1]]
    text <- paste(gsub("[^a-zA-Z]", "", text.split), collapse = " ")
    
    unigrams.list <- private$unigrams(text)
    unigrams.list <-
      lapply(unigrams.list, function(x)
        gsub("[^a-zA-Z]", "", x))
    
    
    is.name <- function(unigram) {
      has.last.name <- tolower(unigram[1]) %in% tolower(private$last.names$name)
      
      has.first.name <- tolower(unigram[1]) %in% tolower(private$first.names)
      
      isname <- has.first.name | has.last.name
      
      return(isname)
    }
    if (length(unigrams.list) > 0) {
      name.indexes <- sapply(unigrams.list, is.name)
      unigrams.list <- unigrams.list[name.indexes]
      name.vec <-
        sapply(unigrams.list, function(x)
          paste(x, collapse = " "))
    } else{
      name.vec <- ""
    }
    
    return(name.vec)
    
  }
  
  ),
  
  
  private =list(
    first.names=NA,
    first.male=NA,
    first.female=NA,
    last.names=NA,
    exceptions=NA,
    reportParams=NA,
    word.vec=NA,
    unigrams=function(text){
      word.vec <- strsplit(text, "\\s+")[[1]]
    }
    
    
    
  )

)


