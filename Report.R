


source("Data.r")
source("ObfuscateR.r")



ReportParameters <- R6Class(
  "ReportParameters",
  public = list(
    showOriginal = FALSE,
    extractedTextLength = 200L,
    instancesToShow = 100L,
    dataFile = "",
    printInstances = FALSE ,
    wrapTextWidth =80,
    #ctor
    initialize = function(showOriginal,
                          extractedTextLength,
                          instancesToShow,
                          dataFile,
                          printInstances,
                          wrapTextWidth) {
      self$showOriginal = showOriginal
      self$extractedTextLength = extractedTextLength
      self$instancesToShow = instancesToShow
      self$dataFile = dataFile
      self$printInstances = printInstances
      self$wrapTextWidth=wrapTextWidth
      
    }
    
  )
)



Report <- R6Class(
  "Report",
  public = list(
    #ctor
    initialize = function(reportParams) {
      #check File Exists
      if (!file.exists(reportParams$dataFile)) {
        stop('file "', reportParams$dataFile, '" does not exist', call. = FALSE)
      }
      
      
      
      private$reportParams = reportParams
      private$rdata = Data$new(dataFile = private$reportParams$dataFile)
      private$obfuscateR= ObfuscateR$new(private$reportParams)
      
      private$patientAgeOverMax <- private$rdata$overMaxAge()
      private$patientIdentifiers <-
        private$rdata$searchBlobColumn('PATIENT ID')
      
      private$surgicalNumbers <-
        private$rdata$searchBlobColumn('SURGICAL #')
      private$patientNameChanges <-
        private$rdata$searchBlobColumn('name changed')
      private$patientDateOfBirth <-
        private$rdata$searchBlobColumn('date of birth')
      private$validatedZipCodes <- private$rdata$validZipCodes()
      private$validPatDrawDates <-
        private$rdata$validPatientDrawDates()
      
      
      
      if(nrow(private$surgicalNumbers)>0){
      private$surgicalNumbers$obfuscatedtext<-NA
      for(i in 1:nrow(private$surgicalNumbers)) {
      private$surgicalNumbers[i,"obfuscatedtext"]<-
        private$obfuscateR$obfuscateString(private$obfuscateR$extractStringUsingPattern('SURGICAL #', private$surgicalNumbers[i,"resultmessage"]))
      
      }
      }
      if(nrow(private$patientIdentifiers)>0){
      private$patientIdentifiers$obfuscatedtext<-NA
      for(i in 1:nrow(private$patientIdentifiers)) {
        
       
        private$patientIdentifiers[i,"obfuscatedtext"]<-
          private$obfuscateR$obfuscateString(private$obfuscateR$extractStringUsingPattern('PATIENT ID', private$patientIdentifiers[i,"resultmessage"]))
        
                                          }
      }
      if(nrow(private$patientNameChanges)>0){
        

      private$patientNameChanges$obfuscatedtext<-NA
      for(i in 1:nrow(private$patientNameChanges)) {
        
        private$patientNameChanges[i,"obfuscatedtext"]<-
          private$obfuscateR$obfuscateString(private$obfuscateR$extractStringUsingPattern('name changed', private$patientNameChanges[i,"resultmessage"]))
       
      }
      
      }
      
    
      if(nrow(private$patientDateOfBirth)>0){
      private$patientDateOfBirth$obfuscatedtext<-NA
      for(i in 1:nrow(private$patientDateOfBirth)) {
        private$patientDateOfBirth[i,"obfuscatedtext"]<-
          private$obfuscateR$obfuscateString(private$obfuscateR$extractStringUsingPattern('date of birth', private$patientDateOfBirth[i,"resultmessage"]))
        
                                                  }
                                            }
      
    
    
    }
    ,
    NL = function() {
      writeLines("")
    }
    
    ,
    printTitle = function() {
      writeLines(paste0("Data Source File:      ","\t\t", basename(reportParams$dataFile)))
      writeLines(paste0("Rows in Dataset:       ","\t\t", private$rdata$totalRows()))
      self$NL()
    }
    #
    ,
    printTotals = function() {
      writeLines("Found PHI Totals:")
      
      private$printLength("Patient Identifiers:  ", private$patientIdentifiers)
      private$printLength("Surgical Numbers:     ", private$surgicalNumbers)
      private$printLength("Name Changes:         ", private$patientNameChanges)
      private$printLength("PAtient Age Over Max: ", private$patientAgeOverMax)
      private$printLength("Date Of Birth:        ", private$patientDateOfBirth)
      private$printLength("Validated Zip Codes:  ", private$validatedZipCodes)
      private$printLength("Patient Draw Dates:   ", private$validPatDrawDates)
      
      self$NL()
    }
    ,
    ShowTotalsdt=function(){
      
      return(data.table("type of PHI infraction" = c("Patient Identifiers:","Surgical Numbers:","Name Changes:","Patient Age Over Max:"
                                  ,"Date Of Birth:","Validated Zip Codes:","Patient Draw Dates:"),
                      "number of instances in dataset" = c(nrow(private$patientIdentifiers),
                                 nrow(private$surgicalNumbers),
                                 nrow(private$patientNameChanges),
                                 nrow(private$patientAgeOverMax),
                                 nrow(private$patientDateOfBirth),
                                 nrow(private$validatedZipCodes),
                                 nrow(private$validPatDrawDates)
                                )  
                      )
            )
    }
    

    ,
    
    getPatDates=function(){
      
      colnames(private$validPatDrawDates)[colnames(private$validPatDrawDates)=="patientdrawdate"] <- "patient draw date"
      return(private$validPatDrawDates)
    },
    
    getValidZipCodes=function(){
      
      colnames(private$validatedZipCodes)[colnames(private$validatedZipCodes)=="cleanzipcodes"] <- "patient address postal code"
      
      return(private$validatedZipCodes)
    },
    
    getPatAgeOverMax=function(){
      colnames(private$patientAgeOverMax)[colnames(private$patientAgeOverMax)=="patientage"] <- "patient age over max"
      
      return(private$patientAgeOverMax)
    },
    
    getSurgicalNumbers=function(){
      
      return(private$surgicalNumbers)
    },
    
    getPatientIdentifiers=function(){
      
      return(private$patientIdentifiers)
    },
    
    
    getNameChanges=function(){
      
      return(private$patientNameChanges)
    },
    
    getpatientDateOfBirth=function(){
      
      return(private$patientDateOfBirth)
    },
    
    
    
    printStringSearchInstances= function(title,instanceCollection, searchString){
      
      p<-private
      o<-p$obfuscateR
      
      writeLines(paste0(
        basename(p$reportParams$dataFile),
        "\t",
        title
      ))
      o$obfuscate(instanceCollection,
                  searchString)
      self$NL()
    }
    ,
    printFieldInstances=function(title, instanceCollection, columnName, rowDecorator){
      
      self$NL()
      if (nrow(instanceCollection) > 0) {
        writeLines(paste0(
          basename(private$reportParams$dataFile),
          "\t",
          title
        ))
        self$NL()
        for (i in 1:nrow(head(instanceCollection, n = private$reportParams$instancesToShow))) {
          row <- instanceCollection[i, ]
          writeLines(paste0(
            "Row:",
            "\t",
            format(paste0(as.character(
              as.integer(row$rownumber) + 1
            ), " "), justify = "right"),
            "\t\t",
            rowDecorator,
            "\t",
            row[columnName]
          ))
        }
        
        
      }
      
      self$NL()
      
    }
    
    ,
    
    printExamples = function() {
      
      
      p<-private
      o<-p$obfuscateR

      
      self$printStringSearchInstances("Patient Id Instances:",p$patientIdentifiers, 'PATIENT ID')
      
      self$printStringSearchInstances("Name Change Instances:",p$patientNameChanges, 'name changed')
      
      self$printStringSearchInstances("Surgical Number Instances:",p$surgicalNumbers, 'SURGICAL #')
      
      self$printStringSearchInstances("Date Of Birth Instances",p$patientDateOfBirth, 'date of birth')
      
      
      self$printFieldInstances("Sample Patient Age Over Max Instances:",p$patientAgeOverMax, "patientage" ,"Age:")
    
      self$printFieldInstances("Sample Zip Code Instances:",p$validatedZipCodes, "cleanzipcodes", "Zip:" )
      
      self$printFieldInstances("Sample Patient Draw Date Instances:",p$validPatDrawDates, "patientdrawdate", "Patient Draw Date:" )
      
    }
    
    
  )
  
  ,
  private = list(
    rdata = NULL,
    obfuscateR=NULL,
    reportParams = NULL,
    patientAgeOverMax = NULL,
    patientIdentifiers = NULL,
    surgicalNumbers = NULL,
    patientNameChanges = NULL,
    patientDateOfBirth = NULL,
    validatedZipCodes = NULL,
    validPatDrawDates = NULL,
    
    printLength = function(instancename, instance)
    {
      writeLines(paste0(instancename, "\t\t", nrow(instance)))
    }
    
  )
)



