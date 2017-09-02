


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
      
      private$surgicalNumbers$obfuscatedtext<-NA

      for(i in 1:nrow(private$surgicalNumbers)) {
      private$surgicalNumbers[i,"obfuscatedtext"]<-
        private$obfuscateR$obfuscateString(private$obfuscateR$extractStringUsingPattern('SURGICAL #', private$surgicalNumbers[i,"resultmessage"]))
      
      }
    
     # print(head(self$getSurgicalNumbers()[,c("rownumber","obfuscatedtext")],n=500))
      #print(private$surgicalNumbers$obfuscatedtext)
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
      
      return(data.table(Title = c("Patient Identifiers:","Surgical Numbers:","Name Changes:","Patient Age Over Max:","Date Of Birth:","Validated Zip Codes:","Patient Draw Dates:"),
                      Counts = c(nrow(private$patientIdentifiers),
                                 nrow(private$surgicalNumbers),
                                 nrow(private$patientNameChanges),
                                 nrow(private$patientAgeOverMax),
                                 nrow(private$patientDateOfBirth),
                                 nrow(private$validatedZipCodes),
                                 nrow(private$validPatDrawDates)
                                      
                     )  ))
      
    }
    
    
    
    
    ,
    
    getPatDates=function(){
      
      return(private$validPatDrawDates)
    },
    
    
    getSurgicalNumbers=function(){
      
      return(private$surgicalNumbers)
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



