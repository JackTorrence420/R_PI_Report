


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
    
    getPatDates=function(){
      
      return(private$validPatDrawDates)
    },
    
    printExamples = function() {
      writeLines(paste0(
        basename(private$reportParams$dataFile),
        "\t",
        "Patient Id Instances:"
      ))
      private$obfuscateR$obfuscate(private$patientIdentifiers,
                'PATIENT ID')
      self$NL()
      writeLines(paste0(
        basename(private$reportParams$dataFile),
        "\t",
        "Name Change Instances:"
      ))
      private$obfuscateR$obfuscate(private$patientNameChanges,
                'name changed')
      self$NL()
      writeLines(paste0(
        basename(private$reportParams$dataFile),
        "\t",
        "Surgical Number Instances:"
      ))
      private$obfuscateR$obfuscate(private$surgicalNumbers, 'SURGICAL #')
      self$NL()
      writeLines(paste0(
        basename(private$reportParams$dataFile),
        "\t",
        "Date Of Birth Instances"
      ))
      private$obfuscateR$obfuscate(private$patientDateOfBirth,
                'date of birth')
      self$NL()
      
      
      self$NL()
      if (nrow(private$patientAgeOverMax) > 0) {
        writeLines(paste0(
          basename(private$reportParams$dataFile),
          "\t",
          "Sample Patient Age Over Max Instances:"
        ))
        self$NL()
        for (i in 1:nrow(head(private$patientAgeOverMax, n = 100))) {
          row <- private$patientAgeOverMax[i, ]
          writeLines(paste0(
            "Row:",
            "\t",
            format(paste0(as.character(
              as.integer(row$rownumber) + 1
            ), " "), justify = "right"),
            "\t\t",
            "Age:",
            "\t",
            row$patientage
          ))
        }
        
        
      }
      
      self$NL()
      if (nrow(private$validatedZipCodes) > 0)
      {
        writeLines(paste0(
          basename(private$reportParams$dataFile),
          "\t",
          "Sample Zip Code Instances:"
        ))
        self$NL()
        for (i in 1:nrow(head(private$validatedZipCodes, n = 100))) {
          row <- private$validatedZipCodes[i, ]
          writeLines(paste0(
            "Row:",
            "\t",
            as.integer(row$rownumber) + 1,
            "\t\t",
            "Zip:",
            "\t",
            row$cleanzipcodes
          ))
        }
      }
      self$NL()
      
      
      if (nrow(private$validPatDrawDates) > 0)
      {
        writeLines(paste0(
          basename(private$reportParams$dataFile),
          "\t",
          "Sample Patient Draw Date Instances:"
        ))
        self$NL()
        for (i in 1:nrow(head(private$validPatDrawDates, n = 100))) {
          row <- private$validPatDrawDates[i, ]
          writeLines(
            paste0(
              "Row:",
              "\t",
              as.integer(row$rownumber) + 1,
              "\t\t",
              "Patient Draw Date:",
              "\t",
              as.Date(row$patientdrawdate, format = '%m/%d/%Y')
            )
          )
        }
      }
      self$NL()
      
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



