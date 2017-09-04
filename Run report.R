source("Report.r")

#sink writes out the report
sink(paste0(sub('\\..*', '', 'PIReport'), format(Sys.time(),'_%Y%m%d_%H%M%S'), '.txt'))


   for (csv in dir(pattern = ".csv$"))
   {
     reportParams =ReportParameters$new(dataFile=csv, showOriginal = TRUE,extractedTextLength=200L, instancesToShow=2L,  wrapTextWidth=80L )
     report<-Report$new(reportParams)
     report$printTitle()
     report$printTotals()
     report$printExamples()
   }

#test 1 report
# reportParams =
#   ReportParameters$new(dataFile="Copy of DeepSee_Diaceutics_20170208_20170228_v1.csv",
#                        showOriginal = FALSE,
#                        extractedTextLength=100L,
#                        instancesToShow=100L,
#                        wrapTextWidth=80L)
# report<-Report$new(reportParams)
# report$printTitle()
# report$printTotals()
# report$printExamples()

sink()





