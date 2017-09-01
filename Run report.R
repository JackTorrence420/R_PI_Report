source("Report.r")

#sink writes out the report
sink(paste0(sub('\\..*', '', 'PIReport'), format(Sys.time(),'_%Y%m%d_%H%M%S'), '.txt'))


   for (csv in dir(pattern = ".csv$"))
   {

     reportParams =ReportParameters$new(dataFile=csv, showOriginal = FALSE,extractedTextLength=100L, instancesToShow=2L, printInstances =FALSE, wrapTextWidth=80L )
     report<-Report$new(reportParams)

     report$printTitle()
     report$printTotals()
     report$printExamples()

   }

sink()





