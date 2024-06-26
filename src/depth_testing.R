library(dataRetrieval)


parameterCd <- "72178" #meters
parameterCd <- "72199" #feet

pCode <- readNWISpCode(parameterCd)

t <- whatNWISsites(stateCd = "ny",
              parameterCd = "72199")

outputData <- readNWISdata(stateCd = "ny",
                           siteStatus = "all",
                           service="site")

siteCd <- "430056078044801"