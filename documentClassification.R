# load libraries
library(tidyverse)
library(Microsoft365R)
library(RSocrata)
library(pdftools)
library(tm)
library(loggit)
library(httr)
library(pdftools)

##########################################################
# Data Connections - SharePoint API 
##########################################################

# connecting to the SP site where documents are stored
site <- get_sharepoint_site(
  site_url = "https://usepa.sharepoint.com/sites/NPDESSectionTeam")
# getting data from document library in NPDES SP site 
permitTrackingSPList <- site$get_list("PermitTrackingDatabase") 
# getting pdf item data 
spList <- permitTrackingSPList$list_items()
# getting drive obj (used to download pdfs temporarily)
permitTrackingDatabaseDrive <- site$get_drive("PermitTrackingDatabase")
# getting drive files
driveFiles <- permitTrackingDatabaseDrive$list_files()
# creating list of file names
fileName <- driveFiles$name 

##########################################################
# Subset by Document Type
##########################################################

# define document types
permit <- "PermitApplication"
coverletter <- "CoverLetter"
noca <- "NOCA"
factsheet <- "Factsheet"

# selecting relevant columns
spList_v1 <- spList %>% 
  select(
    `SPDESNumber`, 
    `ApplicationID`,
    `DocumentType`,
    `PermitActionType`,
    `DocumentContent`,
    `DocumentStatus`,
    `PublicComment`,
    `EDP`,
    `ExDP`,
    `EDPM`,
    `EmailID`,
    `AttachID`,
    `EmailSubject`,
    `EmailDate`,
    `id`,
    `LinkFilenameNoMenu`,
    `LinkFilename`,
    `DocIcon`)

# sum the number of document types detected in files
spList_v2 <- spList_v1 %>% 
  mutate(
    permit = str_count(`DocumentType`, permit),
    coverletter = str_count(`DocumentType`, coverletter),
    noca =  str_count(`DocumentType`, noca),
    factsheet = str_count(`DocumentType`, factsheet),
    sumDoctype = rowSums(
      across(
        c(`permit`, `coverletter`,`noca`, `factsheet`)))
  )

# categorize file type by sum counts 
spList_v3 <- spList_v2 %>% 
  mutate(
    assignedDocType = case_when(
      sumDoctype == 1 & permit  == 1 ~ "Permit",
      sumDoctype == 1 & coverletter  == 1 ~ "CoverLetter",
      sumDoctype == 1 & noca  == 1 ~ "NOCA",
      sumDoctype == 1 & factsheet  == 1 ~ "Factsheet",
      sumDoctype > 1 ~ "Multi",
      sumDoctype == 0 ~ "Undefined"
    )
  )

# creating sample Dfs to test flows 
permitDf <- spList_v3 %>% 
  filter(assignedDocType == "Permit")
samplePermitFileName <- head(permitDf$LinkFilename, n=15)

coverLetterDf <- spList_v3 %>% 
  filter(assignedDocType == "CoverLetter")
sampleCoverLetterFileName <- head(coverLetterDf$LinkFilename, n=15)

factsheetDf <- spList_v3 %>% 
  filter(assignedDocType == "Factsheet")
sampleFactsheetFileName <- head(factsheetDf$LinkFilename, n=15)

multiDf <- spList_v3 %>% 
  filter(assignedDocType == "Multi")
sampleMultiFileName <- head(multiDf$LinkFilename, n=15)

nocaDf <- spList_v3 %>% 
  filter(assignedDocType == "NOCA")
sampleNOCAFileName <- head(nocaDf$LinkFilename, n=15)

# testing workflow for permit files 
# testid <- "DraftPermit.IndSPDES.NY0005801.2024-07-12.pdf"
# permitTrackingDatabaseDrive$download_file(src = testid,overwrite = TRUE)
# return <- pdf_text(testid) 
# file.remove(testid)

# defining empty list
returnList <- list()

# loop that downloads the file, reads the text, and should be saving a new list 
# every time, but is re-writing the data each time note can use pdf_data if 
# going by template, or pdf_text if we want to use the corpus function
for(i in samplePermitFileName) {
  
  permitTrackingDatabaseDrive$download_file(src = i, overwrite = TRUE)
  
  returnList[[i]] <- pdf_data(i) 
  
  file.remove(i)
  
}

##############################################################
# Developing Function for Document Classification
##############################################################

# here want to search for the following and list where the terms occur:
## "Dear" -> cover letter 
## "THIS IS NOT A PERMIT" -> NOCA (page one)
## "CC LIST For Complete Notice" -> NOCA (last page)
## "State Pollutant Discharge System (SPDES) Discharge Permit" -> permit 
###### "Page" -> Page number of permit 
## "SPDES Permit Fact Sheet" -> factsheet
###### "PAGE" -> Page number of factsheet

# for example if the document is titled CoverLetterNPDES123.pdf, and there is a 
# cover letter only, the df would be populated as:

exDf <- data.frame(fileName = "CoverLetterNPDES123.pdf",
                   docType = "CoverLetter", 
                   permit = "NA",
                   factsheet = "NA", 
                   NOCA = "NA", 
                   coverletter = 1,
                   pStart = "NA", 
                   pEnd = "NA",
                   fsStart = "NA",
                   fsEnd = "NA",
                   nocaStart = "NA",
                   nocaEnd = "NA",
                   clStart= 1)

# creating document classification function
docClassification <- function(x) {
  
  isCL <- str_detect(x$text, "Dear") |> # specifying if "Dear" appears classify as CL
    any() |>
    sum()
  
  nocaDf <- x |> 
    summarise(text = paste(text, collapse = " ")) # creating text col of all text from the doc
  
  isNOCA <- nocaDf |>
    pull(text) |> 
    str_detect("THIS IS NOT A PERMIT") |> # specifying if "THIS IS NOT A PERMIT" appears classify as CL
    any() |>
    sum()
  
  pDf <- x |> 
    summarise(text = paste(text, collapse = " ")) 
  
  isPermit <- pDf |> # terms to classify as permit
    pull(text) |> 
    str_detect("State Pollutant Discharge Elimination System \\(SPDES\\) DISCHARGE PERMIT") |> #
    any() |>
    sum()
  
  fsDf <- x |> 
    summarise(text = paste(text, collapse = " "))
  
  isFactsheet <- fsDf |> # terms to classify as factsheet 
    pull(text) |> 
    str_detect("SPDES Permit Fact Sheet") |> 
    any() |> 
    sum()
  
  dataframe <- data.frame(
    isCL = isCL,
    isNOCA = isNOCA,
    isPermit = isPermit,
    isFactsheet = isFactsheet
  )
  
  return(dataframe) # returns a df that specifies if those terms exist 
}

# creating vector of doc names (that were classified)
docnames <- names(returnList) 

## mapping the doc classification and page mapping functions to the returnList
doc_classified_list <- map(
  seq_len(length(returnList)),
  function(doc) {
    
    doc_list <- returnList[[doc]]
    
    page_list <- map(
      seq_len(length(doc_list)),
      function(page) {
        
        page_df <- docClassification(doc_list[[page]]) |>
          mutate(page = page)
        
        return(page_df)
        
      }
    )
    
    page_df <- dplyr::bind_rows(page_list) |>
      mutate(doc_name = docnames[doc])
    
    return(page_df)
    
  }
)

docAllPagesDf <- bind_rows(doc_classified_list) |>
  mutate(pCLPage = isCL * page,
         pPermitPage = isPermit * page,
         pFactsheetPage = isFactsheet * page,
         pNOCAPage = isNOCA *page) |> 
  select(doc_name,isCL,pCLPage,isPermit,pPermitPage,isFactsheet,pFactsheetPage,isNOCA,pNOCAPage,page)

# final doc classification output
docSummaryDf <- docAllPagesDf |> 
  # grouping by document name (so one row per PDF)
  group_by(doc_name) |>
  # summarizing to take max values of doc types
  summarise(
    coverletter = max(isCL),
    permit = max(isPermit),
    factsheet = max(isFactsheet),
    noca = max(isNOCA),
    # mapping pages where documents begin 
    # (note: this assumes one document type detected per PDF)
    clStart = max(ifelse(isCL == 1, pCLPage, 0), na.rm=TRUE),
    pStart = max(ifelse(isPermit == 1, pPermitPage,0), na.rm = TRUE),
    fStart = max(ifelse(isFactsheet == 1, pFactsheetPage,0), na.rm = TRUE),
    nStart = max(ifelse(isNOCA == 1,pNOCAPage,0),na.rm = TRUE)
  ) |> 
  mutate(
    # assigning document type based on doc classification cols
    # undefined docs should be flagged for manual review 
    docType = case_when(   
      rowSums(
        across(
          c(`coverletter`, `permit`, `factsheet`, `noca`)
        )
      ) >1 ~ "Multi",
      permit  == 1 ~ "Permit",
      coverletter  == 1 ~ "CoverLetter",
      noca  == 1 ~ "NOCA",
      factsheet  == 1 ~ "Factsheet",
      rowSums(
        across(
          c(`coverletter`, `permit`, `factsheet`, `noca`)
        )
      ) ==0 ~"Undefined")
  )

##########################################################
# Next Steps - Concatenate Results into .csv 
##########################################################

# To do: the next steps for this workflow is taking the results from the df 
# and concatenating them into a .csv. The workflow will save multiple .csv files 
# (in batches) and then combines them into one master file. 

