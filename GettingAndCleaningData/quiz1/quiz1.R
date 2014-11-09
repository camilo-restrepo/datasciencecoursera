library(xlsx)
library(XML)
data <- read.csv("getdata-data-ss06hid.csv")
q1 <- nrow(subset(data, VAL >= 24))
cols <- 7:15
rows <- 18:23

dat <- read.xlsx("getdata-data-DATA.gov_NGAP.xlsx", sheetIndex=1 ,colIndex=cols, rowIndex=rows, header = TRUE)
doc <- xmlRoot(xmlTreeParse("getdata-data-restaurants.xml"))
dataXml <- xmlToDataFrame(getNodeSet(doc, "//zipcode"))
q4 <- nrow(subset(dataXml, zip == '21231'))