#install.packages("rjson")
#install.packages("readr")
library(readr)
library(rjson)

myargs = commandArgs(trailingOnly=TRUE)


data = read_file(myargs[1], locale = default_locale())
#data = read_file("D:/Datajet/datajetsoftware/output/model_in_63792650225149.json", locale = default_locale())

dataModel = fromJSON(data)


adata = list(name="Tim");
adata = append(adata,(list(age=56)))
adata = append(adata,(list(anArray=c(1,2,3))))


grid = dataModel$grid
headerInfo = dataModel$headerInfo

rows = dataModel$rows
hasTotalRow = dataModel$hasTotalRow

if(hasTotalRow) {
 rows=rows-1
}

nColumns = length(headerInfo)

#make a copy of the last column
grid3 <- grid[[nColumns]]



for( x in 1:rows) {
    ##grid[[2]][[x]] =  as.double(grid[[2]][[x]])/333
    grid3[[x]] =  as.double(grid[[nColumns]][[x]])/2
    
}

if(hasTotalRow) {
    ##grid[[2]][[rows+1]]=""
    grid3[[rows+1]]=""
}

#append the new column array to the grid
grid = append(grid,list(grid3))




#make a copy of the last column in grid
header2 = headerInfo[[nColumns]]
#change its name
header2$name = "Copy"
#append to headerinfo
headerInfo = append(headerInfo,list(header2))


dataModel$grid = grid
dataModel$associatedData = adata
dataModel$headerInfo = headerInfo

#dataModel["associatedData"] = associatedData

finalModel = toJSON(dataModel)

write_file(finalModel,myargs[2])