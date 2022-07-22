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

rows = dataModel$rows
hasTotalRow = dataModel$hasTotalRow

if(hasTotalRow) {
 rows=rows-1
}

for( x in 1:rows) {
    grid[[2]][[x]] =  as.double(grid[[2]][[x]])/333
}

if(hasTotalRow) {
    grid[[2]][[rows+1]]=""
}

dataModel$grid = grid

dataModel$associatedData = adata

#dataModel["associatedData"] = associatedData

finalModel = toJSON(dataModel)

write_file(finalModel,myargs[2])