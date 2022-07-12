#install.packages("rjson")
#install.packages("readr")
library(readr)
library(rjson)

myargs = commandArgs(trailingOnly=TRUE)


data = read_file(myargs[1], locale = default_locale())
#data = read_file("D:/Datajet/datajetsoftware/output/model_in_63792650225149.json", locale = default_locale())

dataModel = fromJSON(data)




grid = dataModel$grid
headerInfo = dataModel$headerInfo

rows = dataModel$rows
hasTotalRow = dataModel$hasTotalRow





#need to remove the last row of we have totals
if(hasTotalRow) {
    dataModel$hasTotalRow=FALSE
    grid[[1]] = grid[[1]][-rows]
    grid[[2]] = grid[[2]][-rows]
    grid[[3]] = grid[[3]][-rows]
    rows=rows-1
    hasTotalRow = FALSE
    dataModel$rows =rows
}


#make a list() of doubles from grid columns 2
xData = as.double(grid[[2]])
#make a list() of doubles from grid columns 3
yData = as.double(grid[[3]])


#run the linear regression
relation <- lm(yData~xData)

#make a dataframe from grid column 2
dataFrame = data.frame(grid[[2]])

#run the prediction
prediction = predict(relation,dataFrame)


#make a new list( copied an existing one)
grid4 = grid[[2]]

#calculate predicted value for each row
for( x in 1:rows) {
    grid4[[x]] =  prediction[x]
   
}


#append the new column array to the grid
grid = append(grid,list(grid4))



#make a copy of the last column in grid
header2 = headerInfo[[3]]
#change its name
header2$name = "Predicted"
#append to headerinfo
headerInfo = append(headerInfo,list(header2))

adata = list(coefficients=relation$coefficients);
#adata = append(adata,(list(residuals=relation$residuals)))
#adata = append(adata,(list(anArray=c(1,2,3))))



dataModel$grid = grid
dataModel$associatedData = adata
dataModel$headerInfo = headerInfo

dataModel$suggestedChart=c("XYXZScatter")



#dataModel["associatedData"] = associatedData

finalModel = toJSON(dataModel)

write_file(finalModel,myargs[2])