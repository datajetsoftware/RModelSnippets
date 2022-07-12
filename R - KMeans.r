#install.packages("rjson")
#install.packages("readr")
library(readr)
library(rjson)
library(tidyverse)
library(cluster)
library(factoextra)

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


#make a dataframe from grid column 2
dataFrame = data.frame(unlist(grid[[2]]),unlist(grid[[3]]))




#calculate kmeans on the dataframe
kmeans2 <- kmeans(dataFrame, centers = 5, nstart = 5)



#append the new column array to the grid
grid = append(grid,list(kmeans2$cluster))


#make a copy of the last column in grid
header2 = headerInfo[[2]]
#change its name
header2$name = "Cluster"
#append to headerinfo
headerInfo = append(headerInfo,list(header2))


adata = list(totss=kmeans2$totss)
adata = append(adata,list(withinss=kmeans2$withinss))
adata = append(adata,list(tot.withinss=kmeans2$tot.withinss))
adata = append(adata,list(betweenss=kmeans2$betweenss))
adata = append(adata,list(size=kmeans2$size))
adata = append(adata,list(iter=kmeans2$iter))
adata = append(adata,list(ifault=kmeans2$ifault))

ncenters = length(kmeans2$centers)/2


cxy = list()

#kmeans2$centers
#kmeans2$centers[,]
#kmeans2$centers[1,2]

for( x in 1:ncenters) {
    cpair = list()
    cpair = append(cpair, kmeans2$centers[x,1])
    cpair = append(cpair, kmeans2$centers[x,2])
    cxy = append(cxy,list(cpair))
}

adata = append(adata,list(centers=cxy))


dataModel$grid = grid
dataModel$headerInfo = headerInfo

dataModel$suggestedChart=c("scatter")
dataModel$associatedData = adata


#dataModel["associatedData"] = associatedData

finalModel = toJSON(dataModel)

write_file(finalModel,myargs[2])