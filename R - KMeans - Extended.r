#install.packages("rjson")
#install.packages("readr")
library(readr)
library(rjson)
library(tidyverse)
library(cluster)
library(factoextra)

myargs = commandArgs(trailingOnly=TRUE)


#use 3 clusters unless passed as a parameter
NCLUSTERS = 3

if( length(myargs) == 3 ) {
    NCLUSTERS = as.integer(myargs[[3]])
}



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
kmeans2 <- kmeans(dataFrame, centers = NCLUSTERS, nstart = NCLUSTERS)



#append the new column array to the grid
grid = append(grid,list(kmeans2$cluster))


#make a copy of the last column in grid
header2 = headerInfo[[2]]
#change its name
header2$name = "Cluster"
#append to headerinfo
headerInfo = append(headerInfo,list(header2))


ncenters = length(kmeans2$centers)/2

adata = list(totss=kmeans2$totss)
adata = append(adata,list(withinss=kmeans2$withinss))
adata = append(adata,list(tot.withinss=kmeans2$tot.withinss))


#withins chart
wsizes=list()
wsizecat=list()

for( x in 1:ncenters) {
    wsizecat = append(wsizecat,x)
    wsizes = append(wsizes,kmeans2$withinss[[x]])
}

wchart = fromJSON("{\"objectType\": \"chart\",\"name\": \"\",\"chartType\": \"bar\"}")
wchart$name = c("Withinss")
wchart$categories=wsizecat
wchart$values=wsizes
adata = append(adata,list(wsizes=wchart))



#add info from the model into an object to be assigned to associatedData
adata = append(adata,list(betweenss=kmeans2$betweenss))
adata = append(adata,list(size=kmeans2$size))
adata = append(adata,list(iter=kmeans2$iter))
adata = append(adata,list(ifault=kmeans2$ifault))




cxy = list()
csizes=list()
csizecat=list()


#make lists of centroids
for( x in 1:ncenters) {
    cpair = list()
    cpair = append(cpair, kmeans2$centers[x,1])
    cpair = append(cpair, kmeans2$centers[x,2])
    cxy = append(cxy,list(cpair))
    
    csizecat = append(csizecat,x)
    csizes = append(csizes,kmeans2$size[[x]])
}

#add the cluster centers
adata = append(adata,list(centers=cxy))
#plottableCentroids are interpreted by the graphics engine as points to plot on a scatter chart
adata = append(adata,list(plottableCentroids=cxy))


#add centres to grid
gridCentresX = list()
gridCentresY = list()
for(x in 1:rows) {
    gridCentresX = append(gridCentresX,kmeans2$centers[kmeans2$cluster[x],1])
    gridCentresY = append(gridCentresY,kmeans2$centers[kmeans2$cluster[x],2])
}
grid = append(grid,list(gridCentresX))
grid = append(grid,list(gridCentresY))

headerCX = headerInfo[[2]]
headerCX$name = "Centre-X"
headerInfo = append(headerInfo,list(headerCX))

headerCY = headerInfo[[2]]
headerCY$name = "Centre-Y"
headerInfo = append(headerInfo,list(headerCY))


#cluster size chart
chart = fromJSON("{\"objectType\": \"chart\",\"name\": \"\",\"chartType\": \"bar\"}")
chart$name = c("Cluster Sizes")
chart$categories=csizecat
chart$values=csizes
adata = append(adata,list(sizes=chart))


#update the dataModel
dataModel$grid = grid
dataModel$headerInfo = headerInfo
dataModel$suggestedChart=c("scatter")
dataModel$associatedData = adata




finalModel = toJSON(dataModel)

write_file(finalModel,myargs[2])