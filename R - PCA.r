#https://www.statology.org/principal-components-analysis-in-r/

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

nColumns = length(headerInfo)

rows = dataModel$rows
hasTotalRow = dataModel$hasTotalRow



#need to remove the last row of we have totals
if(hasTotalRow) {
    dataModel$hasTotalRow=FALSE

    for( x in 2:nColumns) {
        grid[[x]] = grid[[x]][-rows]       
    }

    rows=rows-1
    hasTotalRow = FALSE
    dataModel$rows =rows
}


dataFrame = data.frame( as.double(unlist(grid[[2]])))
for( x in 3:nColumns) {
    dataFrame <- cbind(dataFrame , as.double(unlist(grid[[x]])))
}

#dataFrame <- cbind(dataFrame , as.double(unlist(grid[[3]])))
#dataFrame <- cbind(dataFrame , as.double(unlist(grid[[4]])))
#dataFrame <- cbind(dataFrame , as.double(unlist(grid[[5]])))


for( x in 1:(nColumns-1)) {
colnames(dataFrame)[[x]] = headerInfo[[x+1]]$name
}


results <- prcomp(dataFrame, scale = TRUE)

results$x <- -1*results$x

stdev =  results$sdev 
vlen = length(stdev)

adata = list()
adata = append(adata,list(stdev=stdev))



#prepare and populate the rotation grid
dgrid = fromJSON("{\"objectType\": \"grid\"}")
dgrid$name = c("rotation")
dgrid$tag = c("PCA-GRID")

dheaders=list(1:(vlen+1))
dheaders[1]="Entity"
for( x in 1:vlen) {
    dheaders[x+1]=paste0("PC",as.character(x))
}


rotGrid=list(1:(vlen+1))

for( y in 1:(vlen+1)) 
{ 
    rotGridInner = list(1:vlen+1)

    if(y==1) 
    {
        for( x in 1:vlen) {
            rotGridInner[x]=headerInfo[[x+1]]$name
        }
    } else {
        for( x in 1:vlen) {
            rotGridInner[x]=results$rotation[[x+(y-2)*vlen]]
        }
    }
    rotGrid[y]=list(rotGridInner)
}


dgrid$headers=dheaders
dgrid$data=rotGrid


adata = append(adata,list(rotation=dgrid))



dataModel$associatedData = adata



finalModel = toJSON(dataModel)

write_file(finalModel,myargs[2])