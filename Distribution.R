library(DT)

data = read.csv2('./data/siswa.csv')

baseValue = data[1:100, 8]

minData = floor(min(baseValue) / 10)
maxData = ceiling(max(baseValue) / 10)

rangeData = max(baseValue) - min(baseValue)
lengthOfClass = 1 + 3.3 * log(length(baseValue))
maxLengthClass = ceiling(rangeData / lengthOfClass)

intervalRange = c(floor(min(baseValue)))

agregation = c()
valueOfAgregate = c()
dataValues = c()
medianValues = c()
calculatedValues = c()


while(intervalRange[length(intervalRange)] < max(baseValue)) {
  intervalRange <- append(intervalRange,
                          floor(intervalRange[length(intervalRange)] + maxLengthClass + 1))
}

agr <- 0
loopIndex <- 0
for (itm in intervalRange) {
  loopIndex <- loopIndex + 1
  maxRange = itm + maxLengthClass + 1
  minRange = itm
  agregate = baseValue[baseValue >= minRange & baseValue < maxRange]
  agr <- agr + length(agregate)
  
  if(length(agregate) > 0) {
    agregation <- append(agregation,
                         paste(itm, " - ", maxRange - 1),
                         after = length(agregation)) 
    
    valueOfAgregate <- append(valueOfAgregate,
                              length(agregate),
                              after = length(valueOfAgregate))
    
    dataValues <- append(dataValues,
                         toString(sort.int(agregate))
                         )
    
    medianValues <- append(medianValues,
                           median(agregate)
                           )
    
    calculatedValues <- append(
      calculatedValues,
      median(agregate) * length(agregate)
    ) 
  }
}

tableData <- data.frame(Interval = agregation,
                        Median = medianValues,
                        Jumlah = valueOfAgregate,
                        Total = calculatedValues,
                        Data_Sebaran = dataValues
)


datatable(tableData, options= list(
  pageLength = 20
))
