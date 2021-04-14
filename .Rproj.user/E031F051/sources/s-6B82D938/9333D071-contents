library(DT)

data = read.csv2('./data/siswa.csv')

baseValue = data[1:100, 8]

label <- c("No", "Nilai", "Jumlah")
values <- c(c(1,2), c(3, 4), c(5, 6))

no = c()
nilai = c()
jumlah = c()


minData = floor(min(baseValue) / 10)
maxData = ceiling(max(baseValue) / 10)

differential = c()
agregation = c()
valueOfAgregate = c()
dataValues = c()


for(itm in c(minData:maxData)) {
  agregate = baseValue[baseValue >= itm * 10 & baseValue < (itm + 1) * 10]
  minAgregate = min(agregate)
  maxAgregate = max(agregate)
  if(minAgregate != Inf & maxAgregate != Inf) {
    agregation <- append(agregation,
                         paste(minAgregate," - ", maxAgregate),
                         after = length(agregation)) 
    valueOfAgregate <- append(valueOfAgregate,
                              length(agregate),
                              after = length(valueOfAgregate))
    dataValues <- append(dataValues,
                         toString(agregate),
                         after= length(dataValues))
  }
}

tableData <- data.frame(agregation, valueOfAgregate, dataValues)

datatable(tableData)
