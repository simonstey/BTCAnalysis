library("plotly")
valOutputPerTX <- read.csv("C:/Projects/R/data/tx_2OutputsValues.csv", stringsAsFactors=FALSE)
valOutputPerTX$sum <- valOutputPerTX$value1+valOutputPerTX$value2
valOutputPerTX$ratioV1 <- valOutputPerTX$value1/valOutputPerTX$sum
valOutputPerTX$ratioV2 <- valOutputPerTX$value2/valOutputPerTX$sum
valOutputPerTX$ratio <- valOutputPerTX$value2/valOutputPerTX$value1

nrInputsPerTX <- read.csv("C:/Projects/R/data/sorted_nrInputsPerTX.csv", stringsAsFactors=FALSE)

graph <- plot_ly(x = valOutputPerTX$ratioV1, opacity = 0.6, type = "histogram") %>%
  add_trace(x = valOutputPerTX$ratioV2, opacity = 0.6, type = "histogram") %>%
  layout(barmode="overlay")

inputsPerTx = NULL
amount <- nrInputsPerTX$amount
for(i in 1:100) {
  inputsPerTx = rbind(inputsPerTx, data.frame(nrIn=i,am=100 * length(which(amount == i)) / length(amount)))
}

h <- hist(valOutputPerTX$ratioV1,breaks=100)