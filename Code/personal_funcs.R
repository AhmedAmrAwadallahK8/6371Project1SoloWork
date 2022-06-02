#Personal Functions Needed for Operation
get_train_test_list = function(df, splitPercent){
  dfRowIndices = 1:dim(df)[1]
  dfRowSize = dim(df)[1]
  sampleSize = round(splitPercent * dfRowSize)
  trainIndices = sample(dfRowIndices, sampleSize)
  testIndices = -trainIndices
  train = df[trainIndices,]
  test = df[testIndices,]
  return(list(train, test))
}

get_na_df = function(df){
  allNaRows = rowSums(is.na(df)) > 0
  naDf = df[allNaRows,]
  return(naDf)
}
