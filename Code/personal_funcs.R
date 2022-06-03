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

#Not working yet
get_normalized_df = function(df, variablesToNormalize){
  columns = colnames(df)
  for(col in columns){
    if(col %in% variablesToNormalize){
       df[,col] = get_normalized_feature(df[,col])
    }
  }
  return(df)
}

get_normalized_feature = function(feature){
  normalized_feature = (feature - mean(feature))/sd(feature)
  return(normalized_feature)
}

test = c(1:100)
test2 = c(1:50)

d = data.frame(test_fea = test, test_fea2 = test2)
colnames(d)
str(d)
vtn = c("test_fea2")
for(x in colnames(d)){
  if(x %in% vtn){
    print(x)
  }
}
d = get_normalized_df(d, vtn)
str(d)

get_na_df = function(df){
  allNaRows = rowSums(is.na(df)) > 0
  naDf = df[allNaRows,]
  return(naDf)
}


