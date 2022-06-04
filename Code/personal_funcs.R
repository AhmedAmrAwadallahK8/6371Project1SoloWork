#Only used to check if my functions have been loaded
ahmeds_functions = function(){
  return(0)
}

get_train_val_test_list = function(df, splitPercent){
  dfRowIndices = 1:dim(df)[1]
  dfRowSize = dim(df)[1]
  sampleSize = round(splitPercent * dfRowSize)
  trainIndices = sample(dfRowIndices, sampleSize)
  valTestIndices = -trainIndices
  train = df[trainIndices,]
  valTest = df[valTestIndices,]
  valTestList = get_train_test_list(valTest, 0.5)
  valIndex = 1
  testIndex = 2
  val = valTestList[[valIndex]]
  test = valTestList[[testIndex]]
  return(list(train, val, test))
}

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

get_rmse = function(predictions, targets){
  Residuals = predictions - targets
  SquaredResiduals = Residuals^2
  mse = mean(SquaredResiduals)
  rmse = sqrt(mse)
  return(rmse)
}

get_standardized_df = function(df, variablesToStandardize){
  columns = colnames(df)
  for(col in columns){
    if(col %in% variablesToStandardize){
       df[,col] = get_standardized_feature(df[,col])
    }
  }
  return(df)
}

get_normalized_df = function(df, variablesToNormalize){
  columns = colnames(df)
  for(col in columns){
    if(col %in% variablesToNormalize){
      df[,col] = get_normalized_feature(df[,col])
    }
  }
  return(df)
}

get_standardized_feature = function(feature){
  standardized_feature = (feature - mean(feature))/sd(feature)
  return(standardized_feature)
}

get_normalized_feature = function(feature){
  normalized_feature = (feature - min(feature))/(max(feature) - min(feature))
  return(normalized_feature)
}

get_na_df = function(df){
  allNaRows = rowSums(is.na(df)) > 0
  naDf = df[allNaRows,]
  return(naDf)
}


