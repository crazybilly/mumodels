## ----eval = F------------------------------------------------------------
#  
#  # build training data
#  FY14trainingdata  <- buildAFpredictors(   2014
#                                          , trainingsource = 'C:/lastyearsdata.csv'
#                                         )
#  
#  # build models
#  AFmodels  <- buildAFmodels(FY14trainingdata)
#  
#  
#  # make predictions
#  
#  FY15predictions  <- predictAF(  AFmodels
#                                , source = hallptbl
#                                , currentyear = 2015
#                               )
#  

## ----eval = F------------------------------------------------------------
#  
#  FY14trainingdata  <- buildAFpredictors(  2014
#                                         , yeartype = 'fiscal'
#                                         , trainingsource = 'C:\users\crazybilly\hallpAtFY14start.csv'
#                                      )
#  

## ---- eval = F-----------------------------------------------------------
#  
#  AFmodels  <- buildAFmodels( FY14trainingdata )
#  

## ---- eval = F-----------------------------------------------------------
#  
#  # only build the lm model
#  lm_only  <- buildAFmodels( FY14trainingdata, models = c('lm')
#  
#  # do not build the rf model
#  lm_and_glm_models <- buildAFmodels( FY14trainingdata, models = c('lm', 'glm')

## ---- eval =  F----------------------------------------------------------
#  
#  FY15predictions  <- predictAF(AFmodels, hallptbl, currentyear = 2015)
#  

## ---- eval = F-----------------------------------------------------------
#  
#  # build current data for predictions
#  FY15data  <- buildAFpredictors(2015)
#  
#  # predict based on the already processed data
#  FY15predictions  <- predictAF( AFmodels, source = FY15data, buildsource = F)

