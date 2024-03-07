print("# -------------------------------------------------------------------")
print("#           Training random forest model                             ")
print("# -------------------------------------------------------------------")

import pandas as pd
import numpy as np
from matplotlib import pyplot
import matplotlib.pyplot as plt

import scipy
from scipy import stats

import os
import pickle

###### sklearn (machine learning package)
import sklearn
from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_selection import SelectFromModel
from sklearn.datasets import make_classification
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import RobustScaler
from sklearn.preprocessing import PowerTransformer
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import cross_val_score

#### graphics function
from mpl_toolkits.axes_grid.inset_locator import inset_axes

from scipy.stats import gaussian_kde

def ensure_dir(f):
    d = os.path.dirname(f)
    if not os.path.exists(d):
        os.makedirs(d)

## change directory path 
os.chdir("C:/Users/Admin/Dropbox/OneHealthPoultry")

# This script trains a random forest model to predict farm stock size based on the extracted covariate values. The model is then applied on the simulated farms.

P_Split = True    ## split the data (75% train and 25% test)
P_RF_transform = "PowerTransformer"
      
## select farm training data 
P_Model_Code = ["Broiler_BGD","Broiler_THA", "Broiler_IN.GJ"]


P_Model_Name = P_Model_Code[0]
 
P_Sim_Name = "Broiler_IN.GJ"


if len(P_Model_Code) != 1 : 
    for j in range(1,len(P_Model_Code)) : 
        P_Model_Name = P_Model_Name +"_"+ P_Model_Code[j]
        
### outpufolder
P_OutputFolder = "FDM/03_Validation/Train_model/"+P_Model_Name+"/size_farm_output/"

ensure_dir(P_OutputFolder)

P_BufferDist = 5000

# Read farm training data
pExtractedVals = "FDM/03_Validation/Train_model/"+P_Model_Code[0]+"/"+"ExtractTrain_BufDist_"+str(P_BufferDist)+".csv"
Extr_train = pd.read_csv(pExtractedVals)
Extr_train = Extr_train.dropna()

if len(P_Model_Code) != 1 : 
    for j in range(1,len(P_Model_Code)) : 
        pExtractedVals = "FDM/03_Validation/Train_model/"+P_Model_Code[j]+"/"+"ExtractTrain_BufDist_"+str(P_BufferDist)+".csv"
        Extr_train_add = pd.read_csv(pExtractedVals)
        Extr_train = pd.concat([Extr_train, Extr_train_add])
        
Extr_train = Extr_train.dropna()



## _____________________SCALE LABELS_____________________### 
# Labels are the values we want to predict
if P_RF_transform =="Log" : 
    labels = np.log10(np.array(Extr_train['Stock']))
# perform a robust scaler transform of the dataset
if P_RF_transform =="RobustScalar" :
    labels = np.array(Extr_train['Stock'])
    labels = labels.reshape(-1,1)
    scaler_label = RobustScaler().fit(labels)
    labels = scaler_label.fit_transform(labels)
    labels = labels.ravel()

if P_RF_transform == "PowerTransformer" : 
    labels = np.array(Extr_train['Stock'])
    lambda_ = scipy.stats.boxcox_normmax(labels, brack=(-1.9, 2.0),  method='mle')
    labels = (labels**lambda_ - 1) / lambda_
    #labels = labels.reshape(-1,1)
    #scaler_label = PowerTransformer(method='box-cox', lambdas_=[lmax], standardize=False).fit((labels))  #method='box-cox'
    #labels = scaler_label.fit_transform((labels))
    #labels = labels.ravel()

#lmax = scipy.stats.boxcox_normmax(labels, brack=(-1.9, 2.0),  method='mle')
#y_transform = scipy.stats.boxcox(labels, lmax)
#kurtosis = round((scipy.stats.kurtosis(labels)), ndigits=3)
#skew = round((scipy.stats.skew(labels)), ndigits=3)
#
#
#
#
#    
#    
##Interpreting Skew 
#if -0.5 < skew < 0.5:
#    print ('A skew of {skew} means the distribution is approx. symmetric')
#elif  -0.5 < skew < -1.0 or 0.5 < skew < 1.0:
#    print ('A skew of {skew} means the distribution is moderately skewed')
#else:
#    print ('A skew of {skew} means the distribution is highly skewed')
#
##Interpreting Kurtosis
#if  -0.5 < kurtosis < 0.5:
#    print ('A kurtosis of {kurtosis} means the distribution is approximately normal sometimes called mesokurtic distributions')
#elif kurtosis <= -0.5: 
#    print ('A kurtosis of {kurtosis} means the distribution is light-tailed (negative) sometimes called a platykurtic distributions')
#elif kurtosis >= 0.5:
#    print ('A kurtosis of {kurtosis} means the distribution is heavy-tailed (positive) sometimes called a leptokurtic distribution')
#    

## _____________________END SCALE LABELS_____________________### 


## _____________________SCALE FEATURES _____________________### 
# Remove the labels from the features
# axis 1 refers to the columns
features = Extr_train.drop('Stock', axis = 1)

# Saving feature names for later use
feature_list = list(features.columns)

# Convert to numpy array
features = np.array(features)

# Initialise the Scaler
#scaler_features = StandardScaler().fit(features)

# To scale data
#features = scaler_features.fit_transform(features)
## _____________________END SCALE FEATURES _____________________### 




# Split the data into training and testing sets
if P_Split : 
    train_features, test_features, train_labels, test_labels = train_test_split(features, labels, test_size = 0.25, random_state = 42)
else : 
    train_features = features
    test_features = features
    train_labels = labels
    test_labels = labels
    

score_list = [] 
nb_features =  ["auto", "sqrt", "log2"]
for i in nb_features : 
    
    # Instantiate model with 1000 decision trees
    rf = RandomForestRegressor(n_estimators = 500, random_state = 10, max_features = i)
    
    # Train the model on training data
    rf.fit(train_features, train_labels)
        
    score = rf.score(train_features, train_labels)
    
    score_list.append(score)
    
    
## plot score list 
plt.figure(figsize=(8,4.96))
plt.plot([1,2,3], score_list, linestyle = " ", marker = ".", color = "black")
plt.xticks([1,2,3], nb_features, fontsize = 18)
plt.yticks(fontsize = 18)
plt.xlabel("number of features", fontsize = 18)
plt.ylabel("$R^{2}$", fontsize = 18)
plt.savefig("FDM/03_Validation/Train_model/"+P_Model_Name+"/size_farm_output/2023/score_vs_nb_features.png", dpi = 300, bbox_inches = "tight") 
plt.close() 


# Instantiate model with 1000 decision trees
rf = RandomForestRegressor(n_estimators = 1000, random_state = 42)

# Train the model on training data
rf.fit(train_features, train_labels)


# save the model to disk
filename = P_OutputFolder + P_Model_Name+"_model_BufDist_"+str(P_BufferDist)+"_"+P_RF_transform+".sav"
pickle.dump(rf, open(filename, 'wb'))

print("# -------------------------------------------------------------------")
print("#                       Importance of covariates                     ")
print("# -------------------------------------------------------------------")
     
# get importance of features
importance = rf.feature_importances_
df_imp = pd.DataFrame()
# summarize feature importance
for i,v in enumerate(importance):
    print ('Feature Name : ', feature_list[i])
    print('Feature: %0d, Score: %.5f' % (i,v))
    df_imp[feature_list[i]] = [v]
ensure_dir(P_OutputFolder+str(P_BufferDist)+"/")
df_imp.to_csv(P_OutputFolder+str(P_BufferDist)+"/importance_covariates_BufDist_"+str(P_BufferDist)+"_"+P_RF_transform+".csv")

### plot importance of covariates 
ind = np.argsort(importance)
ind = ind[::-1]
importance_sorted = importance[ind]
feature_list_sorted = np.asarray(feature_list)[ind]
fig, ax = plt.subplots()
x = range(0, len(feature_list_sorted))
plt.bar(x, importance_sorted)
for tick in ax.get_xticklabels():
    tick.set_rotation(90)
ax.set_xticks(x)
ax.set_xticklabels(feature_list_sorted)
plt.ylabel("importance of covariate", fontsize=12)
ensure_dir(P_OutputFolder+str(P_BufferDist)+"/")
plt.savefig(P_OutputFolder+str(P_BufferDist)+"/importance_covariates_BufDist_"+str(P_BufferDist)+"_"+P_RF_transform+".png", dpi = 300, bbox_inches = "tight")
plt.close()

print("# -------------------------------------------------------------------")
print("#                       Test validation on 25%                       ")
print("# -------------------------------------------------------------------")
predictions = rf.predict(test_features) 




print("# -------------------------------------------------------------------")
print("#                       Internal validation                          ")
print("# -------------------------------------------------------------------")
      
      
####### internal validation (predictions where have already data)
# Use the forest's predict method on the test data
nSim = 1
accuracy_list = []
spearman_list = [] 
pearson_list  = [] 
for i in range(nSim) : 
# Read farm training data
    pExtractedVals = "FDM/03_Validation/Train_model/"+P_Model_Code[0]+"/"+"ExtractTrain_BufDist_"+str(P_BufferDist)+".csv"
    Extr_train = pd.read_csv(pExtractedVals)
    Extr_train = Extr_train.dropna()
    
    if len(P_Model_Code) != 1 : 
        for j in range(1,len(P_Model_Code)) : 
            pExtractedVals = "FDM/03_Validation/Train_model/"+P_Model_Code[j]+"/"+"ExtractTrain_BufDist_"+str(P_BufferDist)+".csv"
            Extr_train_add = pd.read_csv(pExtractedVals)
            Extr_train = pd.concat([Extr_train, Extr_train_add])
    Extr_train = Extr_train.dropna()    
    predictions = rf.predict(test_features)   
    
    # Calculate the absolute errors
    if P_RF_transform =="RobustScalar"  :
        y_train = test_labels.reshape(-1,1)
        y_new   = predictions.reshape(-1,1)
        
        y_train = scaler_label.inverse_transform(y_train)
        y_new   = scaler_label.inverse_transform(y_new)
    
    if  P_RF_transform =="PowerTransformer":
        y_train = (test_labels * lambda_ + 1) ** (1 / lambda_)
        y_new   = (predictions * lambda_ + 1) ** (1 / lambda_)
    
        
    if P_RF_transform =="Log" : 
        y_train = 10**(test_labels)
        y_new   = 10**(predictions)
    
    errors = abs( y_new- y_train)   # Print out the mean absolute error (mae)
    #print('Mean Absolute Error:', round(np.mean(errors), 2), 'chickens.')
    
    # Calculate mean absolute percentage error (MAPE)
    mape = 100 * (errors / y_train)# Calculate and display accuracy
    accuracy = 100 - np.mean(mape)
    #print('Accuracy:', round(accuracy, 2), '%.')
    accuracy_list.append(accuracy)



    P_TotalStock = sum(y_train)
    y_new = np.asarray((y_new))
    if P_TotalStock != "NA" : 
        adjFactor = P_TotalStock/sum(y_new)
        y_new = (y_new*adjFactor)
    
    pearson_list.append(scipy.stats.pearsonr(y_new, y_train)[0])
    spearman_list.append(stats.spearmanr(y_new, y_train)[0])
    
dataframe = pd.DataFrame({"pearson":pearson_list, "accuracy":accuracy_list, "spearman": spearman_list})
dataframe.to_csv(P_OutputFolder+str(P_BufferDist)+"/internal_validation"+"_"+P_RF_transform+".csv")


# plot density histogram 
fig, ax = plt.subplots(figsize=(6,5))
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)

density = gaussian_kde(np.log10(y_new))
density2 = gaussian_kde(np.log10(y_train))
x_vals = np.linspace(min(np.log10(y_new)),max(np.log10(y_new)),100) # Specifying the limits of our data
x_vals2 = np.linspace(min(np.log10(y_train)),max(np.log10(y_train)),100) # Specifying the limits of our data
density.covariance_factor = lambda : .2 #Smoothing parameter
density2.covariance_factor = lambda : .2 #Smoothing parameter
density._compute_covariance()
density2._compute_covariance()
plt.plot(x_vals,(density((x_vals))), label="Simulated data")
plt.plot(x_vals2,(density2((x_vals2))),label="Training data")
plt.xticks(fontsize = 13)
plt.yticks(fontsize = 13)
plt.xlabel("log(Farm size)", fontsize = 18)
plt.ylabel("frequency", fontsize = 18)
plt.legend(loc="best", fontsize = 13)
plt.savefig(P_OutputFolder+"2023/density function_trained_sim_"+P_Model_Name+"_"+P_RF_transform+".png", bbox_inches = "tight", dpi = 300)
plt.close()

#bins1 = 30
#bins2 = 30
#pyplot.hist(y_train, bins2, alpha=0.5, label='train')
#pyplot.hist(y_new, bins1, alpha=0.9, label='sim')
#pyplot.xlabel("farm size", fontsize=20)
#pyplot.legend(loc='upper right')
#plt.savefig(P_OutputFolder+str(P_BufferDist)+"/model_"+P_Model_Name+"_sim_BGDGuj_25percent_Powertransformer.png",dpi = 300, bbox_inches="tight")
#plt.close()


print("# -------------------------------------------------------------------")
print("#                       External validation                          ")
print("# -------------------------------------------------------------------")
List_P_Sim = ["Broiler_THA", "Broiler_BGD", "Broiler_IN.GJ"]
accuracy_list = []
spearman_list = [] 
pearson_list  = [] 
rsquared_list = [] 

for ii in range(len(List_P_Sim)) :
    
    # Read farm training data
    pExtractedVals = "FDM/03_Validation/Train_model/"+List_P_Sim[ii]+"/"+"ExtractTrain_BufDist_"+str(P_BufferDist)+".csv"
    
    Extr_train = pd.read_csv(pExtractedVals)
    Extr_train = Extr_train.dropna()

    # axis 1 refers to the columns
    features_ext = Extr_train.drop('Stock', axis = 1)
    # Saving feature names for later use
    feature_list_ext = list(features_ext.columns)
    # Convert to numpy array
    features_ext = np.array(features_ext)
    # To scale data
    #features_ext = scaler_features.fit_transform(features_ext)   
    
    # Use the forest's predict method on the test data
    predictions_ext = rf.predict(features_ext)

    if P_RF_transform =="RobustScalar"  :
        y_predict   = predictions.reshape(-1,1)
        y_predict   = scaler_label.inverse_transform(predictions_ext)
        y_predict   = predictions.ravel()

    if  P_RF_transform =="PowerTransformer":
        y_predict = (predictions_ext * lambda_ + 1) ** (1 / lambda_)
    
    if P_RF_transform =="Log":
        y_predict = 10**(predictions_ext)
        
    
    y_test = np.array(Extr_train['Stock'])
    
    ### readjust number of farms
    P_TotalStock = sum(y_test)
    y_predict = np.asarray((y_predict))
    if P_TotalStock != "NA" : 
        adjFactor = P_TotalStock/sum(y_predict)
        y_predict = (y_predict*adjFactor)

    
    # plot density histogram 
    fig, ax = plt.subplots(figsize=(6,5))
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    
    density = gaussian_kde(np.log10(y_predict))
    density2 = gaussian_kde(np.log10(y_test))
    x_vals = np.linspace(min(np.log10(y_predict)),max(np.log10(y_predict)),100) # Specifying the limits of our data
    x_vals2 = np.linspace(min(np.log10(y_test)),max(np.log10(y_test)),100) # Specifying the limits of our data
    density.covariance_factor = lambda : .2 #Smoothing parameter
    density2.covariance_factor = lambda : .2 #Smoothing parameter
    density._compute_covariance()
    density2._compute_covariance()

    plt.plot(x_vals,(density((x_vals))), label="Simulated data")
    plt.plot(x_vals2,(density2((x_vals2))),label="Training data")
    plt.xticks(fontsize = 13)
    plt.yticks(fontsize = 13)
    plt.xlabel("log(Farm size)", fontsize = 18)
    plt.ylabel("frequency", fontsize = 18)
    plt.legend(loc="best", fontsize = 13)
    plt.savefig(P_OutputFolder+str(P_BufferDist)+"/density function_trained_sim_"+P_Model_Name+"_Sim_"+List_P_Sim[ii]+"_"+P_RF_transform+".png", bbox_inches = "tight", dpi = 300)
    plt.close()
        
    
    
    errors = abs( y_predict - y_test)   # Print out the mean absolute error (mae)
    #print('Mean Absolute Error:', round(np.mean(errors), 2), 'chickens.')
    
    # Calculate mean absolute percentage error (MAPE)
    mape = 100 * (errors / y_test)# Calculate and display accuracy
    accuracy = 100 - np.mean(mape)
    #print('Accuracy:', round(accuracy, 2), '%.')
    ybar = np.sum(y_predict)/len(y_predict)
    ssreg = np.sum((y_predict-ybar)**2)
    sstot = np.sum((y_test - ybar)**2)
    
    accuracy_list.append(accuracy)
    pearson_list.append(scipy.stats.pearsonr(y_predict, y_test)[0])
    spearman_list.append(stats.spearmanr(y_predict, y_test)[0])    
    rsquared_list.append(ssreg/sstot)

dataframe = pd.DataFrame({"Country": List_P_Sim,"pearson":pearson_list, "accuracy":accuracy_list, "spearman": spearman_list,"rsquared":rsquared_list })
dataframe.to_csv(P_OutputFolder+str(P_BufferDist)+"/external_validation"+"_"+P_RF_transform+".csv")



print("# -------------------------------------------------------------------")
print("#           Predicting farm sizes for validation                     ")
print("# -------------------------------------------------------------------")
      
### predict on the same data and compute correlation 
rms       = mean_squared_error(y_new, y_train) 
spearmanr = stats.spearmanr(y_new, y_train)
pearson   = scipy.stats.pearsonr(y_new, y_train)


# External validataion : Thailand, Bengladesh or Thailand
pExtractedVals = "../Train_model/"+P_AdmCode_pred+"/"+"ExtractTrain.csv"
Extr_pred = pd.read_csv(pExtractedVals)
Extr_pred = Extr_pred.dropna()

#### preparing pred features data
stock_pred = np.array(Extr_pred['Stock'])
pred_features = Extr_pred.drop('Stock', axis = 1)
pred_feature_list = list(pred_features.columns)    # Saving feature names for later use
pred_features = np.array(pred_features)


## ________________________SCALE FEATURES _____________________### 
# To scale data
features = scaler_features.fit_transform(pred_features)
## _____________________END SCALE FEATURES _____________________### 


predictions = rf.predict(pred_features)

if P_RF_transform =="RobustScalar"  :
    predictions   = predictions.reshape(-1,1)
    predictions   = scaler_label.inverse_transform(predictions)
    predictions   = predictions.ravel()

if  P_RF_transform =="PowerTransformer":
    predictions = (predictions * lambda_ + 1) ** (1 / lambda_)

P_TotalStock = sum(stock_pred)
farm_size_pred = np.asarray((predictions))
if P_TotalStock != "NA" : 
    adjFactor = P_TotalStock/sum(farm_size_pred)
    farm_size_pred = (farm_size_pred*adjFactor)
    
rms       = mean_squared_error(stock_pred, farm_size_pred) 
spearmanr = stats.spearmanr(stock_pred, farm_size_pred)
pearson   = scipy.stats.pearsonr(stock_pred, farm_size_pred)

errors = abs(farm_size_pred- stock_pred)   # Print out the mean absolute error (mae)
print('Mean Absolute Error:', round(np.mean(errors), 2), 'chickens.')

# Calculate mean absolute percentage error (MAPE)
mape = 100 * (errors / stock_pred)# Calculate and display accuracy
accuracy = 100 - np.mean(mape)
print('Accuracy:', round(accuracy, 2), '%.')





y_new = farm_size_pred
y_train = stock_pred

density = gaussian_kde(np.log10(y_new))
density2 = gaussian_kde(np.log10(y_train))
x_vals = np.linspace(min(np.log10(y_new)),max(np.log10(y_new)),100) # Specifying the limits of our data
x_vals2 = np.linspace(min(np.log10(y_train)),max(np.log10(y_train)),100) # Specifying the limits of our data
density.covariance_factor = lambda : .2 #Smoothing parameter
density2.covariance_factor = lambda : .2 #Smoothing parameter
density._compute_covariance()
density2._compute_covariance()
plt.plot(x_vals,(density((x_vals))), label="Simulated data")
plt.plot(x_vals2,(density2((x_vals2))),label="Observed data")
plt.xticks(fontsize = 13)
plt.yticks(fontsize = 13)
plt.xlabel("log(Farm size)", fontsize = 18)
plt.ylabel("frequency", fontsize = 18)
plt.legend(loc="best", fontsize = 13)
plt.savefig(P_OutputFolder+"/density function_trained_sim_"+P_AdmCode_pred+".png", bbox_inches = "tight", dpi = 300)
plt.close()



print("# -------------------------------------------------------------------")
print("#           Predicting farm sizes using the model                    ")
print("# -------------------------------------------------------------------")


# Read farm simulated data
pExtractedVals = P_ProcessingFolder+"/"+"ExtractPred_fixedN.csv"
Extr_pred = pd.read_csv(pExtractedVals)
Extr_pred = Extr_pred.dropna()

#### preparing pred features data
id_farms = np.array(Extr_pred['ID'])
pred_features = Extr_pred.drop('ID', axis = 1)
pred_feature_list = list(pred_features.columns)    # Saving feature names for later use
pred_features = np.array(pred_features)

## ________________________SCALE FEATURES _____________________### 
# To scale data
features = scaler_features.fit_transform(pred_features)
## _____________________END SCALE FEATURES _____________________### 



# Use the forest's predict method on the test data
predictions = rf.predict(pred_features)


if P_RF_transform =="RobustScalar"  :
    predictions   = predictions.reshape(-1,1)
    predictions   = scaler_label.inverse_transform(predictions)
    predictions   = predictions.ravel()

if  P_RF_transform =="PowerTransformer":
    predictions = (predictions * lambda_ + 1) ** (1 / lambda_)
    #predictions   = predictions.reshape(-1,1)
    #predictions   = 10**(scaler_label.inverse_transform(predictions))
    #predictions   = predictions.ravel()
    
    

if P_RF_transform == "Log" : 
    predictions = 10**(predictions)


print("# -------------------------------------------------------------------")
print("#                   Adjusting farm sizes                             ")
print("# -------------------------------------------------------------------")

P_TotalStock = 663300000*0.68	
farm_size_pred = np.asarray((predictions))
if P_TotalStock != "NA" : 
    adjFactor = P_TotalStock/sum(farm_size_pred)
    farm_size_pred = (farm_size_pred*adjFactor)



# compare distributions
#plt.hist(10**(labels), label = "training data")
#plt.hist((farm_size_pred), label="pred data")
plt.hist(farm_size_pred)
plt.legend(loc="best")
plt.show()


bins1 = 20
bins2 = 20
pyplot.hist(farm_size_pred,  alpha=0.9, label='sim', normed = True)
pyplot.hist(Extr_train['Stock'], alpha=0.5, label='train', normed = True)
pyplot.legend(loc='upper right')
plt.savefig("model_Guj_sim_IND.png",dpi = 300, bbox_inches="tight")
plt.close()

mean, var  = scipy.stats.distributions.norm.fit(farm_size_pred)



print("# -------------------------------------------------------------------")
print("#                   Analysis of distribution                         ")
print("# -------------------------------------------------------------------")

y_new = farm_size_pred
density = gaussian_kde(y_new)
density2 = gaussian_kde(y_train)
x_vals = np.linspace(min(y_new),max(y_new),200) # Specifying the limits of our data
x_vals2 = np.linspace(min(y_train),max(y_train),200) # Specifying the limits of our data
density.covariance_factor = lambda : .5 #Smoothing parameter
density2.covariance_factor = lambda : .5 #Smoothing parameter
density._compute_covariance()
density2._compute_covariance()
plt.plot(x_vals,density(x_vals))
plt.plot(x_vals2,density2(x_vals2))

plt.show()

      
      
import powerlaw

fit = powerlaw.Fit(farm_size_pred, xmin = 1800)

f, ax1 = plt.subplots(figsize=(8,4.96))	
fit.lognormal.plot_ccdf(color = "black", linestyle = '-', linewidth = 2, ax = ax1)
fit.plot_ccdf(color='darkgray', marker="o", markeredgecolor = "white", markeredgewidth=0.1, markevery=50, linestyle = " ")
ax1.set_xlabel('Farm size', fontsize = 20)
ax1.set_ylabel('P(S $>$s)', fontsize = 20)
### insert figure
ax1in = inset_axes(ax1, width = "40%", height = "40%", loc=3, borderpad =4.5)   
ax1in.hist(farm_size_pred*1e-3, normed=False, color="darkgray", edgecolor='white', linewidth=0.1)
#ax1in.set_xticks([])
#ax1in.set_yticks([])
        
plt.savefig("3_Outputs/fixedN/distrib_lognormal_trans_"+P_RF_transform+".png", bbox_inches = "tight", dpi = 300)
plt.close()

mean, var  = scipy.stats.distributions.norm.fit(farm_size_pred)

####### training data distribution 
data = Extr_train['Stock'].tolist()

fit = powerlaw.Fit(data, xmin = (1,700))

f, ax1 = plt.subplots(figsize=(8,4.96))	
fit.lognormal.plot_ccdf(color = "black", linestyle = '-', linewidth = 2, ax = ax1)
fit.plot_ccdf(color='darkgray', marker="o", markeredgecolor = "white", markeredgewidth=0.1, markevery=5, linestyle = " ")
ax1.set_xlabel('Farm size', fontsize = 20)
ax1.set_ylabel('P(S $>$s)', fontsize = 20)
### insert figure
ax1in = inset_axes(ax1, width = "40%", height = "40%", loc=3, borderpad =4.5)   
ax1in.hist(data, normed=False, color="darkgray", edgecolor='white', linewidth=0.1)
#ax1in.set_xticks([])
#ax1in.set_yticks([])
        
plt.savefig("3_Outputs/fixedN/distrib_BGD.png", bbox_inches = "tight", dpi = 300)
plt.close()


fit = powerlaw.Fit(farm_size_pred, xmin = 4000)

f, ax1 = plt.subplots(figsize=(8,4.96))	
fit.lognormal.plot_ccdf(color = "black", linestyle = '-', linewidth = 2, ax = ax1)
fit.plot_ccdf(color='darkgray', marker="o", markeredgecolor = "white", markeredgewidth=0.1, markevery=50, linestyle = " ")
ax1.set_xlabel('Farm size', fontsize = 20)
ax1.set_ylabel('P(S $>$s)', fontsize = 20)
### insert figure
ax1in = inset_axes(ax1, width = "40%", height = "40%", loc=3, borderpad =4.5)   
ax1in.hist(farm_size_pred*1e-3, normed=False, color="darkgray", edgecolor='white', linewidth=0.1)
#ax1in.set_xticks([])
#ax1in.set_yticks([])
        
plt.savefig("3_Outputs/fixedN/distrib_lognormal_trans_"+P_RF_transform+".png", bbox_inches = "tight", dpi = 300)
plt.close()

data = farm_size_pred
bins=500
y, x = np.histogram(data, bins=bins, density=True)
x = (x + np.roll(x, -1))[:-1] / 2.0
sse = np.inf
sse_thr = 0.10
ks = np.inf
dist_names = ['norm', 'truncexpon', 'skewnorm', 'pareto', 't', 'lognorm', 'invgamma', 'exponweib','invgauss', 'loggamma', 'cauchy', 'chi', 'chi2', 'expon','powerlaw']
# Pour chaque distribution
for name in dist_names:
    dist = getattr(stats, name)
    parameters = dist.fit(data)
    
    ks_temp, pvalue = stats.kstest(data, name, parameters)

    print (name, ks_temp)

    # Si le SSE est ddiminué, enregistrer la loi
    if ks_temp < ks :
        best_name = name
        ks = ks_temp

    #Si en dessous du seuil, quitter la boucle
    #if model_sse < sse_thr :
    #   break
    
    

















      




