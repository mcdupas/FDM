import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

import scipy
from scipy import stats

import os
import pickle

###### sklearn (machine learning package)
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

# function to ensure directory exists
def ensure_dir(f):
    d = os.path.dirname(f)
    if not os.path.exists(d):
        os.makedirs(d)

print("# -------------------------------------------------------------------")
print("#           Training random forest model                             ")
print("# -------------------------------------------------------------------")


print("Hello wrold!")

## change directory path 
os.chdir("C:/Users/Admin/Dropbox/OneHealthPoultry/Projects/01_FDM")

# This script trains a random forest model to predict farm stock size based on the extracted covariate values. The model is then applied on the simulated farms.

P_Split = True    ## split the data (75% train and 25% test)
P_RF_transform = "PowerTransformer"
      
## select farm training data
P_Model_Code = [ "Layer_IN.GJ"]

P_Model_Name = P_Model_Code[0]
if len(P_Model_Code) != 1 : 
    for j in range(1,len(P_Model_Code)) : 
        P_Model_Name = P_Model_Name +"_"+ P_Model_Code[j]
        
### outpufolder
P_SaveSizeFarmFolder = "03_Results/02_FarmSize"
P_OutputFolder = "03_Results/02_FarmSize/02_Outputs/"+P_Model_Name+"/"
ensure_dir(P_OutputFolder)

P_BufferDist = 5000

# Read farm training data
pExtractedVals = P_SaveSizeFarmFolder + "/01_TrainData/"+ P_Model_Code[0] + "_ExtractTrain.csv"
Extr_train = pd.read_csv(pExtractedVals)
Extr_train = Extr_train.dropna()

if len(P_Model_Code) != 1 : 
    for j in range(1,len(P_Model_Code)) : 
        pExtractedVals =  P_SaveSizeFarmFolder + "/01_TrainData/"+ P_Model_Code[j] + "_ExtractTrain.csv"
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
scaler_features = StandardScaler().fit(features)

# To scale data
features = scaler_features.fit_transform(features)
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
plt.savefig(P_OutputFolder+"/score_vs_nb_features.png", dpi = 300, bbox_inches = "tight") 
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

df_imp.to_csv(P_OutputFolder+"/importance_covariates_BufDist_"+str(P_BufferDist)+"_"+P_RF_transform+".csv")

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
plt.savefig(P_OutputFolder+"/importance_covariates_BufDist_"+str(P_BufferDist)+"_"+P_RF_transform+".png", dpi = 300, bbox_inches = "tight")
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
    pExtractedVals = P_SaveSizeFarmFolder + "/01_TrainData/"+ P_Model_Code[0] + "_ExtractTrain.csv"
    Extr_train = pd.read_csv(pExtractedVals)
    Extr_train = Extr_train.dropna()
    
    if len(P_Model_Code) != 1 : 
        for j in range(1,len(P_Model_Code)) : 
            pExtractedVals = P_SaveSizeFarmFolder + "/01_TrainData/"+ P_Model_Code[j] + "_ExtractTrain.csv"
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
dataframe.to_csv(P_OutputFolder+"/internal_validation"+"_"+P_RF_transform+".csv")


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
plt.savefig(P_OutputFolder+"/density function_trained_sim_"+P_Model_Name+"_"+P_RF_transform+".png", bbox_inches = "tight", dpi = 300)
plt.close()



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
    pExtractedVals = P_SaveSizeFarmFolder + "/01_TrainData/"+ List_P_Sim[ii]+ "_ExtractTrain.csv"
    
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
    plt.savefig(P_OutputFolder+"/density function_trained_sim_"+P_Model_Name+"_Sim_"+List_P_Sim[ii]+"_"+P_RF_transform+".png", bbox_inches = "tight", dpi = 300)
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
dataframe.to_csv(P_OutputFolder+"/external_validation"+"_"+P_RF_transform+".csv")


















      




