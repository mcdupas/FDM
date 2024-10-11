"""Module providing a function printing python version."""
import os
import pickle
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import scipy
from scipy import stats
from scipy.stats import gaussian_kde
import itertools

###### sklearn (machine learning package)
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import RobustScaler
from sklearn.preprocessing import StandardScaler


# function to ensure directory exists
def ensure_dir(f):
    """Function to ensure directory exists.
    If the directory does not exist, it will be created.
    Args:   f(str): directory path
    Returns: None
    """
    d = os.path.dirname(f)
    if not os.path.exists(d):
        os.makedirs(d)


print("# -------------------------------------------------------------------")
print("#           Training random forest model                             ")
print("# -------------------------------------------------------------------")


## change directory path
os.chdir("C:/Users/Admin/Dropbox/OneHealthPoultry/Projects/01_FDM")

# This script trains a random forest model to predict farm stock size based on the
# extracted covariate values. The model is then applied on the simulated farms.

SPLIT = True  ## split the data (75% train and 25% test)
RF_TRANSFORM = "PowerTransformer"

## select farm training data
model_code = ["Broiler_BGD"]

MODEL_NAME = model_code[0]
if len(model_code) != 1:
    for j in range(1, len(model_code)):
        MODEL_NAME = MODEL_NAME + "_" + model_code[j]


### outpufolder
SAVE_SIZE_FARM_FOLDER = "03_Results/02_FarmSize"
OUTPUT_FOLDER = "03_Results/02_FarmSize/02_Outputs/" + MODEL_NAME + "/"
ensure_dir(OUTPUT_FOLDER)

BUFFER_DISTANCE = 5000

# Read farm training data

EXTRACTED_VALS_FILE = (
    SAVE_SIZE_FARM_FOLDER + "/01_TrainData/" + model_code[0] + "_ExtractTrain_BufDist_5000.csv"
)

extr_train = pd.read_csv(EXTRACTED_VALS_FILE)
extr_train = extr_train.dropna()


if len(model_code) != 1:
    for j in range(1, len(model_code)):
        EXTRACTED_VALS_FILE = (
            SAVE_SIZE_FARM_FOLDER
            + "/01_TrainData/"
            + model_code[j]
            + "_ExtractTrain_BufDist_5000.csv"
        )
        extr_train_add = pd.read_csv(EXTRACTED_VALS_FILE)
        extr_train = pd.concat([extr_train, extr_train_add])

extr_train = extr_train.dropna()


## _____________________SCALE LABELS_____________________###
# Labels are the values we want to predict
if RF_TRANSFORM == "Log":
    labels = np.log10(np.array(extr_train["Stock"]))
# perform a robust scaler transform of the dataset
if RF_TRANSFORM == "RobustScalar":
    labels = np.array(extr_train["Stock"])
    labels = labels.reshape(-1, 1)
    scaler_label = RobustScaler().fit(labels)
    labels = scaler_label.fit_transform(labels)
    labels = labels.ravel()

if RF_TRANSFORM == "PowerTransformer":
    labels = np.array(extr_train["Stock"])
    lambda_ = scipy.stats.boxcox_normmax(labels, brack=(-1.9, 2.0), method="mle")
    labels = (labels**lambda_ - 1) / lambda_

## _____________________END SCALE LABELS_____________________###


## _____________________SCALE FEATURES _____________________###
# Remove the labels from the features
# axis 1 refers to the columns
features = extr_train.drop("Stock", axis=1)

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
if SPLIT:
    train_features, test_features, train_labels, test_labels = train_test_split(
        features, labels, test_size=0.2, random_state=42
    )
else:
    train_features = features
    test_features = features
    train_labels = labels
    test_labels = labels


score_list = []
nb_features = ["auto", "sqrt", "log2"]
for i in nb_features:
    # Instantiate model with 1000 decision trees
    rf = RandomForestRegressor(n_estimators = 500, random_state = 10, max_features = i)

    # Train the model on training data
    rf.fit(train_features, train_labels)

    score = rf.score(train_features, train_labels)

    score_list.append(score)


## plot score list
plt.figure(figsize=(8, 4.96))
plt.plot([1, 2, 3], score_list, linestyle=" ", marker=".", color="black")
plt.xticks([1, 2, 3], nb_features, fontsize=18)
plt.yticks(fontsize=18)
plt.xlabel("number of features", fontsize=18)
plt.ylabel("$R^{2}$", fontsize=18)
plt.savefig(
    OUTPUT_FOLDER + "/score_vs_nb_features_" + MODEL_NAME + "_" + RF_TRANSFORM + ".png",
    dpi=300,
    bbox_inches="tight",
)
plt.close()

print("# -------------------------------------------------------------------")
print("#           Train and save models (RF and BRT)                       ")
print("# -------------------------------------------------------------------")

# Instantiate model with 1000 decision trees
rf = RandomForestRegressor(n_estimators=500, random_state=42)

# Train the model on training data
rf.fit(train_features, train_labels)

params_brt = {
    "n_estimators": 500,
    "max_depth": 4,
    "min_samples_split": 5,
    "learning_rate": 0.01,
    "loss": "squared_error",
}

reg = GradientBoostingRegressor(**params_brt)
reg.fit(train_features, train_labels)


# save the model to disk
filename = (
    OUTPUT_FOLDER
    + MODEL_NAME
    + "_modelRF_BufDist_"
    + str(BUFFER_DISTANCE)
    + "_"
    + RF_TRANSFORM
    + ".sav"
)
pickle.dump(rf, open(filename, "wb"))

filename = (
    OUTPUT_FOLDER
    + MODEL_NAME
    + "_modelBRT_BufDist_"
    + str(BUFFER_DISTANCE)
    + "_"
    + RF_TRANSFORM
    + ".sav"
)
pickle.dump(reg, open(filename, "wb"))



print("# -------------------------------------------------------------------")
print("#                       Importance of covariates                     ")
print("# -------------------------------------------------------------------")

# get importance of features
importance = rf.feature_importances_
df_imp = pd.DataFrame()
# summarize feature importance
for i, v in enumerate(importance):
    print("Feature Name : ", feature_list[i])
    print(f"Feature: {i:0d}, Score: {v:.5f}")
    df_imp[feature_list[i]] = [v]

df_imp.to_csv(
    OUTPUT_FOLDER
    + "/importance_covariates_BufDist_"
    + str(BUFFER_DISTANCE)
    + "_"
    + RF_TRANSFORM
    + ".csv"
)

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
plt.savefig(
    OUTPUT_FOLDER
    + "/importance_covariates_BufDist_"
    + str(BUFFER_DISTANCE)
    + "_"
    + RF_TRANSFORM
    + ".png",
    dpi=300,
    bbox_inches="tight",
)
plt.close()

print("# -------------------------------------------------------------------")
print("#                       Test validation on 20%                       ")
print("# -------------------------------------------------------------------")
predictions = rf.predict(test_features)
predictions_brt = reg.predict(test_features)

print(scipy.stats.pearsonr(predictions_brt, test_labels))
print("# -------------------------------------------------------------------")
print("#                       Internal validation                          ")
print("# -------------------------------------------------------------------")

# Calculate the absolute errors
if RF_TRANSFORM == "RobustScalar":
    y_new = predictions.reshape(-1, 1)
    y_train = test_labels.reshape(-1, 1)
    y_new = scaler_label.inverse_transform(y_new)
    y_train = scaler_label.inverse_transform(y_train)

if RF_TRANSFORM == "PowerTransformer":
    y_new = (predictions * lambda_ + 1) ** (1 / lambda_)
    y_train = (test_labels * lambda_ + 1) ** (1 / lambda_)

if RF_TRANSFORM == "Log":
    y_new = 10 ** (predictions)
    y_train = 10 ** (test_labels)

errors = abs(y_new - y_train)  # Print out the mean absolute error (mae)
# print('Mean Absolute Error:', round(np.mean(errors), 2), 'chickens.')

# Calculate mean absolute percentage error (MAPE)
mape = 100 * (errors / y_train)  # Calculate and display accuracy
accuracy = 100 - np.mean(mape)


P_TotalStock = sum(y_train)
y_new = np.asarray((y_new))
if P_TotalStock != "NA":
    adjFactor = P_TotalStock / sum(y_new)
    y_new = y_new * adjFactor

pearson = scipy.stats.pearsonr(y_new, y_train)[0]
spearman = stats.spearmanr(y_new, y_train)[0]
pearson_log = scipy.stats.pearsonr(predictions, test_labels)[0]
spearman_log = stats.spearmanr(predictions, test_labels)[0]

df = pd.DataFrame(
    {
        "Pearson_log": [pearson_log],
        "Spearman_log": [spearman_log],
        "Pearson": [pearson],
        "Spearman": [spearman],
        "Accuracy": [accuracy],
    }
)
df.to_csv(OUTPUT_FOLDER + "/internal_validation" + "_" + RF_TRANSFORM + ".csv")


# plot density histogram
fig, ax = plt.subplots(figsize=(6, 5))
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)

density = gaussian_kde(np.log10(y_new))
density2 = gaussian_kde(np.log10(y_train))
x_vals = np.linspace(
    min(np.log10(y_new)), max(np.log10(y_new)), 100
)  # Specifying the limits of our data
x_vals2 = np.linspace(
    min(np.log10(y_train)), max(np.log10(y_train)), 100
)  # Specifying the limits of our data
density.covariance_factor = lambda: 0.2  # Smoothing parameter
density2.covariance_factor = lambda: 0.2  # Smoothing parameter
density._compute_covariance()
density2._compute_covariance()
plt.plot(x_vals, (density((x_vals))), label="Simulated data")
plt.plot(x_vals2, (density2((x_vals2))), label="Training data")
plt.xticks(fontsize=13)
plt.yticks(fontsize=13)
plt.xlabel("log(Farm size)", fontsize=18)
plt.ylabel("frequency", fontsize=18)
plt.legend(loc="best", fontsize=13)
plt.savefig(
    OUTPUT_FOLDER
    + "/density function_trained_sim_"
    + MODEL_NAME
    + "_"
    + RF_TRANSFORM
    + ".png",
    bbox_inches="tight",
    dpi=300,
)
plt.close()


print("# -------------------------------------------------------------------")
print("#                       Predictions on simulated data                ")
print("# -------------------------------------------------------------------")
scale_factor_list = [0.5,1,2]
variance_factor_list = [0.5,1,2]
NSIM = 100
SIMULATION_FOLDER = "03_Results/01_SPP/"+ model_code[0]+"/02_Simulations/02_Scenario1/"

feature_list_bgd = [i+"_BGD" for i in feature_list]
rename_columns_dict = dict(zip(feature_list_bgd, feature_list))

# Create combinations of scale and variance factors
combinations = list(itertools.product(scale_factor_list, variance_factor_list))

# loop over the combinations
for scale_factor, variance_factor in combinations:
    print(f"Scale Factor: {scale_factor}, Variance Factor: {variance_factor}")
    for k in range(NSIM) :
        EXTRACTED_FILE = (
            SIMULATION_FOLDER
            + "Simu_fixed_var"
            + str(variance_factor)
            + "_sc"
            + str(scale_factor)
            + "/ExtractPred_Simufixed_fit_kppm_var"
            + str(variance_factor)
            + "_sc"
            + str(scale_factor)
            + "_sim_"
            + str(k + 1)
            + ".csv"
        )

        extr_sim_spp = pd.read_csv(EXTRACTED_FILE)
        extr_sim_spp = extr_sim_spp.dropna()

        # Rename the columns
        extr_sim_spp = extr_sim_spp.rename(columns=rename_columns_dict )

        # select features of the RF model
        features = np.array(extr_sim_spp[feature_list])

        predictions = rf.predict(features)

        # rescale the predictions
        if RF_TRANSFORM == "RobustScalar":
            y_new = predictions.reshape(-1, 1)
            y_new = scaler_label.inverse_transform(y_new)

        if RF_TRANSFORM == "PowerTransformer":
            y_new = (predictions * lambda_ + 1) ** (1 / lambda_)

        if RF_TRANSFORM == "Log":
            y_new = 10 ** (predictions)

        # adjust to the total number of chickens
        P_TotalStock = sum(y_train)
        y_new = np.asarray((y_new))
        if P_TotalStock != "NA":
            adjFactor = P_TotalStock / sum(y_new)
            y_new = y_new * adjFactor

        # save the predictions
        df = pd.DataFrame({"ID": np.array(extr_sim_spp["ID"]), "Stock": [int(ii) for ii in y_new]})

        # save the dataframe in a csv file
        STOCK_SIM_FILE = (
            SIMULATION_FOLDER
            + "Simu_fixed_var"
            + str(variance_factor)
            + "_sc"
            + str(scale_factor)
            + "/Stock_"
            + str(variance_factor)
            +"_sc"
            + str(scale_factor)
            +"_sim_"
            + str(k+1)
            +".csv"
        )

        df.to_csv(
            STOCK_SIM_FILE,
            index=False,
        )
