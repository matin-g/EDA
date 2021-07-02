import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

data = pd.read_csv('kaggle_survey_2020_responses.csv')
data.drop(index=data.index[0], axis=0, inplace=True)
data = data[['Q1', 'Q2', 'Q3', 'Q4', 'Q6', 'Q24']]
data = data[(data['Q2'] != 'Prefer not to say') &
            (data['Q2'] != 'Nonbinary') &
            (data['Q2'] != 'Prefer to self-describe')]
data = data[data['Q3'] != 'Other']
data = data[data['Q4'] != 'I prefer not to answer']
data = data.dropna()
data.reindex()

for i in range(len(data)):
    if i == 0:
        for j in range(len(data.iloc[:, i])):
            ageString = str(data.iloc[j, i])
            if ageString == '70+':
                ageSim = np.random.randint(int(ageString[:-1]), 80 + 1)
                data.iloc[j, i] = ageSim
            else:
                ageRange = ageString.split('-')
                ageSim = np.random.randint(int(ageRange[0]), int(ageRange[1]) + 1)
                data.iloc[j, i] = ageSim

    if i == 4:
        for j in range(len(data.iloc[:, i])):
            codingString = str(data.iloc[j, i])
            if codingString == '< 1 years':
                codingSim = np.random.uniform(0, 1)
                data.iloc[j, i] = codingSim
            elif codingString == 'I have never written code':
                data.iloc[j, i] = 0
            elif codingString == '20+ years':
                codingSim = np.random.randint(20, 30 + 1)
                data.iloc[j, i] = codingSim
            else:
                codingRange = codingString[:-6].split('-')
                codingSim = np.random.randint(int(codingRange[0]), int(codingRange[1]) + 1)
                data.iloc[j, i] = codingSim

    if i == 5:
        for j in range(len(data.iloc[:, i])):
            wageString = str(data.iloc[j, i])
            if wageString == '$0-999':
                wageRange = wageString[1:].split('-')
                wageSim = np.random.randint(int(wageRange[0]), int(wageRange[1]) + 1)
                data.iloc[j, i] = wageSim
            elif wageString == '> $500,000':
                wageRange = wageString[1:].split('-')
                wageSim = np.random.randint(500000, 650000 + 1)
                data.iloc[j, i] = wageSim
            else:
                wageRange = wageString.replace(',', '').split('-')
                wageSim = np.random.randint(int(wageRange[0]), int(wageRange[1]) + 1)
                data.iloc[j, i] = wageSim

data.to_csv('cleanData.csv', index=False)

