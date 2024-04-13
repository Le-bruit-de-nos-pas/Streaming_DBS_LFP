import json
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import scipy.stats as stats
from scipy.ndimage import gaussian_filter1d


f = open('Report_Json_Session_Report_20221228T052940.json')
data = json.load(f)






########################################### Calibrations ###########################################




data['CalibrationTests'][0]['Channel'] # 'ZERO_TWO_LEFT'

xaxis = np.array(
    list(
        range(
            1, 1+len(
                np.array(
                    data['CalibrationTests'][0]['TimeDomainData']
                )
            )
        )
    )
) / 250


yaxis = stats.zscore(
    np.array(
        data['CalibrationTests'][0]['TimeDomainData']
    )
)


df_calibration_Left_Zero_Two = pd.concat(
    [pd.DataFrame(data=xaxis), pd.DataFrame(data=yaxis)], axis=1
     )

df_calibration_Left_Zero_Two.columns = ['Time[s]', 'Left 0-2']


plt.figure(figsize=(18,1))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'beige', 'axes.grid' : True })
sns.lineplot(x="Time[s]", y="Left 0-2", data=df_calibration_Left_Zero_Two, linewidth=1, color='crimson').\
    set(ylabel="Left 0-2 \n")
plt.show()





data['CalibrationTests'][1]['Channel'] # 'ZERO_TWO_RIGHT'

xaxis = np.array(
    list(
        range(
            1, 1+len(
                np.array(
                    data['CalibrationTests'][1]['TimeDomainData']
                )
            )
        )
    )
) / 250


yaxis = stats.zscore(
    np.array(
        data['CalibrationTests'][1]['TimeDomainData']
    )
)


df_calibration_Right_Zero_Two = pd.concat(
    [pd.DataFrame(data=xaxis), pd.DataFrame(data=yaxis)], axis=1
     )

df_calibration_Right_Zero_Two.columns = ['Time[s]', 'Right 0-2']




plt.figure(figsize=(18,1))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'beige', 'axes.grid' : True })
sns.lineplot(x="Time[s]", y="Right 0-2", data=df_calibration_Right_Zero_Two, linewidth=1, color='midnightblue').\
    set(ylabel="Right 0-2 \n")
plt.show()





data['CalibrationTests'][2]['Channel'] # 'ONE_THREE_LEFT'

xaxis = np.array(
    list(
        range(
            1, 1+len(
                np.array(
                    data['CalibrationTests'][2]['TimeDomainData']
                )
            )
        )
    )
) / 250


yaxis = stats.zscore(
    np.array(
        data['CalibrationTests'][2]['TimeDomainData']
    )
)


df_calibration_Left_One_Three = pd.concat(
    [pd.DataFrame(data=xaxis), pd.DataFrame(data=yaxis)], axis=1
     )

df_calibration_Left_One_Three.columns = ['Time[s]', 'Left 1-3']




plt.figure(figsize=(18,1))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'beige', 'axes.grid' : True })
sns.lineplot(x="Time[s]", y="Left 1-3", data=df_calibration_Left_One_Three, linewidth=1, color='crimson').\
    set(ylabel="Left 1-3 \n")
plt.show()




data['CalibrationTests'][3]['Channel'] # 'ONE_THREE_RIGHT'

xaxis = np.array(
    list(
        range(
            1, 1+len(
                np.array(
                    data['CalibrationTests'][3]['TimeDomainData']
                )
            )
        )
    )
) / 250


yaxis = stats.zscore(
    np.array(
        data['CalibrationTests'][3]['TimeDomainData']
    )
)


df_calibration_Right_One_Three = pd.concat(
    [pd.DataFrame(data=xaxis), pd.DataFrame(data=yaxis)], axis=1
     )

df_calibration_Right_One_Three.columns = ['Time[s]', 'Right 1-3']




plt.figure(figsize=(18,1))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'beige', 'axes.grid' : True })
sns.lineplot(x="Time[s]", y="Right 1-3", data=df_calibration_Right_One_Three, linewidth=1, color='midnightblue').\
    set(ylabel="Right 1-3 \n")
plt.show()




data['CalibrationTests'][4]['Channel'] # 'ZERO_TWO_LEFT'

xaxis = np.array(
    list(
        range(
            1, 1+len(
                np.array(
                    data['CalibrationTests'][4]['TimeDomainData']
                )
            )
        )
    )
) / 250


yaxis = stats.zscore(
    np.array(
        data['CalibrationTests'][4]['TimeDomainData']
    )
)


df_calibration_Left_Zero_Two = pd.concat(
    [pd.DataFrame(data=xaxis), pd.DataFrame(data=yaxis)], axis=1
     )

df_calibration_Left_Zero_Two.columns = ['Time[s]', 'Left 0-2']




plt.figure(figsize=(18,1))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'beige', 'axes.grid' : True })
sns.lineplot(x="Time[s]", y="Left 0-2", data=df_calibration_Left_Zero_Two, linewidth=1, color='crimson').\
    set(ylabel="Left 0-2 \n")
plt.show()






data['CalibrationTests'][5]['Channel'] # 'ZERO_TWO_RIGHT'

xaxis = np.array(
    list(
        range(
            1, 1+len(
                np.array(
                    data['CalibrationTests'][5]['TimeDomainData']
                )
            )
        )
    )
) / 250


yaxis = stats.zscore(
    np.array(
        data['CalibrationTests'][5]['TimeDomainData']
    )
)


df_calibration_Right_Zero_Two = pd.concat(
    [pd.DataFrame(data=xaxis), pd.DataFrame(data=yaxis)], axis=1
     )

df_calibration_Right_Zero_Two.columns = ['Time[s]', 'Right 0-2']




plt.figure(figsize=(18,1))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'beige', 'axes.grid' : True })
sns.lineplot(x="Time[s]", y="Right 0-2", data=df_calibration_Right_Zero_Two, linewidth=1, color='midnightblue').\
    set(ylabel="Right 0-2 \n")
plt.show()





data['CalibrationTests'][6]['Channel'] # 'ONE_THREE_LEFT'

xaxis = np.array(
    list(
        range(
            1, 1+len(
                np.array(
                    data['CalibrationTests'][6]['TimeDomainData']
                )
            )
        )
    )
) / 250


yaxis = stats.zscore(
    np.array(
        data['CalibrationTests'][6]['TimeDomainData']
    )
)


df_calibration_Left_One_Three = pd.concat(
    [pd.DataFrame(data=xaxis), pd.DataFrame(data=yaxis)], axis=1
     )

df_calibration_Left_One_Three.columns = ['Time[s]', 'Left 1-3']




plt.figure(figsize=(18,1))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'beige', 'axes.grid' : True })
sns.lineplot(x="Time[s]", y="Left 1-3", data=df_calibration_Left_One_Three, linewidth=1, color='crimson').\
    set(ylabel="Left 1-3 \n")
plt.show()




data['CalibrationTests'][7]['Channel'] # 'ONE_THREE_RIGHT'

xaxis = np.array(
    list(
        range(
            1, 1+len(
                np.array(
                    data['CalibrationTests'][7]['TimeDomainData']
                )
            )
        )
    )
) / 250


yaxis = stats.zscore(
    np.array(
        data['CalibrationTests'][7]['TimeDomainData']
    )
)


df_calibration_Right_One_Three = pd.concat(
    [pd.DataFrame(data=xaxis), pd.DataFrame(data=yaxis)], axis=1
     )

df_calibration_Right_One_Three.columns = ['Time[s]', 'Right 1-3']




plt.figure(figsize=(18,1))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'beige', 'axes.grid' : True })
sns.lineplot(x="Time[s]", y="Right 1-3", data=df_calibration_Right_One_Three, linewidth=1, color='midnightblue').\
    set(ylabel="Right 1-3 \n")
plt.show()






########################################### Frequency vs Magnitude ###########################################


# 'SensingElectrodeConfigDef.ZERO_AND_TWO'
# Peak Frequency 17.58
# Peak Magnitude 2.626953125

 data['LFPMontage'][0]['SensingElectrodes']

LFPFrequency_Zero_to_Two = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][0]['LFPFrequency']
    )
)

LFPFrequency_Zero_to_Two.rename(columns={0:"LFPFrequency"}, inplace=True)

LFPMagnitude_Zero_to_Two = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][0]['LFPMagnitude']
    )
)

LFPMagnitude_Zero_to_Two.rename(columns={0:"LFPMagnitude"}, inplace=True)

df = pd.concat([LFPFrequency_Zero_to_Two, LFPMagnitude_Zero_to_Two], axis=1, ignore_index=True)
df.rename(columns={0:"LFPFrequency"}, inplace=True)
df.rename(columns={1:"LFPMagnitude"}, inplace=True)

df1=df
df1['label'] = "ZERO_AND_TWO"

plt.figure(figsize=(10,6))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'aliceblue', 'axes.grid' : True })
sns.lineplot(x="LFPFrequency", y="LFPMagnitude", data=df, linewidth=6, color='firebrick').\
    set(title='LFP STN Left Zero <-> Two',
        xlabel="\n LFP Frequency (Hz)",
        ylabel="LFP Amplitude (uVp) \n")
plt.show()






# 'SensingElectrodeConfigDef.ZERO_AND_ONE'
# Peak Frequency 17.58
# Peak Magnitude 2.55859375

 data['LFPMontage'][1]['SensingElectrodes']

LFPFrequency_Zero_to_One = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][1]['LFPFrequency']
    )
)

LFPFrequency_Zero_to_One.rename(columns={0:"LFPFrequency"}, inplace=True)

LFPMagnitude_Zero_to_One = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][1]['LFPMagnitude']
    )
)

LFPMagnitude_Zero_to_One.rename(columns={0:"LFPMagnitude"}, inplace=True)

df = pd.concat([LFPFrequency_Zero_to_One, LFPMagnitude_Zero_to_One], axis=1, ignore_index=True)
df.rename(columns={0:"LFPFrequency"}, inplace=True)
df.rename(columns={1:"LFPMagnitude"}, inplace=True)

df2=df
df2['label'] = "ZERO_AND_ONE"

plt.figure(figsize=(10,6))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'aliceblue', 'axes.grid' : True })
sns.lineplot(x="LFPFrequency", y="LFPMagnitude", data=df, linewidth=6, color='firebrick').\
    set(title='LFP STN Left Zero <-> One',
        xlabel="\n LFP Frequency (Hz)",
        ylabel="LFP Amplitude (uVp) \n")
plt.show()





# 'SensingElectrodeConfigDef.ONE_AND_TWO'
# Peak Frequency 0
# Peak Magnitude 1.576171875

 data['LFPMontage'][2]['SensingElectrodes']

LFPFrequency_One_to_Two = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][2]['LFPFrequency']
    )
)

LFPFrequency_One_to_Two.rename(columns={0:"LFPFrequency"}, inplace=True)

LFPMagnitude_One_to_Two = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][2]['LFPMagnitude']
    )
)

LFPMagnitude_One_to_Two.rename(columns={0:"LFPMagnitude"}, inplace=True)

df = pd.concat([LFPFrequency_One_to_Two, LFPMagnitude_One_to_Two], axis=1, ignore_index=True)
df.rename(columns={0:"LFPFrequency"}, inplace=True)
df.rename(columns={1:"LFPMagnitude"}, inplace=True)

df3=df
df3['label'] = "ONE_AND_TWO"


plt.figure(figsize=(10,6))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'aliceblue', 'axes.grid' : True })
sns.lineplot(x="LFPFrequency", y="LFPMagnitude", data=df, linewidth=6, color='firebrick').\
    set(title='LFP STN Left One <-> Two',
        xlabel="\n LFP Frequency (Hz)",
        ylabel="LFP Amplitude (uVp) \n")
plt.show()



# 'SensingElectrodeConfigDef.TWO_AND_THREE'
# Peak Frequency 18.55
# Peak Magnitude 0.8642578125

 data['LFPMontage'][3]['SensingElectrodes']

LFPFrequency_Two_to_Three = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][3]['LFPFrequency']
    )
)

LFPFrequency_Two_to_Three.rename(columns={0:"LFPFrequency"}, inplace=True)

LFPMagnitude_Two_to_Three = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][3]['LFPMagnitude']
    )
)

LFPMagnitude_Two_to_Three.rename(columns={0:"LFPMagnitude"}, inplace=True)

df = pd.concat([LFPFrequency_Two_to_Three, LFPMagnitude_Two_to_Three], axis=1, ignore_index=True)
df.rename(columns={0:"LFPFrequency"}, inplace=True)
df.rename(columns={1:"LFPMagnitude"}, inplace=True)

df4=df
df4['label'] = "TWO_AND_THREE"


plt.figure(figsize=(10,6))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'aliceblue', 'axes.grid' : True })
sns.lineplot(x="LFPFrequency", y="LFPMagnitude", data=df, linewidth=6, color='firebrick').\
    set(title='LFP STN Left Two <-> Three',
        xlabel="\n LFP Frequency (Hz)",
        ylabel="LFP Amplitude (uVp) \n")
plt.show()







# 'SensingElectrodeConfigDef.One_AND_THREE'
# Peak Frequency 15.63
# Peak Magnitude 1.119140625

 data['LFPMontage'][4]['SensingElectrodes']

LFPFrequency_One_to_Three = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][4]['LFPFrequency']
    )
)

LFPFrequency_One_to_Three.rename(columns={0:"LFPFrequency"}, inplace=True)

LFPMagnitude_One_to_Three = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][4]['LFPMagnitude']
    )
)

LFPMagnitude_One_to_Three.rename(columns={0:"LFPMagnitude"}, inplace=True)

df = pd.concat([LFPFrequency_One_to_Three, LFPMagnitude_One_to_Three], axis=1, ignore_index=True)
df.rename(columns={0:"LFPFrequency"}, inplace=True)
df.rename(columns={1:"LFPMagnitude"}, inplace=True)

df5=df
df5['label'] = "ONE_AND_THREE"



plt.figure(figsize=(10,6))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'aliceblue', 'axes.grid' : True })
sns.lineplot(x="LFPFrequency", y="LFPMagnitude", data=df, linewidth=6, color='firebrick').\
    set(title='LFP STN Left One <-> Three',
        xlabel="\n LFP Frequency (Hz)",
        ylabel="LFP Amplitude (uVp) \n")
plt.show()









# 'SensingElectrodeConfigDef.ZERO_AND_THREE'
# Peak Frequency 17.58
# Peak Magnitude 2.000243902206421

 data['LFPMontage'][5]['SensingElectrodes']

LFPFrequency_Zero_to_Three = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][5]['LFPFrequency']
    )
)

LFPFrequency_Zero_to_Three.rename(columns={0:"LFPFrequency"}, inplace=True)

LFPMagnitude_Zero_to_Three = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][5]['LFPMagnitude']
    )
)

LFPMagnitude_Zero_to_Three.rename(columns={0:"LFPMagnitude"}, inplace=True)

df = pd.concat([LFPFrequency_Zero_to_Three, LFPMagnitude_Zero_to_Three], axis=1, ignore_index=True)
df.rename(columns={0:"LFPFrequency"}, inplace=True)
df.rename(columns={1:"LFPMagnitude"}, inplace=True)

df6=df
df6['label'] = "ZERO_AND_THREE"


plt.figure(figsize=(10,6))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'aliceblue', 'axes.grid' : True })
sns.lineplot(x="LFPFrequency", y="LFPMagnitude", data=df, linewidth=6, color='firebrick').\
    set(title='LFP STN Left Zero <-> Three',
        xlabel="\n LFP Frequency (Hz)",
        ylabel="LFP Amplitude (uVp) \n")
plt.show()


# Pooled Left STN

df_concated = pd.concat([df1, df2, df3, df4, df5, df6])
df_concated = df_concated.sort_values(by = ['label', 'LFPFrequency'])

plt.figure(figsize=(10,6))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'snow', 'axes.grid' : True })
sns.lineplot(x="LFPFrequency", y="LFPMagnitude", data=df_concated, linewidth=6, hue='label', palette="magma").\
    set(title='Left STN LFPs across channels',
        xlabel="\n LFP Frequency (Hz)",
        ylabel="LFP Amplitude (uVp) \n")
plt.show()








# 'SensingElectrodeConfigDef.ZERO_AND_TWO'
# Peak Frequency 18.55
# Peak Magnitude 2.6953125

 data['LFPMontage'][6]['SensingElectrodes']

LFPFrequency_Zero_to_Two = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][6]['LFPFrequency']
    )
)

LFPFrequency_Zero_to_Two.rename(columns={0:"LFPFrequency"}, inplace=True)

LFPMagnitude_Zero_to_Two = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][6]['LFPMagnitude']
    )
)

LFPMagnitude_Zero_to_Two.rename(columns={0:"LFPMagnitude"}, inplace=True)

df = pd.concat([LFPFrequency_Zero_to_Two, LFPMagnitude_Zero_to_Two], axis=1, ignore_index=True)
df.rename(columns={0:"LFPFrequency"}, inplace=True)
df.rename(columns={1:"LFPMagnitude"}, inplace=True)

df7=df
df7['label'] = "ZERO_AND_TWO"


plt.figure(figsize=(10,6))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'aliceblue', 'axes.grid' : True })
sns.lineplot(x="LFPFrequency", y="LFPMagnitude", data=df, linewidth=6, color='firebrick').\
    set(title='LFP STN Right Zero <-> Two',
        xlabel="\n LFP Frequency (Hz)",
        ylabel="LFP Amplitude (uVp) \n")
plt.show()










# 'SensingElectrodeConfigDef.ZERO_AND_ONE'
# Peak Frequency 17.58
# Peak Magnitude 0.94921875

 data['LFPMontage'][7]['SensingElectrodes']

LFPFrequency_Zero_to_One = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][7]['LFPFrequency']
    )
)

LFPFrequency_Zero_to_One.rename(columns={0:"LFPFrequency"}, inplace=True)

LFPMagnitude_Zero_to_One = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][7]['LFPMagnitude']
    )
)

LFPMagnitude_Zero_to_One.rename(columns={0:"LFPMagnitude"}, inplace=True)

df = pd.concat([LFPFrequency_Zero_to_One, LFPMagnitude_Zero_to_One], axis=1, ignore_index=True)
df.rename(columns={0:"LFPFrequency"}, inplace=True)
df.rename(columns={1:"LFPMagnitude"}, inplace=True)


df8=df
df8['label'] = "ZERO_AND_ONE"

plt.figure(figsize=(10,6))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'aliceblue', 'axes.grid' : True })
sns.lineplot(x="LFPFrequency", y="LFPMagnitude", data=df, linewidth=6, color='firebrick').\
    set(title='LFP STN Right Zero <-> One',
        xlabel="\n LFP Frequency (Hz)",
        ylabel="LFP Amplitude (uVp) \n")
plt.show()









# 'SensingElectrodeConfigDef.ONE_AND_TWO'
# Peak Frequency 18.55
# Peak Magnitude 3.474609375

 data['LFPMontage'][8]['SensingElectrodes']

LFPFrequency_One_to_Two = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][8]['LFPFrequency']
    )
)

LFPFrequency_One_to_Two.rename(columns={0:"LFPFrequency"}, inplace=True)

LFPMagnitude_One_to_Two = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][8]['LFPMagnitude']
    )
)

LFPMagnitude_One_to_Two.rename(columns={0:"LFPMagnitude"}, inplace=True)

df = pd.concat([LFPFrequency_One_to_Two, LFPMagnitude_One_to_Two], axis=1, ignore_index=True)
df.rename(columns={0:"LFPFrequency"}, inplace=True)
df.rename(columns={1:"LFPMagnitude"}, inplace=True)


df9=df
df9['label'] = "ONE_AND_TWO"

plt.figure(figsize=(10,6))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'aliceblue', 'axes.grid' : True })
sns.lineplot(x="LFPFrequency", y="LFPMagnitude", data=df, linewidth=6, color='firebrick').\
    set(title='LFP STN Right One <-> Two',
        xlabel="\n LFP Frequency (Hz)",
        ylabel="LFP Amplitude (uVp) \n")
plt.show()





# 'SensingElectrodeConfigDef.TWO_AND_THREE'
# Peak Frequency 17.58
# Peak Magnitude 1.4072265625

 data['LFPMontage'][9]['SensingElectrodes']

LFPFrequency_Two_to_Three = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][9]['LFPFrequency']
    )
)

LFPFrequency_Two_to_Three.rename(columns={0:"LFPFrequency"}, inplace=True)

LFPMagnitude_Two_to_Three = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][9]['LFPMagnitude']
    )
)

LFPMagnitude_Two_to_Three.rename(columns={0:"LFPMagnitude"}, inplace=True)

df = pd.concat([LFPFrequency_Two_to_Three, LFPMagnitude_Two_to_Three], axis=1, ignore_index=True)
df.rename(columns={0:"LFPFrequency"}, inplace=True)
df.rename(columns={1:"LFPMagnitude"}, inplace=True)


df10=df
df10['label'] = "TWO_AND_THREE"

plt.figure(figsize=(10,6))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'aliceblue', 'axes.grid' : True })
sns.lineplot(x="LFPFrequency", y="LFPMagnitude", data=df, linewidth=6, color='firebrick').\
    set(title='LFP STN Right Two <-> Three',
        xlabel="\n LFP Frequency (Hz)",
        ylabel="LFP Amplitude (uVp) \n")
plt.show()










# 'SensingElectrodeConfigDef.ONE_AND_THREE'
# Peak Frequency 18.55
# Peak Magnitude 2.423828125

 data['LFPMontage'][10]['SensingElectrodes']

LFPFrequency_One_to_Three = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][10]['LFPFrequency']
    )
)

LFPFrequency_One_to_Three.rename(columns={0:"LFPFrequency"}, inplace=True)

LFPMagnitude_One_to_Three = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][10]['LFPMagnitude']
    )
)

LFPMagnitude_One_to_Three.rename(columns={0:"LFPMagnitude"}, inplace=True)

df = pd.concat([LFPFrequency_One_to_Three, LFPMagnitude_One_to_Three], axis=1, ignore_index=True)
df.rename(columns={0:"LFPFrequency"}, inplace=True)
df.rename(columns={1:"LFPMagnitude"}, inplace=True)

df11=df
df11['label'] = "ONE_AND_THREE"

plt.figure(figsize=(10,6))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'aliceblue', 'axes.grid' : True })
sns.lineplot(x="LFPFrequency", y="LFPMagnitude", data=df, linewidth=6, color='firebrick').\
    set(title='LFP STN Right One <-> Three',
        xlabel="\n LFP Frequency (Hz)",
        ylabel="LFP Amplitude (uVp) \n")
plt.show()









# 'SensingElectrodeConfigDef.Zero_AND_THREE'
# Peak Frequency 19.53
# Peak Magnitude 1.6953125

 data['LFPMontage'][11]['SensingElectrodes']

LFPFrequency_Zero_to_Three = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][11]['LFPFrequency']
    )
)

LFPFrequency_Zero_to_Three.rename(columns={0:"LFPFrequency"}, inplace=True)

LFPMagnitude_Zero_to_Three = pd.DataFrame(
    data=np.array(
        data['LFPMontage'][11]['LFPMagnitude']
    )
)

LFPMagnitude_Zero_to_Three.rename(columns={0:"LFPMagnitude"}, inplace=True)

df = pd.concat([LFPFrequency_Zero_to_Three, LFPMagnitude_Zero_to_Three], axis=1, ignore_index=True)
df.rename(columns={0:"LFPFrequency"}, inplace=True)
df.rename(columns={1:"LFPMagnitude"}, inplace=True)

df12=df
df12['label'] = "ZERO_AND_THREE"

plt.figure(figsize=(10,6))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor': 'aliceblue', 'axes.grid' : True })
sns.lineplot(x="LFPFrequency", y="LFPMagnitude", data=df, linewidth=6, color='firebrick').\
    set(title='LFP STN Right Zero <-> Three',
        xlabel="\n LFP Frequency (Hz)",
        ylabel="LFP Amplitude (uVp) \n")
plt.show()



# Pooled Right STN

df_concated = pd.concat([df7, df8, df9, df10, df11, df12])
df_concated = df_concated.sort_values(by = ['label', 'LFPFrequency'])

plt.figure(figsize=(10,6))
sns.set_context("notebook")
sns.set(font_scale=10)
sns.set(rc={'axes.facecolor':'snow', 'axes.grid' : True })
sns.lineplot(x="LFPFrequency", y="LFPMagnitude", data=df_concated, linewidth=6, hue='label', palette="magma").\
    set(title='Right STN LFPs across channels',
        xlabel="\n LFP Frequency (Hz)",
        ylabel="LFP Amplitude (uVp) \n")
plt.show()



######################################## mA vs LFP ########################################


data['BrainSenseLfp'][0]['TherapySnapshot']

'''
{'ActiveGroup': 'GroupIdDef.GROUP_A', 
 'HighPassFilterInHertz': 1, 
 'SensingBlankingDurationInMicroseconds': 2000, 
 'Left': {'SensingChannel': 'SensingChannelDef.ZERO_TWO_LEFT', 
          'PulseWidthInMicroSecond': 60, 'RateInHertz': 125, 
          'LowerLimitInMilliAmps': 1.0, 
          'UpperLimitInMilliAmps': 1.0, 
          'FrequencyInHertz': 17.57,
          'FrequencyIndex': 18, 
          'UpperLfpThreshold': 30.0, 
          'LowerLfpThreshold': 20.0, 
          'AveragingDurationInMilliSeconds': 3000,
          'DetectionBlankingDurationInMilliSeconds': 2000}}
'''

len(data['BrainSenseLfp'][0]['LfpData']) #116

list_LFPs = []
for x in range(len(data['BrainSenseLfp'][0]['LfpData'])):
    list_LFPs.append(data['BrainSenseLfp'][0]['LfpData'][x]['Left']['LFP'])

list_LFPs = pd.DataFrame(data=np.array(list_LFPs))

list_mAs = []
for x in range(len(data['BrainSenseLfp'][0]['LfpData'])):
    list_mAs.append(data['BrainSenseLfp'][0]['LfpData'][x]['Left']['mA'])

list_mAs = pd.DataFrame(data=np.array(list_mAs))

list_Time = []
for i in range(0, 116, 1):
    list_Time.append(i)

list_Time = pd.DataFrame(data=np.array(list_Time))

temp = pd.concat([list_Time, list_LFPs, list_mAs], axis=1, ignore_index=True)
temp.rename(columns={0:"Time"}, inplace=True)
temp.rename(columns={1:"LFP"}, inplace=True)
temp.rename(columns={2:"mA"}, inplace=True)

temp = temp[temp.LFP<40000]
temp = temp[temp.LFP<6000]
temp['Time'] = temp['Time']/2

temp2 = temp.copy()

temp2['LFP'] = stats.zscore(temp2['LFP'])
temp2['mA'] = stats.zscore(temp2['mA'])



temp2['LFPsmooth'] = gaussian_filter1d(temp2['LFP'], sigma=6)
temp2['mAsmooth'] = gaussian_filter1d(temp2['mA'], sigma=6)


# plt.figure(figsize=(25,8))
sns.set_context("notebook")
sns.set(rc={'axes.facecolor':'snow', 'axes.grid' : False })
fig, ax1 = plt.subplots()
ax2 = ax1.twinx()
sns.lineplot(x=temp2['Time'], y=temp2['LFPsmooth'], linewidth=4,
             color='firebrick', ax=ax1).set(xlabel="\n Time [s]",
                                            ylabel="Beta Power \n (Gaussian filter over standd. data)\n")
sns.lineplot(x=temp2['Time'], y=temp2['mAsmooth'],  linestyle="dashed" ,
             linewidth=2, color='midnightblue', ax=ax2).set(
    xlabel="\n Time [s]", ylabel="\n Stimulation Amplitude \n (mA, Gaussian filter over standd. data)",
title="Left STN  Zero <-> Two   \n Frequency [17.57 Hz] LFP Thresholds [20-30] \n Avg. Duration [3000ms]  PulseWidth [60 us] Rate [125 Hz]")
plt.show()

