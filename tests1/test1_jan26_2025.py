import json



# Load the JSON file

file_path = '/mnt/data/Report_Json_Session_Report_20240323T174308.json'

with open(file_path, 'r') as file:

    data = json.load(file)



# Organize data for analysis

results = {

    "stimulation_parameters": {},

    "impedance_data": {},

    "signal_analysis": {},

    "battery_status": {},

    "group_history": [],

}



# Extract relevant sections



# 1. Stimulation Parameters

if "Stimulation" in data:

    results["stimulation_parameters"]["initial_status"] = data["Stimulation"].get("InitialStimStatus", "Unknown")

    results["stimulation_parameters"]["final_status"] = data["Stimulation"].get("FinalStimStatus", "Unknown")



if "Groups" in data:

    initial_groups = data["Groups"].get("Initial", [])

    final_groups = data["Groups"].get("Final", [])

    results["stimulation_parameters"]["initial_groups"] = initial_groups

    results["stimulation_parameters"]["final_groups"] = final_groups



# 2. Impedance Data

if "Impedance" in data:

    results["impedance_data"] = data["Impedance"]



# 3. Signal Analysis

if "Groups" in data:

    groups = data["Groups"].get("Initial", [])

    sensing_data = []

    for group in groups:

        if "ProgramSettings" in group:

            programs = group["ProgramSettings"].get("SensingChannel", [])

            sensing_data.extend(programs)

    results["signal_analysis"]["sensing_channels"] = sensing_data



# 4. Battery Status

if "BatteryInformation" in data:

    results["battery_status"] = data["BatteryInformation"]



# 5. Group Configuration History

if "GroupHistory" in data:

    results["group_history"] = data["GroupHistory"]



# Return a summary of extracted data for display

results_summary = {

    "stimulation_parameters": {

        "initial_status": results["stimulation_parameters"].get("initial_status"),

        "final_status": results["stimulation_parameters"].get("final_status"),

        "initial_groups_count": len(results["stimulation_parameters"].get("initial_groups", [])),

        "final_groups_count": len(results["stimulation_parameters"].get("final_groups", [])),

    },

    "impedance_data_count": len(results["impedance_data"]),

    "sensing_channels_count": len(results["signal_analysis"].get("sensing_channels", [])),

    "battery_status": results["battery_status"],

    "group_history_count": len(results["group_history"]),

}



results_summary




import pandas as pd



# Detailed breakdown: Stimulation parameters (Initial and Final Groups)

initial_groups = results["stimulation_parameters"]["initial_groups"]

final_groups = results["stimulation_parameters"]["final_groups"]



# Simplify group information for easier visualization

def extract_group_info(groups):

    extracted = []

    for group in groups:

        group_id = group.get("GroupId", "Unknown")

        active = group.get("ActiveGroup", False)

        left_settings = group.get("ProgramSettings", {}).get("LeftHemisphere", {}).get("Programs", [])

        right_settings = group.get("ProgramSettings", {}).get("RightHemisphere", {}).get("Programs", [])

        extracted.append({

            "GroupId": group_id,

            "Active": active,

            "Left_Programs": len(left_settings),

            "Right_Programs": len(right_settings)

        })

    return pd.DataFrame(extracted)



# Create summaries for initial and final groups

initial_group_df = extract_group_info(initial_groups)

final_group_df = extract_group_info(final_groups)



# Impedance data overview

impedance_data = results["impedance_data"]



# Signal analysis overview

sensing_channels = results["signal_analysis"].get("sensing_channels", [])



# Create summary DataFrames

impedance_df = pd.DataFrame(impedance_data)

sensing_channel_df = pd.DataFrame(sensing_channels)



# Display key summaries

{

    "initial_group_summary": initial_group_df,

    "final_group_summary": final_group_df,

    "impedance_data_overview": impedance_df.head(),

    "sensing_channel_overview": sensing_channel_df.head()

}



import matplotlib.pyplot as plt



# Extract and plot PSD values and Signal Frequencies for sensing channels

sensing_setup = sensing_channels[0].get("SensingSetup", {})

frequencies = sensing_setup.get("ChannelSignalResult", {}).get("SignalFrequencies", [])

psd_values = sensing_setup.get("ChannelSignalResult", {}).get("SignalPsdValues", [])



# Plot PSD vs. Frequency

plt.figure(figsize=(10, 6))

plt.plot(frequencies, psd_values, label="PSD", color="blue", linewidth=2)

plt.title("Power Spectral Density (PSD) vs Frequency", fontsize=14)

plt.xlabel("Frequency (Hz)", fontsize=12)

plt.ylabel("PSD Value", fontsize=12)

plt.grid(True, linestyle="--", alpha=0.7)

plt.legend()

plt.show()



# Impedance Data: Extract and simplify for visualization

left_hemisphere = impedance_data[0]["Hemisphere"][0]["SessionImpedance"]

right_hemisphere = impedance_data[0]["Hemisphere"][1]["SessionImpedance"]



# Focus on Monopolar values for simplicity

left_mono = pd.DataFrame(left_hemisphere["Monopolar"])

right_mono = pd.DataFrame(right_hemisphere["Monopolar"])



# Plot Monopolar Impedance for Left and Right Hemispheres

plt.figure(figsize=(14, 6))



plt.subplot(1, 2, 1)

plt.bar(left_mono["Electrode2"], left_mono["ResultValue"], color="teal")

plt.title("Left Hemisphere Monopolar Impedance", fontsize=14)

plt.xticks(rotation=45, fontsize=10)

plt.ylabel("Impedance (Ohms)", fontsize=12)

plt.grid(axis="y", linestyle="--", alpha=0.7)



plt.subplot(1, 2, 2)

plt.bar(right_mono["Electrode2"], right_mono["ResultValue"], color="coral")

plt.title("Right Hemisphere Monopolar Impedance", fontsize=14)

plt.xticks(rotation=45, fontsize=10)

plt.ylabel("Impedance (Ohms)", fontsize=12)

plt.grid(axis="y", linestyle="--", alpha=0.7)



plt.tight_layout()

plt.show()





frequency_bands = {
    "Delta": (0.5, 4),
    "Theta": (4, 8),
    "Alpha": (8, 12),
    "Beta": (12, 30),
    "Gamma": (30, 100),
}




def calculate_band_power(frequencies, psd_values, bands):
    band_powers = {}
    for band, (low, high) in bands.items():
        mask = (np.array(frequencies) >= low) & (np.array(frequencies) <= high)
        band_powers[band] = np.sum(np.array(psd_values)[mask])
    return band_powers


import matplotlib.pyplot as plt

band_powers = calculate_band_power(frequencies, psd_values, frequency_bands)
plt.bar(band_powers.keys(), band_powers.values(), color="skyblue", edgecolor="black")
plt.title("Signal Frequency Band Power Distribution")
plt.xlabel("Frequency Band")
plt.ylabel("Power")
plt.grid(axis="y", linestyle="--", alpha=0.7)
plt.show()
