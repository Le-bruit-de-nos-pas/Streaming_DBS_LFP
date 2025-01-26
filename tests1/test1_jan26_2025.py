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

