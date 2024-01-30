##################################
#     ChoiceTimeCalculator       #
#     SENSEable Design Lab       #
##################################
# v1.0
# 3/4/2021
##################################
# To run, ~
# EXP: 'python ChoiceTimeCalculator.py'
##################################
# Authors: 
# Sermarini
##################################


import sys
import os
import csv
from datetime import datetime
import pandas as pd

datetime_format = '%H:%M:%S.%f'
register_duration = 2.0 # Seconds user spends on a facade before its registered
base_df = pd.DataFrame({ # Used as base for copying and adding to
    "DYNAMIC_0" : 0.0,
    "DYNAMIC_30" : 0.0,
    "DYNAMIC_60" : 0.0,
    "DYNAMIC_90" : 0.0,
    "FIN_10_30" : 0.0,
    "FIN_10_70" : 0.0,
    "FIN_20_30" : 0.0,
    "FIN_20_70" : 0.0,
    "LOUVER_10_30" : 0.0,
    "LOUVER_10_70" : 0.0,
    "LOUVER_20_30" : 0.0,
    "LOUVER_20_70" : 0.0,
    "FRITT_15_5" : 0.0,
    "FRITT_15_10" : 0.0,
    "FRITT_30_5" : 0.0,
    "FRITT_30_10" : 0.0},
    index=["BASE"])
facade_names = {
    "facade1_0" : "DYNAMIC_0",
    "facade1_30" : "DYNAMIC_30",
    "facade1_60" : "DYNAMIC_60",
    "facade1_90" : "DYNAMIC_90",
    "fin_10_30" : "FIN_10_30",
    "fin_10_70" : "FIN_10_70",
    "fin_20_30" : "FIN_20_30",
    "fin_20_70" : "FIN_20_70",
    "louver_hor_10_30" : "LOUVER_10_30",
    "louver_hor_10_75" : "LOUVER_10_70",
    "louver_hor_20_30" : "LOUVER_20_30",
    "louver_hor_20_75" : "LOUVER_20_70",
    "fritt_15_5" : "FRITT_15_5",
    "fritt_15_10" : "FRITT_15_10",
    "fritt_30_5" : "FRITT_30_5",
    "fritt_30_10" : "FRITT_30_10"
}
calendar = {
    "October" : 10,
    "November" : 11,
    "December" : 12,
    "January" :1, 
    "February" :2, 
    "March" : 3, 
    "April" : 4, 
    "May" : 5, 
    "June" : 6, 
    "July" : 7, 
    "August" : 8, 
    "September" : 9, 
}

subjects_log = []
facade_log = []
datetime_log = []
seconds_log = []
time_spent_log = []

# Times when to cut off readings for each subject
# Only get the AR - Conf ones. Wasn't properly logging before then
def get_termination_times():
    file_name = "Subjects\\Log.csv"
    assert os.path.exists(file_name), "Error file " + file_name + " not found in directory"
    df = pd.read_csv(file_name,
        encoding='latin1',
        sep=",",
        skiprows=1,
        usecols=['Participant ID', 'Year', 'Month', 'Day', 'Condition', 'Room', 'EndTime'])
    # Remove all but AR-conf
    df = df[df.Condition == "AR"]
    df = df[df.Room == "Conference"]
    datetimes = convert_datetimes(df)
    df["DateTime"] = datetimes
    print("Termination times:")
    print(df)
    """
    for index, row in df.iterrows():
        participant_id = row["Participant ID"]
        timestamp = row["DateTime"]
        print(participant_id, "-",timestamp.strftime('%m/%d/%Y, %H:%M:%S.%f')[:-3])
    """
    return df

# Return list of each datetime in order passed in
def convert_datetimes(df):
    datetimes = []
    # Read rows
    for i in range(0, len(df.index)):
        # Get date
        year = int(df["Year"].values[i])
        month = calendar[df["Month"].values[i]]
        day = int(df["Day"].values[i])
        # Get end time
        endTime = df["EndTime"].values[i]
        if endTime == "NaN":
            datetimes.append(None)
            continue
        hour = int(endTime.split(".")[0])
        if hour < 8: # Tests are sometime between 10 am and 6 pm, so 8 am will be safe cutoff
            hour = hour + 12 # Convert to 24 hour count
        minute = int(endTime.split(".")[1])
        second = int(endTime.split(".")[2])
        millisecond = int(endTime.split(".")[3].strip())
        # Convert to datetime
        timestamp = datetime(int(year), int(month), int(day), int(hour), int(minute), int(second), int(int(millisecond) * 1000.0))
        datetimes.append(timestamp)
    return datetimes

def get_time_from_csv_cell(cell):
    # Date
    date = cell.split("-")[0]
    year = date.split("_")[0]
    month = date.split("_")[1]
    day = date.split("_")[2]
    # Time
    time = cell.split("-")[1]
    hour = time.split(":")[0]
    hour = int(hour) - 5 # Convert from utc to eastern
    minute = time.split(":")[1]
    second = time.split(":")[2].split(".")[0]
    millisecond = time.split(":")[2].split(".")[1].strip()
    return datetime(int(year), int(month), int(day), int(hour), int(minute), int(second), int(int(millisecond) * 1000.0))

# Return datetime for correct subject from "termination_times" dataframe
def find_termination_time(subject, termination_times):
    for index, row in termination_times.iterrows():
        participant_id = row["Participant ID"]
        if str(subject) == str(participant_id):
            print("TERIMNATION TIME FOUND")
            print(subject, "-",row["DateTime"])
            return row["DateTime"]
    return None


def parse_subject(subject, reader, termination_time=None):
    ## Init
    # Time spent on each
    df = base_df.copy()
    df = df.rename(index={"BASE" : subject})
    bGUI_Test_Complete = False
    i = 0
    # For data logging
    last_facade = None
    last_timestamp = None
    start_timestamp = None
    for row in reader:
        if i == 0:
            i = i + 1
            continue
        # Get info from row
        log_type = row[0]
        timestamp = get_time_from_csv_cell(row[2]) 
        # Subject has confirmed to me they have made a decision so cut it off here
        if(termination_time != None and timestamp >= termination_time): # Past termination time, so subject has finished
                    print("TERMINATION REACHED - Termination time -", termination_time.strftime('%m/%d/%Y, %H:%M:%S.%f')[:-3])
                    print("Terminating at time -", timestamp.strftime('%m/%d/%Y, %H:%M:%S.%f')[:-3])
                    print(timestamp)
                    df_facade_name = facade_names[last_facade]
                    df[df_facade_name].values[0] = df[df_facade_name].values[0] + time_on_last_facade
                    print(subject, "termination time reached. Current Time:", timestamp.strftime('%m/%d/%Y, %H:%M:%S.%f')[:-3])
                    break
        # GUI training complete so start tracking
        if log_type == "GUI_Test_Complete":
            #print(subject + " - " + get_time_from_csv_cell(row[2]).strftime(datetime_format)[:-3])
            bGUI_Test_Complete = True
        # Head data not relevant atm
        if log_type == "Head": 
            continue
        # Subject changed a setting in the menu
        if log_type == "Settings_Changed":
            if bGUI_Test_Complete == False: # GUI Test not complete so still in training
                continue
            setting_changed = row[9]
            if setting_changed == "Facade":
                new_facade = row[3]
                if last_timestamp != None:
                    time_on_last_facade = (timestamp - last_timestamp).total_seconds()
                    df_facade_name = facade_names[last_facade]
                    if time_on_last_facade >= register_duration:
                        # Time sum 
                        df[df_facade_name].values[0] = df[df_facade_name].values[0] + time_on_last_facade
                        print("Time spent on facade (this time):", time_on_last_facade)
                        print("Time spent on",df_facade_name, "so far:", df[df_facade_name].values[0])
                        # Changes log
                        subjects_log.append(subject)
                        facade_log.append(df_facade_name)
                        #datetime_log.append(timestamp)
                        datetime_log.append(last_timestamp)
                        #seconds_log.append((timestamp - start_timestamp).total_seconds())
                        seconds_log.append((last_timestamp - start_timestamp).total_seconds())
                        time_spent_log.append(time_on_last_facade)
                    else:
                        print("Not enough time on", df_facade_name, ":", time_on_last_facade)
                    print("=================================")
                else: # First log
                    start_timestamp = timestamp
                # Update trackers for next change
                last_facade = new_facade
                last_timestamp = timestamp
        i = i + 1
    #print(df)
    return df

if __name__=='__main__':
    # Get termination times
    termination_times = get_termination_times()

    # Read subjects
    subjects = os.listdir("Subjects")
    df_master = base_df.copy()
    for subject in subjects:
        if "." in subject: # Only get directories, don't need the random files floating around
            continue
        # Open file
        file_name = "Subjects\\" + subject + "\\" + subject + ".csv"
        assert os.path.exists(file_name), "Error file " + file_name + " not found in directory"
        with open(file_name, "r", newline='') as csvfile:
            reader = csv.reader(csvfile, delimiter=',', quotechar='|')
            termination_time = find_termination_time(subject, termination_times)
            df = parse_subject(subject, reader, termination_time)
            df_master = df_master.append(df, ignore_index = False)

    # Create output file for time sums
    df_master = df_master.drop(index="BASE") # Remove base row
    print(df_master)
    df_master.to_csv("Subjects\\FacadeTimes.csv")

    # Create output file for when changes happened
    df_log = pd.DataFrame({
        "Subject" : subjects_log,
        "Facade" : facade_log,
        "DatetimeChanged" : datetime_log,
        "TimeSwitchTo" : seconds_log,
        "TimeSpentOn" : time_spent_log
        })
    df_log.to_csv("Subjects\\FacadeChoiceLog.csv")
