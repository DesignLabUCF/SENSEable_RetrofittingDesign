##################################
#        SubjectMovement         #
#     SENSEable Design Lab       #
##################################
# v1.0
# 8/28/2021
##################################
# To run, ~
# EXP: 'python SubjectMovement.py'
##################################
# Authors: 
# Sermarini
##################################


import sys
import os
import csv
from datetime import datetime
import pandas as pd
import math
import numpy as np # For angle vectors
import random # For testing 

datetime_format = '%H:%M:%S.%f'
register_duration = 2.0 # Seconds user spends on a facade before its registered
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
facade_optimality_df = None
subject_rooms_df = None

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
    #print("Termination times:")
    #print(df)
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

def distance_between_two_points(x1, y1, z1, x2, y2, z2):
    d = math.sqrt((x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2)
    return d
    #P1(1,1,0) and point P2(2,1,2)
    #d = ((2 - 1)2 + (1 - 1)2 + (2 - 0)2)1/2

# Credit: https://stackoverflow.com/questions/2827393/angles-between-two-n-dimensional-vectors-in-python
def unit_vector(v):
    # Catch invalid division and return 'skip' for this timestamp 
    if(np.linalg.norm(v) == 0):
        return v, False
    return (v / np.linalg.norm(v)), True

def deg_to_rad(deg):
    return deg * (math.pi / 180.0)

# Credit: https://stackoverflow.com/questions/2827393/angles-between-two-n-dimensional-vectors-in-python
def rotation_between_two_angles(x1, y1, z1, x2, y2, z2):
    x1 = deg_to_rad(x1)
    y1 = deg_to_rad(y1)
    #z1 = deg_to_rad(z1) # Ignore roll for now
    v1 = np.array([x1,y1,0])
    u_v1, v1_pass_test = unit_vector(v1)

    x2 = deg_to_rad(x2)
    y2 = deg_to_rad(y2)
    #z2 = deg_to_rad(z2) # Ignore roll for now
    v2 = np.array([x2,y2,0])
    u_v2, v2_pass_test = unit_vector(v2)

    # Invalid output, so skip
    #if v1 == None or v2 == None:
    #    return 0
    if v1_pass_test == False or v2_pass_test == False:
        return 0
    return np.arccos(np.clip(np.dot(u_v1, u_v2), -1.0, 1.0)) # Radians

def eye_rotation(x1, y1, z1, x2, y2, z2):
    v1 = np.array([x1,y1,0])
    u_v1, v1_pass_test = unit_vector(v1)

    v2 = np.array([x2,y2,0])
    u_v2, v2_pass_test = unit_vector(v2)

    # Invalid output, so skip
    #if v1 == None or v2 == None:
    #    return 0
    if v1_pass_test == False or v2_pass_test == False:
        return 0
    return np.arccos(np.clip(np.dot(u_v1, u_v2), -1.0, 1.0)) # Radians

def time_between_timestamps(current_timestamp, previous_timestamp):
    return (current_timestamp - previous_timestamp).microseconds / 1000000.0

def get_subject_room(subject):
    room = subject_rooms_df.loc[(subject_rooms_df["Participant ID"] == int(subject)), "Room"].values[0]
    if "CUT" in room:
        room = room[:-6]
    return room
    #return "Office"

def check_facade(room, facade):
    row = facade_optimality_df.loc[(facade_optimality_df["Room"] == room) & (facade_optimality_df["Facade"] == facade)]
    optimal = bool((int)(row["Optimal"]))
    return optimal

def parse_subject(subject, reader, room, termination_time=None):
    bGUI_Test_Complete = False
    i = 0
    # Position logging
    previous_position = None
    previous_rotation = None
    previous_gaze = None
    previous_gaze_timestamp = None # Gaze needs special rules since it is not logged every single time
    previous_timestamp = None
    distances = []
    rotations = []
    gaze_rotations = []
    # Facade logging
    #current_facade = None
    current_facade = "FIN_20_30"
    distances_on_optimal = []
    rotations_on_optimal = []
    gaze_on_optimal = [] # UPDATE
    gaze_time_on_optimal = [] # Update
    time_on_optimal = []
    distances_on_poor = []
    rotations_on_poor = []
    gaze_on_poor = [] # UPDATE
    gaze_time_on_poor = [] # Update
    time_on_poor = []
    # Time
    start_timestamp = None
    times = [] 
    gaze_times = [] # Gaze needs special rules since it is not logged every single time
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
                    print(subject, "termination time reached. Current Time:", timestamp.strftime('%m/%d/%Y, %H:%M:%S.%f')[:-3])
                    break
        # GUI training complete so start tracking
        if log_type == "GUI_Test_Complete":
            bGUI_Test_Complete = True
            print("GUI_Test_Complete")
        # Movement tracking
        if log_type == "Head" and bGUI_Test_Complete == True: 
            # Head position
            position = (float(row[10]), float(row[11]), float(row[12]))
            # Head rotation
            rotation = (float(row[13]), float(row[14]), float(row[15]))
            # Eye gaze
            gaze = None
            gaze_valid = bool(row[22])
            if gaze_valid == True:
                gaze = (float(row[26]), float(row[27]), float(row[28]))
                #previous_gaze_timestamp = timestamp
            else:
                previous_gaze = None
                previous_gaze_timestamp = None
            if(previous_position != None):
                # Body
                distance = distance_between_two_points(position[0], position[1], position[2], previous_position[0], previous_position[1], previous_position[2])
                distances.append(distance)
                # Head
                rotation_travelled = rotation_between_two_angles(rotation[0], rotation[1], rotation[2], previous_rotation[0], previous_rotation[1], previous_rotation[2])
                rotations.append(rotation_travelled)
                # Eye gaze
                gaze_travelled = 0
                if (gaze_valid == True) and (previous_gaze != None): # Only use gaze if it and the most previous data row are valid
                    # TODO review gaze distance calculation
                    gaze_distance = distance_between_two_points(gaze[0], gaze[1], gaze[2], previous_gaze[0], previous_gaze[1], previous_gaze[2]) # Original (THIS ONE IS CORRECT BECAUSE THEY ARE VECTORS)
                    #gaze_distance = rotation_between_two_angles(gaze[0], gaze[1], gaze[2], previous_gaze[0], previous_gaze[1], previous_gaze[2]) # 2nd
                    #gaze_distance = eye_rotation(gaze[0], gaze[1], gaze[2], previous_gaze[0], previous_gaze[1], previous_gaze[2])
                    gaze_rotations.append(gaze_distance)
                    gaze_time = time_between_timestamps(timestamp, previous_gaze_timestamp)
                    gaze_times.append(gaze_time)
                    if current_facade != None:
                        is_optimal = check_facade(room, current_facade)
                        if is_optimal:
                            gaze_on_optimal.append(gaze_distance)
                            gaze_time_on_optimal.append(gaze_time)
                        else:
                            gaze_on_poor.append(gaze_distance)
                            gaze_time_on_poor.append(gaze_time)
                # Time
                time = time_between_timestamps(timestamp, previous_timestamp)
                times.append(time)
                # Whether the facade is optimal or not
                if current_facade != None:
                    is_optimal = check_facade(room, current_facade)
                    if is_optimal:
                            distances_on_optimal.append(distance)
                            rotations_on_optimal.append(rotation_travelled)
                            time_on_optimal.append(time)
                    else:
                            distances_on_poor.append(distance)
                            rotations_on_poor.append(rotation_travelled)
                            time_on_poor.append(time)
            # Update tracking values
            previous_position = position
            previous_rotation = rotation
            previous_timestamp = timestamp
            if gaze_valid == True:
                previous_gaze = gaze
                previous_gaze_timestamp = timestamp
        # Subject changed a setting in the menu
        if log_type == "Settings_Changed" and bGUI_Test_Complete == True:
            setting_changed = row[9]
            if setting_changed == "Facade":
                current_facade = facade_names[row[3]]

            # This was a misguided experiment VVVV
            """
            setting_changed = row[9]
            if setting_changed == "Facade":
                new_facade = facade_names[row[3]]
                if previous_facade_timestamp != None:
                    time_on_previous_facade = (timestamp - previous_facade_timestamp).total_seconds()
                    is_facade_good = check_facade(subject_room, facade) #
            previous_facade = new_facade
            previous_facade_timestamp = timestamp
            """
        i = i + 1
    
    # Raw body and head
    distance_sum = 0
    rotations_sum = 0
    time_sum = 0
    for i in range(0, len(distances)):
        distance_sum = distance_sum + distances[i]
        rotations_sum = rotations_sum + rotations[i]
        time_sum = time_sum + times[i]
    # Facade related
    optimal_distance_sum = 0
    optimal_rotations_sum = 0
    optimal_time_sum = 0
    for i in range(0, len(distances_on_optimal)):
        optimal_distance_sum = optimal_distance_sum + distances_on_optimal[i]
        optimal_rotations_sum = optimal_rotations_sum + rotations_on_optimal[i]
        optimal_time_sum = optimal_time_sum + time_on_optimal[i]
    poor_distance_sum = 0
    poor_rotations_sum = 0
    poor_time_sum = 0
    for i in range(0, len(distances_on_poor)):
        poor_distance_sum = poor_distance_sum + distances_on_poor[i]
        poor_rotations_sum = poor_rotations_sum + rotations_on_poor[i]
        poor_time_sum = poor_time_sum + time_on_poor[i]
    # Eye gaze
    gaze_sum = 0
    gaze_time_sum = 0
    gaze_ratio = 0
    for i in range(0, len(gaze_rotations)):
        gaze_sum = gaze_sum + gaze_rotations[i]
        gaze_time_sum = gaze_time_sum + gaze_times[i]
    if gaze_time_sum != 0:
        gaze_ratio = (float(gaze_sum) / float(gaze_time_sum))
    gaze_on_optimal_sum = 0
    gaze_time_optimal_sum = 0
    gaze_optimal_ratio = 0
    for i in range(0, len(gaze_on_optimal)):
        gaze_on_optimal_sum = gaze_on_optimal_sum + gaze_on_optimal[i]
        gaze_time_optimal_sum = gaze_time_optimal_sum + gaze_time_on_optimal[i]
    if gaze_on_optimal_sum != 0:
        gaze_optimal_ratio = (float(gaze_on_optimal_sum) / float(gaze_time_optimal_sum))
    gaze_on_poor_sum = 0
    gaze_time_poor_sum = 0
    gaze_poor_ratio = 0
    for i in range(0, len(gaze_on_poor)):
        gaze_on_poor_sum = gaze_on_poor_sum + gaze_on_poor[i]
        gaze_time_poor_sum = gaze_time_poor_sum + gaze_time_on_poor[i]
    if gaze_on_optimal_sum != 0:
        gaze_poor_ratio = (float(gaze_on_poor_sum) / float(gaze_time_poor_sum))

    return (float(distance_sum) / float(time_sum)), (float(rotations_sum) / float(time_sum)), (float(optimal_distance_sum) / float(optimal_time_sum)), (float(poor_rotations_sum) / float(optimal_time_sum)), (float(poor_distance_sum) / float(poor_time_sum)), (float(poor_rotations_sum) / float(poor_time_sum)), gaze_ratio, gaze_optimal_ratio, gaze_poor_ratio, float(time_sum), float(optimal_time_sum), float(poor_time_sum)
    """
    ratios = []
    ratios_sum = 0
    for i in range(0, len(distances)):
        ratio = float(distances[i]) / float(times[i])
        ratios_sum = ratios_sum + ratio
        ratios.append(ratio)
    return (ratio / float(len(ratios)))
    """


def main(argv):
    movement_ratio_data = [] # [(subject id, movement ratio, rotation ratio), (~, ~), ...]

    # Get termination times
    termination_times = get_termination_times()

    # Get facade optimality
    global facade_optimality_df
    file_name = "Rankings\\FacadeOptimalData.csv"
    assert os.path.exists(file_name), "Error file " + file_name + " not found in directory"
    facade_optimality_df = pd.read_csv(file_name,
        encoding='latin1',
        sep=",",
        skiprows=0)
    print(facade_optimality_df)

    # Get subject rooms
    global subject_rooms_df
    file_name = "Subjects\\Log.csv"
    assert os.path.exists(file_name), "Error file " + file_name + " not found in directory"
    subject_rooms_df = pd.read_csv(file_name,
        encoding='latin1',
        sep=",",
        skiprows=1,
        usecols=['Participant ID', 'Room'])

    # Read subjects
    subjects = os.listdir("Subjects")
    for subject in subjects:
        if "." in subject: # Only get directories, don't need the random files floating around
            continue
        # Open file
        file_name = "Subjects\\" + subject + "\\" + subject + ".csv"
        #assert os.path.exists(file_name), "Error file " + file_name + " not found in directory"
        if(os.path.exists(file_name)):
            subject_room = get_subject_room(subject)
            with open(file_name, "r", newline='') as csvfile:
                reader = csv.reader(csvfile, delimiter=',', quotechar='|')
                termination_time = find_termination_time(subject, termination_times)
                body_ratio, head_ratio, optimal_body_ratio, optimal_head_ration, poor_body_ratio, poor_head_ratio, gaze_ratio, gaze_optimal_ratio, gaze_poor_ratio, time_sum, optimal_time_sum, poor_time_sum = parse_subject(subject, reader, subject_room, termination_time)
                movement_ratio_data.append((subject, body_ratio, head_ratio, optimal_body_ratio, optimal_head_ration, poor_body_ratio, poor_head_ratio, gaze_ratio, gaze_optimal_ratio, gaze_poor_ratio, time_sum, optimal_time_sum, poor_time_sum))
                print(movement_ratio_data[len(movement_ratio_data) - 1])

    # Make dataframe and create output file
    subjects = []
    movement_ratios = []
    for r in movement_ratio_data:
        subjects.append(r[0])
        subjects.append(r[1])
        subjects.append(r[2])
        subjects.append(r[3])
        subjects.append(r[4])
        subjects.append(r[5])
        subjects.append(r[6])
        subjects.append(r[7])
        subjects.append(r[8])
        subjects.append(r[9])
        subjects.append(r[10])
        subjects.append(r[11])
        subjects.append(r[12])
    out_data = pd.DataFrame(movement_ratio_data, columns=["Participant ID", "Movement Ratio", "Head Ratio", "Optimal Movement Ratio", "Optimal Head Ratio", "Poor Movement Ratio", "Poor Head Ratio", "Gaze Ratio", "Optimal Gaze Ratio", "Poor Gaze Ratio", "Total Time", "Total Time Optimal", "Total Time Poor"])
    print(out_data)
    out_data.to_csv("Subjects\\SubjectMovement.csv")

######### MAIN #########
if __name__=='__main__':
    main(sys.argv[1:])