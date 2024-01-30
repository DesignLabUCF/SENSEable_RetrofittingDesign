##################################
#       FredFitFileReader        #
#     SENSEable Design Lab       #
##################################
# v1.0
# 10/08/2021
##################################
# To run, 
#  arg1: Processed .csv file name (with extension) input file
#  arg2: Trimmed .csv file name(with extension) output file
# EXP: python FitReader.py input.csv output.csv
##################################


## Garmin FIT docs
# https://developer.garmin.com/fit/protocol/
## Fit timestamp converter
# http://www.nlvocables.com/blog/?p=969
## Java conv. Tool
# https://developer.garmin.com/fit/fitcsvtool/windows/


import sys
#import fitparse
import datetime
import pytz
import csv


"""
fit_events format
[[
  datetime in UTC,
  datetime in EST,
  heart rate,
  stress level
  ], 
  ...]
"""


UTC_REFERENCE = 631065600
datetime_format = "%m/%d/%Y, %H:%M:%S"
desired_timezone = "America/New_York" # https://en.wikipedia.org/wiki/List_of_tz_database_time_zones


def main():
	#input_filename = "D:\\UnrealProjects\\AR_Facades\\Data\\Watch\\1\\108722155948_WELLNESS.csv" # sys.argv[1]
	#output_filename = "D:\\UnrealProjects\\AR_Facades\\Data\\Watch\\1\\1_heartrate.csv"
	input_filename = sys.argv[1]
	output_filename = sys.argv[2]
	fit_events = [] 

	with open(input_filename, newline="") as csvfile:
		reader = csv.reader(csvfile)
		latest_timestamp = 0
		for row in reader:
			heart_rate_timestamp_offset = 0 # TODO remove line
			heart_rate_timestamp = 0 # TODO remove line 
			heart_rate = 0 # TODO remove line
			# Timestamp event
			if row[3] == "timestamp":
				# Read in values from converted csv
				latest_timestamp = int(row[4])
			# Heart rate event
			elif row[6] == "heart_rate":
				# Read in values from converted csv
				heart_rate_timestamp_offset = int(row[4])
				heart_rate = int(row[7])
				# Irrelevant reading
				if heart_rate == 0 or heart_rate == 1:
					continue
				# Combine 16 bit timestamp with upper end of 32 bit timestamp
				# https://www.thisisant.com/forum/viewthread/6374/
				heart_rate_timestamp = ((heart_rate_timestamp_offset - (latest_timestamp & 65535)) & 65535) + latest_timestamp
				fit_events.append(get_formatted_event_data(heart_rate_timestamp, heart_rate, -1))
			## Stress event
			elif row[6] == "stress_level_value":
				stress_timestamp = int(row[4])
				stress_level = int(row[7])
				# Irrelevant reading
				if stress_level == -1 or stress_level == 1 or stress_level == -2:
					continue
				fit_events.append(get_formatted_event_data(stress_timestamp, -1, stress_level))

		fit_events = combine_fit_event_entries(fit_events)

		with open(output_filename, 'w', newline='') as csvfile:
			writer = csv.writer(csvfile)
			writer.writerow(["Index", \
				"timestamp_datetime_utc", \
				"utc_hour", \
				"utc_min", \
				"utc_sec", \
				"timestamp_datetime_est", \
				"est_hour", \
				"est_min", \
				"est_sec", \
				"heart_rate", \
				"stress_level"])
			for i in range(0, len(fit_events)):
				print("Writing line " + str(i) + "/" + str(len(fit_events)) + "...")
				datetime_utc = fit_events[i][0].strftime(datetime_format)
				utc_hour = int(fit_events[i][0].strftime("%H"))
				utc_min = int(fit_events[i][0].strftime("%M"))
				utc_sec = int(fit_events[i][0].strftime("%S"))
				datetime_est = fit_events[i][1].strftime(datetime_format)
				est_hour = int(fit_events[i][1].strftime("%H"))
				est_min = int(fit_events[i][1].strftime("%M"))
				est_sec = int(fit_events[i][1].strftime("%S"))
				heart_rate = int(fit_events[i][2])
				stress_level = int(fit_events[i][3])
				writer.writerow([str(i), \
					datetime_utc, \
					utc_hour, \
					utc_min, \
					utc_sec, \
					datetime_est,
					est_hour, \
					est_min, \
					est_sec, \
					heart_rate, \
					stress_level])
			print(output_filename + " generated!")


def get_formatted_event_data(timestamp_int, heart_rate, stress_level):
				datetime_utc = datetime.datetime.utcfromtimestamp(UTC_REFERENCE + timestamp_int)
				datetime_est = datetime_utc.replace(tzinfo=pytz.utc).astimezone(pytz.timezone(desired_timezone))
				return [datetime_utc, datetime_est, heart_rate, stress_level]


def combine_fit_event_entries(input_fit_events):
	print("Combining event entries with identical timestamps...")
	# Loop thru backwards so removal is safe
	for i in range(len(input_fit_events) - 1, -1, -1):
		hour = int(input_fit_events[i][0].strftime("%H"))
		minute = int(input_fit_events[i][0].strftime("%M"))
		second = int(input_fit_events[i][0].strftime("%S"))
		# Check against other entries
		for j in range(0, len(input_fit_events)):
			if i == j: 
				continue
			comp_hour = int(input_fit_events[j][0].strftime("%H"))
			comp_minute = int(input_fit_events[j][0].strftime("%M"))
			comp_second = int(input_fit_events[j][0].strftime("%S"))
			# Two entries have the same time stamp
			if hour == comp_hour and minute == comp_minute and second == comp_second:
				print("Combining event indexes " + str(i) + " and " + str(j))
				heart_rate = max(input_fit_events[i][2], input_fit_events[j][2])
				stress_level = max(input_fit_events[i][3], input_fit_events[j][3])
				input_fit_events[i][2] = heart_rate
				input_fit_events[i][3] = stress_level
				input_fit_events.pop(j)
				break
	return input_fit_events


########## MAIN ##########
if __name__ == "__main__":
	main()