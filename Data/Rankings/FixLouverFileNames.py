import sys
import os

files = os.listdir()
for file in files:
	if ("louver" in file) and ("70" in file):
		new_name = file.replace("70", "75")
		os.rename(file, new_name)
		print("Replaced file: " + file + " => " + new_name)