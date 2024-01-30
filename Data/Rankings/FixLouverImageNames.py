import sys
import os

try:
	imgs = os.listdir()
	for img in imgs:
		if img[-4:] != ".png":
			continue
		new_name = img
		if("e5" in new_name):
			new_name = new_name.replace("e5", "5")
		if("e10" in new_name):
			new_name = new_name.replace("e10", "10")
		if("r10" in new_name):
			new_name = new_name.replace("r10", "10")
		if("r20" in new_name):
			new_name = new_name.replace("r20", "20")
		if("r30" in new_name):
			new_name = new_name.replace("r30", "30")
		print(img + " => " + new_name)
		os.rename(img, new_name)
except Exception as e:
	raise e
	sys.exit()