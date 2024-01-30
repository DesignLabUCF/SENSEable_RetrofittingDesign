import sys
from PIL import Image
from os import listdir
import csv

def main():
	out_facades = []
	out_total = []
	out_white = []
	out_blocked = []
	img_sets = listdir("ViewFactorImages")
	for img_set in img_sets:
		imgs = listdir("ViewFactorImages" + "\\" + img_set)
		for img in imgs:
			if img[-4:] != ".png":
				continue
			print("==== " + img + "====")
			try:
				image = Image.open("ViewFactorImages" + "\\" + img_set + "\\" + img)
				width, height = image.size
				white = 0
				blocked = 0
				for x in range(width):
					for y in range(height):
						if is_white(image.getpixel((x, y))):
							white = white + 1
						else:
							blocked = blocked + 1
				print("Total:\n" + str(width*height))
				print("White:\n" + str(white))
				print("Blocked:\n" + str(blocked))
				out_facades.append(img[:-4])
				out_total.append(width*height)
				out_white.append(white)
				out_blocked.append(blocked)
			except Exception as e:
				raise e
	# Write info to CSV for easy copy paste to Google Sheet/R
	with open("CalcDaylightOutput\\out_ViewFactor.csv", 'w', newline='') as csvfile:
		writer = csv.writer(csvfile)
		writer.writerow(["Time", "Facade", "Thru", "Blocked", "Total"])
		for i in range(0, len(out_facades)):
			writer.writerow(["9", out_facades[i], out_white[i], out_blocked[i], out_total[i]])
			writer.writerow(["12", out_facades[i], out_white[i], out_blocked[i], out_total[i]])
			writer.writerow(["3", out_facades[i], out_white[i], out_blocked[i], out_total[i]])

def is_white(pixel):
	#print(pixel)
	if pixel == (255, 255, 255, 255):
		return True
	else:
		return False

if __name__ == "__main__":
	main()