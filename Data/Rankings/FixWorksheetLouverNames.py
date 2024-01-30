import sys
from openpyxl import load_workbook
from openpyxl import worksheet

try:
	wb = load_workbook("conf_Pos1.xlsx")
	for sheet_name in wb:
		ws = wb[sheet_name.title]
		#new_name = file.replace("70", "75")
		print(sheet_name)
		if("e5" in ws.title):
			ws.title = ws.title.replace("e5", "5")
		if("e10" in ws.title):
			ws.title = ws.title.replace("e10", "10")
		if("r10" in ws.title):
			ws.title = ws.title.replace("r10", "10")
		if("r20" in ws.title):
			ws.title = ws.title.replace("r20", "20")
		if("r30" in ws.title):
			ws.title = ws.title.replace("r30", "30")
	wb.save("conf_Pos1.xlsx")
except Exception as e:
	raise e
	sys.exit()