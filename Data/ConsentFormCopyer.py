##################################
#       ConsentFormCopyer        #
#     SENSEable Design Lab       #
##################################
# v1.0
# 4/19/2021
##################################
# EXP: 'python ConsentFormCopier.py'
##################################
# Authors: 
# Sermarini
##################################

import sys
import shutil
import os


def main(argv):
	assert len(argv) >= 1, "Error: No arguments for subjects and output directory location."
	assert len(argv) >= 2, "Error: No argument for output directory"
	assert os.path.isdir(argv[0]), "Error: " + argv[0] + " not a valid directory."

	output_dir = os.path.join(argv[1], "FRED_ConsentForms")
	if not os.path.isdir(output_dir):
		os.mkdir(output_dir)

	for file in os.listdir(argv[0]):
		dir_path = os.path.join(argv[0], file)
		if os.path.isdir(dir_path):
			subject = file
			consent_form_src = os.path.join(dir_path, subject + ".pdf")
			consent_form_dest = os.path.join(output_dir, subject + ".pdf")
			print("Copying", consent_form_src, "to", consent_form_dest)
			shutil.copy2(consent_form_src, consent_form_dest)
			print("====== SUCCESS ======")


if __name__=='__main__':
    main(sys.argv[1:])