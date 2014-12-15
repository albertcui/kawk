#A simple test script to test for equality of 
#the output after parsed by the trees and printed by the pretty printer
#This will be expanded to accomodate more tests in the future

import sys
mode = sys.argv[1]
inputFileName = sys.argv[2]
outputFileName = sys.argv[3]
semanticReject = False
semantic = False
syntax = True
syntaxReject = False
if (inputFileName[:8] == "semantic"):
	semantic = True
	syntax = False
	if (inputFileName[-11:] == "rejectsem.k"):
		semanticReject = True 
else:
	print inputFileName[-8:] == "reject.k"
	if (inputFileName[-8:] == "reject.k"):
		syntaxReject = True

shouldReject = False

#convert the string by taking out spaces, newlines, and tabs
def convert_str(thestring):
	testStr = ''.join(thestring.split())
	return testStr

#read the files
file1 = open(inputFileName, 'r')
inputFileStr = file1.read()
file2 = open(outputFileName, 'r')
outputFileStr = file2.read()

#convert to string
inString = str(inputFileStr)
outString = str(outputFileStr)

inputTestStr = convert_str(inString)
outputTestStr = convert_str(outString)

if (syntax == True and mode == "pretty"):
	print inputFileName
	print outputFileName
	if (outputTestStr == inputTestStr and syntaxReject == False):
		print "Syntax test ACCEPTED\n"
	elif (outputTestStr != inputTestStr and syntaxReject == True):
		print "Syntax test ACCEPTED\n"
	else:
		print "Syntax test REJECTED"
		print "The input file:"
		print inputFileStr
		print "The output file:"
		print outputFileStr
		print "\n"

elif (semantic == True and mode == "semantic" ):
	print inputFileName
	print outputFileName

	#print outputTestStr
	try:
		outputTestStr.index("Fatalerror:")
		if (semanticReject == True):
			print "Semantic test ACCEPTED\n"
		elif (semanticReject == False):
			print "Semantic Test REJECTED"
			print "The input file:"
			print inputFileStr
			print "The error:"
			print outputFileStr
			print "\n"
	except ValueError:
		if (semanticReject == False):
			print "Semantic test ACCEPTED\n"
		elif (semanticReject == True):
			print "Semantic Test REJECTED"
			print "The input file:"
			print inputFileStr
			print "The error:"
			print outputFileStr
			print "\n"
