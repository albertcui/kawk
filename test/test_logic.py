#A simple test script to test for equality of 
#the output after parsed by the trees and printed by the pretty printed
#This will be expanded to accomodate more tests in the future

import sys
inputFileName = sys.argv[1]
outputFileName = sys.argv[2]

def convert_str(thestring):
	testStr = ''.join(thestring.split())
	return testStr

#read the files
file = open(inputFileName, 'r')
inputFileStr = file.read()
file = open(outputFileName, 'r')
outputFileStr = file.read()

#convert to string
inString = str(inputFileStr)
outString = str(outputFileStr)

#convert the string by taking out spaces, newlines, and tabs
inputTestStr = convert_str(inString)
outputTestStr = convert_str(outString)
print inputFileName
print outputFileName
if (outputTestStr == inputTestStr):
	print "ACCEPT\n"
else:
	print "REJECT "
	print "The input file:"
	print inputFileStr
	print "The output file:"
	print outputFileStr
	print "\n"
