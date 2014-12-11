#A simple test script to test for equality of 
#the output after parsed by the trees and printed by the pretty printer
#This will be expanded to accomodate more tests in the future

#Author: Karen Nan 

import sys
inputFileName = sys.argv[1]
outputFileName = sys.argv[2]
#flag that 
shouldReject = False

#convert the string by taking out spaces, newlines, and tabs
def convert_str(thestring):
	testStr = ''.join(thestring.split())
	return testStr

#read the files
file = open(inputFileName, 'r')
inputFileStr = file.read()
file = open(outputFileName, 'a+')
file.write(" ")
outputFileStr = file.read()

#convert to string
inString = str(inputFileStr)
outString = str(outputFileStr)


inputTestStr = convert_str(inString)
outputTestStr = convert_str(outString)


print outputTestStr

# if the test should 'reject', the overall test will   
if (inputTestStr[:6] == "REJECT"):
	shouldReject = True

print inputFileName
print outputFileName
if (outputTestStr == inputTestStr and shouldReject == False):
	print "ACCEPT\n"
else:
	print "REJECT "
	print "The input file:"
	print inputFileStr
	print "The output file:"
	print outputFileStr
	print "\n"
