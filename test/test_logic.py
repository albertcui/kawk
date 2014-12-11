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
file1 = open(inputFileName, 'r')
inputFileStr = file1.read()
file2 = open(outputFileName, 'a')
file2.write(" ")
file2.close()

file2 = open(outputFileName, 'r')
outputFileStr = file2.read()


#convert to string
inString = str(inputFileStr)
outString = str(outputFileStr)


inputTestStr = convert_str(inString)
outputTestStr = convert_str(outString)



# if the test should 'reject', the overall test will   
if (inputTestStr[:6] == "REJECT"):
	inputToCompare = inputTestStr[6:]
	shouldReject = True
else:
	inputToCompare = inputTestStr

print inputFileName
print outputFileName
if (outputTestStr == inputToCompare and shouldReject == False):
	print "ACCEPT\n"
elif (outputTestStr != inputToCompare and shouldReject == True):
	print "ACCEPT\n"
else:
	print "REJECT "
	print "The input file:"
	print inputFileStr
	print "The output file:"
	print outputFileStr
	print "\n"
