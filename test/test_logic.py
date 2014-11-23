import sys
inputFileName = sys.argv[1]
outputFileName = sys.argv[2]

def convert_str(fileName):
	file = open(fileName, 'r')
	outputStr = file.read()
	testStr = ''.join(outputStr.split())
	return testStr

inputFile = convert_str(inputFileName)
outputFile = convert_str(outputFileName)

print inputFileName
print outputFileName
if (outputFile == inputFile):
	print "ACCEPT\n"
else:
	print "REJECT\n"