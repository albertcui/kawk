#/bin/bash

./code_gen < "$2" > "Program.java" 2>&1
javac "Program.java"
java "Program" > "output_java_$2.txt" 2>&1