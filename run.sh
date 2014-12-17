#/bin/bash

./code_gen < "$1" > "Program.java" 2>&1
javac "Program.java"
java "Program" | tee  "output_java_$1.txt" 2>&1
