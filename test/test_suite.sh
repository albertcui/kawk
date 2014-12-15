#/bin/bash
echo "-----------------------------------------"
echo "-----------------------------------------"
echo "----------BEGIN PRETTY PRINT-------------"
echo "-----------------------------------------"
echo "-----------------------------------------"

cd ../
make clean
if make pretty
	then
		cd test/
		for filename in *.k; do
			.././pretty < "$filename" > "output_$filename" 2>&1
			python test_logic.py pretty "$filename" "output_$filename"
		done
else
	exit
fi
cd ../
if make
	then
		cd test/
		echo "-----------------------------------------"
		echo "-----------------------------------------"
		echo "-----------BEGIN SEMANTIC CHK------------"
		echo "-----------------------------------------"
		echo "-----------------------------------------"
		rm -f test/output_semantic*.k
		for filename in semantic*.k; do
			.././semantic < "$filename" > "output_$filename" 2>&1
			python test_logic.py semantic "$filename" "output_$filename"
		done
		echo "-----------------------------------------"
		echo "-----------------------------------------"
		echo "-----------------END---------------------"
		echo "-----------------------------------------"
		echo "-----------------------------------------"
else
	exit
fi