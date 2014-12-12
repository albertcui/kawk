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
			.././pretty < "$filename" > "output_$filename"
			python test_logic.py "$filename" "output_$filename"
		done
else
	exit
fi
if make
	then
		cd test/
		echo "-----------------------------------------"
		echo "-----------------------------------------"
		echo "-----------BEGIN SEMANTIC CHK------------"
		echo "-----------------------------------------"
		echo "-----------------------------------------"
		for filename in *.k; do
			.././semantic < "$filename" > "output_$filename"
		done
		echo "-----------------------------------------"
		echo "-----------------------------------------"
		echo "-----------------END---------------------"
		echo "-----------------------------------------"
		echo "-----------------------------------------"
else
	exit
fi