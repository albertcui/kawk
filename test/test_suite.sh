#/bin/bash
rm output*.k
for filename in *.k; do
	.././pretty < "$filename" > "output_$filename"
	python test_logic.py "$filename" "output_$filename"
done

