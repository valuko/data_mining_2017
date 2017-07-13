import csv
import random

input_file = "ncmp.csv"
output_file = "ncmp_out_20.csv"
result = []

chances_selected = 0.3
limit = 20000
items_cnt = 0
lines_read = 0

with open(input_file, "rb") as csvfile:
    reader = csv.reader(csvfile, delimiter=',')
    for line in reader:
        if items_cnt > limit:
            break

        lines_read += 1
        if lines_read == 1 or random.random() < chances_selected:
            result.append(line)
            items_cnt += 1

    csvfile.close()

# Handle the output file
with open(output_file, "wb") as outfile:
    writer = csv.writer(outfile, delimiter=',')
    writer.writerows(result)

    outfile.close()

print "Writing done"
