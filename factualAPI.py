
from factual import Factual
import json
import csv

def find_coffee(zips, file_path):
	"""Downloads data describing all coffee shops contained in each ZIP code in zips and 
	saves the results using the path indicated by file_path. Uses the factual.com API.""" 
	cafes = []
	num_zips_complete = 0
	factual = Factual('<api key here>') #enter factual.com API key
	rests = factual.table('restaurants-us')
	for code in zips:
		filtered = rests.filters({
			"$and": [{"postcode": {"$eq": code},
			          "cuisine": {"$includes": "Coffee"}, 
			          "category_ids": {"$includes": 342}}]
			})
		cafes_new = filtered.include_count(True)
		num_exists = cafes_new.total_row_count()
		if num_exists > 500:
			print "Too many cafes in %d." % code
			break
		num_found = 0
		while num_found < num_exists:
			data = cafes_new.limit(50).offset(num_found).data()
			cafes.extend(data) 
			num_found = num_found + len(data)
		num_zips_complete += 1
		print "Finished %d ZIP code(s) out of %d total." % (num_zips_complete, len(zips))
	with open(file_path, 'w') as f:
		json.dump(cafes, f)

#reads a file produced by makeCoffeeData.R to compile a list of ZIP codes
zips = []
with open("zips.csv", "rb") as f:
	reader = csv.reader(f)
	reader.next() #skip header row
	for row in reader: 
		zips.append(row[1])

#dowloads and saves the relevant coffee shop data
find_coffee(zips, "cafes.txt")

