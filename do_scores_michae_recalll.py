import csv

## Builds concreteness dictionary
conc_dict = {}
with open("concreteness.csv", "r") as f: # change name of file and put in here
	reader = csv.reader(f)
	next(reader) #.next skips header
	for row in reader:
		conc_dict[row[0]] = float(row[2])

with open("michael_study_recall.csv") as f:
	with open("recall_michael_scores.csv", "w") as g:
		reader = csv.reader(f, quotechar = '"') 
		writer = csv.writer(g)
		writer.writerow(["id", "msg_sum", "msg_avg"])
		for row in reader:
			id_ = row[0]
			print id_
			message = row[1].split()
			#recall = row[2].split()
			to_write = [id_]

			msg_sum = 0
			msg_word_count = 0
			for word in message:
				word = word.split('.')[0].lower()
				if word in conc_dict.keys():
					msg_sum += conc_dict[word]
					msg_word_count += 1

			to_write.append(str(msg_sum))
			to_write.append(str(msg_sum/float(msg_word_count)))

			#rec_sum = 0
			#rec_word_count = 0
			#for word in recall:
				#word = word.split('.')[0].lower()
				#if word in conc_dict.keys():
					#rec_sum += conc_dict[word]
					#rec_word_count += 1
			#to_write.append(str(rec_sum))
			#to_write.append(str(rec_sum/float(rec_word_count)))
			print to_write
			writer.writerow(to_write)
