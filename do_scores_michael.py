import csv
import re

## Builds concreteness dictionary
conc_dict = {}
with open("concreteness.csv", "r") as f: # change name of file and put in here
	reader = csv.reader(f)
	next(reader) #.next skips header
	for row in reader:
		conc_dict[row[0]] = float(row[2])

with open("michael_passages.csv") as f:
	with open("passage_scores.csv", "w") as g:
		reader = csv.reader(f, quotechar = '"')
		writer = csv.writer(g)
		writer.writerow(["id", "msg_sum", "msg_avg", "msg_word_count"])
		for row in reader:
			id_ = row[0]
			print id_
			message = row[1].split()
			to_write = [id_]

			msg_sum = 0
			msg_word_count = 0
			for i in range(0, len(message)-1):
				word1 = re.sub("[\.,-/\\?!;: ]", "", message[i]).lower()
				word2 = re.sub("[\.,-/\\?!;: ]", "", message[i+1]).lower()
				phrase = word1 + " " + word2
				if phrase in conc_dict:
					msg_sum += conc_dict[phrase]
					msg_word_count += 1
				else:
					if i == 0 and word1 in conc_dict:
						msg_sum += conc_dict[word1]
						msg_word_count += 1
					if word2 in conc_dict:
						msg_sum += conc_dict[word2]
						msg_word_count += 1

			to_write.append(str(msg_sum))
			to_write.append(str(msg_sum/float(msg_word_count)))

			print to_write
			writer.writerow(to_write)
