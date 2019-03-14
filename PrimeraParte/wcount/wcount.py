from collections import Counter
import fileinput
import re

def main():
  counter = Counter()

  for line in fileinput.input():
    filtered = re.sub("[,\.!?;,:\-\"']", '', line.rstrip())
    words = filtered.split()
    for word in words:
      counter[word.lower()] += 1

  sw = list(enumerate(sorted(counter.items(), key=lambda p: -p[1])))

  max_len = 0
  for i, (word, freq) in sw:
    if i > 200:
      break
    elif len(word) > max_len:
      max_len = len(word)

  for i, (word, freq) in sw:
    if i > 200:
      break
    else:
      print(("{:>" + str(max_len) + "}: {}").format(word, freq))


if __name__ == "__main__":
  main()
