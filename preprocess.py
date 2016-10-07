# coding: utf8

from __future__ import unicode_literals
import codecs
import os
import re, string
import nltk
from multiprocessing import Pool
from collections import Counter


def sanitizer(import_filename,export_filename):
    # Import en utf-8 converti en unicode
    raw = codecs.open(import_filename, 'r', 'utf-8',errors ="ignore")
    raw = raw.readlines()

    accepted_chars = "çéèêëàâäùüûöôïî"+string.letters+' '
    include = set(accepted_chars)
    cleaned_raw = list()
    # Retrait de la ponctuation (remplacée par '')
    p = Pool()
    for line in raw:
        # Replace some punctuation with spaces
        line = re.sub("#",' ',line)
        line = re.sub("'",' ',line)
        line = re.sub("-",' ',line)
        line = re.sub("_",' ',line)
        line = re.sub("\n", ' ', line)

        # Split line in words
        words = line.split(' ')
        # Suppression stopwords
        stopwords = nltk.corpus.stopwords.words('french')
        for word in nltk.corpus.stopwords.words('english'): stopwords.append(word)
        for word in words:
            if word in stopwords: del words[words.index(word)]
        # Remove links and mentions, and little words
        words = [word for word in words if
             not (word.find("bit.ly")>-1 or word.startswith("@") or word.find("http")>-1 or len(word) <= 2)]
        # Lowercase
        words = ' '.join(words).lower()
        # Cleanup punctuation
        words = ''.join(ch for ch in words if ch in include)

        cleaned_raw.append(words)

    p.close()
    # Write the cleaned text in output directory
    thefile = codecs.open(export_filename, 'w', 'utf-8',errors ="ignore")

    for item in set(cleaned_raw):
        thefile.write("%s\n" % item)


path_input = '/home/remi/ImportData/'
path_output = '/home/remi/SanitizeData/'
for filename in os.listdir(path_input):
    sanitizer(path_input+filename,path_output+filename)
