'''
Script to load a .tsv file with base URLS, get all subpages and their content 
and store them in a json file
'''
import requests
from bs4 import BeautifulSoup
from bs4.element import Comment
from tqdm import tqdm
import json
import chardet
import pandas as pd
import justext
import time
import urllib.robotparser
import hashlib
import requests_html
import os
from deep_translator import MyMemoryTranslator

#rp = urllib.robotparser.RobotFileParser() https://docs.python.org/3/library/urllib.robotparser.html

def translate(text):
	translated_text = MyMemoryTranslator(source="nl", target="en").translate(text=text)
	return translated_text

def save_html(html, path):
    with open(path, 'wb') as f:
        f.write(html)

def open_html(path):
    with open(path, 'rb') as f:
        return f.read()


def tag_visible(element):
    if element.parent.name in ['style', 'script', 'head', 'title', 'meta', '[document]']:
        return False
    if isinstance(element, Comment):
        return False
    return True


def text_from_html(body):
	soup = BeautifulSoup(body, 'html.parser')
	texts = soup.findAll(string=True)
	visible_texts = filter(tag_visible, texts) 
	text = u" ".join(t.strip() for t in visible_texts)
	## Add text from json-ld, script types
	text+=get_ld_json(text)
	return text

def get_ld_json(text):
	parser = "html.parser"
	soup = BeautifulSoup(text, parser)
	try:
		json_dict = json.loads("".join(soup.find("script", {"type":"application/ld+json"}).contents))
		dict_string = str(json_dict)
	except:
		return '' # Empty string
	return dict_string

def getdata(url):
	'''Check if url already stored, if not store & return body text'''
	md5 = hashlib.md5(url.encode(encoding="ascii",errors="replace")).hexdigest()
	md5_filename = data_dir+md5+'.txt'
	#check if stored on disk, if not request
	if not os.path.isfile(md5_filename): 
		r = requests.get(url)
		with open(md5_filename, 'w') as file:
			file.write(r.text)
			r.raise_for_status() # ensure we notice bad responses
			text = r.text
	else:
		with open(md5_filename,'rb') as file:
			   text = file.read()
	return text

def get_links(website_link):
	html_data = getdata(website_link)
	soup = BeautifulSoup(html_data, "html.parser")
	list_links = []
	for link in soup.find_all("a", href=True):
		# Append to list if new link contains original link
		if str(link["href"]).startswith((str(website_link))):
			list_links.append(link["href"])

		# Include all href that do not start with website link but with "/"
		if str(link["href"]).startswith("/"):
			if link["href"] not in dict_href_links:
				#print(link["href"])
				dict_href_links[link["href"]] = None
				link_with_www = website_link + link["href"][1:]
				#print("adjusted link =", link_with_www)
				list_links.append(link_with_www)

	# Convert list of links to dictionary and define keys as the links and the values as "Not-checked"
	dict_links = dict.fromkeys(list_links, "Not-checked")
	return dict_links

def check_minimum_time_passed(link):
		link_base = "/".join(link.split("/")[0:3])+"/"
		t0 = time.time()
		if time_dict[link_base] == None:
			time_dict[link_base] = time.time() # Store last tiem website was queried
			return True
		elif t0 - time_dict[link_base] > min_time_interval :
			time_dict[link_base] = time.time()
			return True
		else:
			return False # Meaning you should skip this link for now


def get_subpage_links(l,min_time_interval):
	for link in tqdm(l):
		if check_minimum_time_passed(link) and l[link] == "Not-checked":
			## Try except to drop dead links or SSLErrors
			try:
				dict_links_subpages = get_links(link)		
				# Change the dictionary value of the link to "Checked"
				l[link] = "Checked"
			except:
				dict_links_subpages = {}
				print('Error raised when requesting: {}'.format(link))
		else:
			# Create an empty dictionary in case every link is checked
			dict_links_subpages = {}
		# Add new dictionary to old dictionary
		l = {**dict_links_subpages, **l}
	
	return l
## Not used, text_from_html() returns more text
# def get_just_text(html):
# 	paragraphs_text = ''
# 	paragraphs = justext.justext(html, justext.get_stoplist("English"))
# 	for paragraph in paragraphs:
# 		if not paragraph.is_boilerplate:
# 			paragraphs_text+=" "+paragraph.text
# 	return(paragraphs_text)		
		




##############################################################################
##  MAIN PROGRAM STARTS HERE
##############################################################################

### 1)Open Tab Seperated File, detect encoding, load as dataframe
file_name= "Data set jclepro.txt"
## Extra code to detect encoding, MAC encoding in this case
with open(file_name, 'rb') as f:
    result = chardet.detect(f.read())  # or readline if the file is large
df = pd.read_csv(file_name,sep='\t',encoding=result['encoding'])
df = df.dropna(axis=0)

## 2)  create dictionary of website URLS, set status as 'Not-checked'
# dict_links = {website:"Not-checked"}
url_list = df['URL']
base_urls = [str("/".join(u[0:3]))+"/" for u in df['URL'].str.split("/")]
time_dict = dict(zip(base_urls,[None] * len(base_urls)))
header_dict = time_dict = dict(zip(base_urls,[None] * len(base_urls)))
status_list = len(url_list)*["Not-checked"]
dict_links = dict(zip(url_list, status_list))

## Set base variables and counters
counter, counter2 = None, 0
iteration = 0
max_iterations = 2
min_time_interval = 12 #Minimallywait 10 seconds to query site, some extra since time is before request
data_dir = "web_data/"

## 3) For each website start with base level url, get sub_urls and content
all_results = []
one_website_results = {}
# create empty dict
dict_href_links = {}
## Keeps on parsing subpages for urls, untill all ar in dict_links	
## Pages are stored in web-data to avoid burdening websites to much
json_fileout = open("data.json", "w")
while counter != 0: 
	iteration +=1
	counter2 +=1
	## Get the subpages of all the links in curren level
	if iteration < max_iterations:
		dict_links = get_subpage_links(dict_links,min_time_interval)

	# Count number of non-values and set counter to 0 if there are no values within the dictionary equal to the string "Not-checked"
	# https://stackoverflow.com/questions/48371856/count-the-number-of-occurrences-of-a-certain-value-in-a-dictionary-in-python

	# Print some statements
	counter = sum(value == "Not-checked" for value in dict_links.values())
	print("")
	print("THIS IS LOOP ITERATION NUMBER", counter2)
	print("LENGTH OF DICTIONARY WITH LINKS =", len(dict_links))
	print("NUMBER OF 'Not-checked' LINKS = ", counter)
	print("")
	## Now get info for each link
	for link in dict_links:
		if not check_minimum_time_passed(link) and iteration > 1:
			continue #skip for now if not minimal time passed
		# Get content html, if not html, dropo
		try:
			data  = getdata(link)
			content = text_from_html(data)
			dict_links[link] = content
		except:
			#drop links that are not html
			dict_links[link] = 'NO_HTML_CONTENT'

##Process results	
json.dump(dict_links, json_fileout)
df = pd.DataFrame(dict_links.items(), columns=['url', 'text'])
df.dropna(inplace=True)
df.to_csv('results_per_link.tsv', sep="\t")
url_col = df['url']
# Split on "/", collect the first three fragments and join, the base url
base_url =   ["/".join(u[0:3])+"/" for u in url_col.str.split("/")]
df['base_url'] = base_url
## Add translated text
df['text_en'] = df['text'].apply(lambda x: translate(text) if type(x) == str else text)
translate_batch

aggregation_functions = {"text","url"}
## Aggregatre the columns 'url' and 'text based on shared base_url
df2 = df.groupby(['base_url']).agg(lambda x: ' ;;; '.join(set(x))).reset_index()
df2.to_csv('results_per_base_url.tsv', sep="\t")