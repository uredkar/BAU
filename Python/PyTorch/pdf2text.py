
import json   
import os
from tika import parser  
rootdir = 'F:/'

def pdf2text(filename):
    # opening pdf file
    parsed_pdf = parser.from_file(filename)
    return parsed_pdf['content'] 
    

filenames = {}

for subdir, dirs, files in os.walk(rootdir):
    for file in files:
        fullpath = os.path.join(subdir, file)
        ##print(file,",",os.path.join(subdir, file))
        if fullpath.endswith('.pdf'):
            if not file in filenames:
                filenames[file] = fullpath
                try:
                    text = pdf2text(fullpath)
                    with open("c:/temp/text/"+file +".txt","w",encoding="utf-8") as f:
                        f.write(text)
                except:
                    print(file,"An exception occurred")
                
json_object = json.dumps(filenames, indent = 4) 
print(json_object)

