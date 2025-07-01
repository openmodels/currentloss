import os, time
import pandas as pd
from chatwrap import gemini
import common

source = 'websci' #'scopus'

df, knowndoi = common.load_search(source)

if os.path.exists(f"{source}-gemini.csv"):
    known = pd.read_csv(f"{source}-gemini.csv")
else:
    known = pd.DataFrame(columns=['EID', 'Verdict', 'Comments'])

results = []

for index, row in df.iterrows():
    if known.EID.isin([row.EID]).any():
        continue
    if knowndoi.isin([row.DOI]).any():
        continue
    print(index / len(df))
    starttime = time.time()

    fullprompt = common.get_fullprompt(row)

    response = gemini.single_prompt(fullprompt)

    verdict = common.interpret_response(response)
    
    resout = pd.DataFrame([{"EID": row.EID, "Verdict": verdict, "Comments": response.replace("\n", " ")}])
    resout.to_csv(f"{source}-gemini.csv", mode='a', header=False)

    endtime = time.time()
    time.sleep(max(1 - (endtime - starttime), 0)) # 24*60*60 / 1490

