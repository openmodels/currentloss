# Run from ~/projects/arachne/venv2024

import os, pickle, json, datetime
import pandas as pd
from dotenv import load_dotenv
from openai import OpenAI
import common

source = 'websci' #'scopus'

def submit_batch(client, pathwork, pathsave, count):
    df, knowndoi = common.load_search(source)

    if os.path.exists(f"{source}-openai.csv"):
        known = pd.read_csv(f"{source}-openai.csv")
    else:
        known = pd.DataFrame(columns=['EID', 'Verdict', 'Comments'])

    with open(pathwork, 'w') as fp:
        for idx in reversed(df.index):
            if known.EID.isin([idx]).any():
                continue
            if knowndoi.isin([df.loc[idx, 'DOI']]).any():
                continue
            print(idx)
            fullprompt = common.get_fullprompt(df.loc[idx, :])

            line = dict(custom_id=str(idx),
                        method='POST',
                        url="/v1/chat/completions",
                        body={'model': 'gpt-4o',
                              "messages": [{"role": "user", "content": fullprompt}],
                              'max_tokens': 1000})
    
            json.dump(line, fp)
            fp.write("\n")
            count -= 1
            if count == 0:
                break

    batch_input_file = client.files.create(
        file=open(pathwork, "rb"),
        purpose="batch"
    )

    batch_input_file_id = batch_input_file.id
    
    batch = client.batches.create(
        input_file_id=batch_input_file_id,
        endpoint="/v1/chat/completions",
        completion_window="24h",
        metadata={'type': 'litrev'}
    )

    with open(pathsave, 'wb') as fp:
        pickle.dump(batch, fp)

def check_batch(client, pathsave):
    with open(pathsave, 'rb') as fp:
        batch = pickle.load(fp)

    batch = client.batches.retrieve(batch.id)
    if batch.status in ['failed', 'expired', 'cancelling', 'cancelled']:
        print("Batch status: " + batch.status)
    elif batch.status == 'completed':
        print("Processing batch.")
        content = client.files.content(batch.output_file_id)
        lines = content.text.strip().split('\n')
        for line in lines:
            rowcont = json.loads(line)
            response = rowcont['response']['body']['choices'][0]['message']['content']

            verdict = common.interpret_response(response)
    
            resout = pd.DataFrame([{"EID": rowcont['custom_id'], "Verdict": verdict, "Comments": response.replace("\n", " ")}])
            resout.to_csv(f"{source}-openai.csv", mode='a', header=False)
        return True

    print("Still processing.")
    return False # Don't delete

if __name__ == '__main__':
    load_dotenv()
    OpenAI.api_key = os.getenv('OPENAI_API_KEY')
    client = OpenAI()

    if os.path.exists("waiting.pkl"):
        print("Checking batch.")
        if check_batch(client, "waiting.pkl"):
            os.remove("waiting.pkl")

    if not os.path.exists("waiting.pkl"):
        print("Submitting batch.")
        submit_batch(client, "batch.jsonl", "waiting.pkl", 10000)
