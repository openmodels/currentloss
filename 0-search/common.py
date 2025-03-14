import pandas as pd

prompt = "I am performing a meta-analysis of econometric models of temperature on economic growth. These papers use regression-based techniques to understand how variation in weather results in variation in GDP growth rates. I am only interested in papers that do this globally, with at least national-level resolution.\n\nThe following is a paper identified by very general Scopus search: XXX\n\nBased on this information, should this paper be included in the meta-analysis? Please classify this as (XG) Not GDP growth (studying a different dependent variable), (XE) Not econometric (using non-causal or non-statistical methods), (XW) Not global (region-specific), (XN) No new empirics (using previously-published work), or (P) Plausibly appropriate. You may use multiple restrictions (XG, XE, XW, XN), but do not combine them with P. If this abstract is not otherwise filtered (P), classify it as (PL) Unlikely, (PM) Somewhat likely, or (PH) Very likely. Provide a succinct explanation, and only mention codes identified for this paper."

def load_search(source):
    if source == 'scopus':
        df = pd.read_csv("scopus.csv")
        knowndoi = df.DOI[:0]
    elif source == 'websci':
        df_list = []
        for i in range(1, 37):
            file_name = f'savedrecs{i}.xls'
            df = pd.read_excel(file_name)
            df_list.append(df)
        df = pd.concat(df_list, ignore_index=True)
        df['EID'] = range(len(df))
        df['Title'] = df['Article Title']
        scopusdf = pd.read_csv("scopus.csv")
        knowndoi = scopusdf.DOI

    return df, knowndoi

def get_fullprompt(row):
    paperinfo = f"{row.Title}:\nAbstract: {row.Abstract}\nKeywords: {row['Author Keywords']}"
    return prompt.replace("XXX", paperinfo)

def interpret_response(response):
    restrictions = (["Not GDP growth"] if "XG" in response else []) + (["Not econometric"] if "XE" in response else []) + (["Not global"] if "XW" in response else []) + (["No new empirics"] if "XN" in response else [])
    plausibility = (["Unlikely"] if "PL" in response else []) + (["Somewhat"] if "PM" in response else []) + (["Likely"] if "PH" in response else [])

    if len(restrictions) > 0 and len(plausibility) == 0:
        verdict = ", ".join(restrictions)
    elif len(restrictions) == 0 and len(plausibility) == 1:
        verdict = plausibility[0]
    else:
        verdict = "Undetermined"

    return verdict

