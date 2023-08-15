# NAVER Papago Text Translation API
import urllib.request
import json
import numpy as np
import pandas as pd
from numbers import Number

def ja2ko0(text, api_key_id, api_key):
    '''simple ja2ko0'''
    text = urllib.parse.quote(text)
    data = "source=ja&target=ko&text=" + text
    url = "https://naveropenapi.apigw.ntruss.com/nmt/v1/translation"
    request = urllib.request.Request(url)
    request.add_header("X-NCP-APIGW-API-KEY-ID", api_key_id)
    request.add_header("X-NCP-APIGW-API-KEY", api_key)
    response = urllib.request.urlopen(request, data=data.encode("utf-8"))
    rescode = response.getcode()
    if (rescode==200):
        response_body = response.read().decode('utf-8')
        response_json = json.loads(response_body)
        translated_text = response_json['message']['result']['translatedText']
    else:
        translated_text = "Error Code:" + rescode
    return translated_text

def ja2ko(text_obj, api_key_id, api_key):
    '''generalized ja2ko0'''
    if isinstance(text_obj, Number):
        translated = text_obj
    if isinstance(text_obj, str):
        translated = ja2ko0(text_obj, api_key_id, api_key)
    if isinstance(text_obj, (list, np.ndarray, pd.Series)):
        translated = []
        for text in text_obj:
            if isinstance(text, Number):
                translated_text = text
            elif pd.notna(text):
                text = urllib.parse.quote(text)
                data = "source=ja&target=ko&text=" + text
                url = "https://naveropenapi.apigw.ntruss.com/nmt/v1/translation"
                request = urllib.request.Request(url)
                request.add_header("X-NCP-APIGW-API-KEY-ID", api_key_id)
                request.add_header("X-NCP-APIGW-API-KEY", api_key)
                response = urllib.request.urlopen(request, data=data.encode("utf-8"))
                rescode = response.getcode()
                if (rescode==200):
                    response_body = response.read().decode('utf-8')
                    response_json = json.loads(response_body)
                    translated_text = response_json['message']['result']['translatedText']
                else:
                    translated_text = "Error Code:" + rescode
            else:
                translated_text = np.nan
            translated.append(translated_text)
    return translated

def ja2ko4dat(data, api_key_id, api_key):
    '''ja2ko for data'''
    return data.apply(lambda x: ja2ko(x, api_key_id, api_key))

def ja2ko4uni(data, api_key_id, api_key):
    '''ja2ko for unique data columns'''
    columns = data.columns
    data_unique = []
    translated = []
    for column in data.columns:
        column_data_unique = list(data.loc[:, column].unique())
        if np.nan in column_data_unique:
            column_data_unique.remove(np.nan)
        data_unique.append(column_data_unique)
        translated_text = ja2ko(column_data_unique, api_key_id, api_key)
        translated.append(translated_text)
        print(column)
    translated_data = pd.DataFrame()
    for i, _ in enumerate(columns):
        translated_data = pd.concat([
            translated_data, 
            pd.DataFrame({columns[i]+'_ja': data_unique[i], columns[i]+'_ko': translated[i]})], axis=1)
    return translated_data

def read_sheets(path):
    return [sht for sht in pd.read_excel(
        path, sheet_name=None, skiprows=0, engine='openpyxl').keys()]

def save_xlsx(data, sheets, path):
    writer = pd.ExcelWriter(path)
    for i, d in enumerate(data):
        d.to_excel(writer, sheet_name=f'{sheets[i]}', index=False)
    writer.save()
