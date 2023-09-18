import pandas as pd
import matplotlib.pyplot as plt
import nltk
from nltk import word_tokenize, sent_tokenize
from nltk.corpus import stopwords
from nltk.stem import LancasterStemmer, WordNetLemmatizer, PorterStemmer
from wordcloud import WordCloud, STOPWORDS
from textblob import TextBlob
import re

# Importing the dataset
df = pd.read_excel(r'C:\Users\admin\Desktop\test\bitcoin.xlsx')
df["row_id"] = df.index + 1
df_copy = df.copy()


df_copy['text'] = df_copy['text'].astype(str)


df_copy['text'] = df_copy['text'].apply(lambda x: " ".join(x.lower() for x in x.split()))


def clean_text(text):
    text = text.lower()
    text = re.sub(r'[0-9]+', "", text)
    text = re.sub(r"[-()\"#@;:<>{}+=~|.?,!%&]", "", text)
    return text

stop = stopwords.words('english')
df_copy['text'] = df_copy['text'].apply(lambda x: " ".join(x for x in x.split() if x not in stop))


st = PorterStemmer()
df_copy['text'] = df_copy['text'].apply(lambda x: " ".join([st.stem(word) for word in x.split()]))

def senti(x):
    return TextBlob(x).sentiment  

df_copy['senti_score'] = df_copy['text'].apply(senti)

print(df_copy.senti_score.head())

x = df_copy.senti_score[1][0]


score = []
for i in range(len(df_copy.senti_score)):
    score.append(df_copy.senti_score[i][0])
    
    
subj =[]
for i in range(len(df_copy.senti_score)):
    subj.append(df_copy.senti_score[i][1])


print('scorul cu indexul 1 este : {}', score[1])
print('subiectivitatea cu indexul 1 este: {}', subj[1])


review = []
for i in range(len(score)):
    if score[i] == 0 :
        review.append('neutral')
    elif score[i] > 0:
        review.append('positive')
    else:
        review.append('negative')
    

sent = pd.DataFrame(score, review)

print(sent.head())

#sent.to_csv('c:/Users/admin/Desktop/test/scoruri.csv')

subj = pd.DataFrame(subj)
subj.to_csv('c:/Users/admin/Desktop/test/subj.csv')
# =============================================================================
# C:\Users\admin\Desktop\test\scoruri.csv
# =============================================================================




