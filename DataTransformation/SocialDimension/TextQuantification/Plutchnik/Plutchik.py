#this script calculate plutchnik's emotion probabilities for each text in microblog data
"""
Created on Sat Apr 21 00:09:16 2018

@author: ruthikwu
"""
###these methods is based on Trained Neural Networks developed by
#colneric & Demser, Calefato, Lanubile & Noviellie

#from emotion_predictor import EmotionPredictor
from pandas import read_csv


#model = EmotionPredictor(classification='plutchik', setting='mc')

datadir = "../../../../Database/raw/SocialDimension/microblogs/Microblogs.csv"

with open(datadir, "r+", encoding='latin-1') as ff:
    text = [str(i.split(",")[3]) for i in ff.readlines()]



probs = model.predict_probabilities(text)

probs.to_csv("../../../../Database/cleaned/SocialDimension/microprobs.csv")
