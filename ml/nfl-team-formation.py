import numpy as np
import pandas as pd

df = pd.read_csv("/Users/sanjayaravind/Desktop/NFL/Madden NFL 16 - Full Ratings-Table 1.csv")
# RB -[HB,FB], DL-[DT,LE,RE] LB-[MLB,LOLB,ROLB],DB-[CB,SS,FS],QB,OL-[C,LT,RT,LG,RG],TE,WR,K,P,
team_names=["49ers","Bears","Bengals","Bills","Broncos","Browns","Buccaneers","Cardinals","Chargers","Chiefs","Colts","Cowboys","Dolphins","Eagles","Falcons",
            "Giants","Jaguars","Jets","Lions","Packers","Panthers","Patriots","Raiders","Rams","Ravens","Redskins","Saints","Seahawks","Steelers","Texans","Titans","Vikings"]
final_team={
    "QB":[],
    "RB":[],
    "OL":[],
    "TE":[],
    "WR":[],
    "DL":[],
    "LB":[],
    "DB":[],
    "K":[],
    "P":[]
}
min_pos={
    "QB":3,
    "RB":4,
    "OL":9,
    "TE":3,
    "WR":6,
    "DL":9,
    "LB":7,
    "DB":10,
    "K":1,
    "P":1
}
temp_df=[]
overall_scores=[]
print(df)
for i in range(0,len(team_names)):
    final_team = {
        "QB": [],
        "RB": [],
        "OL": [],
        "TE": [],
        "WR": [],
        "DL": [],
        "LB": [],
        "DB": [],
        "K": [],
        "P": []
    }
    teams = df[df['Team']==team_names[i]]
    overall_scores = [team_names[i]]
    for j in teams.iterrows():
        if(j[1]['Position'] == 'QB'):
            final_team['QB'].append(j[1]['Overall'])
        elif (j[1]['Position'] == 'HB' or j[1]['Position'] == 'FB'):
            final_team['RB'].append(j[1]['Overall'])
        elif(j[1]['Position'] in['C','LT','RT','LG','RG']):
            final_team['OL'].append(j[1]['Overall'])
        elif(j[1]['Position'] =='TE'):
            final_team['TE'].append(j[1]['Overall'])
        elif(j[1]['Position'] =='WR'):
            final_team['WR'].append(j[1]['Overall'])
        elif(j[1]['Position'] in['DT','LE','RE']):
            final_team['DL'].append(j[1]['Overall'])
        elif(j[1]['Position'] in['MLB','LOLB','ROLB']):
            final_team['LB'].append(j[1]['Overall'])
        elif(j[1]['Position'] in['CB','SS','FS']):
            final_team['DB'].append(j[1]['Overall'])
        elif(j[1]['Position'] == 'K'):
            final_team['K'].append(j[1]['Overall'])
        elif(j[1]['Position']=='P'):
            final_team['P'].append(j[1]['Overall'])
    for k in final_team.keys():
        final_team[k].sort(reverse=True)
        if(len(final_team[k])<min_pos[k]):
            for m in range(len(final_team[k]),min_pos[k]):
                final_team[k].append(0)
        overall_scores = np.concatenate((overall_scores,final_team[k][0:min_pos[k]]),axis=0).tolist()
    temp_df.append(overall_scores)
print(len(temp_df[0]))
final_df=pd.DataFrame(np.array(temp_df),columns=['Team','Quarter Back_1','Quarter Back_2','Quarter Back_3','Running Back_1','Running Back_2','Running Back_3','Running Back_4','Offensive Lineman_1','Offensive Lineman_2',
                               'Offensive Lineman_3','Offensive Lineman_4','Offensive Lineman_5','Offensive Lineman_6','Offensive Lineman_7','Offensive Lineman_8','Offensive Lineman_9',
                               'Tight End_1','Tight End_2','Tight End_3','Wide Receiver_1','Wide Receiver_2','Wide Receiver_3','Wide Receiver_4','Wide Receiver_5','Wide Receiver_6',
                               'Defensive Lineman_1','Defensive Lineman_2','Defensive Lineman_3','Defensive Lineman_4','Defensive Lineman_5','Defensive Lineman_6','Defensive Lineman_7','Defensive Lineman_8','Defensive Lineman_9',
                               'Line Backer_1','Line Backer_2','Line Backer_3','Line Backer_4','Line Backer_5','Line Backer_6','Line Backer_7',
                               'Defensive Back_1','Defensive Back_2','Defensive Back_3','Defensive Back_4','Defensive Back_5','Defensive Back_6','Defensive Back_7','Defensive Back_8','Defensive Back_9','Defensive Back_10'
                               ,'Kicker_1','Punter_1'])

final_df.to_csv('nfl_team_ratings_16.csv', sep='\t')



