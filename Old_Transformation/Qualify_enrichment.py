import fastf1
fastf1.Cache.enable_cache('/Users/johnwu/ClassWork/E 109/F1_Project/F1-Stats-Project/F1-Cache')
import pandas as pd

qualify = pd.read_csv('Relevant_Qualifying.csv')

for index, col in qualify.iterrows():
    year = col['year']
    round = col['round']
    driver_num = str(col['number'])
    # fetching session 
    session = fastf1.get_session(year,round,'Q')
    session.load()
    # adding mean throttle and brake
    qualify.loc[index,'Throttle'] = session.car_data[driver_num]['Throttle'].mean()
    qualify.loc[index,'Brake'] = session.car_data[driver_num]['Brake'].mean()
    
qualify.to_csv('Relevant_Qualifying.csv')

