import fastf1
fastf1.Cache.enable_cache('/Users/johnwu/ClassWork/E 109/Project/Formula-1-Project/Formula 1 (non-SQLite)/F1-Cache')
import pandas as pd

circuits = pd.read_csv('circuits.csv')
qualify = pd.read_csv('qualifying.csv')
races = pd.read_csv('races.csv')

relevant_races = races[(races['year']>=2014) & (races['year']<= 2021)]
relevant_qualify = qualify[qualify['raceId'].isin(relevant_races['raceId'])]

race_dates  = relevant_races[['raceId','year','round']]
relevant_qualify = relevant_qualify.merge(race_dates, on='raceId',how='left')

for index, col in relevant_races.iterrows():
    year = col['year']
    round = col['round']
    print('Running f1 api')
    session = fastf1.get_session(year,round,'R')
    try:
        session._load_weather_data()
        relevant_races.loc[index,'Air Temp'] = session.weather_data['AirTemp'].mean()
        relevant_races.loc[index,'Track Temp'] = session.weather_data['TrackTemp'].mean()
        relevant_races.loc[index,'Wind Speed'] = session.weather_data['WindSpeed'].mean()
        rain = session.weather_data['Rainfall'].value_counts(normalize=True)
        print('data loaded')
        if True in rain.index:
            if rain[True] >= 0.9:
                relevant_races.loc[index,'Rainfall'] = 'Full Race'
            elif rain[True] >= 0.7:
                relevant_races.loc[index,'Rainfall'] = '3/4 Race'
            elif rain[True] >= 0.45:
                relevant_races.loc[index,'Rainfall'] = '1/2 Race'
            elif rain[True] >= 0.2:
                relevant_races.loc[index,'Rainfall'] = '1/4 Race'
            else:
                relevant_races.loc[index,'Rainfall'] = 'Minimal'
        else:
            relevant_races.loc[index,'Rainfall'] = 'Dry'

    except fastf1.api.SessionNotAvailableError:
        relevant_races.loc[index,'Rainfall'] = 'Not Available'
        relevant_races.loc[index,'Air Temp'] = 'Not Available'
        relevant_races.loc[index,'Track Temp'] = 'Not Available'
        relevant_races.loc[index,'Wind Speed'] = 'Not Available'

# Removing all races where rainfall, air temp, etc. is not available
new_races = relevant_races[relevant_races['Rainfall']!='Not Available']
new_race_id = new_races['raceId']

# finding inner join with qualifying and races
relevant_qualifying = relevant_qualify.merge(new_race_id, on='raceId', how='inner')
relevant_races.to_csv('Relevant_Races.csv')

# this is to lower the amount of API pulls
new_qualifying = relevant_qualifying.drop_duplicates(subset=['raceId'])

for index, col in new_qualifying.iterrows():
    year = col['year']
    round = col['round']
    print('Running f1 api')
    session = fastf1.get_session(year,round,'Q')
    try:
        session._load_weather_data()
        new_qualifying.loc[index,'Air Temp'] = session.weather_data['AirTemp'].mean()
        new_qualifying.loc[index,'Track Temp'] = session.weather_data['TrackTemp'].mean()
        new_qualifying.loc[index,'Wind Speed'] = session.weather_data['WindSpeed'].mean()
        rain = session.weather_data['Rainfall'].value_counts(normalize=True)
        print('data loaded')
        if True in rain.index:
            if rain[True] >= 0.9:
                new_qualifying.loc[index,'Rainfall'] = 'Full Race'
            elif rain[True] >= 0.7:
                new_qualifying.loc[index,'Rainfall'] = '3/4 Race'
            elif rain[True] >= 0.45:
                new_qualifying.loc[index,'Rainfall'] = '1/2 Race'
            elif rain[True] >= 0.2:
                new_qualifying.loc[index,'Rainfall'] = '1/4 Race'
            else:
                new_qualifying.loc[index,'Rainfall'] = 'Minimal'
        else:
            new_qualifying.loc[index,'Rainfall'] = 'Dry'

    except fastf1.api.SessionNotAvailableError:
        new_qualifying.loc[index,'Rainfall'] = 'Not Available'
        new_qualifying.loc[index,'Air Temp'] = 'Not Available'
        new_qualifying.loc[index,'Track Temp'] = 'Not Available'
        new_qualifying.loc[index,'Wind Speed'] = 'Not Available'

new_qualifying = new_qualifying[['raceId','Rainfall','Air Temp', 'Track Temp', 'Wind Speed']]

relevant_qualify = relevant_qualify.merge(new_qualifying, on='raceId',how='left')
relevant_qualify.to_csv('Relevant_Qualifying.csv')