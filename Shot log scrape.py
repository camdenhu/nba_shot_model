from urllib.request import urlopen
import json
import csv

def getPlayerID(year):

    season = str(year - 1) + '-' + str(year)[-2:]
    url = 'http://stats.nba.com/stats/commonallplayers?IsOnlyCurrentSeason=0&LeagueID=00&'
    url = url + 'Season=' + season

    response = urlopen(url)
    string = response.read().decode('utf-8')
    data = json.loads(string)

    headers = data['resultSets'][0]['headers']
    players = data['resultSets'][0]['rowSet']

    players_dict = []

    for player in players:

        player_dict = dict(zip(headers, player))
        if int(player_dict['TO_YEAR']) >= year-1 and int(player_dict['FROM_YEAR']) <= year:
            players_dict.append(player_dict)

    return players_dict

def getPlayerShot(player_id, player_name, season, season_type_string):

    url = 'http://stats.nba.com/stats/playerdashptshotlog?' + \
          'DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&' + \
          'Location=&Month=0&OpponentTeamID=0&Outcome=&Period=0&' + \
          'PlayerID=' + str(player_id) + '&Season=' + season + '&SeasonSegment=&' + \
          'SeasonType=' + season_type_string + '&TeamID=0&VsConference=&VsDivision='

    response = urlopen(url)
    string = response.read().decode('utf-8')
    data = json.loads(string)

    headers = data['resultSets'][0]['headers']
    shots = data['resultSets'][0]['rowSet']

    shots_dict = []

    for shot in shots:

        shot_dict = dict(zip(headers, shot))
        shot_dict['SHOOTER_ID'] = player_id
        shot_dict['SHOOTER'] = player_name
        matchup = shot_dict['MATCHUP'].split('-')
        shot_dict['DATE'] = matchup[0].strip()
        shot_dict['MATCHUP'] = matchup[1].strip()

        shot_dict['GAME_CLOCK'] = int(shot_dict['GAME_CLOCK'].split(':')[0]) * 60 + int(shot_dict['GAME_CLOCK'].split(':')[1])

        if shot_dict['GAME_CLOCK'] >= 540:
            shot_dict['GAME_CLOCK_PERIOD'] = 1
        elif shot_dict['GAME_CLOCK'] >= 360:
            shot_dict['GAME_CLOCK_PERIOD'] = 2
        elif shot_dict['GAME_CLOCK'] >= 180:
            shot_dict['GAME_CLOCK_PERIOD'] = 3
        else:
            shot_dict['GAME_CLOCK_PERIOD'] = 4

        shots_dict.append(shot_dict)

    return shots_dict

def getYearShots(year, season_type):

    season_type_string = season_type.replace(' ', '+')

    season = str(year - 1) + '-' + str(year)[-2:]
    players = getPlayerID(year)

    all_shots = []

    for player in players:

        print(player['DISPLAY_LAST_COMMA_FIRST'])
        
        player_shots = getPlayerShot(player['PERSON_ID'], player['DISPLAY_LAST_COMMA_FIRST'], season, season_type_string)
        all_shots = all_shots + player_shots

    return all_shots

def main():

    season_types = ['Regular Season', 'Playoffs']

    for year in range(2014, 2016):

        for season_type in season_types:

            output_file = str(year) + '_' + season_type.lower().replace(' ', '_') + '_shots.csv'
            
            shots = getYearShots(year, season_type)
            
            with open(output_file, 'w', newline = '') as io:

                fieldnames = ['SHOOTER_ID', 'SHOOTER', 'GAME_ID', 'DATE', 'MATCHUP',
                              'LOCATION', 'W', 'FINAL_MARGIN',
                              'PERIOD', 'GAME_CLOCK', 'GAME_CLOCK_PERIOD', 'SHOT_CLOCK', 
                              'SHOT_NUMBER', 'PTS_TYPE', 'PTS', 'FGM',
                              'SHOT_DIST', 'TOUCH_TIME', 'DRIBBLES', 'CLOSE_DEF_DIST',
                              'CLOSEST_DEFENDER_PLAYER_ID', 'CLOSEST_DEFENDER', 'SHOT_RESULT']

                writer = csv.DictWriter(io, delimiter=',', fieldnames = fieldnames)
                writer.writeheader()

                for shot in shots:

                    writer.writerow(shot)



if __name__ == '__main__':
    main()
