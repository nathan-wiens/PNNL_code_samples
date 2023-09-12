#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct 24 23:02:20 2022

@author: nathanwiens
"""

import googlemaps
import requests
import matplotlib.pyplot as plt
from datetime import datetime
import pandas as pd
import numpy as np

# API keys for open weather and google maps
weather_api_key = '6ac7e9d162be223dd33458ed99af5dd9'
geocoding_api_key = 'AIzaSyBv8MCAQihp-AhlbEyFPk4ZAVJi_8jON9Y'

# Base URL for open weather
base_url = 'https://api.openweathermap.org/data/2.5/forecast?units=imperial&'

def get_3h_temps(location):
    # This function takes in a location, geocodes it for use with the 
    # Openweather API, and returns a dataframe containing a temperature forecast
    # for the given location with a 3-hour timestep
    
    # Begining of geocoding
    gmaps = googlemaps.Client(key=geocoding_api_key)
    
    # Passes user input location to geocoding service
    geocode_result = gmaps.geocode(location)
    
    # Determines lat and lon coordinates for user input location
    lat, lon = list(geocode_result[0].pop('geometry').pop('location').values())
    
    # Constructs api address call with information from geocoding call
    complete_url = base_url + 'lat=' + str(lat) + '&lon=' + str(lon) + '&appid=' + weather_api_key
    
    # Calls Openweather API
    response = requests.get(complete_url)
    
    x = response.json()
   
    # Creates list from json file for data of 40 timesteps
    days = x['list']
    
    # pulls out date time and temperature data for all 40 timesteps
    temps = []
    dt_epoch = []
    for d in days:
        main = d['main']
        temp = main['temp']
        temps.append(temp)
        dt_epoch.append(d['dt'])
        
    # converts epoch to datetime format
    dt = []
    for epoch in dt_epoch:
        dt.append(datetime.fromtimestamp(epoch).strftime('%m-%d %H:%M'))
        
    # constucts time and temperature dataframe
    d = {'time': dt, 'temp': temps}
    temp_df = pd.DataFrame(d)
    
    return(temp_df)
        
def plot_weather_data(weather_df):
    # This function plots data from a dataframe with a time and temp column
    
    plt.plot(weather_df['time'], weather_df['temp'], color = 'r')
    plt.title(input_city + " 5-Day Temperature Forecast")
    plt.ylabel("Temperature (F)")
    plt.xlabel("Time")
    plt.xticks(np.arange(0, len(weather_df)+1, 5),rotation = 45)
    plt.show()


# Gets user input
input_city = input("Search City (City ST): ")


search_temps = get_3h_temps(input_city)
plot_weather_data(search_temps)



