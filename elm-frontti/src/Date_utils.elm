module Date_utils exposing (int_to_month_string)
import Array

-- import Dict exposing (Dict)

months = Array.fromList ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

int_to_month_string i = Array.get (i - 1) months         
