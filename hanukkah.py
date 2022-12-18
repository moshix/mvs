from datetime import datetime
from calendar import monthrange

def hanukkah_dates(year):
  # Calculate the date of Hanukkah for the given year
  # Hanukkah always falls on the 25th day of the Jewish month of Kislev
  # and the Jewish calendar is based on lunar cycles
  a = (year * 12 + 17) % 19
  b = (year - 1) // 100
  c = (year - 1) % 100
  d = b // 4
  e = b % 4
  f = (b + 8) // 25
  g = (b - f + 1) // 3
  h = (19 * a + b - d - g + 15) % 30
  i = c // 4
  k = c % 4
  l = (32 + 2 * e + 2 * i - h - k) % 7
  m = (a + 11 * h + 22 * l) // 451
  month = (h + l - 7 * m + 114) // 31
  day = ((h + l - 7 * m + 114) % 31) + 1
  
  # Return the date as a string in the format "Month Day, Year"
  return datetime(year, month, day).strftime("%B %d, %Y")

# Print the dates of Hanukkah for the next 10 years
for year in range(datetime.now().year, datetime.now().year + 10):
  print(f"Hanukkah {year}: {hanukkah_dates(year)}")
