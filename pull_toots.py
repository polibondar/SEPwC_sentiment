import json
import requests
import pandas as pd

hashtag = 'climatechange'
URL = f'https://mastodon.social/api/v1/timelines/tag/{hashtag}'
params = {
    'limit': 50
}

since = pd.Timestamp('now', tz='utc') - pd.DateOffset(months=6)
is_end = False

results = []

while True:
    r = requests.get(URL, params=params)
    toots = json.loads(r.text)
    if 'error' in toots:
        print("Error")
        is_end = True
        break

    if len(toots) == 0:
        continue

    for t in toots:
        try:
            timestamp = pd.Timestamp(t["created_at"], tz='utc')
            print(timestamp, since)
            if timestamp <= since:
                is_end = True
                break
            results.append(t)        
        except TypeError:
            pass
    
    if is_end:
        break
    
    if (len(toots) == 1):
        max_is = toots['id']
    else:
        max_id = toots[-1]['id']
    params['max_id'] = max_id
    
df = pd.DataFrame(results)
df.to_csv("test.csv")

